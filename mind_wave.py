#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import openai
import queue
import threading
import traceback
import os
import sys
import base64
from epc.server import ThreadingEPCServer
from functools import wraps
from utils import (get_command_result, init_epc_client, eval_in_emacs, logger, close_epc_client, get_emacs_func_result, message_emacs, string_to_base64)

def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

class MindWave:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(mind-wave_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        # Get API key.
        api_key = self.chat_get_api_key()
        if api_key is not None:
            openai.api_key = api_key

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # Pass epc port and webengine codec information to Emacs when first start mind-wave.
        eval_in_emacs('mind-wave--first-start', self.server.server_address[1])

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # All LSP server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        # Build thread queue.
        self.thread_queue = []

        # Build subtitles dict.
        self.subtitle_dict = {}

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)

                print(message)

                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                message = self.message_queue.get(True)

                print(message)

                self.message_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def chat_get_api_key(self):
        user_emacs_dir = get_emacs_func_result("get-user-emacs-directory")
        mind_wave_dir = os.path.join(user_emacs_dir, "mind-wave")
        mind_wave_chat_api_key_file_path = os.path.join(mind_wave_dir, "chatgpt_api_key.txt")
        key = None
        if os.path.exists(mind_wave_chat_api_key_file_path):
            with open(mind_wave_chat_api_key_file_path, "r") as f:
                api_key = f.read().strip()
                if api_key != "":
                    key = api_key
        else:
            key = os.environ.get("OPENAI_API_KEY")

        if key is None:
                message_emacs(f"ChatGPT API key not found, please copy it from https://platform.openai.com/account/api-keys, and fill API key in file: {mind_wave_chat_api_key_file_path}. Or set the enviroment OPENAI_API_KEY")

        return key

    def send_completion_request(self, messages):
        response = openai.ChatCompletion.create(
            model = "gpt-3.5-turbo",
            messages = messages)

        result = ''
        for choice in response.choices:
            result += choice.message.content

        return (result, response)

    def send_stream_request(self, messages, callback):
        response = openai.ChatCompletion.create(
            model = "gpt-3.5-turbo",
            messages = messages,
            temperature=0,
            stream=True)

        for chunk in response:
            delta = chunk.choices[0].delta
            if len(delta) == 0:
                result_type = "end"
                result_content = ""
            elif "role" in delta:
                result_type = "start"
                result_content = ""
            elif "content" in delta:
                result_type = "content"
                result_content = string_to_base64(delta["content"])

            callback(result_type, result_content)

    @threaded
    def chat_ask(self, buffer_file_name, buffer_content, prompt):
        content = self.chat_parse_content(buffer_content)

        messages = content + [{"role": "user", "content": prompt}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-chat-ask--response", buffer_file_name, result_type, result_content)

        self.send_stream_request(messages, callback)

    def chat_parse_content(self, buffer_content):
        text = base64.b64decode(buffer_content).decode("utf-8")

        messages = []

        lines = text.split('\n')  # split the text into lines
        role = ''  # initialize the role
        content = ''  # initialize the content

        for line in lines:
            if line.startswith('------ '):
                if role:  # output the content of the previous role
                    messages.append({ "role": role, "content": content })
                role = line.strip('------ ').strip().lower()  # get the current role
                content = ''  # reset the content for the current role
            else:
                content += line  # append the line to the content for the current role

        # output the content of the last role
        if role:
            messages.append({ "role": role, "content": content })

        default_system = {"role": "system", "content": "You are a helpful assistant."}
        if len(messages) == 0:
            messages.append(default_system)
        elif messages[0]["role"] != "system":
            messages = [default_system] + messages

        return messages

    @threaded
    def parse_title(self, buffer_file_name, text_content):
        text = base64.b64decode(text_content).decode("utf-8")
        (result, _) = self.send_completion_request(
            [{"role": "system", "content": "你是一个语言学家"},
             {"role": "user", "content": f"给下面这段话起一个标题：\n{text}"}])

        eval_in_emacs("mind-wave-parse-title--response", buffer_file_name, result)

    @threaded
    def adjust_text(self, buffer_file_name, text_content, text_start, text_end, role, prompt, notify_end):
        text = base64.b64decode(text_content).decode("utf-8")
        (result, _) = self.send_completion_request(
            [{"role": "system", "content": role},
             {"role": "user", "content": f"{prompt}：\n{text}"}])

        eval_in_emacs("mind-wave-adjust-text--response", buffer_file_name, result, text_start, text_end, notify_end)

    def action_code(self, buffer_name, buffer_file_name, major_mode, code, prompt, callback_template, notify_start, notify_end):
        text = base64.b64decode(code).decode("utf-8")

        messages = [{"role": "system", "content": "你是一个计算机教授"},
                    {"role": "user", "content": f"{prompt}： \n{text}"}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-split-window--response",
                          buffer_file_name,
                          f"mind-wave-{callback_template}-{buffer_name}",
                          major_mode,
                          result_type,
                          result_content,
                          notify_start,
                          notify_end)

        self.send_stream_request(messages, callback)

    @threaded
    def summary_video(self, buffer_name, video_id, prompt, notify_start, notify_end):
        try:
            from youtube_transcript_api import YouTubeTranscriptApi
        except ImportError:
            message_emacs("Please use pip3 install package 'youtube_transcript_api' first.")
            return

        message_emacs(f"Parse subtitles for video id: {video_id}...")

        # Use the OpenAI Edit endpoint to add punctuation, so that the Completion endpoint can summarize it properly
        if video_id in self.subtitle_dict:
            text = self.subtitle_dict[video_id]
        else:
            message_emacs(f"Get subtitle for video id: {video_id}")

            transcript = YouTubeTranscriptApi.get_transcript(video_id)

            # Convert the transcript list object to plaintext so that we can use it with OpenAI
            transcript_text = " ".join(line["text"] for line in transcript)

            message_emacs("Add punctuation to subtitle...")

            response = openai.Edit.create(
                model="text-davinci-edit-001",
                input=transcript_text,
                instruction="Add punctuation to the text.",
            )
            text = response["choices"][0]["text"]

            self.subtitle_dict[video_id] = text

            message_emacs("Finish punctuation to subtitle.")

        self.summary_text(buffer_name, prompt, notify_start, notify_end, text, video_id)

    @threaded
    def summary_web(self, buffer_name, url, prompt, notify_start, notify_end):
        import shutil

        if not shutil.which("readable"):
            message_emacs("Please install 'readable' cli tool first")
            return

        text = get_command_result(f"readable {url} -p 'text-content'")

        self.summary_text(buffer_name, prompt, notify_start, notify_end, text, url)

    def summary_text(self, buffer_name, prompt, notify_start, notify_end, text, template):
        messages = [{"role": "system", "content": "你是语文老师"},
                    {"role": "user", "content": f"{prompt}： \n{text}"}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-summary--response",
                          buffer_name,
                          f"mind-wave-summary-{template}",
                          "text-mode",
                          result_type,
                          result_content,
                          notify_start,
                          notify_end)

        self.send_stream_request(messages, callback)

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("MindWave(sys.argv[1:])")
    else:
        MindWave(sys.argv[1:])
