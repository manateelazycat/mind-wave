#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2023 Andy Stewart
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
from epc.server import ThreadingEPCServer
from functools import wraps
from utils import (get_command_result, get_emacs_var, get_emacs_vars, init_epc_client, eval_in_emacs, logger, close_epc_client, message_emacs, string_to_base64, decode_text)

def catch_exception(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except:
            message_emacs(traceback.format_exc())
    return wrapper

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
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # Get API key.
        api_key = self.chat_get_api_key()
        if api_key is not None:
            openai.api_key = api_key

        openai.api_base, openai.api_type, openai.api_version = get_emacs_vars(["mind-wave-api-base", "mind-wave-api-type", "mind-wave-api-version"])

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

        # Build thread queue.
        self.thread_queue = []

        # Build subtitles dict.
        self.subtitle_dict = {}

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                self.event_queue.get(True)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def chat_get_api_key(self):
        mind_wave_chat_api_key_file_path = os.path.expanduser(get_emacs_var("mind-wave-api-key-path"))
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

    @catch_exception
    def send_completion_request(self, messages, model="gpt-3.5-turbo"):
        if openai.api_type == 'azure':
            response = openai.ChatCompletion.create(
                engine = model,
                messages = messages)
        else:
            response = openai.ChatCompletion.create(
                model = model,
                messages = messages)

        result = ''
        for choice in response.choices:
            result += choice.message.content

        return (result, response)

    @catch_exception
    def send_stream_request(self, messages, callback, model="gpt-3.5-turbo"):
        if openai.api_type == 'azure':
            response = openai.ChatCompletion.create(
                engine = model,
                messages = messages,
                temperature=0,
                stream=True)
        else:
            response = openai.ChatCompletion.create(
                model = model,
                messages = messages,
                temperature=0,
                stream=True)

        for chunk in response:
            (result_type, result_content) = self.get_chunk_result(chunk)
            callback(result_type, result_content)

    @threaded
    def chat_ask(self, buffer_file_name, buffer_content, prompt):
        content, model = self.chat_parse_content(buffer_content)

        messages = content
        if prompt:
            messages = content + [{"role": "user", "content": prompt}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-chat-ask--response", buffer_file_name, result_type, result_content)

        self.send_stream_request(messages, callback, model)

    def parse_lines(self, lines):
        messages = []
        role = ''
        content = ''
        model = "gpt-3.5-turbo"

        for line in lines:
            if line.startswith('# : '):
                begin = line.find(":") + 1
                model_content = line[begin:].strip()
                if model_content != "":
                    model = model_content
            elif line.startswith('# > ') or line.startswith('## > '):
                if role:
                    messages.append({"role": role, "content": content})
                begin = line.find('>') + 1
                end = line.find(':')
                role = line[begin:end].strip().lower()
                content = line[end + 2:]
            else:
                content += line

        if role:
            messages.append({"role": role, "content": content})

        return messages, model

    def add_default_system_message(self, messages):
        default_system = {"role": "system", "content": "You are a helpful assistant."}
        if len(messages) == 0:
            messages.append(default_system)
        elif messages[0]["role"] != "system":
            messages = [default_system] + messages

        return messages

    def chat_parse_content(self, buffer_content):
        text = decode_text(buffer_content)
        lines = text.splitlines(True)
        messages, model = self.parse_lines(lines)
        messages = self.add_default_system_message(messages)

        return messages, model

    @threaded
    def parse_title(self, buffer_file_name, text_content, role, prompt):
        text = decode_text(text_content)
        (result, _) = self.send_completion_request(
            [{"role": "system", "content": role},
             {"role": "user", "content": f"{prompt}：\n{text}"}],
            get_emacs_var("mind-wave-parse-title-model")
        )

        eval_in_emacs("mind-wave-parse-title--response", buffer_file_name, result)

    @threaded
    def async_text(self, buffer_file_name, text_content, text_start, text_end, role, prompt, notify_start, notify_end):
        text = decode_text(text_content)

        if text_content == "":
            content = f"{prompt}"
        else:
            content = f"{prompt}：\n{text}"

        messages = [{"role": "system", "content": role},
                    {"role": "user", "content": content}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-async-text--response",
                          buffer_file_name,
                          result_type,
                          result_content,
                          text_start,
                          text_end,
                          notify_start,
                          notify_end)

        self.send_stream_request(messages, callback, get_emacs_var("mind-wave-async-text-model"))

    @threaded
    def action_code(self, buffer_name, major_mode, code, role, prompt, callback_template, notify_start, notify_end):
        text = decode_text(code)

        messages = [{"role": "system", "content": role},
                    {"role": "user", "content": f"{prompt}： \n{text}"}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-split-window--response",
                          buffer_name,
                          f"mind-wave-{callback_template}-{buffer_name}",
                          major_mode,
                          result_type,
                          result_content,
                          notify_start,
                          notify_end)

        self.send_stream_request(messages, callback, get_emacs_var("mind-wave-action-code-model"))

    @threaded
    def explain_word(self, buffer_name, major_mode, sentence, word, callback_template, notify_start, notify_end):
        sentence_text = decode_text(sentence)

        messages = [{"role": "system", "content": "你是一位英语词义语法专家， 你在教我英语， 我给你一句英文句子， 和这个句子中的一个单词， 请用中文帮我解释一下，这个单词在句子中的意思和句子本身的意思. 并举几个相同意思的英文例句，并用中文解释例句。如果你明白了请说同意，然后我们开始。"},
                    {"role": "assistant", "content": "好的，我明白了，请给我这个句子和单词。"},
                    {"role": "user", "content": f"句子是:{sentence_text}\n 单词是：{word}"}]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-split-window--response",
                          buffer_name,
                          f"mind-wave-{callback_template}-{buffer_name}",
                          major_mode,
                          result_type,
                          result_content,
                          notify_start,
                          notify_end)

        self.send_stream_request(messages, callback, get_emacs_var("mind-wave-explain-word-model"))

    def get_video_subtitle(self, video_id):
        from youtube_transcript_api import YouTubeTranscriptApi

        message_emacs(f"Get subtitles for video id: {video_id}...")

        if video_id in self.subtitle_dict:
            text = self.subtitle_dict[video_id]
        else:
            transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=["zh-Hans", "en"])
            text = " ".join(line["text"] for line in transcript)

            self.subtitle_dict[video_id] = text

        return text

    @threaded
    def summary_video(self, buffer_name, video_id, role, prompt, notify_start, notify_end):
        import importlib
        if importlib.find_loader("youtube_transcript_api") is None:
            message_emacs("Please use pip3 install package 'youtube_transcript_api' first.")
            return

        text = self.get_video_subtitle(video_id)

        self.summary_text(buffer_name, role, prompt, notify_start, notify_end, text, video_id)

    @threaded
    def git_commit(self, dir, role, prompt):
        diff_string = get_command_result(f"cd {dir} ; git diff")

        (result, _) = self.send_completion_request(
            [{"role": "system", "content": role},
             {"role": "user", "content": f"{prompt}：\n{diff_string}"}],
            get_emacs_var("mind-wave-git-commit-model"))

        eval_in_emacs("mind-wave-generate-commit-name--response", result)

    @threaded
    def summary_web(self, buffer_name, url, role, prompt, notify_start, notify_end):
        import shutil

        if not shutil.which("readable"):
            message_emacs("Please install 'readable' cli tool first")
            return

        text = get_command_result(f"readable {url} -p 'text-content'")

        self.summary_text(buffer_name, role, prompt, notify_start, notify_end, text, url)

    def summary_text(self, buffer_name, role, prompt, notify_start, notify_end, text, template):
        part_size = 3000
        message_parts = [text[i:i + part_size] for i in range(0, len(text), part_size)]

        def callback(result_type, result_content):
            eval_in_emacs("mind-wave-split-window--response",
                          buffer_name,
                          f"mind-wave-summary-{template}",
                          "text-mode",
                          result_type,
                          result_content,
                          notify_start,
                          notify_end)

        self.send_stream_part_request(role, prompt, message_parts, callback)

    def send_stream_part_request(self, role, prompt, message_parts, callback):
        if not message_parts:
            return

        text = message_parts[0]
        messages = [{"role": "system", "content": role},
                    {"role": "user", "content": f"{prompt}： \n{text}"}]

        try:
            if openai.api_type == 'azure':
                response = openai.ChatCompletion.create(
                    engine = "gpt-3.5-turbo",
                    messages = messages,
                    temperature=0,
                    stream=True)
            else:
                response = openai.ChatCompletion.create(
                    model = "gpt-3.5-turbo",
                    messages = messages,
                    temperature=0,
                    stream=True)

            for chunk in response:
                (result_type, result_content) = self.get_chunk_result(chunk)
                callback(result_type, result_content)

                if result_type == "end":
                    self.send_stream_part_request(role, prompt, message_parts[1:], callback)
        except:
            message_emacs(traceback.format_exc())

    def get_chunk_result(self, chunk):
        delta = chunk.choices[0].delta
        if not delta:
            return ("end", "")
        elif "role" in delta:
            return ("start", "")
        elif "content" in delta:
            return ("content", string_to_base64(delta["content"]))

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
