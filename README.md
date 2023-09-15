English | [简体中文](./README.zh-CN.md)

# mind-wave
mind-wave is an Emacs AI plugin developed using ChatGPT API, which can be deeply integrated into Emacs to improve its efficiency in various aspects.

As mind-wave is developed based on multithreading technology, ChatGPT will not block Emacs during calculation.

## Installation
1. Register [OpenAI](https://platform.openai.com)
2. Obtain [OpenAI API Key](https://platform.openai.com/account/api-keys), and save the API Key to the `~/.emacs.d/mind-wave/chatgpt_api_key.txt` file (Or set the environment variable OPENAI_API_KEY.).
3. Install Python dependencies: `pip3 install -U openai epc sexpdata six`.
4. Install [markdown-mode](https://github.com/jrblevin/markdown-mode)
5. Use `git clone` to download this repository and replace the `load-path` path in the configuration below.
6. Add the following code to your configuration file `~/.emacs`:
```elisp
(add-to-list 'load-path "<path-to-mind-wave>")

(require 'mind-wave)
```

## Usage
### Conversation Mode
1. Create a `test.chat` file to automatically enter `mind-wave-chat-mode`.
2. Execute the command `mind-wave-chat-ask` (press Ctrl + j), input your question, and wait for ChatGPT to respond.
3. Execute the command `mind-wave-chat-continue` (press Ctrl + u) to continue the conversation.
4. Execute the command `mind-wave-chat-generate-title` (press Ctrl + i) to generate a new title based on the content.

If you want to change the topic, create a new `*.chat` file and continue asking ChatGPT questions.

### Multi-line Input
There are two ways of inputting multi-line content:

1. Execute the command `mind-wave-chat-ask-with-multiline` (press Ctrl + Shift + j), input multiple questions, and wait for ChatGPT to respond.
2. Execute the command `mind-wave-chat-ask-insert-line` to insert the `----- User ------` separator, continue inputting multiple lines in the buffer, and finally execute `mind-wave-chat-ask-send-buffer`.

### Document mode
Selected Content:

1. Execute the command `mind-wave-translate-to-english`, ChatGPT will automatically replace the selected area with the translated content.
2. Execute the command `mind-wave-proofreading-doc`, ChatGPT will automatically replace the selected area with the polished document. 
3. Execute the command `mind-wave-explain-word`, ChatGPT will automatically explain the meaning of the words in the current sentence and provide similar example sentences.
4. Execute the command `mind-wave-adjust-text`. ChatGPT will adjust the text or code according to your instructions.
5. Execute the command `mind-wave-check-typos`, ChatGPT will fix typos.

### Code Mode
Move the cursor to the function you want to refactor.

1. Execute the command `mind-wave-generate-code`, ChatGPT will output the code in the current buffer according to the prompt.
2. Execute the command `mind-wave-refactory-code`, ChatGPT will automatically split the screen and display the refactored code and suggestions on the right side of the screen.
3. Execute the command `mind-wave-comment-code`, ChatGPT will automatically split the screen and display the commented code on the right side of the screen.
4. Execute the command `mind-wave-explain-code`, ChatGPT will automatically split the screen and display the explanation of the code on the right side of the screen.
5. Execute the command `mind-wave-explain-point`, ChatGPT will automatically split the screen and display the explanation of the API at the cursor position on the right side of the screen.
6. Execute the command `mind-wave-generate-commit-name`, ChatGPT will automatically analyze the current diff content and generate a patch name.
7. Execute the command `mind-wave-refactory-code-with-input`, ChatGPT will automatically split the screen and display the refactored code and suggestions on the right side of the screen according to your prompt.

Code-related commands will automatically adjust the window layout, and you can use `mind-wave-restore-window-configuration` to restore the previous window layout at any time.

### Summary Mode
1. Open a video website and execute the command `mind-wave-summary-video`. ChatGPT will automatically retrieve the video subtitles and analyze the summary of the video (YouTube Transcript API installation is required).
2. Open a text website and execute the command `mind-wave-summary-web`. ChatGPT will automatically retrieve the core content of the webpage and analyze the summary of the webpage (nodejs-readability-cli installation is required).

## Model Selection
mind-wave defaults to using the `gpt-3.5-turbo` model. If you have already received an internal testing invitation from OpenAI, you can set the following models to `gpt-4` or `gpt-4-32k`.

* mind-wave-chat-model: Chat file conversation model, default is `gpt-3.5-turbo`, you can use the `mind-wave-chat-model` command to switch models
* mind-wave-async-text-model: Asynchronous text model, default is `gpt-3.5-turbo`
* mind-wave-action-code-model: Asynchronous code model, default is `gpt-3.5-turbo`
* mind-wave-explain-word-model: English word explanation model, default is `gpt-3.5-turbo`
* mind-wave-parse-title-model: Analyze chat file title model, default is `gpt-3.5-turbo`, it is not recommended to switch to `gpt-4`, as `gpt-4` is slower
* mind-wave-git-commit-model: Generate git diff model, default is `gpt-3.5-turbo`, it is not recommended to switch to `gpt-4`, as `gpt-4` is slower

## Feedback Issues

Please use the command `emacs -q` and only add the mind-wave configuration for comparison testing. If `emacs -q` can work properly, please check your personal configuration file.

If the problem still exists in the `emacs -q` environment, please [submit an issue](https://github.com/manateelazycat/mind-wave/issues/new) and attach the contents of the `*mind-wave*` window to help us troubleshoot. There are many clues there that can help us investigate the problem.

* If you encounter a crash, please collect the crash information using the following steps:
  1. Install gdb and enable the option `(setq mind-wave-enable-debug t)`
  2. Use the command `mind-wave-restart-process` to restart the MIND-WAVE process.
  3. Send the contents of `*mind-wave*` window when the next crash occurs.
  
## Contributor

<a href = "https://github.com/manateelazycat/mind-wave/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/mind-wave"/>
</a>
