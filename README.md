English | [简体中文](./README.zh-CN.md)

# mind-wave
mind-wave is an Emacs AI plugin developed using ChatGPT API, which can be deeply integrated into Emacs to improve its efficiency in various aspects.

As mind-wave is developed based on multithreading technology, ChatGPT will not block Emacs during calculation.

## Installation
1. Register [OpenAI](https://platform.openai.com)
2. Obtain [OpenAI API Key](https://platform.openai.com/account/api-keys), and save the API Key to the `~/.emacs.d/mind-wave/chatgpt_api_key.txt` file (do not disclose the API Key to others).
3. Install Python dependencies: `pip3 install openai epc sexpdata six`.
4. Use `git clone` to download this repository and replace the `load-path` path in the configuration below.
5. Add the following code to your configuration file `~/.emacs`:
```elisp
(add-to-list 'load-path "<path-to-mind-wave>")

(require 'mind-wave)
```

## Usage
### Conversation mode
1. Create a `test.chat` file, which will automatically enter `mind-wave-chat-mode`.
2. Execute the command `mind-wave-chat-ask` (press Ctrl + j), enter the question, and wait for ChatGPT to answer.

If you want to change the topic, create a new `*.chat` file and continue asking ChatGPT.

### 文档模式
选中内容（请注意，不要选择太多，ChatGPT 的 API 有大小限制）

1. 执行命令 `mind-wave-translate-to-english`，ChatGPT 获得翻译后会自动替换选中区域的内容。
1. 执行命令 `mind-wave-proofreading-doc`，ChatGPT 会用润色后的文档自动替换选中区域的内容。

### Code Refactoring Mode
Move the cursor to the desired function for refactoring.

1. Execute the command `mind-wave-refactory-code`, ChatGPT will automatically split the screen to display the refactored code and suggestions for improvement on the right.
2. Execute the command `mind-wave-comment-code`, ChatGPT will automatically split the screen to display code with comments on the right.
3. Execute the command `mind-wave-explain-code`, ChatGPT will automatically split the screen to display an explanation for the code on the right.
