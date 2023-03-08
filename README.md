English | [简体中文](./README.zh-CN.md)

# mind-wave
mind-wave is an Emacs AI plugin developed using ChatGPT API, which can be deeply integrated into Emacs to improve its efficiency in various aspects.

As mind-wave is developed based on multithreading technology, ChatGPT will not block Emacs during calculation.

## Installation
1. Register [OpenAI](https://platform.openai.com)
2. Obtain [OpenAI API Key](https://platform.openai.com/account/api-keys), and save the API Key to the `~/.emacs.d/mind-wave/chatgpt_api_key.txt` file (do not disclose the API Key to others).
3. Install Python dependencies: `pip3 install openai`.
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

### Translation mode
1. Select the content you want to translate (note that do not select too much, ChatGPT API has size limitations).
2. Execute the command `mind-wave-translate-to-english`, ChatGPT will automatically replace the selected area with the translated content.
