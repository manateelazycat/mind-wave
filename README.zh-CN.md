[English](./README.md) | 简体中文

# mind-wave
mind-wave 是一款利用 ChatGPT API 开发的 Emacs AI 插件，能够深度集成于 Emacs，从而提高 Emacs 在各方面的工作效率。

由于 mind-wave 是基于多线程技术开发的，因此 ChatGPT 在计算时不会卡住 Emacs。

## 安装
1. 注册 [OpenAI](https://platform.openai.com)
2. 获取 [OpenAI API Key](https://platform.openai.com/account/api-keys)， 并将 API Key 保存到 `~/.emacs.d/mind-wave/chatgpt_api_key.txt` 文件中（请勿泄露 API Key 给他人）
3. 安装 Python 依赖：`pip3 install openai`
4. 使用 `git clone` 下载此仓库，并替换下面配置中的 `load-path` 路径
5. 将以下代码添加到您的配置文件 `~/.emacs` 中：
```elisp
(add-to-list 'load-path "<path-to-mind-wave>")

(require 'mind-wave)
```

## 使用
### 对话模式
1. 新建 `test.chat` 文件，将自动进入 `mind-wave-chat-mode`
2. 执行命令 `mind-wave-chat-ask`（按下 Ctrl + j），输入问题，等待 ChatGPT 回答

如果您想更换话题，请新建一个新的 `*.chat` 文件，然后继续向 ChatGPT 提问即可。

### 翻译模式
1. 选中您要翻译的内容（请注意，不要选择太多，ChatGPT 的 API 有大小限制）
2. 执行命令 `mind-wave-translate-to-english`，ChatGPT 获得翻译后会自动替换选中区域的内容。
