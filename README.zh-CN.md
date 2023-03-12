[English](./README.md) | 简体中文

# mind-wave
mind-wave 是一款利用 ChatGPT API 开发的 Emacs AI 插件，能够深度集成于 Emacs，从而提高 Emacs 在各方面的工作效率。

由于 mind-wave 是基于多线程技术开发的，因此 ChatGPT 在计算时不会卡住 Emacs。

## 安装
1. 注册 [OpenAI](https://platform.openai.com)
2. 获取 [OpenAI API Key](https://platform.openai.com/account/api-keys)， 并将 API Key 保存到 `~/.emacs.d/mind-wave/chatgpt_api_key.txt` 文件中（或设置环境变量 OPENAI_API_KEY）
3. 安装 Python 依赖：`pip3 install openai epc sexpdata six`
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
3. 执行命令 `mind-wave-chat-continue` (按下 Ctrl + u), 继续回答
4. 执行命令 `mind-wave-chat-generate-title` (按下 Ctrl + i), 根据内容重新生成标题

如果您想更换话题，请新建一个新的 `*.chat` 文件，然后继续向 ChatGPT 提问即可。

### 多行输入
多行输入有两种方式：
1. 执行命令 `mind-wave-chat-ask-with-multiline`（按下 Ctrl + Shift + j），输入多行问题，等待 ChatGPT 回答
2. 执行命令 `mind-wave-chat-ask-insert-line` 插入 `----- User ------` 分隔符， 在 Buffer 继续输入多行内容， 最后执行 `mind-wave-chat-ask-send-buffer`

### 文档模式
选中内容（请注意，不要选择太多，ChatGPT 的 API 有大小限制）

1. 执行命令 `mind-wave-translate-to-english`，ChatGPT 获得翻译后会自动替换选中区域的内容。
1. 执行命令 `mind-wave-proofreading-doc`，ChatGPT 会用润色后的文档自动替换选中区域的内容。

### 代码模式
光标移动到想要重构的函数

1. 执行命令 `mind-wave-refactory-code`, ChatGPT 会自动分屏， 在屏幕右边先后重构后的代码和重构建议
2. 执行命令 `mind-wave-comment-code`, ChatGPT 会自动分屏， 在屏幕右边显示带注释的代码
2. 执行命令 `mind-wave-explain-code`, ChatGPT 会自动分屏， 在屏幕右边显示代码的讲解

### 摘要模式
1. 打开视频网站, 执行命令 `mind-wave-summary-video`, ChatGPT 会自动获取视频字幕， 并分析视频概要 (需要安装 `youtube_transcript_api`)
2. 打开文本网站, 执行命令 `mind-wave-summary-web`, ChatGPT 会自动获取网页中的核心内容， 并分析网页概要 (需要安装 `nodejs-readability-cli`)
