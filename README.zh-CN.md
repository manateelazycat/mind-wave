[English](./README.md) | 简体中文

# mind-wave
mind-wave 是一款利用 ChatGPT API 开发的 Emacs AI 插件， 能够深度集成于 Emacs， 从而提高 Emacs 在各方面的工作效率。

由于 mind-wave 是基于多线程技术开发的， 因此 ChatGPT 在计算时不会卡住 Emacs。

## 安装
1. 注册 [OpenAI](https://platform.openai.com)
2. 获取 [OpenAI API Key](https://platform.openai.com/account/api-keys)， 并将 API Key 保存到 `~/.emacs.d/mind-wave/chatgpt_api_key.txt` 文件中（或设置环境变量 OPENAI_API_KEY）
3. 安装 Python 依赖： `pip3 install -U openai epc sexpdata six`
4. 安装 [markdown-mode](https://github.com/jrblevin/markdown-mode)
5. 使用 `git clone` 下载此仓库， 并替换下面配置中的 `load-path` 路径
6. 将以下代码添加到您的配置文件 `~/.emacs` 中：
```elisp
(add-to-list 'load-path "<path-to-mind-wave>")

(require 'mind-wave)
```

## 使用
### 对话模式
1. 新建 `test.chat` 文件， 将自动进入 `mind-wave-chat-mode`
2. 执行命令 `mind-wave-chat-ask`（按下 Ctrl + j）， 输入问题， 等待 ChatGPT 回答
3. 执行命令 `mind-wave-chat-continue` (按下 Ctrl + u), 继续回答
4. 执行命令 `mind-wave-chat-generate-title` (按下 Ctrl + i), 根据内容重新生成标题

如果您想更换话题， 请新建一个新的 `*.chat` 文件， 然后继续向 ChatGPT 提问即可。

### 多行输入
多行输入有两种方式：
1. 执行命令 `mind-wave-chat-ask-with-multiline`（按下 Ctrl + Shift + j）， 输入多行问题， 等待 ChatGPT 回答
2. 执行命令 `mind-wave-chat-ask-insert-line` 插入 `----- User ------` 分隔符， 在 Buffer 继续输入多行内容， 最后执行 `mind-wave-chat-ask-send-buffer`

### 文档模式
选中内容（请注意， 不要选择太多， ChatGPT 的 API 有大小限制）

1. 执行命令 `mind-wave-translate-to-english`， ChatGPT 获得翻译后会自动替换选中区域的内容。
2. 执行命令 `mind-wave-proofreading-doc`， ChatGPT 会用润色后的文档自动替换选中区域的内容。
3. 执行命令 `mind-wave-explain-word`, ChatGPT 会自动解释当前句子中单词的意思， 并给出类似例句。
4. 执行命令 `mind-wave-adjust-text`, ChatGPT 根据你的指令来调整文字或代码
5. 执行命令 `mind-wave-check-typos`, ChatGPT 修复错别字

### 代码模式
光标移动到想要重构的函数

1. 执行命令 `mind-wave-generate-code`, ChatGPT 会根据提示， 在当前 buffer 输出代码
2. 执行命令 `mind-wave-refactory-code`, ChatGPT 会自动分屏， 在屏幕右边先后重构后的代码和重构建议
3. 执行命令 `mind-wave-comment-code`, ChatGPT 会自动分屏， 在屏幕右边显示带注释的代码
4. 执行命令 `mind-wave-explain-code`, ChatGPT 会自动分屏， 在屏幕右边显示代码的讲解
5. 执行命令 `mind-wave-explain-point`, ChatGPT 会自动分屏， 在屏幕右边显示光标处 API 的讲解
6. 执行命令 `mind-wave-generate-commit-name`, ChatGPT 会自动分析当前的 diff 内容， 并生成一个补丁名称
7. 执行命令 `mind-wave-refactory-code-with-input`, ChatGPT 会自动分屏， 根据你的提示， 在屏幕右边先后重构后的代码和重构建议

代码相关命令会自动调整窗口布局， 你随时可以用 `mind-wave-restore-window-configuration` 恢复之前的窗口布局。

### 摘要模式
1. 打开视频网站, 执行命令 `mind-wave-summary-video`, ChatGPT 会自动获取视频字幕， 并分析视频概要 (需要安装 `youtube_transcript_api`)
2. 打开文本网站, 执行命令 `mind-wave-summary-web`, ChatGPT 会自动获取网页中的核心内容， 并分析网页概要 (需要安装 `nodejs-readability-cli`)

## 模型选择
mind-wave 默认使用 `gpt-3.5-turbo` 模型， 如果你已经获得了 OpenAI 的内部测试邀请， 你可以设置下面模型为 `gpt-4` or `gpt-4-32k`.

* mind-wave-chat-model: Chat 文件聊天模型， 默认是 `gpt-3.5-turbo`, 你可以使用 `mind-wave-chat-model` 命令来切换模型
* mind-wave-async-text-model: 异步文字模型， 默认是 `gpt-3.5-turbo`
* mind-wave-action-code-model: 异步代码模型， 默认是 `gpt-3.5-turbo`
* mind-wave-explain-word-model: 英文单词解释模型， 默认是 `gpt-3.5-turbo`
* mind-wave-parse-title-model: 分析 chat 文件标题模型， 默认是 `gpt-3.5-turbo`, 建议不用切换成 `gpt-4`, `gpt-4`速度较慢
* mind-wave-git-commit-model: 生成 git diff 模型， 默认是 `gpt-3.5-turbo`, 建议不用切换成 `gpt-4`, `gpt-4`速度较慢

## 反馈问题
请用命令 `emacs -q` 并只添加 mind-wave 配置做一个对比测试， 如果 `emacs -q` 可以正常工作， 请检查你个人的配置文件。

如果`emacs -q`环境下问题依旧， 请到[这里](https://github.com/manateelazycat/mind-wave/issues/new) 反馈, 并附带 `*mind-wave*` 窗口的内容给我们提交 issue， 那里面有很多线索可以帮助我们排查问题。。

* 如果你遇到崩溃的问题, 请用下面的方式来收集崩溃信息:
  1. 先安装 gdb 并打开选项 `(setq mind-wave-enable-debug t)`
  2. 使用命令 `mind-wave-restart-process` 重启 MIND-WAVE 进程
  3. 在下次崩溃时发送 `*mind-wave*` 的内容
  
## 贡献者

<a href = "https://github.com/manateelazycat/mind-wave/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/mind-wave"/>
</a>
