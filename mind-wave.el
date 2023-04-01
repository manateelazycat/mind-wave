;;; mind-wave.el --- Mind Wave  -*- lexical-binding: t -*-

;; Filename: mind-wave.el
;; Description: Mind Wave
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-03-09 14:10:12
;; Version: 0.1
;; Last-Updated: 2023-03-20 15:23:53 +0800
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/mind-wave
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Mind-Wave
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET mind-wave RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mind-wave-epc)
(require 'markdown-mode)

(when (version< emacs-version "30")
  (defun file-name-concat (&rest parts)
    (cl-reduce (lambda (a b) (expand-file-name b a)) parts)))

(defgroup mind-wave nil
  "Mind-Wave group."
  :group 'applications)

(defcustom mind-wave-auto-change-title t
  "Whether to automatically change the title according to the content."
  :type 'boolean
  :group 'mind-wave)

(defcustom mind-wave-api-key-path (expand-file-name (file-name-concat user-emacs-directory "mind-wave" "chatgpt_api_key.txt"))
  "The path to store OpenAI API Key."
  :type 'boolean
  :group 'mind-wave)

(defcustom mind-wave-api-base "https://api.openai.com/v1"
  "OpenAI API base url"
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-api-type "open_ai"
  "OpenAI API type"
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-api-version ""
  "OpenAI API version"
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-translate-role "You are an English Translator."
  "Role for translate."
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-code-role "You are a computer professor."
  "Role for code."
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-summary-role "You are a language teacher."
  "Role for summary."
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-title-role "You are a linguist."
  "Role for title."
  :type 'string
  :group 'mind-wave)

(defcustom mind-wave-proofreading-role "You are a high level writer."
  "Role for proofreading."
  :type 'string
  :group 'mind-wave)

(defvar mind-wave-lang (or (ignore-errors (car (split-string (getenv "LANG") "\\.")))
                           (car (split-string current-language-environment "-"))))

(defvar mind-wave-server nil
  "The Mind-Wave Server.")

(defvar mind-wave-python-file (expand-file-name "mind_wave.py" (if load-file-name
                                                                   (file-name-directory load-file-name)
                                                                 default-directory)))

(defvar mind-wave-server-port nil)

(cl-defmacro mind-wave--with-file-buffer (filename &rest body)
  "Evaluate BODY in buffer with FILEPATH."
  (declare (indent 1))
  `(cl-dolist (buffer (buffer-list))
     (when-let* ((file-name (buffer-file-name buffer))
                 (match-buffer (or (string-equal file-name ,filename)
                                   (string-equal (file-truename file-name) ,filename))))
       (with-current-buffer buffer
         ,@body)
       (cl-return))))

(defun mind-wave-output-lang ()
  (pcase mind-wave-lang
    ("zh_CN" "Chinese")
    ("Chinese" "Chinese")
    (_ "English")))

(defun mind-wave--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p mind-wave-server)
    (setq mind-wave-server
          (mind-wave-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (mind-wave-epc-define-method mngr 'eval-in-emacs 'mind-wave--eval-in-emacs-func)
               (mind-wave-epc-define-method mngr 'get-emacs-var 'mind-wave--get-emacs-var-func)
               (mind-wave-epc-define-method mngr 'get-emacs-vars 'mind-wave--get-emacs-vars-func)
               (mind-wave-epc-define-method mngr 'get-user-emacs-directory 'mind-wave--user-emacs-directory)
               ))))
    (if mind-wave-server
        (setq mind-wave-server-port (process-contact mind-wave-server :service))
      (error "[Mind-Wave] mind-wave-server failed to start")))
  mind-wave-server)

(defun mind-wave--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun mind-wave--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun mind-wave--get-emacs-vars-func (&rest vars)
  (mapcar #'mind-wave--get-emacs-var-func vars))

(defvar mind-wave-epc-process nil)

(defvar mind-wave-internal-process nil)
(defvar mind-wave-internal-process-prog nil)
(defvar mind-wave-internal-process-args nil)

(defcustom mind-wave-name "*mind-wave*"
  "Name of Mind-Wave buffer."
  :type 'string)

(defcustom mind-wave-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run mind_wave.py."
  :type 'string)

(defcustom mind-wave-enable-debug nil
  "If you got segfault error, please turn this option.
Then Mind-Wave will start by gdb, please send new issue with `*mind-wave*' buffer content when next crash."
  :type 'boolean)

(defcustom mind-wave-enable-profile nil
  "Enable this option to output performance data to ~/mind-wave.prof."
  :type 'boolean)

(defun mind-wave--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defvar mind-wave-is-starting nil)
(defvar mind-wave-first-call-method nil)
(defvar mind-wave-first-call-args nil)

(defun mind-wave-decode-base64 (base64-string)
  (decode-coding-string (base64-decode-string base64-string) 'utf-8))

(defun mind-wave-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (mind-wave-epc-live-p mind-wave-epc-process)
      (mind-wave-deferred-chain
        (mind-wave-epc-call-deferred mind-wave-epc-process (read method) args))
    (setq mind-wave-first-call-method method)
    (setq mind-wave-first-call-args args)
    (mind-wave-start-process)))

(defun mind-wave-restart-process ()
  "Stop and restart Mind-Wave process."
  (interactive)
  (setq mind-wave-is-starting nil)

  (mind-wave-kill-process)
  (mind-wave-start-process)
  (message "[Mind-Wave] Process restarted."))

(defun mind-wave-start-process ()
  "Start Mind-Wave process if it isn't started."
  (setq mind-wave-is-starting t)
  (unless (mind-wave-epc-live-p mind-wave-epc-process)
    ;; start epc server and set `mind-wave-server-port'
    (mind-wave--start-epc-server)
    (let* ((mind-wave-args (append
                            (list mind-wave-python-file)
                            (list (number-to-string mind-wave-server-port))
                            (when mind-wave-enable-profile
                              (list "profile"))
                            )))

      ;; Set process arguments.
      (if mind-wave-enable-debug
          (progn
            (setq mind-wave-internal-process-prog "gdb")
            (setq mind-wave-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" mind-wave-python-command) mind-wave-args)))
        (setq mind-wave-internal-process-prog mind-wave-python-command)
        (setq mind-wave-internal-process-args mind-wave-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq mind-wave-internal-process
              (apply 'start-process
                     mind-wave-name mind-wave-name
                     mind-wave-internal-process-prog mind-wave-internal-process-args)))
      (set-process-query-on-exit-flag mind-wave-internal-process nil))))

(defvar mind-wave-stop-process-hook nil)

(defun mind-wave-kill-process ()
  "Stop Mind-Wave process and kill all Mind-Wave buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'mind-wave-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (mind-wave--kill-python-process))

(add-hook 'kill-emacs-hook #'mind-wave-kill-process)

(defun mind-wave--kill-python-process ()
  "Kill Mind-Wave background python process."
  (when (mind-wave-epc-live-p mind-wave-epc-process)
    ;; Cleanup before exit Mind-Wave server process.
    (mind-wave-call-async "cleanup")
    ;; Delete Mind-Wave server process.
    (mind-wave-epc-stop-epc mind-wave-epc-process)
    ;; Kill *mind-wave* buffer.
    (when (get-buffer mind-wave-name)
      (kill-buffer mind-wave-name))
    (setq mind-wave-epc-process nil)
    (message "[Mind-Wave] Process terminated.")))

(defun mind-wave--first-start (mind-wave-epc-port)
  "Call `mind-wave--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq mind-wave-epc-process (make-mind-wave-epc-manager
                               :server-process mind-wave-internal-process
                               :commands (cons mind-wave-internal-process-prog mind-wave-internal-process-args)
                               :title (mapconcat 'identity (cons mind-wave-internal-process-prog mind-wave-internal-process-args) " ")
                               :port mind-wave-epc-port
                               :connection (mind-wave-epc-connect "localhost" mind-wave-epc-port)
                               ))
  (mind-wave-epc-init-epc-layer mind-wave-epc-process)
  (setq mind-wave-is-starting nil)

  (when (and mind-wave-first-call-method
             mind-wave-first-call-args)
    (mind-wave-deferred-chain
      (mind-wave-epc-call-deferred mind-wave-epc-process
                                   (read mind-wave-first-call-method)
                                   mind-wave-first-call-args)
      (setq mind-wave-first-call-method nil)
      (setq mind-wave-first-call-args nil)
      )))

(defun mind-wave--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

(defvar mind-wave-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")   #'mind-wave-chat-ask)
    (define-key map (kbd "C-S-j") #'mind-wave-chat-ask-with-multiline)
    (define-key map (kbd "C-,")   #'mind-wave-chat-ask-insert-line)
    (define-key map (kbd "C-S-m") #'mind-wave-chat-ask-send-buffer)
    (define-key map (kbd "C-u")   #'mind-wave-chat-continue)
    (define-key map (kbd "C-i")   #'mind-wave-chat-generate-title)
    map)
  "Mind Wave Chat Keymap")

(define-derived-mode mind-wave-chat-mode gfm-mode "Mind-Wave"
  (setq-local markdown-hide-markup markdown-hide-markup-in-view-modes)
  (setq-local markdown-fontify-code-blocks-natively t)
  (add-to-invisibility-spec 'markdown-markup))

(defun mind-wave-chat--index-function ()
  "Create and return imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details.
Currently just grab the lines below '------ User ------\\n'"
  (let (index)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward "------ User ------\n" (point-max) t)
        (setq index
              (append index
                      (list
                       (cons
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))
                        (point))))))
      index)))

(add-hook 'mind-wave-chat-mode-hook
          ;; For imenu support
          (lambda ()
            (setq imenu-create-index-function
                  #'mind-wave-chat--index-function)))

(add-to-list 'auto-mode-alist '("\\.chat$" . mind-wave-chat-mode))

(defun mind-wave-get-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun mind-wave-chat-ask-with-message (prompt)
  (save-excursion
    (goto-char (point-max))
    (unless (equal (point) (point-min))
      (insert "\n"))
    (insert "------ User ------\n")
    (insert (format "%s\n\n" prompt)))

  (message "Wait ChatGPT...")
  (mind-wave-call-async "chat_ask"
                        (buffer-file-name)
                        (mind-wave--encode-string (mind-wave-get-buffer-string))
                        prompt
                        ))

(defun mind-wave-chat-ask ()
  (interactive)
  (let ((prompt (read-string "Ask ChatGPT: ")))
    (if (string-empty-p (string-trim prompt))
        (message "Please don't send empty question.")
      (mind-wave-chat-ask-with-message prompt)
      )))

(defun mind-wave-chat-ask-insert-line-with-role (role)
  (insert (concat
           (if (equal (point) (point-min))
               ""
             "\n")
           (format "------ %s ------\n" role)))
  (message "[Mind-Wave] Continue input, do `mind-wave-chat-ask-send-buffer` when finish input.")
  )

(defun mind-wave-chat-ask-insert-line ()
  (interactive)
  (mind-wave-chat-ask-insert-line-with-role "User")
  )

(defun mind-wave-chat-ask-insert-line-system ()
  (interactive)
  (mind-wave-chat-ask-insert-line-with-role "System")
  )

(defun mind-wave-chat-ask-insert-line-assistant ()
  (interactive)
  (mind-wave-chat-ask-insert-line-with-role "Assistant")
  )

(defun mind-wave-chat-ask-send-buffer ()
  (interactive)
  (goto-char (line-end-position))
  (insert "\n")
  (message "Wait ChatGPT...")
  (mind-wave-call-async "chat_ask"
                        (buffer-file-name)
                        (mind-wave--encode-string (mind-wave-get-buffer-string))
                        ""
                        ))

(defun mind-wave-chat-ask-with-multiline ()
  (interactive)
  (let* ((bufname (buffer-name))
         (edit-buffer (generate-new-buffer (format "mind-wave-edit-buffer-%s" bufname))))
    (split-window-below -10)
    (other-window 1)
    (with-current-buffer edit-buffer
      (mind-wave-edit-mode)
      (set (make-local-variable 'mind-wave-edit-buffer-name) bufname))
    (switch-to-buffer edit-buffer)
    (mind-wave--edit-set-header-line)))

(defun mind-wave--edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          "\\<eaf-edit-mode-map>"
          " Mind-Wave Edit Mode: "
          "Confirm with `\\[C-c C-c]', "
          "Cancel with `\\[C-c C-k]'. "
          ))))

(defvar mind-wave-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'mind-wave-edit-mode-cancel)
    (define-key map (kbd "C-c C-c") #'mind-wave-edit-mode-confirm)
    map))

(defun mind-wave-edit-mode-cancel ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[Mind-Wave] Edit cancelled!"))

(defun mind-wave-edit-mode-confirm ()
  (interactive)
  (let* ((bufname mind-wave-edit-buffer-name)
         (prompt (mind-wave-get-buffer-string)))
    (kill-buffer)
    (delete-window)

    (switch-to-buffer bufname)
    (mind-wave-chat-ask-with-message prompt)))

(define-derived-mode mind-wave-edit-mode text-mode "mind-wave/edit"
  "The major mode to edit focus text input.")

(defun mind-wave-chat-continue ()
  (interactive)
  (mind-wave-chat-ask-with-message
   (format "Continue, and output with %s" (mind-wave-output-lang))
   ))

(defun mind-wave-chat-generate-title ()
  (interactive)
  (mind-wave-chat-parse-title t))

(defun mind-wave-chat-parse-title (force)
  (interactive)
  (when (or (not (and (string-prefix-p "#" (buffer-name))
                      (string-suffix-p "#.chat" (buffer-name))))
            force)
    (message "Generate title from chat content...")
    (mind-wave-call-async "parse_title"
                          (buffer-file-name)
                          (mind-wave--encode-string (mind-wave-get-buffer-string))
                          mind-wave-title-role
                          (format "Give the following passage a %s title without quotation marks" (mind-wave-output-lang))
                          )))

(defun mind-wave-parse-title--response (filename title)
  (mind-wave--with-file-buffer
      filename
    (message "Generate title done.")
    (set-visited-file-name (format "#%s#.chat" (string-replace "/" "_" title)))
    (delete-file filename)
    (save-buffer)))

(defun mind-wave-adjust-text ()
  (interactive)
  (let* ((info (mind-wave-get-region-or-function))
         (code-start (nth 0 info))
         (code-end (nth 1 info))
         (code-text (nth 2 info)))
    (mind-wave-call-async "adjust_text"
                          (buffer-file-name)
                          (mind-wave--encode-string code-text)
                          code-start
                          code-end
                          mind-wave-code-role
                          (read-string "Prompt: ")
                          "Adjust text"
                          )
    (message "Adjust text...")))

(defun mind-wave-translate-to-english ()
  (interactive)
  (let* ((info (mind-wave-get-region-or-sexp))
         (translate-start (nth 0 info))
         (translate-end (nth 1 info))
         (translate-text (nth 2 info)))
    (message "Translate...")
    (mind-wave-call-async "adjust_text"
                          (buffer-file-name)
                          (mind-wave--encode-string translate-text)
                          translate-start
                          translate-end
                          mind-wave-translate-role
                          "Please translate the following paragraph, if the content includes Markdown content, the translated content should keep the same Markdown syntax"
                          "Translate done"
                          )))

(defun mind-wave-explain-word ()
  (interactive)
  (message "Explaining word...")
  (let ((word (nth 2 (mind-wave-get-region-or-thing 'word))))
    (mind-wave-call-async "explain_word"
                          (buffer-name)
                          "text-mode"
                          (mind-wave--encode-string (nth 2 (mind-wave-get-region-or-sentence)))
                          word
                          "Explain word"
                          "ChatGPT explaining..."
                          "ChatGPT explain finish.")))

(defun mind-wave-proofreading-doc ()
  (interactive)
  (let* ((info (mind-wave-get-region-or-sexp))
         (translate-start (nth 0 info))
         (translate-end (nth 1 info))
         (translate-text (nth 2 info)))
    (message "Proofreading...")
    (mind-wave-call-async "adjust_text"
                          (buffer-file-name)
                          (mind-wave--encode-string translate-text)
                          translate-start
                          translate-end
                          mind-wave-proofreading-role
                          (format "Please help me proofread the following paragraph with %s." (mind-wave-output-lang))
                          "Proofread done"
                          )))

(defun mind-wave-adjust-text--response (filename translate translate-start translate-end notify-end)
  (message notify-end)
  (mind-wave--with-file-buffer
      filename
    (when (region-active-p)
      (deactivate-mark))

    (goto-char translate-start)
    (delete-region translate-start translate-end)
    (insert translate)))

(defun mind-wave-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (ignore-errors
    (save-excursion
      (let ((point (point)))
        (beginning-of-defun)
        (parse-partial-sexp (point) point)))))

(defun mind-wave-in-string-p (&optional state)
  (or (nth 3 (or state (mind-wave-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (1- (point)) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (1- (point)) 'face) 'font-lock-doc-face))
      ))

(defun mind-wave-get-region-or-thing (thing)
  (save-excursion
    (let (translate-start translate-end)
      (if (region-active-p)
          (progn
            (setq translate-start (region-beginning))
            (setq translate-end (region-end)))
        (setq translate-start (beginning-of-thing thing))
        (setq translate-end (end-of-thing thing)))

      (list translate-start translate-end (buffer-substring-no-properties translate-start translate-end)))))

(defun mind-wave-get-region-or-sexp ()
  (mind-wave-get-region-or-thing 'sexp))

(defun mind-wave-get-region-or-sentence ()
  (save-excursion
    (let (translate-start translate-end)
      (cond ((region-active-p)
             (setq translate-start (region-beginning))
             (setq translate-end (region-end)))
            ((mind-wave-in-string-p)
             (setq translate-start (beginning-of-thing 'string))
             (setq translate-end (end-of-thing 'string)))
            (t
             (setq translate-start (beginning-of-thing 'sentence))
             (setq translate-end (end-of-thing 'sentence))))

      (list translate-start translate-end (buffer-substring-no-properties translate-start translate-end)))))

(defun mind-wave-get-region-or-function ()
  (let (code-start code-end)
    (if (region-active-p)
        (progn
          (setq code-start (region-beginning))
          (setq code-end (region-end)))
      (let ((function-node (mind-wave-get-function-node)))
        (setq code-start (treesit-node-start function-node))
        (setq code-end (treesit-node-end function-node))))

    (list code-start code-end (buffer-substring-no-properties code-start code-end))))

(defun mind-wave-get-function-node ()
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent) '("call_expression" "declaration" "function_definition")))))

(defun mind-wave-show-chat-window (buffername mode)
  (setq mind-wave-window-configuration-before-split (current-window-configuration))
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (get-buffer-create buffername)
  (switch-to-buffer buffername)
  (funcall (intern mode))
  (read-only-mode -1))

(defun mind-wave-restore-window-configuration ()
  (interactive)
  (when mind-wave-window-configuration-before-split
    (set-window-configuration mind-wave-window-configuration-before-split)
    (setq mind-wave-window-configuration-before-split nil)))

(defun mind-wave-refactory-code (&optional prompt)
  (interactive)
  (message "Refactoring...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (nth 2 (mind-wave-get-region-or-function)))
                        mind-wave-code-role
                        (or prompt
                            (format
                             "Please help me refactor the following code. Please reply with the refactoring explanation in %s, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'."
                             (mind-wave-output-lang)))
                        "refactory"
                        "ChatGPT refactoring..."
                        "ChatGPT refactory finish."))

(defun mind-wave-refactory-code-with-input ()
  (interactive)
  (mind-wave-refactory-code (read-string "Prompt: ")))

(defun mind-wave-comment-code ()
  (interactive)
  (message "Commenting...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (nth 2 (mind-wave-get-region-or-function)))
                        mind-wave-code-role
                        "Please add code comments to the following code, with the comments written in English within the code, and output the code including the comments."
                        "comment"
                        "ChatGPT commenting..."
                        "ChatGPT comment finish."))

(defun mind-wave-explain-code ()
  (interactive)
  (message "Explaining...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (nth 2 (mind-wave-get-region-or-function)))
                        mind-wave-code-role
                        (format "Please explain in detail the meaning of the following code, in %s" (mind-wave-output-lang))
                        "explain"
                        "ChatGPT explaining..."
                        "ChatGPT explain finish."))

(defun mind-wave-generate-commit-name ()
  (interactive)
  (message "Git commit name generating...")
  (mind-wave-call-async "generate_git_commit_name"
                        default-directory
                        mind-wave-code-role
                        "Please generate a patch title for the following diff content, with a concise and informative summary instead of a mechanical list. The title should not exceed 100 characters in length, unless word is a proper noun or the beginning of a sentence, other words are lowercase."))

(defun mind-wave-generate-commit-name--response (patch-name)
  (when (active-minibuffer-window)
    (insert patch-name)))

(defvar mind-wave-summary-template (format "Your output should use the following template:
### Summary
### Facts
- [Emoji] Bulletpoint

Your task is to summarize the text I give you in up to seven concise  bulletpoints and start with a short, high-quality summary. Pick a suitable emoji for every bullet point. Your response should be in %s. Use the following text:"
                                           (mind-wave-output-lang)
                                           ))

(defvar mind-wave-window-configuration-before-split nil)

(defun mind-wave-summary-video ()
  (interactive)
  (let ((video-id (if eaf--buffer-url
                      (cadr (split-string eaf--buffer-url "="))
                    (read-string "YouTube Video ID: "))))

    (mind-wave-call-async "summary_video"
                          (buffer-name)
                          video-id
                          mind-wave-summary-role
                          mind-wave-summary-template
                          "ChatGPT summary video..."
                          "ChatGPT summary video finish.")))

(defun mind-wave-summary-web ()
  (interactive)
  (let ((url (if eaf--buffer-url
                 eaf--buffer-url
               (read-string "Web URL: "))))

    (message "Parse web content...")
    (mind-wave-call-async "summary_web"
                          (buffer-name)
                          url
                          mind-wave-summary-role
                          mind-wave-summary-template
                          "ChatGPT summary web..."
                          "ChatGPT summary web finish.")))

(defun mind-wave-chat-ask--response (filename type answer)
  (mind-wave--with-file-buffer filename
    (pcase type
      ("start"
       (goto-char (point-max))
       (insert "\n------ Assistant ------\n")
       (message "ChatGPT speaking..."))
      ("content"
       (save-excursion
         (goto-char (point-max))
         (insert (mind-wave-decode-base64 answer))))
      ("end"
       (save-excursion
         (goto-char (point-max))
         (insert "\n\n"))
       (when mind-wave-auto-change-title
         (mind-wave-chat-parse-title nil))
       (message "ChatGPT response finish.")
       ))))

(defun mind-wave-split-window--response (buffer
                                         buffername
                                         mode
                                         type
                                         answer
                                         start-message
                                         end-message)
  (pcase type
    ("start"
     (select-window (get-buffer-window buffer))
     (mind-wave-show-chat-window buffername mode)
     (message start-message))
    ("content"
     (with-current-buffer (get-buffer-create buffername)
       (save-excursion
         (goto-char (point-max))
         (insert (mind-wave-decode-base64 answer)))))
    ("end"
     (select-window (get-buffer-window buffer))
     (with-current-buffer (get-buffer-create buffername)
       (save-excursion
         (goto-char (point-max))
         (insert "\n\n")))
     (message end-message)
     )))

(provide 'mind-wave)

;;; mind-wave.el ends here
