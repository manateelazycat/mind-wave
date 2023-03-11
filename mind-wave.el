;;; mind-wave.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: mind-wave.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/mind-wave
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
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

(defgroup mind-wave nil
  "Mind-Wave group."
  :group 'applications)

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

(defcustom mind-wave-enable-log nil
  "Enable this option to print log message in `*mind-wave*' buffer, default only print message header."
  :type 'boolean)

(defcustom mind-wave-enable-profile nil
  "Enable this option to output performance data to ~/mind-wave.prof."
  :type 'boolean)

(defun mind-wave--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun mind-wave-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (mind-wave-deferred-chain
    (mind-wave-epc-call-deferred mind-wave-epc-process (read method) args)))

(defvar mind-wave-is-starting nil)

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
  (setq mind-wave-is-starting nil))

(defun mind-wave--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

(defvar mind-wave-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" #'mind-wave-chat-ask)
    (define-key map "\C-h" #'mind-wave-chat-continue)
    (define-key map "\C-i" #'mind-wave-chat-change-system)
    map)
  "Mind Wave Chat Keymap")

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
                        (mind-wave--encode-string (buffer-string))
                        prompt
                        ))

(defun mind-wave-chat-ask ()
  (interactive)
  (let ((prompt (read-string "Ask ChatGPT: ")))
    (if (string-empty-p (string-trim prompt))
        (message "Please don't send empty question.")
      (mind-wave-chat-ask-with-message prompt)
      )))

(defun mind-wave-chat-continue ()
  (interactive)
  (mind-wave-chat-ask-with-message "继续"))

(defun mind-wave-chat-ask--response (filename type answer)
  (mind-wave--with-file-buffer filename
    (goto-char (point-max))
    (pcase type
      ("start"
       (insert "\n------ Assistant ------\n")
       (message "ChatGPT speaking..."))
      ("content"
       (insert answer))
      ("end"
       (insert "\n\n")
       (mind-wave-rename)
       (message "ChatGPT response finish.")
       ))))

(defun mind-wave-rename ()
  (let ((buffername (buffer-name)))
    (unless (and (string-prefix-p "#" buffername)
                 (string-suffix-p "#.chat" buffername))
      (mind-wave-call-async "parse_title"
                            (buffer-file-name)
                            (mind-wave--encode-string (buffer-string))))))

(defun mind-wave-parse-title--response (filename title)
  (mind-wave--with-file-buffer
      filename
    (set-visited-file-name (format "#%s#.chat" title))
    (delete-file filename)
    (save-buffer)))

(defun mind-wave-chat-change-system ()
  (interactive)
  (message "*******"))

(define-minor-mode mind-wave-chat-mode
  "Mind Wave mode."
  :keymap mind-wave-chat-mode-map
  :init-value nil
  (if mind-wave-chat-mode
      (mind-wave-chat-mode-enable)
    (mind-wave-chat-mode-disable)))

(defun mind-wave-chat-mode-enable ()
  "Enable Mind Wave mode."
  (message "Mind Wave mode enable"))

(defun mind-wave-chat-mode-disable ()
  "Disable Mind Wave mode."
  (message "Mind Wave mode disable"))

(add-to-list 'auto-mode-alist '("\\.chat$" . mind-wave-chat-mode))

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
                          "You are an English teacher."
                          "请帮我把下面这段话翻译成英文， 结果不要带引号， 保持同样的格式"
                          "Translate done"
                          )))

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
                          "你是一个高水平的作家"
                          "请帮我润色一下下面这段话"
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

(defun mind-wave-get-region-or-sexp ()
  (let (translate-start translate-end)
    (if (region-active-p)
        (progn
          (setq translate-start (region-beginning))
          (setq translate-end (region-end)))
      (setq translate-start (beginning-of-thing 'sexp))
      (setq translate-end (end-of-thing 'sexp)))

    (list translate-start translate-end (buffer-substring-no-properties translate-start translate-end))))

(defun mind-wave-get-function-node ()
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (parent)
     (member (treesit-node-type parent) '("call_expression" "declaration" "function_definition")))))

(defun mind-wave-get-function-string ()
  (let (code-start code-end)
    (if (region-active-p)
        (progn
          (setq code-start (region-beginning))
          (setq code-end (region-end)))
      (let ((function-node (mind-wave-get-function-node)))
        (setq code-start (treesit-node-start function-node))
        (setq code-end (treesit-node-end function-node))))

    (buffer-substring-no-properties code-start code-end)))

(defun mind-wave-split-window--response (filename
                                         buffername
                                         mode
                                         type
                                         answer
                                         start-message
                                         end-message)
  (pcase type
    ("start"
     (mind-wave--with-file-buffer filename
       (delete-other-windows)
       (split-window-horizontally)
       (other-window 1)
       (get-buffer-create buffername)
       (switch-to-buffer buffername)
       (funcall (intern mode))
       (message start-message)))
    ("content"
     (save-excursion
       (with-current-buffer (get-buffer-create buffername)
         (insert answer))))
    ("end"
     (mind-wave--with-file-buffer filename
       (select-window (get-buffer-window buffer)))
     (message end-message)
     )))

(defun mind-wave-refactory-code ()
  (interactive)
  (message "Refactoring...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (buffer-file-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (mind-wave-get-function-string))
                        "请帮我重构一下下面这段代码"
                        "refactory"
                        "ChatGPT refactoring..."
                        "ChatGPT refactory finish."))

(defun mind-wave-comment-code ()
  (interactive)
  (message "Commenting...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (buffer-file-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (mind-wave-get-function-string))
                        "请给下面这段代码增加代码注释， 要求注释用英文写在代码中， 并输出包括注释的代码"
                        "comment"
                        "ChatGPT commenting..."
                        "ChatGPT comment finish."))

(defun mind-wave-explain-code ()
  (interactive)
  (message "Explaining...")
  (mind-wave-call-async "action_code"
                        (buffer-name)
                        (buffer-file-name)
                        (format "%s" major-mode)
                        (mind-wave--encode-string (mind-wave-get-function-string))
                        "请详细解释一下下面这段代码的意思"
                        "explain"
                        "ChatGPT explaining..."
                        "ChatGPT explain finish."))

(unless mind-wave-is-starting
  (mind-wave-start-process))

(provide 'mind-wave)

;;; mind-wave.el ends here
