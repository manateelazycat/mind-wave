;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro mind-wave-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar mind-wave-deferred-debug nil
  "Debug output switch.")

(defvar mind-wave-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun mind-wave-deferred-log (&rest args)
  "[internal] Debug log function."
  (when mind-wave-deferred-debug
    (with-current-buffer (get-buffer-create "*mind-wave-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" mind-wave-deferred-debug-count (apply #'format args)))))
    (cl-incf mind-wave-deferred-debug-count)))

(defvar mind-wave-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro mind-wave-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`mind-wave-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal mind-wave-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar mind-wave-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar mind-wave-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `mind-wave-deferred-post-task' and `mind-wave-deferred-worker'.")

(defun mind-wave-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`mind-wave-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack mind-wave-deferred-queue)
    (mind-wave-deferred-log "QUEUE-POST [%s]: %s" (length mind-wave-deferred-queue) pack)
    (run-at-time mind-wave-deferred-tick-time nil 'mind-wave-deferred-worker)
    d))

(defun mind-wave-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when mind-wave-deferred-queue
    (let* ((pack (car (last mind-wave-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq mind-wave-deferred-queue (nbutlast mind-wave-deferred-queue))
      (condition-case err
          (setq value (mind-wave-deferred-exec-task d which arg))
        (error
         (mind-wave-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: mind-wave-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `mind-wave-deferred-resignal')
;; cancel      : a canceling function (default `mind-wave-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct mind-wave-deferred-object
  (callback 'identity)
  (errorback 'mind-wave-deferred-resignal)
  (cancel 'mind-wave-deferred-default-cancel)
  next status value)

(defun mind-wave-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun mind-wave-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (mind-wave-deferred-log "CANCEL : %s" d)
  (setf (mind-wave-deferred-object-callback d) 'identity)
  (setf (mind-wave-deferred-object-errorback d) 'mind-wave-deferred-resignal)
  (setf (mind-wave-deferred-object-next d) nil)
  d)

(defun mind-wave-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (mind-wave-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "mind-wave-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (mind-wave-deferred-object-callback d)
                    (mind-wave-deferred-object-errorback d)))
        (next-deferred (mind-wave-deferred-object-next d)))
    (cond
     (callback
      (mind-wave-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((mind-wave-deferred-object-p value)
                                             (mind-wave-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (mind-wave-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (mind-wave-deferred-post-task next-deferred 'ok value)
                                               (setf (mind-wave-deferred-object-status d) 'ok)
                                               (setf (mind-wave-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (mind-wave-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (mind-wave-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (mind-wave-deferred-object-status d) 'ng)
                                            (setf (mind-wave-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (mind-wave-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (mind-wave-deferred-resignal arg)))))))

(defun mind-wave-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (mind-wave-deferred-object-next prev) next)
  (cond
   ((eq 'ok (mind-wave-deferred-object-status prev))
    (setf (mind-wave-deferred-object-status prev) nil)
    (let ((ret (mind-wave-deferred-exec-task
                next 'ok (mind-wave-deferred-object-value prev))))
      (if (mind-wave-deferred-object-p ret) ret
        next)))
   ((eq 'ng (mind-wave-deferred-object-status prev))
    (setf (mind-wave-deferred-object-status prev) nil)
    (let ((ret (mind-wave-deferred-exec-task next 'ng (mind-wave-deferred-object-value prev))))
      (if (mind-wave-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun mind-wave-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-mind-wave-deferred-object :callback callback)
    (make-mind-wave-deferred-object)))

(defun mind-wave-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (mind-wave-deferred-exec-task d 'ok arg))

(defun mind-wave-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (mind-wave-deferred-exec-task d 'ng arg))

(defun mind-wave-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (mind-wave-deferred-post-task d 'ok arg))

(defun mind-wave-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (mind-wave-deferred-callback-post (mind-wave-deferred-new callback))."
  (let ((d (if callback
               (make-mind-wave-deferred-object :callback callback)
             (make-mind-wave-deferred-object))))
    (mind-wave-deferred-callback-post d arg)
    d))

(defun mind-wave-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-mind-wave-deferred-object :callback callback)))
    (mind-wave-deferred-set-next d nd)))

(defun mind-wave-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-mind-wave-deferred-object :errorback callback)))
    (mind-wave-deferred-set-next d nd)))

(defvar mind-wave-epc-debug nil)

(defun mind-wave-epc-log (&rest args)
  (when mind-wave-epc-debug
    (with-current-buffer (get-buffer-create "*mind-wave-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun mind-wave-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar mind-wave-epc-uid 1)

(defun mind-wave-epc-uid ()
  (cl-incf mind-wave-epc-uid))

(defvar mind-wave-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct mind-wave-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun mind-wave-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return mind-wave-epc-connection object."
  (mind-wave-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (mind-wave-epc-uid))
         (connection-name (format "mind-wave-epc con %s" connection-id))
         (connection-buf (mind-wave-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-mind-wave-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (mind-wave-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (mind-wave-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (mind-wave-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun mind-wave-epc-process-sentinel (connection process msg)
  (mind-wave-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (mind-wave-epc-connection-name connection) process msg)
  (mind-wave-epc-disconnect connection))

(defun mind-wave-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (mind-wave-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (mind-wave-epc-connection-process connection)))
    (mind-wave-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun mind-wave-epc-disconnect (connection)
  (let ((process (mind-wave-epc-connection-process connection))
        (buf (mind-wave-epc-connection-buffer connection))
        (name (mind-wave-epc-connection-name connection)))
    (mind-wave-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (mind-wave-epc-log "!! Disconnected finished [%s]" name)))

(defun mind-wave-epc-process-filter (connection process message)
  (mind-wave-epc-log "INCOMING: [%s] [%S]" (mind-wave-epc-connection-name connection) message)
  (with-current-buffer (mind-wave-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (mind-wave-epc-process-available-input connection process)))

(defun mind-wave-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (mind-wave-deferred-new callback)
             (mind-wave-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun mind-wave-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (mind-wave-deferred-callback-post d event))))

(defun mind-wave-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (mind-wave-epc-net-have-input-p)
      (let ((event (mind-wave-epc-net-read-or-lose process))
            (ok nil))
        (mind-wave-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'mind-wave-epc-signal-send
                         (cons (mind-wave-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (mind-wave-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (mind-wave-epc-process-available-input connection process)))))))

(defun mind-wave-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (mind-wave-epc-net-decode-length))))

(defun mind-wave-epc-net-read-or-lose (_process)
  (condition-case error
      (mind-wave-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun mind-wave-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (mind-wave-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun mind-wave-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun mind-wave-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct mind-wave-epc-manager
  "Root object that holds all information related to an EPC activity.

`mind-wave-epc-start-epc' returns this object.

title          : instance name for displaying on the `mind-wave-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : mind-wave-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct mind-wave-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar mind-wave-epc-live-connections nil
  "[internal] A list of `mind-wave-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun mind-wave-epc-server-process-name (uid)
  (format "mind-wave-epc-server:%s" uid))

(defun mind-wave-epc-server-buffer-name (uid)
  (format " *%s*" (mind-wave-epc-server-process-name uid)))

(defun mind-wave-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (mind-wave-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (mind-wave-epc-disconnect (mind-wave-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 mind-wave-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq mind-wave-epc-live-connections (delete mngr mind-wave-epc-live-connections))
    ))

(defun mind-wave-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun mind-wave-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an mind-wave-epc-connection instance."
  (let* ((mngr mngr)
         (conn (mind-wave-epc-manager-connection mngr))
         (channel (mind-wave-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (mind-wave-epc-log "SIG CALL: %S" args)
                    (apply 'mind-wave-epc-handler-called-method ,mngr (mind-wave-epc-args args))))
               (return
                . (lambda (args)
                    (mind-wave-epc-log "SIG RET: %S" args)
                    (apply 'mind-wave-epc-handler-return ,mngr (mind-wave-epc-args args))))
               (return-error
                . (lambda (args)
                    (mind-wave-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'mind-wave-epc-handler-return-error ,mngr (mind-wave-epc-args args))))
               (epc-error
                . (lambda (args)
                    (mind-wave-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'mind-wave-epc-handler-epc-error ,mngr (mind-wave-epc-args args))))
               (methods
                . (lambda (args)
                    (mind-wave-epc-log "SIG METHODS: %S" args)
                    (mind-wave-epc-handler-methods ,mngr (caadr args))))
               ) do
             (mind-wave-epc-signal-connect channel method body))
    (push mngr mind-wave-epc-live-connections)
    mngr))

(defun mind-wave-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (mind-wave-epc-manager-connection mngr)))
    (mind-wave-epc-net-send conn (cons method messages))))

(defun mind-wave-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (mind-wave-epc-manager-methods mngr)
           if (eq method-name (mind-wave-epc-method-name i))
           do (cl-return i)))

(defun mind-wave-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (mind-wave-epc-manager-methods mngr)
                  collect
                  (list
                   (mind-wave-epc-method-name i)
                   (or (mind-wave-epc-method-arg-specs i) "")
                   (or (mind-wave-epc-method-docstring i) "")))))
    (mind-wave-epc-manager-send mngr 'return uid info)))

(defun mind-wave-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (mind-wave-epc-manager-methods mngr))
           (method (mind-wave-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (mind-wave-epc-log "ERR: No such method : %s" name)
        (mind-wave-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (mind-wave-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((mind-wave-deferred-object-p ret)
                (mind-wave-deferred-nextc ret
                                          (lambda (xx) (mind-wave-epc-manager-send mngr 'return uid xx))))
               (t (mind-wave-epc-manager-send mngr 'return uid ret))))
          (error
           (mind-wave-epc-log "ERROR : %S" err)
           (mind-wave-epc-manager-send mngr 'return-error uid err))))))))

(defun mind-wave-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (mind-wave-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (mind-wave-epc-manager-sessions mngr) ret)))

(defun mind-wave-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (mind-wave-epc-manager-sessions mngr))))
    (cond
     (pair
      (mind-wave-epc-log "RET: id:%s [%S]" uid args)
      (mind-wave-epc-manager-remove-session mngr uid)
      (mind-wave-deferred-callback (cdr pair) args))
     (t                                 ; error
      (mind-wave-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun mind-wave-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (mind-wave-epc-manager-sessions mngr))))
    (cond
     (pair
      (mind-wave-epc-log "RET-ERR: id:%s [%S]" uid args)
      (mind-wave-epc-manager-remove-session mngr uid)
      (mind-wave-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (mind-wave-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun mind-wave-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (mind-wave-epc-manager-sessions mngr))))
    (cond
     (pair
      (mind-wave-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (mind-wave-epc-manager-remove-session mngr uid)
      (mind-wave-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (mind-wave-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun mind-wave-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (mind-wave-epc-uid))
        (sessions (mind-wave-epc-manager-sessions mngr))
        (d (mind-wave-deferred-new)))
    (push (cons uid d) sessions)
    (setf (mind-wave-epc-manager-sessions mngr) sessions)
    (mind-wave-epc-manager-send mngr 'call uid method-name args)
    d))

(defun mind-wave-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-mind-wave-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (mind-wave-epc-manager-methods mngr))))
    (setf (mind-wave-epc-manager-methods mngr) methods)
    method))

(defun mind-wave-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'mind-wave-epc-nothing))
    (mind-wave-deferred-chain
     d
     (mind-wave-deferred-nextc it
                               (lambda (x) (setq result x)))
     (mind-wave-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'mind-wave-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (mind-wave-epc-connection-process (mind-wave-epc-manager-connection mngr))
         0 mind-wave-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun mind-wave-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (mind-wave-epc-sync mngr (mind-wave-epc-call-deferred mngr method-name args)))

(defun mind-wave-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (mind-wave-epc-connection-process (mind-wave-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar mind-wave-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`mind-wave-epc-manager' instance]).
When the server process accepts the client connection, the
`mind-wave-epc-manager' instance is created and stored in this variable
`mind-wave-epc-server-client-processes'. This variable is used for the management
purpose.")

;; mind-wave-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `mind-wave-epc-manager' instances
(cl-defstruct mind-wave-epc-server name process port connect-function)

(defvar mind-wave-epc-server-processes nil
  "[internal] A list of ([process object] . [`mind-wave-epc-server' instance]).
This variable is used for the management purpose.")

(defun mind-wave-epc-server-get-manager-by-process (proc)
  "[internal] Return the mind-wave-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in mind-wave-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun mind-wave-epc-server-accept (process)
  "[internal] Initialize the process and return mind-wave-epc-manager object."
  (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (mind-wave-epc-uid))
         (connection-name (format "mind-wave-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-mind-wave-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (mind-wave-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (mind-wave-epc-process-sentinel connection p e)))
    (make-mind-wave-epc-manager :server-process process :port t
                                :connection connection)))

(defun mind-wave-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (mind-wave-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (mind-wave-epc-server-accept process)))
            (push (cons process mngr) mind-wave-epc-server-client-processes)
            (mind-wave-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process mind-wave-epc-server-client-processes)) _d)
        (when pair
          (mind-wave-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (mind-wave-epc-stop-epc (cdr pair))
          (setq mind-wave-epc-server-client-processes
                (assq-delete-all process mind-wave-epc-server-client-processes))
          ))
      nil))))

(defun mind-wave-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "MIND-WAVE EPC Server %s" (mind-wave-epc-uid)))
       (buf (mind-wave-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (mind-wave-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-mind-wave-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          mind-wave-epc-server-processes)
    main-process))

(provide 'mind-wave-epc)
;;; mind-wave-epc.el ends here
