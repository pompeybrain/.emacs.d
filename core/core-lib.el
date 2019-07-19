;;; core-lib.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defconst user-local-directory (concat user-emacs-directory ".local/"))

(defun add-hooks (hook-names fn)
  "add hooks fn.")

(defmacro quiet (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun quiet-save ()
  (interactive)
  "Save buffer quiet."
  (quiet
    (save-buffer)))

(defvar shellcommand-output "*shell-output*" "shellcommand output buffer")

(defun shellcommand-filter (proc output)
  "Filter shellcommand output to shellcommand buffer."
  (with-current-buffer shellcommand-output
    (insert (replace-regexp-in-string "\^M" "" output))))

(defun shellcommand-sentinel (proc event)
  "Filter shellcommand output to shellcommand buffer."
  (display-buffer shellcommand-output))

(defun shellcommand (command &optional proc-name)
  "Excute shell command and output for specific buffer."
  (let ((output-buffer (get-buffer-create shellcommand-output)))
    (with-current-buffer output-buffer
      (erase-buffer))
    (let ((proc (start-process-shell-command (or proc-name "shellcommand-process") nil command)))
      (set-process-filter proc 'shellcommand-filter)
      (set-process-sentinel proc 'shellcommand-sentinel))))

(provide 'core-lib)
;;; core-lib ends here
