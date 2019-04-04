;;; quiet.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; make emacs more quiet
;;; Code:

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

(provide 'quiet)
;;; quiet.el ends here
