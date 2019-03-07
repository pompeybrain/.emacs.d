;;; process-wrapper.el -*- coding: utf-8; lexical-binding: t; -*-

(defun sync-process (command args input-buffer &optional output-handler error-handler)
  "Call sync process command with friendly output and error handle."
  (let ((input-file (make-temp-file command))
        (output-buffer (get-buffer-create (concat command "-output")))
        (error-file (make-temp-file command))
        (error-buffer (get-buffer-create (concat command "-error")))
        (error-output nil)
        (error-message nil))
    (unwind-protect
        (progn
          (if input-buffer
              (with-current-buffer input-buffer
                (save-restriction
                  (widen)
                  (write-region nil nil input-file)))
            (setq input-file nil))
          (if (zerop (apply 'call-process command input-file (list output-buffer error-file)
                            nil args))
              (progn
                (when output-handler
                  (funcall output-handler output-buffer)))
            (setq error-message "command [%s] failed:\n %s")))
      (with-current-buffer error-buffer
        (erase-buffer)
        (insert-file-contents error-file)
        (setq error-output (buffer-string))
        (if error-handler
            (funcall error-handler error-output)
          (unless (string-empty-p error-output)
            (if error-message
                (setq error-message (format error-message command (buffer-string)))
              (setq error-message (format "command [%s] succeeded but has warn:\n %s" command (buffer-string)))))
          ))
      (kill-buffer error-buffer)
      (delete-file input-file)
      (delete-file error-file)
      (kill-buffer output-buffer)
      (when error-message (message error-message))
      )))

(provide 'process-wrapper)
