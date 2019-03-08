;;; process-wrapper.el -*- coding: utf-8; lexical-binding: t; -*-

(defun sync-process (command args input-buffer &optional output-handler error-handler)
  "Call sync process command with friendly output and error handle."
  (let ((input-file (make-temp-file command))
        (output-buffer (get-buffer-create (concat command "-output")))
        (output-string nil)
        (res-code nil)
        (error-file (make-temp-file command))
        (error-buffer (get-buffer-create (concat command "-error")))
        (error-output nil))
    (unwind-protect
        (progn
          (if input-buffer
              (with-current-buffer input-buffer
                (save-restriction
                  (widen)
                  (write-region nil nil input-file)))
            (setq input-file nil))
          (setq res-code (apply 'call-process command input-file (list output-buffer error-file)
                                nil args)))
      (with-current-buffer output-buffer
        (setq output-string (buffer-string)))
      (with-current-buffer error-buffer
        (erase-buffer)
        (insert-file-contents error-file)
        (setq error-output (buffer-string)))
      (kill-buffer error-buffer)
      (when input-file (delete-file input-file))
      (delete-file error-file)
      (kill-buffer output-buffer)
      (unless (string-empty-p error-output)
        (when error-handler
          (funcall error-handler error-output)))
      (if (zerop res-code)
          (progn
            (unless (or error-handler (string-empty-p error-output))
              (message "command [%s] succeeded but has warn:\n %s" command error-output))
            (if output-handler
                (funcall output-handler output-string)
              output-string))
        (message "command [%s] failed:\n %s" command error-output)))))

(provide 'process-wrapper)