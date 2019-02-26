;;; init-dashboard

(defvar dashboard-buffer-name "*dashboard*")

(setq initial-buffer-choice #'dashboard-init-buffer)

(add-hook 'window-setup-hook #'dashboard-init)

(defun dashboard-init-buffer ()
  "Initial dashboard buffer."
  (get-buffer-create dashboard-buffer-name)
  )

(defun dashboard-init ()
  "Initial Dashboard."
  (dashboard-init-recentf)
  (dashboard-init-time)
  )

(defun dashboard-init-recentf ()
  "Display recentf list."
  (insert "\n" (propertize "[Recent Files]" 'face 'font-lock-keyword-face)  "\n")
  ;;; TODO: cons new alist bind char key and filename top 10 file
  ;; (mapcar #'dashboard-insert-file-name recentf-list) 
  )

(defun dashboard-init-time ()
  "Display init time."
  (insert "\n" (propertize
		(format "Emacs ready in %s with %d garbage collections."
			(format "%.2f seconds"
				(float-time
				 (time-subtract after-init-time before-init-time)))
			gcs-done) 'face 'font-lock-comment-face) "\n"))

(defun dashboard-insert-file-name (filename)
  "Insert file name row."
  (insert filename "\n"))

(provide 'init-dashboard)
