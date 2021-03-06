;;; dashboard.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defvar dashboard-buffer-name "*dashboard*" "dashboard buffer name")

(defvar dashboard-key-list '("a" "s" "d" "f" "g" "h" "j" "k" "l" "n") "key list")

(setq initial-buffer-choice #'dashboard-init-buffer)

(add-hook 'window-setup-hook #'dashboard-init)

(defun dashboard-init-buffer ()
  "Initial dashboard buffer."
  (get-buffer-create dashboard-buffer-name))

(defun dashboard-init ()
  "Initial Dashboard."
  (with-current-buffer dashboard-buffer-name
    (dashboard-banner)
    (dashboard-recentf)
    (dashboard-startup-time)
    (dashboard-init-mode)
    (when (fboundp 'fly-insert-mode-activate)
      (fly-insert-mode-activate))))

(defun dashboard-banner ()
  "display banner emacs."
  (insert "\n\n")
  (mapc (lambda (line)
          (insert "\t" (propertize line 'face 'font-lock-function-name-face) " \n"))
        '(" _____ _    _ ____  ____ ____ "
          "/  __// \\__/|/  _ \\/   _Y ___\\ "
          "|  \\  | |\\/||| / \\||  / |    \\ "
          "|  /_ | |  ||| |-|||  \\_\\___ | "
          "\\____\\\\_/  \\|\\_/ \\|\\____|____/ "))
  (insert "\n\n"))


(defun dashboard-recentf ()
  "Display recentf list."
  (insert "\n" (propertize "Recent Files: " 'face 'font-lock-warning-face)  "\n")
  (dashboard-cons-key-file-alist dashboard-key-list recentf-list))

(defun dashboard-startup-time ()
  "Display init time."
  (insert "\n"
          (propertize
           (format "Emacs ready in %s with %d garbage collections."
                   (format "%.2f seconds"
                           (float-time
                            (time-subtract after-init-time before-init-time)))
                   gcs-done) 'face 'font-lock-comment-face) "\n"))

(defun dashboard-cons-key-file-alist (keylist filelist)
  "Insert file name row and cons alist"
  (let ((key (car keylist)) (file (car filelist)))
    (when (and key file)
      (insert (propertize (format "[%s]" key) 'face 'font-lock-keyword-face)
              " " (abbreviate-file-name file) "\n")
      (define-key dashboard-mode-map (kbd key) (dashboard-open-file-fun file))
      (dashboard-cons-key-file-alist (cdr keylist) (cdr filelist)))))

(defun dashboard-open-file-fun (filename)
  "Open key associate file."
  (interactive)
  (function
   (lambda ()
     (interactive)
     (find-file filename)
     (when (fboundp 'fly-command-mode-activate)
       (fly-command-mode-activate)))))

(defun dashboard-init-mode ()
  "Initial dashboard-mode keybind."
  (unless (eq major-mode 'dashboard-mode)
    (dashboard-mode)))

(define-derived-mode dashboard-mode special-mode
  "dashboard"
  :syntax-table nil
  :abbrev-table nil
  "Major mode for my dashboard buffer."
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local default-directory "~/"))

(provide 'dashboard)
;;; dashboard.el ends here
