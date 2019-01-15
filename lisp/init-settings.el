;;; package -- summary
;;; Commentary:
;;; Code:

(setq make-backup-files nil)
;;;###autoload
(defun setting-after-init ()
  "Config something don't need early."
  ;; 自动保存到文件，而不是另外的
  (setq auto-save-default nil)

  (auto-save-visited-mode t)

;;;自动加载更改过的文件
  (global-auto-revert-mode 1)

;;;别名
  (fset 'yes-or-no-p 'y-or-n-p)

  (show-paren-mode t)

  (electric-pair-mode t)
  
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none))

  (recentf-mode t)
  
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
    (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/\\.cache/.*" (getenv "HOME")))

  (global-set-key (kbd "C-c C-k") 'kill-current-buffer)

  (global-set-key (kbd "C-c g") 'goto-line)

  (global-set-key (kbd "M-;") 'comment-line)

  (global-set-key (kbd "M-j") 'delete-indentation)

  (global-visual-line-mode 1)
  ;; prevent some operation for region like C-w
  (setq mark-even-if-inactive nil)

  (global-set-key (kbd "C-c c") 'ispell-buffer))

(add-hook 'after-init-hook #'setting-after-init)

;;;###autoload
(defun kill-and-switch-buffer ()
  "Kill current buffer and switch other default buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer (previous-buffer)))

;; set for some specify lang
;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)

(add-hook 'minibuffer-setup-hook
          (lambda ()
	    (visual-line-mode -1)))

(add-hook 'after-init-hook
          (lambda ()
	    (column-number-mode t)
	    (global-hl-line-mode t)))

;; line number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init-settings)
;;; init-settings.el ends here
