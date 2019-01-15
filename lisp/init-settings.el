;;; package -- summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun setting-after-init ()
  "Config something don't need early."
  ;; 自动保存到文件，而不是另外的
  (setq auto-save-default nil)

  (auto-save-visited-mode t)

  ;;关闭自动生成备份
  (setq make-backup-files nil)

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
  )

(add-hook 'after-init-hook #'setting-after-init)

;; 快速打开配置文件
;;;###autoload
;; (defun open-init-file()
;;   "Open init file."
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
;; (global-set-key (kbd "<f2>") 'open-init-file)


;;;###autoload
(defun kill-and-switch-buffer ()
  "Kill current buffer and switch other default buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer (previous-buffer)))

;; set for some specify lang
;; (setq-default tab-width 2)
;; (setq-default indent-tabs-mode nil)

(global-visual-line-mode 1)

(add-hook 'minibuffer-setup-hook
          (lambda ()
	    (visual-line-mode -1)))

(add-hook 'after-init-hook
          (lambda ()
	    (column-number-mode t)
	    (global-hl-line-mode t)))

;; prevent some operation for region like C-w
(setq mark-even-if-inactive nil)



;; line number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-c c") 'ispell-buffer)

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
