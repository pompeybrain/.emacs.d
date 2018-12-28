(setq debug-on-error t)

;; 关闭自动保存
(setq auto-save-default nil)

;;关闭自动生成备份
(setq make-backup-files nil)

;;;自动加载更改过的文件
(global-auto-revert-mode 1)

;;;别名
(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)

(electric-pair-mode t)

;; 快速打开配置文件
(defun open-init-file()
	"Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(recentf-mode t)
(setq-default recentf-max-menu-items 10)

(defun kill-and-switch-buffer ()
	"Kill current buffer and switch other default buffer."
	(interactive)
	(kill-buffer (current-buffer))
	(switch-to-buffer (previous-buffer) ))

(global-set-key (kbd "C-c C-k") 'kill-current-buffer)

(global-set-key (kbd "M-;") 'comment-line)

(global-set-key (kbd "M-j") 'delete-indentation)

(setq-default tab-width 2)

(global-visual-line-mode 1)
(add-hook 'minibuffer-setup-hook (lambda ()
																	 (visual-line-mode -1)))

(provide 'init-settings)
;;; init-settings.el ends here
