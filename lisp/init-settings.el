;;; init-settings.el -*- lexical-binding: t -*-
;;; package -- summary
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(global-visual-line-mode 1)

;;;###autoload
(defun setting-after-init ()
  "Config something don't need early."
  ;; close debug on error
  (setq debug-on-error nil)
  ;; 自动保存到文件，而不是另外的
  (setq auto-save-default nil)

  (auto-save-visited-mode t)

  (setq require-final-newline t)
;;;自动加载更改过的文件
  (global-auto-revert-mode 1)

;;; disabled tooltip
  (tooltip-mode -1)

;;;别名
  (fset 'yes-or-no-p 'y-or-n-p)

  (electric-pair-mode t)

  ;; subword
  (global-subword-mode 1)

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none))

  (setq recentf-max-saved-items 30)
  (recentf-mode t)
  (column-number-mode t)

  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/\\.cache/.*" (getenv "HOME")))

  ;; prevent some operation for region like C-w
  ;; (setq mark-even-if-inactive nil)
  )

(add-hook 'after-init-hook #'setting-after-init)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;###autoload
(defun kill-and-switch-buffer ()
  "Kill current buffer and switch other default buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer (previous-buffer)))

;; indentation config
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(add-hook 'minibuffer-setup-hook (lambda () (visual-line-mode -1)))

;;;###autoload
(defun setup-prog-mode ()
  "Setup prog mode."
  (show-paren-mode t)
  (hs-minor-mode +1))

(setq make-backup-files nil)
;; audio
(setq ring-bell-function 'ignore)

(add-hook 'prog-mode-hook #'setup-prog-mode)

;;;###autoload
(defun switch-theme ()
  "Switch light theme for read."
  (interactive)
  (load-theme 'leuven))

(provide 'init-settings)
;;; init-settings.el ends here
