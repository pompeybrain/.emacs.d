;;; core-settings ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defcustom user-local-directory nil "local directory.")

(setq-default inhibit-startup-screen t
              auto-save-default nil
              auto-save-list-file-prefix nil
              require-final-newline t
              backup-directory-alist `(("." . ,(concat user-local-directory "backups")))
              recentf-save-file (concat user-local-directory "recentf")
              recentf-max-saved-items 30
              eww-bookmarks-directory user-local-directory
              url-configuration-directory (concat user-local-directory "url/")
              tab-width 4
              indent-tabs-mode nil
              js-indent-level 2
              make-backup-files nil
              ring-bell-function 'ignore
              custom-file (concat user-local-directory "custom.el")
              auto-revert-verbose nil
              ns-use-native-fullscreen nil
              ns-use-fullscreen-animation nil
              )

(auto-save-visited-mode t)
(global-auto-revert-mode 1)
(electric-pair-mode t)
(global-subword-mode 1)
(recentf-mode t)
(column-number-mode t)
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/\\.local/.*" (getenv "HOME")))

(defun setup-prog-mode ()
  "Setup prog mode."
  (show-paren-mode t)
  (hs-minor-mode +1))

(add-hook 'prog-mode-hook #'setup-prog-mode)


(add-hook 'emacs-startup-hook (lambda ()
                                (setq debug-on-error nil)))

(provide 'core-settings)
;;; core-settings ends here
