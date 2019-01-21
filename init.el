;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; prevent auto-save-list dir
(setq auto-save-list-file-prefix nil)

(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

(setq custom-file (expand-file-name "custom.el" emacs-d))
(load custom-file)

(add-to-list 'load-path (expand-file-name "lisp" emacs-d) t)

(load-theme 'zenburn)

(require 'init-packages)
(require 'init-settings)
(add-to-list 'load-path (expand-file-name "site-lisp/" emacs-d) t)
(require 'init-site-lisp)
(require 'init-keybinds)

(provide 'init)
;;; init.el ends here
