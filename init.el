;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; prevent auto-save-list dir
(setq auto-save-list-file-prefix nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/lisp/" t)

(load-theme 'zenburn)

(require 'init-packages)
(require 'init-settings)
(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)
(require 'init-site-lisp)
(require 'init-keybinds)

(provide 'init)
;;; init.el ends here
