;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; prevent auto-save-list dir
(setq auto-save-list-file-prefix nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/lisp/" t)

(load-theme 'atom-one-dark)

(require 'init-packages)
(require 'init-settings)
(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)
(require 'init-site-lisp)
(require 'init-hydra)
;; (require 'init-ui)

;; (require 'package)
;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
   ;;;                       ("melpa" . "http://elpa.emacs-china.org/melpa/")))
;; (setq package-archives
;;      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;              ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; (package-initialize)

(provide 'init)
;;; init.el ends here
