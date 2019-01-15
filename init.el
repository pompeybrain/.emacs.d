;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq debug-on-error t)

;;; set gc
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; prevent auto-save-list dir
(setq auto-save-list-file-prefix nil)

(require 'package)

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/lisp/" t)
(require 'init-ui)
(require 'init-settings)
(require 'init-packages)
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/" t)
;; (require 'init-site-lisp)
(provide 'init)
;;; init.el ends here
