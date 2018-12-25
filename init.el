;; -*- lexical-binding: t -*-

;;; This file;; set gc 
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/" t)
(require 'init-ui)
(require 'init-settings)
(require 'init-packages)
;; (require 'init-manages)
(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)
(require 'init-site-lisp)
