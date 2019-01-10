;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; set gc
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'package)

(add-to-list 'package-archives
						 '("melpa" . "http://elpa.emacs-china.org/melpa/") t)

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'load-path "~/.emacs.d/lisp/" t)
(require 'init-ui)
(require 'init-settings)
(require 'init-packages)

(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)
(require 'init-site-lisp)
(provide 'init)
;;; init.el ends here
