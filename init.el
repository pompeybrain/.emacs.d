;;; package--Summary ;; -*- lexical-binding: t -*-
;;; Code:

;; (setq debug-on-error t)                 

;;; set title  early-init has the best appearance
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; ;;; set gc
;; (let ((normal-gc-cons-threshold (* 32 1024 1024))
;;       (init-gc-cons-threshold (* 512 1024 1024))
;;       (default-file-name-handler-alist file-name-handler-alist)
;;       (file-name-handler-alist nil))
;;   (setq gc-cons-threshold init-gc-cons-threshold
;;         gc-cons-percentage 0.6)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
;;                              gc-cons-percentage 0.1
;;                              file-name-handler-alist
;;                              default-file-name-handler-alist))))

(add-to-list 'load-path (expand-file-name "lib/" user-emacs-directory) t)
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory) t)
(add-to-list 'load-path (expand-file-name "packages/" user-emacs-directory) t)
(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory) t)

(require 'core-lib)
(require 'core-ui)
(require 'core-settings)

;; (setq package-archives
;;       '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; (package-initialize)

(require 'config-packages)

(require 'core-keybind)


(provide 'init)
;;; init.el ends here
