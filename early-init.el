;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

;;; set gc
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; set font
(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :weight 'normal)
(setq-default line-spacing 3)

;; 左右边界
(fringe-mode '(8 . 0))
(setq overflow-newline-into-fringe nil)
(setq indicate-empty-lines nil)

(setq ns-use-proxy-icon nil)
;; set title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; set cursor
(setq-default cursor-type '(bar . 2))

;;; how to set frame size and position in mac os very early
;;; defaults write org.gnu.Emacs Width 150
;;; defaults write org.gnu.Emacs Height 50
;;; defaults write org.gnu.Emacs Top 150
;;; defaults write org.gnu.Emacs Left 400

;; (require 'package)
(setq package-archives
     '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
             ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(provide 'early-init)
;;; early-init.el ends here

