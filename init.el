;;; package--Summary ;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)

;;; set gc
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024))
      (default-file-name-handler-alist file-name-handler-alist)
      (file-name-handler-alist nil))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                             gc-cons-percentage 0.1
                             file-name-handler-alist
                             default-file-name-handler-alist))))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; set font
(set-face-attribute 'default nil :family "Fira Code")
;; (set-face-attribute 'default nil :family "Menlo")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :weight 'normal)
(mac-auto-operator-composition-mode)	;macport for fira code
(setq-default line-spacing 3)

(setq custom-safe-themes t)
;; (load-theme 'leuven)
(load-theme 'aod)
;; (load-theme 'zenburn)

;; 左右边界
(fringe-mode '(8 . 0))
(setq overflow-newline-into-fringe nil)
(setq indicate-empty-lines nil)

;; set title
;;;defaults write org.gnu.Emacs HideDocumentIcon YES for macport version
;; (setq ns-use-proxy-icon nil)		;for 26
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

(global-hl-line-mode +1)

;; set cursor
;; (setq-default cursor-type '(bar . 2))

;;; how to set frame size and position in mac os very early
;;; defaults write org.gnu.Emacs Width 150
;;; defaults write org.gnu.Emacs Height 50
;;; defaults write org.gnu.Emacs Top 150
;;; defaults write org.gnu.Emacs Left 400

(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)
;; prevent auto-save-list dir
(setq auto-save-list-file-prefix nil)

(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

(setq custom-file (expand-file-name "custom.el" emacs-d))
(load custom-file)

(add-to-list 'load-path (expand-file-name "lib/" emacs-d) t)
(add-to-list 'load-path (expand-file-name "lisp/" emacs-d) t)
(add-to-list 'load-path (expand-file-name "site-lisp/" emacs-d) t)

(add-to-list 'load-path (expand-file-name "git-lisp/" emacs-d) t)

(let ((default-directory (expand-file-name "git-lisp/" emacs-d)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'init-packages)
(require 'init-settings)
(require 'init-site-lisp)
(require 'init-keybinds)
(require 'init-dashboard)
(require 'init-prettier)
;; (require 'init-git-lisp)
(provide 'init)
;;; init.el ends here
