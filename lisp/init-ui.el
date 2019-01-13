;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-
(load-theme 'zenburn t)

;; set font
(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :weight 'normal)
(setq-default line-spacing 3)

;; 左右边界
(fringe-mode 0)

;; set title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)
(setq-default linum-format "%4d ")
(set-face-attribute 'linum nil :weight 'normal)

;; set cursor
(setq-default cursor-type '(bar . 2))

;; audio
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(provide 'init-ui)
;;; init-ui.el ends here


;;; how to set frame size and position in mac os
;;; defaults write org.gnu.Emacs Width 150
;;; defaults write org.gnu.Emacs Height 50
;;; defaults write org.gnu.Emacs Top 150
;;; defaults write org.gnu.Emacs Left 400
