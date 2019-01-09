;; -*- lexical-binding: t -*-
(setq inhibit-startup-screen t)
(load-theme 'zenburn t)

;; set title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭菜单栏
;; (menu-bar-mode t)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)
(setq-default linum-format "%3d  ")

;; set font
(set-face-attribute 'default nil :family "Consolas")
(set-face-attribute 'default nil :height 160)
(set-face-attribute 'default nil :weight 'normal)
(setq-default line-spacing 4)

;; set cursor
(setq-default cursor-type '(bar . 2))

;; set pair match face
;; (set-face-background 'show-paren-match "#55575e")

;; (let ((no-border '(internal-border-width . 0)))
  ;; (add-to-list 'default-frame-alist no-border)
  ;; (add-to-list 'initial-frame-alist no-border))

;; 左右边界
(fringe-mode 0)

(provide 'init-ui)
;;; init-ui.el ends here
