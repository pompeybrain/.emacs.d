(load-theme 'atom-one-dark t)

(setq inhibit-startup-screen t)

;; set title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; (setq frame-title-format '("%b"))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭菜单栏
(menu-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)


;; 显示行号
(global-linum-mode 1)

;; set font
(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :height 150)


;; 关闭启动帮助画面
;; (setq inhibit-splash-screen 1)

(provide 'init-ui)
