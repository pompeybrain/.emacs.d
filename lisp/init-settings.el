;;;关闭自动保存
(setq auto-save-default nil)

;; 关闭自动生成备份
(setq make-backup-files nil)

;;;自动加载更改过的文件
(global-auto-revert-mode 1)

;;;别名
(fset 'yes-or-no-p 'y-or-n-p)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭菜单栏
(menu-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(provide 'init-settings)
