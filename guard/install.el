;;; install.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-refresh-contents)

(defun install-packages ()
  (message "Removing installed packages: ")
  (delete-directory "elpa" t)
  (message "Starting install packages: ")
  (package-reinstall 'use-package)
  (load-file "init.el"))

(install-packages)

(provide 'install)
;;; install.el ends here
