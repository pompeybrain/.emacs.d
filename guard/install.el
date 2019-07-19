;;; install.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(defvar doom-core-packages
  '(use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if missing) and shouldn't be deleted.")

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

(defun install-package (packages)
  "Install packages list PACKAGES provide."
  (let ((not-installed-packages (cl-remove-if #'package-installed-p packages)))
    (when not-installed-packages
      (message "Installing packages:")
      (package-refresh-contents)
      (dolist (package not-installed-packages)
        (let ((inhibit-message t))
          (package-install package))
        (if (package-installed-p package)
            (message "✓ Installed %s" package)
          (error "✕ Couldn't install %s" package)))
      (message "Installing packages...done"))))

(provide 'install)
;;; install.el ends here

