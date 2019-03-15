;;; core-package.el --- package management system -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defvar doom-core-packages
  '(use-package quelpa async)
  "A list of packages that must be installed (and will be auto-installed if missing) and shouldn't be deleted.")

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

(provide 'core-package)
;;; core-package.el ends here
