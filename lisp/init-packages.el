(require 'package)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa-stable/") t)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar pompey-packages '(
			  ;; helpful plugins
			  helm
			  rainbow-delimiters
			  paredit
			  neotree
			  all-the-icons
			  company
			  yasnippet
	      		  ) "Default packages")
(setq packages-selected-packages pompey-packages)

(defun pompey-packages-installed-p ()
  (loop for pkg in pompey-packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (pompey-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg pompey-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'init-packages)
