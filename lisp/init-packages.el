;;  __        __             __   ___
;; |__)  /\  /  ` |__/  /\  / _` |__
;; |    /~~\ \__, |  \ /~~\ \__> |___
;;                      __   ___        ___      ___
;; |\/|  /\  |\ |  /\  / _` |__   |\/| |__  |\ |  |
;; |  | /~~\ | \| /~~\ \__> |___  |  | |___ | \|  |
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives '("melpa" . "http://elpa.emacs-china.org/melpa-stable/") 1))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar pompey-packages '(
			  ;; --- Major Mode ---
			  web-mode
			  js2-mode
			  ;; --- Minor Mode ---
			  neotree
			  company
			  smex
			  yasnippet
			  ido-ubiquitous
			  ido-vertical-mode
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
