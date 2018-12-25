(require 'package)
(add-to-list 'package-archives
						 '("melpa" . "http://elpa.emacs-china.org/melpa/") t)

(package-initialize)

(eval-when-compile
	(require 'use-package))

(use-package delight
	:ensure t)

(use-package auto-package-update
	:ensure t
	:config
	(setq auto-package-update-delete-old-version t)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe))

(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode))

(use-package company
	:ensure t
	:delight
	:config
	(global-company-mode)
	:bind (:map company-active-map
							("C-n" . 'company-select-next-or-abort)
							("C-p" . 'company-select-previous-or-abort)
							))

(use-package all-the-icons
	:ensure t)

(use-package neotree
	:ensure t
	:config
	(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	:bind ("C-c d". neotree-dir))

(use-package typescript-mode
	:ensure t)

(use-package geiser
	:ensure t
	:config
	(setq scheme-program-name "chez")
	(setq geiser-chez-binary "chez")
	(setq geiser-active-implementations '(chez)))

(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-jump-mode
	:ensure t
	:bind ("C-c j". ace-jump-mode))

(use-package helm
	:ensure t
	:delight
	:config
	(helm-mode t)
	(setq helm-split-window-in-side-p           t
				helm-buffers-fuzzy-matching           t
				helm-move-to-line-cycle-in-source     t
				helm-ff-search-library-in-sexp        t
				helm-ff-file-name-history-use-recentf t)
	:bind (("M-x" . helm-M-x)
				 ("C-c o" . helm-occur)
				 ("C-c f" . helm-find-files)))

(use-package exec-path-from-shell
	:ensure t
	:config
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(use-package eglot
	:ensure t)

;; (use-package lsp-mode
;; 	:ensure t
;; 	:commands lsp
;; 	:hook(prog-mode . lsp))

;; (use-package lsp-ui
;; 	:ensure t
;; 	:commands lsp-ui-mode)

;; (use-package company-lsp
;; 	:ensure t
;; 	:commands company-lsp)


;; ;; cl - Common Lisp Extension
;; (require 'cl)

;; ;; Add Packages
;; (defvar pompey-packages '(
;; 			  ;; helpful plugins
;; 			  helm
;; 			  rainbow-delimiters
;; 			  paredit
;; 			  neotree
;; 			  all-the-icons
;; 			  company
;; 			  yasnippet
;; 			  ;; lsp-mode
;; 			  ;; lsp-ui
;; 			  ;; company-lsp
;; 			  ace-jump-mode
;; 			  ) "Default packages")
;; (setq packages-selected-packages pompey-packages)

;; (defun pompey-packages-installed-p ()
;;   (loop for pkg in pompey-packages
;; 	when (not (package-installed-p pkg)) do (return nil)
;; 	finally (return t)))

;; (unless (pompey-packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg pompey-packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))

(provide 'init-packages)
