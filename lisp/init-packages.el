;;; Code:
(eval-when-compile
	(require 'use-package))

(use-package delight
	:ensure t)

(use-package auto-package-update
	:ensure t
	:config
	(setq auto-package-update-delete-old-versions t)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe))

(use-package flycheck
	:ensure t
	:delight
	:config
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
	:defer t
	:ensure t)

(use-package neotree
	:ensure t
	:bind ("C-c d". neotree-dir)
	:config
	(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	(setq neo-autorefresh nil)
	(setq neo-show-hidden-files t)
	(add-to-list 'neo-hidden-regexp-list "\\.git") ;;; doesn't work
	)

(use-package js2-mode
	:ensure t
	:mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(defun setup-ts-mode()
	"Setup typescript mode configs."
	(setq tab-always-indent nil))

(use-package tide
	:ensure t
	:after (typescript-mode js2-mode company flycheck)
	:hook(
				((typescript-mode js2-mode) . tide-setup)
				((typescript-mode js2-mode) . tide-hl-identifier-mode)))

(add-hook 'js2-mode-hook (lambda () "setup tide for js2-mode" (interactive) (tide-setup)))

;; Prettier-emacs can't surpport config files, and need
;; config to use node_modules/bin/prettier, so don't use it temporarily.
;; (use-package  prettier-js
;; 	:ensure t
;; 	:hook((typescript-mode . prettier-js-mode)
;; 				(js2-mode . prettier-js-mode)))

(use-package geiser
	:ensure t
	:defer t
	:config
	(setq scheme-program-name "chez")
	(setq geiser-chez-binary "chez")
	(setq geiser-active-implementations '(chez))
  (setq geiser-mode-start-repl-p t))

(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-jump-mode
	:ensure t
	:bind ("C-c j". ace-jump-mode))

(use-package helm
	:ensure t
	:delight
	:init
	(add-hook 'after-init-hook 'helm-mode t)
	:bind (("M-x" . helm-M-x)
				 ("C-c o" . helm-occur)
				 ("C-c f" . helm-find-files)
				 ("C-c r". helm-recentf))
	:config
	(require 'helm-config)
	(setq helm-split-window-inside-p            t
				helm-buffers-fuzzy-matching           t
				helm-move-to-line-cycle-in-source     t
				helm-ff-search-library-in-sexp        t
				helm-ff-file-name-history-use-recentf t))

(use-package exec-path-from-shell
	:ensure t
	:config
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(use-package eglot
	:ensure t
	:defer t)

(provide 'init-packages)
;;; init-packages.el ends here
