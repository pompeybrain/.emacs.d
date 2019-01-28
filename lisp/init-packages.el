;;; package -- Summary
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :init
  (add-hook 'after-init-hook
	    (lambda ()
	      (diminish 'visual-line-mode)
	      (diminish 'eldoc-mode)
	      (diminish 'flymake-mode))))

(use-package auto-package-update
  :ensure t
  :defer 2
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (global-flycheck-mode +1)
  (flycheck-pos-tip-mode +1))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5))

(use-package company
  :ensure t
  :defer 1
  :diminish
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (global-company-mode)
  ;; :bind (:map company-active-map
	      ;; ("C-n" . 'company-select-next-or-abort)
	      ;; ("C-p" . 'company-select-previous-or-abort)
	      ;; )
  )

(use-package all-the-icons
  :defer t
  :ensure t)

;;;###autoload
(defun iopen-dir-tree ()
  "Smart open dir tree, use treemacs."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (if
		  t
		  (treemacs--select-visible-window)
		()
		))
    ('exists  (treemacs--select-not-visible-window))
    ('none    (treemacs--init)))
  ;; TODO: need read treemacs src to know how to get treemacs status
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-show-hidden-files          t
          treemacs-width                      30)
    (treemacs-resize-icons 14)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-git-mode 'simple)
    (setq treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
          treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face))
    ;; (add-to-list treemacs-ignored-file-predicates (lambda ())) TODO: add ignore dirs and files
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))
      ))
  :bind
  (:map global-map
        ("C-x d"     . treemacs)
        ("M-0"       . treemacs-select-window))
  (:map treemacs-mode-map
	("C" . treemacs-create-file)
	("RET" . treemacs-visit-node-no-split)
	))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil))

(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;;;###autoload
(defun setup-tide-mode ()
  "Setup tide config."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (company-mode +1)
  (setq tab-width 2)
  (setq company-tooltip-align-annotations t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  ;; (add-hook 'before-save-hook 'tide-format-before-save);; TODO: work with tide
  ;; (setq-default tide-format-options
  ;; 		'(:indentSize 2 :tabSize: 2 :ConvertTabsToSpaces t))
  )

;; aligns annotation to the right hand side
(use-package tide
  :ensure t
  :diminish
  :defer t
  :init
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

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
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind ("M-SPC". ace-jump-mode))

(use-package ivy
  :ensure t
  :defer t
  :diminish
  :init
  (setq ivy-initial-inputs-alist nil)
  (add-hook 'after-init-hook (ivy-mode 1))
  :bind
  (:map ivy-mode-map
	("RET" . #'ivy-alt-done)))

(use-package swiper
  :ensure t
  :defer t)

(use-package counsel
  :ensure t
  :defer t
  :init
  ;; (setq counsel-grep-base-command
  ;; 	"rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind(("C-s" . counsel-grep-or-swiper)
        ("M-x" . counsel-M-x)
        ("C-c f" . counsel-find-file)
        ("M-r" . counsel-recentf)
        ("C-c s" . counsel-rg))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind("C-x g" . #'magit-status))

(use-package which-key
  :ensure t
  :defer 1
  :diminish
  :config
  (which-key-mode t))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  ;; debug
  ;; (add-hook 'editorconfig-after-apply-functions
  ;; 	    (lambda (props)
  ;; 	      (message "editorconfig applied\n")
  ;; 	      (maphash (lambda (key value)
  ;; 			 (message "%s : %s"  key value)) props)))
  (editorconfig-mode +1))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package hl-todo
  :ensure t
  :defer 2
  :diminish
  :config
  (global-hl-todo-mode +1))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  :mode "\\.html\\'")

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode))

(use-package diff-hl
  :ensure t
  :defer 1
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode +1))

(use-package smex
  :ensure t
  :defer 2)

(use-package projectile
  :ensure t
  :defer t
  :diminish
  :init
  (setq projectile-cache-file (expand-file-name ".cache/projectile.cache" emacs-d))
  (setq projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld" emacs-d))
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile
  :ensure t
  :defer t
  :diminish
  :config
  (counsel-projectile-mode +1)
  :bind-keymap ("C-x p" . projectile-command-map)
  :bind ("C-x f" . #'counsel-projectile-find-file))

(use-package hydra
  :ensure t
  :defer 1)

(use-package dart-mode
  :ensure t
  :defer t
  :mode
  ("\\.dart\\'"))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yaml\\'"))

(use-package eglot
  :ensure t
  :defer t
  :init
  (add-hook 'dart-mode-hook 'eglot-ensure)
  :bind
  ("M-i" . eglot-help-at-point))

(use-package popup
  :ensure t
  :defer t)

(use-package helpful
  :ensure t
  :defer t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h C" . helpful-command))

;; web-mode highlight bug
(use-package vue-html-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  ("\\.md\\'"))

(provide 'init-packages)
;;; init-packages.el ends here
