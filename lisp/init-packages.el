;;; init-packages.el ;; -*- lexical-binding: t -*-
;;; package -- Summary
;;; Commentary:
;;; Code:
(defvar emacs-d)

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :init
  (add-hook
   'after-init-hook
   (lambda ()
     (diminish 'visual-line-mode)
     (diminish 'eldoc-mode)
     (diminish 'flymake-mode)))
  (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode))))

;; need global install eslint tslint
(use-package flycheck
  :ensure t
  :defer 1
  :diminish
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-global-modes '(not emacs-lisp-mode))
  (global-flycheck-mode +1))

(use-package company-quickhelp
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer 1
  :diminish
  :config
  (setq company-minimum-prefix-length 2)
  (setq-default company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-require-match nil)
  (global-company-mode 1)
  (company-quickhelp-mode 1)
  )

(use-package all-the-icons
  :defer t
  :ensure t)

;;;###autoload
;; (defun iopen-dir-tree ()
;;   "Smart open dir tree, use treemacs."
;;   (interactive)
;;   (pcase (treemacs-current-visibility)
;;     ('visible (if
;;                   t
;;                   (treemacs--select-visible-window)
;;                 ()
;;                 ))
;;     ('exists  (treemacs--select-not-visible-window))
;;     ('none    (treemacs--init)))
;;   ;; TODO: need read treemacs src to know how to get treemacs status
;;   )
(defvar winum-keymap)
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
          treemacs-width                      40)
    (treemacs-resize-icons 14)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (treemacs-git-mode 'simple)
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
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  ;; (setq-default tide-format-options
  ;; 		'(:indentSize 2 :tabSize: 2 :ConvertTabsToSpaces t))
  )

;; aligns annotation to the right hand side
(use-package tide
  :ensure t
  :diminish
  :defer t
  :init
  (setq tide-hl-identifier-idle-time 10)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq-default scheme-program-name "chez")
  (setq-default geiser-chez-binary "chez")
  (setq-default geiser-active-implementations '(chez))
  (setq-default geiser-mode-start-repl-p t))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :defer t
  :diminish
  :init
  (setq ivy-initial-inputs-alist nil)
  (add-hook 'after-init-hook (ivy-mode 1))
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done)))

(use-package swiper
  :ensure t
  :defer t)

(use-package counsel
  :ensure t
  :defer t
  :init
  ;; (setq counsel-grep-base-command
  ;; 	"rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind(
        ;; ("C-s" . counsel-grep-or-swiper)
        ("M-x" . counsel-M-x)
        ;; ("C-c f" . counsel-find-file)
        ;; ("M-r" . counsel-recentf)
        )
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
  (setq transient-history-file "~/.emacs.d/.cache/transient-history.el")
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind("C-x g" . magit-status))

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
  (editorconfig-mode +1))

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
  :defer 2
  :init
  (setq smex-save-file (concat emacs-d ".cache/smex-items")))

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
  )

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

(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-major-mode-icon nil)
  :hook (after-init . doom-modeline-init))

(use-package yasnippet
  :ensure t
  :defer 1
  :config
  (yas-global-mode 1))

;; popup
;; mac childframe bug need more test on macport or use code from lazycat
(use-package posframe
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t
  :mode
  ("\\.json\\'"))

(use-package shut-up
  :ensure t
  :defer t)

(provide 'init-packages)
;;; init-packages.el ends here
