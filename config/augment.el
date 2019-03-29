;;; augment ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package shut-up
  :ensure t
  :defer t)

(use-package all-the-icons
  :defer t
  :ensure t)

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
  ;; :bind (:map ivy-minibuffer-map
  ;;             ("RET" . ivy-alt-done))
  )

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

(use-package which-key
  :ensure t
  :defer 1
  :diminish
  :config
  (which-key-mode t))

(use-package hl-todo
  :ensure t
  :defer 2
  :diminish
  :config
  (global-hl-todo-mode +1))

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
  (setq smex-save-file (concat user-local-directory "smex-items")))

(use-package projectile
  :ensure t
  :defer t
  :diminish
  :init
  (setq projectile-cache-file (concat user-local-directory "projectile.cache")
        projectile-known-projects-file (concat user-local-directory "projectile-bookmarks.eld"))
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


(use-package helpful
  :ensure t
  :defer t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h C" . helpful-command))

(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-major-mode-icon nil)
  :hook (after-init . doom-modeline-init))

(provide 'augment)
;;; augment ends here
