;;; tools ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

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

(use-package magit
  :ensure t
  :defer t
  :init
  (setq transient-history-file (concat user-local-directory "transient-history.el")
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind("C-x g" . magit-status))


(provide 'tools)
;;; tools ends here