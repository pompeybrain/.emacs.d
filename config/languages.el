;;; languages ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;;; highlight buffer in tide-jump-to-definition has problem.
(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-strict-missing-semi-warning nil))

(defun setup-tide-mode ()
  "Setup tide config."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (company-mode +1)
  (setq tab-width 2)
  (setq company-tooltip-align-annotations t)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :diminish
  :defer t
  :init
  (setq tide-hl-identifier-idle-time 10)
  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
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

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-current-element-highlight t)
  :mode "\\.html\\'")

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode))

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

(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  ("\\.md\\'"))

(use-package json-mode
  :ensure t
  :defer t
  :mode
  ("\\.json\\'"))

;;; builtin package
(setq css-indent-offset 2)

(provide 'languages)
;;; languages ends here
