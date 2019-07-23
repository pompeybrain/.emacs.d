;;; languages ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :defer t
  :init
   (add-hook 'js-mode-hook 'js2-minor-mode)
  :config
  (setq js2-strict-missing-semi-warning nil))

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
  :init
  (setq dart-format-on-save t
        dart-formatter-show-errors 'echo)
  :mode
  ("\\.dart\\'"))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yaml\\'"))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :init
  (setq lsp-session-file (concat user-local-directory "lsp-sessions")
        lsp-auto-guess-root t
        lsp-prefer-flymake nil
        lsp-auto-execute-action nil
        lsp-eldoc-enable-hover nil)
  :hook ((dart-mode typescript-mode js2-mode) . lsp))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-flycheck-enable t
        lsp-ui-peek-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 1
        lsp-ui-doc-include-signature nil))

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp)

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
