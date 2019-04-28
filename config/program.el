;;; program ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :ensure t
  :defer 1
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :defer 1
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-require-match nil
        company-show-numbers t)
  :config
  (global-company-mode 1)
  (company-quickhelp-mode 1)            
  )

(use-package company-quickhelp
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer 1
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)
        flycheck-indication-mode nil
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-temp-prefix "/tmp/flycheck"
        flycheck-global-modes '(not emacs-lisp-mode lisp-interaction-mode scss-mode))
  :config
  (global-flycheck-mode +1))

(provide 'program)
;;; program ends here
