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
  :config
  (setq company-minimum-prefix-length 2)
  (setq-default company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-require-match nil)
  (global-company-mode 1)
  ;; (company-quickhelp-mode 1)            
  )

;; (use-package company-quickhelp
;;   :ensure t
;;   :defer t)

(use-package flycheck
  :ensure t
  :defer 1
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode nil
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-global-modes '(not emacs-lisp-mode))
  (global-flycheck-mode +1))

(provide 'program)
;;; program ends here
