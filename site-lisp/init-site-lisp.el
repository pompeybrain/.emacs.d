;; init some el file in site-lisp

;; (require 'thing-edit)

;; (use-package flymake-diagnostic-at-point
;;   :after flymake
;;   :init
;;   (setq flymake-diagnostic-at-point-error-prefix "Error: ")
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; (require 'flycheck-popup-tip)		

;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(require 'color-rg)

(provide 'init-site-lisp)
