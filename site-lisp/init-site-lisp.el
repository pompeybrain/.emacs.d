;; init some el file in site-lisp

;; (require 'thing-edit)

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(require 'flycheck-posframe)
;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(require 'color-rg)

(provide 'init-site-lisp)
