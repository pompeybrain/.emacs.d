;; set company
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; set yasnippet
(yas-global-mode 1)

(add-to-list 'exec-path "/usr/local/bin/")

(setq scheme-program-name "chez")
(setq geiser-chez-binary "chez")
(setq geiser-active-implementations '(chez))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "C-c j") 'ace-jump-mode)

(require 'lsp-mode)

(add-hook 'prog-mode-hook 'lsp)

(require 'init-helm)

;;; (add-hook 'scheme-mode-hook 'run-geiser)
(provide 'init-manages)
