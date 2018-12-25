;; set company
;; (add-hook 'after-init-hook 'global-company-mode)

;; (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort) 
;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; set yasnippet
;; (yas-global-mode 1)

;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (global-set-key (kbd "C-x d") 'neotree-dir)



;;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; (global-set-key (kbd "C-c j") 'ace-jump-mode)

(require 'init-helm)

;;; (add-hook 'scheme-mode-hook 'run-geiser)
(provide 'init-manages)
