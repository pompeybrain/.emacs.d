
;; theme
(load-theme 'spacemacs-dark 1)
;; set company
(add-hook 'after-init-hook 'global-company-mode)

;; set yasnippet
(yas-global-mode 1)

;; set smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; set ido
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
;; set major mode
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(provide 'init-manages)
