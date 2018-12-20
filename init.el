;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/" t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)

(require 'init-ui)

;;; (setq show-paren-mode t)
;;; (setq electric-pair-mode t)

(require 'init-settings)

(require 'init-packages)
(require 'init-manages)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-safe-themes
   (quote
    ("e4a7a2a1d9224751785732a33cc00debfa020ad65d02758bf916f1a8676e7310" default)))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   (quote
    (geiser yasnippet company neotree paredit rainbow-delimiters helm)))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(show-paren-mode t)
(electric-pair-mode t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-set-key (kbd "C-x d") 'neotree-dir)
;; some package short config
(require 'init-helm)
(require 'init-site-lisp)
