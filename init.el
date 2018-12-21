;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/" t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/" t)

(require 'init-ui)

;;; (setq show-paren-mode t)
;;; (setq electric-pair-mode t)

(require 'init-settings)

(require 'init-packages)

(require 'init-manages)


(show-paren-mode t)
(electric-pair-mode t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-set-key (kbd "C-x d") 'neotree-dir)
;; some package short config

(require 'init-site-lisp)

