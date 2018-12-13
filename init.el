(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-ui)

(require 'package)
(add-to-list 'package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
 				 ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)


;; (require 'init-settings)

;; (require 'init-packages)
;; (require 'init-manages)
