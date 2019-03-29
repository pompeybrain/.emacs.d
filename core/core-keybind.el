;;; core-keybind ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(bind-key "M-q" #'save-buffers-kill-terminal)

(bind-key "M-c" #'icopy)
(bind-key "M-y" #'yank)
(bind-key "M-n" #'next-line)
(bind-key "M-p" #'previous-line)
(bind-key "M-s" #'save-buffer)
(bind-key "M-j" 'delete-indentation)
(bind-key "C-r" #'query-replace)
(bind-key "C-M-f" 'toggle-frame-fullscreen)
(bind-key "C-c C-k" 'kill-current-buffer)

(global-set-key (kbd "<swipe-left>") nil)
(global-set-key (kbd "<swipe-right>") nil)

(require 'fly-keys)
(xah-fly-keys 1)

(provide 'core-keybind)
;;; core-keybind ends here
