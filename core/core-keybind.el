;;; core-keybind ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'fly-edit)

(defun define-keymap-keys (keymap-name key-cmd-alist)
  "define keys in a keymap."
  (mapc
   (lambda (pair)
     (define-key keymap-name (kbd (car pair)) (cdr pair)))
   key-cmd-alist))


(defvar fly-keymap (make-sparse-keymap) "Keybinding map for command mode editing.")


(defvar fly-keymap-list
  '(
    ("'" . eval-last-sexp)
    (";" . move-end-of-line)
    ("0" . balance-windows)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . delete-window)
    ("4" . split-window-right)
    ("5" . fly-format)
    ("9" . select-string)
    ("a" . counsel-M-x)
    ("b" . xah-toggle-letter-case)
    ("c" . copy-line-or-region)
    ("d" . delete-char)
    ("e" . backward-kill-word)
    ("f" . deactivate-fly-command)
    ("g" . lookup-word-or-selection)
    ("h" . move-beginning-of-line)
    ("i" . previous-line)
    ("j" . backward-char)
    ("k" . next-line)
    ("l" . forward-char)
    ("m" . nil)
    ("n" . fly-next-20-lines)
    ("o" . forward-word)
    ("p" . fly-prev-20-lines)
    ("q" . save-buffer)
    ("r" . kill-word)
    ("s" . extend-selection)
    ("t" . set-mark-command)
    ("u" . backward-word)
    ("v" . yank)
    ("w" . xah-shrink-whitespaces)
    ("x" . xah-cut-line-or-region)
    ("y" . undo)
    ("." . nil)
    ("," . other-window)
    ("z" . comment-line-or-region)
    ;; ("/" . goto-matching-bracket)
    ))

(define-keymap-keys fly-keymap fly-keymap-list)

(define-minor-mode fly-command-mode
  "A command modal keybinding set."
  t "Fly" fly-keymap
  (progn
    ;; when going into minibuffer, deactive command mode.
    (add-hook 'minibuffer-setup-hook 'deactivate-fly-command)
    (add-hook 'minibuffer-exit-hook 'activate-fly-command))
  ;; (add-to-list 'emulation-mode-map-alists '((cons xah-fly-keys xah-fly-key-map )))
  )

(defun deactivate-fly-command ()
  (interactive)
  (fly-command-mode -1)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq mode-line-front-space "I")
  (force-mode-line-update)
  )

(defun activate-fly-command ()
  (interactive)
  (fly-command-mode 1)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq mode-line-front-space "C")
  (force-mode-line-update)
  )


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

;; (require 'fly-keys)
;; (xah-fly-keys 1)


(provide 'core-keybind)
;;; core-keybind ends here
