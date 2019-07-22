;;; fly-keys.el -*- coding: utf-8; lexical-binding: t; -*-

;;; modified xah-fly-keys to my and  renamed fly-keys

;;; Thanks xahlee: https://github.com/xahlee/xah-fly-keys

;;; Commentary:

;;; fly-keys is a modal mode like vi, key choices is decided by myself and only support qwerty layout keybord.

;;; This file is not part of GNU Emacs.

;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs

(require 'xah-helper)     ; xahlee's helper package
(require 'fly-edit)       ; my package
(require 'lookup)         ;my package

(defvar fly-command-mode-activate-hook nil "Hook for `fly-command-mode-activate'")
(defvar fly-insert-mode-activate-hook nil "Hook for `fly-insert-mode-activate'")

(defvar fly-use-control-key t "if nil, do not bind any control key. When t, standard keys for open, close, paste, are bound.")
(defvar fly-use-meta-key t "if nil, do not bind any meta key.")

(defun fly-define-keys (@keymap-name @key-cmd-alist)
  "Map `define-key' over a alist @key-cmd-alist."
  (interactive)
  (mapc
   (lambda ($pair)
     (define-key @keymap-name (kbd (car $pair)) (cdr $pair)))
   @key-cmd-alist))

(defvar fly-key-map (make-sparse-keymap) "Keybinding for `fly-keys' minor mode.")

(defvar fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq fly-insert-state-q t)

(defun fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun fly-command-mode-activate ()
  "Activate command mode and run `fly-command-mode-activate-hook'
Version 2017-07-07"
  (interactive)
  (fly-command-mode-init)
  (run-hooks 'fly-command-mode-activate-hook))

(defun fly-command-mode-activate-no-hook ()
  "Activate command mode. Does not run `fly-command-mode-activate-hook'
Version 2017-07-07"
  (interactive)
  (fly-command-mode-init))

(defun fly-insert-mode-activate ()
  "Activate insertion mode.
Version 2017-07-07"
  (interactive)
  (fly-insert-mode-init)
  (run-hooks 'fly-insert-mode-activate-hook))

(defun show-fly-keymap ()
  "Show fly-keys keymap use which-key."
  (interactive)
  (which-key-show-keymap 'fly-key-map))

(defun fly-keys-off ()
  "Turn off fly-keys minor mode."
  (interactive)
  (progn
    (remove-hook 'minibuffer-setup-hook 'fly-insert-mode-activate)
    (remove-hook 'minibuffer-exit-hook 'fly-command-mode-activate)
    (remove-hook 'fly-command-mode-activate-hook 'fly-save-buffer-if-file)
    (remove-hook 'shell-mode-hook 'fly-insert-mode-activate))
  (fly-insert-mode-activate)
  (fly-keys 0))

(define-minor-mode fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout.
URL `http://ergoemacs.org/misc/ergoemacs_vi_mode.html'"
  t "Flykeys" fly-key-map
  (progn
    (add-hook 'minibuffer-setup-hook 'fly-insert-mode-activate)
    (add-hook 'minibuffer-exit-hook 'fly-command-mode-activate)
    (add-hook 'fly-command-mode-activate-hook 'fly-save-buffer-if-file))
  (fly-command-mode-activate))

(defun fly-command-mode-init ()
  "Set command mode keys."
  (interactive)
  (mapc
   (lambda ($pair)
     (define-key fly-key-map (kbd (car $pair)) (cdr $pair)))
   fly-command-mode-keys)
  (progn
    (setq fly-insert-state-q nil )
    (modify-all-frames-parameters (list (cons 'cursor-type 'box))))
  (setq mode-line-front-space "C")
  (force-mode-line-update))

(defun fly-insert-mode-init ()
  "Set insertion mode keys"
  (interactive)
  (mapc
   (lambda ($pair)
     (define-key fly-key-map (kbd (car $pair)) nil))
   fly-command-mode-keys)
  (progn
    (setq fly-insert-state-q t )
    (modify-all-frames-parameters (list (cons 'cursor-type 'bar))))
  (setq mode-line-front-space "I")
  (force-mode-line-update))

(fly-define-keys
 (define-prefix-command 'fly-dot-keymap)
 '(("." . highlight-symbol-at-point)
   ("g" . unhighlight-regexp)
   ("c" . highlight-lines-matching-regexp)
   ("h" . highlight-regexp)
   ("t" . highlight-phrase)
   ("p" . isearch-forward-symbol-at-point)))

(fly-define-keys
 (define-prefix-command 'fly-c-keymap)
 '(("," . xah-open-in-external-app)
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("e" . ibuffer)
   ("u" . xah-open-file-at-cursor)
   ("h" . recentf-open-files)
   ("i" . xah-copy-file-path)
   ("l" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("o" . xah-show-in-desktop)
   ("p" . xah-open-last-closed)
   ("f" . xah-open-recently-closed)
   ("y" . xah-list-recently-closed)
   ("r" . xah-open-file-fast)
   ("s" . write-file)))

(fly-define-keys
 (define-prefix-command 'fly-y-keymap)
 '(("RET" . insert-char)
   ("SPC" . xah-insert-unicode)
   ("b" . xah-insert-black-lenticular-bracket【】)
   ("c" . xah-insert-ascii-single-quote)
   ("d" . xah-insert-double-curly-quote“”)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace) ; {}
   ("i" . xah-insert-curly-single-quote‘’)
   ("l" . xah-insert-formfeed)
   ("m" . xah-insert-corner-bracket「」)
   ("n" . xah-insert-square-bracket) ; []
   ("p" . xah-insert-single-angle-quote‹›)
   ("r" . xah-insert-tortoise-shell-bracket〔〕 )
   ("s" . xah-insert-string-assignment)
   ("t" . xah-insert-paren)
   ("u" . xah-insert-date)
   ("w" . xah-insert-angle-bracket〈〉)
   ("y" . xah-insert-double-angle-quote«»)
   ("W" . xah-insert-double-angle-bracket《》)))

(fly-define-keys
 (define-prefix-command 'fly-e-keymap)
 '(("b" . eval-buffer)
   ("d" . fly-js-dev)
   ("r" . eval-region)
   ("l" . eval-last-sexp)))

(fly-define-keys
 (define-prefix-command 'fly-h-keymap)
 '(
   (";" . Info-goto-emacs-command-node)
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("h" . describe-function)
   ("i" . info)
   ("j" . man)
   ("k" . describe-key)
   ("K" . Info-goto-emacs-key-command-node)
   ("l" . view-lossage)
   ("m" . xah-describe-major-mode)
   ("n" . describe-variable)
   ("o" . describe-language-environment)
   ("p" . finder-by-keyword)
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ("u" . elisp-index-search)
   ("v" . apropos-value)
   ("z" . describe-coding-system)))

(fly-define-keys
 ;; commands here are “harmless”, they don't modify text etc.
 ;; they turn on minor/major mode, change display, prompt, start shell, etc.
 (define-prefix-command 'fly-n-keymap)
 '(
   ("SPC" . whitespace-mode)
   ;; RET
   ;; TAB
   ;; DEL
   ("," . abbrev-mode)
   ("." . toggle-frame-fullscreen)
   ("'" . frame-configuration-to-register)
   (";" . window-configuration-to-register)
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("5" . visual-line-mode)
   ("6" . calendar)
   ("7" . calc)
   ;; 8
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ;; f
   ("g" . xah-toggle-read-novel-mode)
   ("h" . widen)
   ("i" . make-frame-command)
   ("j" . flyspell-buffer)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ;; m
   ("n" . narrow-to-region)
   ("o" . variable-pitch-mode)
   ("p" . read-only-mode)
   ;; q
   ;; r
   ;; s
   ("t" . narrow-to-defun)
   ("u" . shell)
   ;; v
   ("w" . eww)
   ("x" . save-some-buffers)
   ;; y
   ("z" . abort-recursive-edit)))

(fly-define-keys
 ;; kinda replacement related
 (define-prefix-command 'fly-r-keymap)
 '(
   ("SPC" . rectangle-mark-mode)
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("3" . number-to-register)
   ("4" . increment-register)
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("e" . call-last-kbd-macro)
   ("g" . kill-rectangle)
   ("l" . clear-rectangle)
   ("i" . xah-space-to-newline)
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("p" . kmacro-end-macro)
   ("r" . yank-rectangle)
   ("u" . xah-quote-lines)
   ("y" . delete-whitespace-rectangle)))

(fly-define-keys
 (define-prefix-command 'fly-t-keymap)
 '(
   ("SPC" . xah-clean-whitespace)
   ("TAB" . move-to-column)

   ("1" . xah-append-to-register-1)
   ("2" . xah-clear-register-1)

   ("3" . xah-copy-to-register-1)
   ("4" . xah-paste-from-register-1)

   ("8" . xah-clear-register-1)
   ("7" . xah-append-to-register-1)

   ("." . sort-lines)
   ("," . sort-numeric-fields)
   ("'" . reverse-region)
   ;; a
   ;; b
   ("c" . goto-char)
   ("d" . mark-defun)
   ("e" . list-matching-lines)
   ("f" . goto-line )
   ;; g
   ("h" . xah-close-current-buffer )
   ("i" . delete-non-matching-lines)
   ("j" . copy-to-register)
   ("k" . insert-register)
   ("l" . xah-escape-quotes)
   ("m" . xah-make-backup-and-save)
   ("n" . repeat-complex-command)
   ;; o
   ("p" . query-replace-regexp)
   ;; q
   ("r" . copy-rectangle-to-register)
   ;; s
   ("t" . repeat)
   ("u" . delete-matching-lines)
   ;; v
   ("w" . xah-next-window-or-frame)
   ;; x
   ("y" . delete-duplicate-lines)
   ;; z
   ))

(fly-define-keys
 (define-prefix-command 'fly-w-keymap)
 '(
   ("DEL" . xah-delete-current-file)
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
   ("w" . delete-frame)
   ("j" . xah-run-current-file)))

(fly-define-keys
 (define-prefix-command 'fly-leader-key-map)
 '(
   ;; ("SPC" . fly-insert-mode-activate)
   ;; ("RET" . execute-extended-command)
   (";" . nil)
   ("," . fly-w-keymap)
   ("0" . delete-window)
   ("1" . delete-other-windows)
   ("2" . split-window-below)
   ("3" . nil)
   ("4" . split-window-right)
   ("5" . balance-windows)
   ("6" . xah-upcase-sentence)
   ("7" . nil)
   ("8" . ispell-word)
   ("9" . auto-save-visited-mode)
   (";" . nil)
   ("a" . mark-whole-buffer)
   ("b" . nil)
   ("c" . hs-toggle-hiding)
   ("d" . treemacs)
   ("e" . fly-e-keymap)
   ("f" . find-file)
   ("g" . ace-jump-mode)
   ("h" . beginning-of-buffer)
   ("i" . fly-c-keymap)
   ("j" . fly-h-keymap)
   ("k" . show-fly-keymap)
   ("l" . recenter-top-bottom)
   ("m" . counsel-imenu)
   ("n" . end-of-buffer)
   ("o" . fly-r-keymap)
   ("p" . counsel-projectile-find-file)
   ("q" . fly-save-all-buffer)
   ("r" . counsel-recentf)
   ("s" . counsel-rg)
   ("t" . xah-show-kill-ring)
   ("u" . counsel-projectile-switch-project)
   ("v" . magit-status)
   ("w" . nil)
   ("x" . nil)
   ("y" . nil)
   ("z" . nil)))

(define-key fly-key-map (kbd "M-SPC") 'fly-command-mode-activate-no-hook)

(defvar fly-command-mode-keys
  '(("SPC" . fly-leader-key-map)
    ("'" . eval-last-sexp)
    ;; ("[" . hippie-expand)              
    ;; ("-" . xah-backward-punct )
    ;; ("=" . xah-forward-punct)
    ;; ("`" . other-frame)
    (";" . xah-end-of-line-or-block)
    ("." . xah-select-text-in-quote)
    ("," . xah-next-window-or-frame)
    ("/" . xah-goto-matching-bracket)
    ("0" . balance-windows)
    ("1" . delete-other-windows)
    ("2" . split-window-below)
    ("3" . delete-window)
    ("4" . split-window-right)
    ("5" . fly-format)          
    ;; ("6" . xah-select-block)           
    ("7" . xah-select-line)
    ;; ("8" . xah-extend-selection)
    ("a" . counsel-M-x)
    ("b" . fly-format)
    ("c" . xah-copy-line-or-region)
    ("d" . delete-char)
    ("e" . xah-backward-kill-word)
    ("f" . fly-insert-mode-activate)
    ("g" . lookup)
    ("h" . xah-beginning-of-line-or-block)
    ("i" . previous-line)
    ("j" . backward-char)
    ("k" . next-line)
    ("l" . forward-char)
    ("m" . switch-to-buffer)
    ("n" . fly-next-20-lines)
    ("o" . forward-word)
    ("p" . fly-prev-20-lines)
    ("q" . quiet-save)
    ("r" . xah-kill-word)
    ("s" . xah-extend-selection)
    ("t" . set-mark-command)
    ("u" . backward-word)
    ("v" . xah-paste-or-paste-previous)
    ("w" . xah-shrink-whitespaces)
    ("x" . xah-cut-line-or-region)
    ("y" . undo)
    ("z" . xah-comment-dwim))
  "alist of key and it's command in command mode.")

(provide 'fly-keys)
;;; fly-keys.el ends here
