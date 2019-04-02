;;; core-keybind ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(require 'hydra)
(require 'fly-keys)


(bind-key "M-q" #'save-buffers-kill-terminal)

(bind-key "M-y" #'yank)
(bind-key "M-j" 'delete-indentation)
(bind-key "C-r" #'query-replace)
(bind-key "C-M-f" 'toggle-frame-fullscreen)
(bind-key "C-c C-k" 'kill-current-buffer)

(global-set-key (kbd "<swipe-left>") nil)
(global-set-key (kbd "<swipe-right>") nil)

(defhydra hydra-flycheck
  (:hint nil :foreign-keys warn)
  "Errors"
  ("k"  flycheck-next-error                                       "Next")
  ("i"  flycheck-previous-error                                   "Previous")
  ("j" flycheck-first-error                                      "First")
  ("l"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil "Quit"))

(defhydra hydra-smerge
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(xah-fly-keys 1)

(provide 'core-keybind)
;;; core-keybind ends here

;; (require 'fly-edit)

;; (defun define-keymap-keys (keymap-name key-cmd-alist)
;;   "define keys in a keymap."
;;   (mapc
;;    (lambda (pair)
;;      (define-key keymap-name (kbd (car pair)) (cdr pair)))
;;    key-cmd-alist))


;; (defvar fly-keymap (make-sparse-keymap) "Keybinding map for command mode editing.")


;; (defvar fly-keymap-list
;;   '(
;;     ("'" . eval-last-sexp)
;;     (";" . move-end-of-line)
;;     ("0" . balance-windows)
;;     ("1" . delete-other-windows)
;;     ("2" . split-window-below)
;;     ("3" . delete-window)
;;     ("4" . split-window-right)
;;     ("5" . fly-format)
;;     ("9" . select-string)
;;     ("a" . counsel-M-x)
;;     ("b" . xah-toggle-letter-case)
;;     ("c" . copy-line-or-region)
;;     ("d" . delete-char)
;;     ("e" . backward-kill-word)
;;     ("f" . deactivate-fly-command)
;;     ("g" . lookup-word-or-selection)
;;     ("h" . move-beginning-of-line)
;;     ("i" . previous-line)
;;     ("j" . backward-char)
;;     ("k" . next-line)
;;     ("l" . forward-char)
;;     ("m" . nil)
;;     ("n" . fly-next-20-lines)
;;     ("o" . forward-word)
;;     ("p" . fly-prev-20-lines)
;;     ("q" . save-buffer)
;;     ("r" . kill-word)
;;     ("s" . extend-selection)
;;     ("t" . set-mark-command)
;;     ("u" . backward-word)
;;     ("v" . yank)
;;     ("w" . shrink-whitespaces)
;;     ("x" . cut-line-or-region)
;;     ("y" . undo)
;;     ("." . nil)
;;     ("," . other-window)
;;     ("z" . comment-line-or-region)
;;     ) "Fly-command-keymap-alist.")

;; (define-keymap-keys fly-keymap fly-keymap-list)

;; (define-minor-mode fly-command-mode
;;   "A command modal keybinding set."
;;   :lighter "Fly"
;;   :keymap fly-keymap
;;   (progn
;;     ;; when going into minibuffer, deactive command mode.
;;     (add-hook 'minibuffer-setup-hook 'deactivate-fly-command)
;;     (add-hook 'minibuffer-exit-hook 'activate-fly-command))
;;   (add-to-list 'emulation-mode-map-alists '((cons fly-command-mode fly-keymap))))

;; (defun deactivate-fly-command ()
;;   "Deactivate fly command."
;;   (interactive)
;;   (fly-command-mode -1)
;;   (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
;;   (setq mode-line-front-space "I")
;;   (force-mode-line-update))

;; (defun activate-fly-command ()
;;   (interactive)
;;   (fly-command-mode 1)
;;   (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
;;   (setq mode-line-front-space "C")
;;   (force-mode-line-update))

;; (defun show-fly-keymap ()
;;   "Show fly-keys keymap use which-key."
;;   (interactive)
;;   (which-key-show-keymap 'fly-keymap))

;; (bind-key "M-<SPC>" #'activate-fly-command)

