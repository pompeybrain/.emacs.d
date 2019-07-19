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

(fly-keys 1)

(provide 'core-keybind)
;;; core-keybind ends here
