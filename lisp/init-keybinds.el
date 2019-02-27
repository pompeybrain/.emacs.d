;;; package -- Summary
;;; Commentary:
;;; Code:

;;; hydra
(with-eval-after-load 'hydra

  (defhydra ierror-hydra (:color pink :hint nil :foreign-keys warn)
    "
	\tflymake error and warn lookup:
	\t_n_ : next error
	\t_p_ : prev error
	\t_q_ : quit
  "
    ("n" #'flymake-goto-next-error)
    ("p" #'flymake-goto-prev-error)
    ("q" nil))

  (defhydra ijump-hydra (:hint nil :foreign-keys warn)
    "
  	\t jump and page scroll:
  	\t_n_ : page down
  	\t_p_ : page up
  	\t_l_ : line center <-> top <-> bottom
  	\t_j_ : ace jump
  	\t_s_ : swiper search
        \t_q_ : quit
  "
    ("n" #'scroll-up-command)
    ("p" #'scroll-down-command)
    ("l" #'recenter-top-bottom)
    ("j" #'ace-jump-mode :exit t)
    ("s" #'swiper :exit t)
    ("q" nil))

  (defhydra iorg-hydra (:hint nil :foreign-keys warn)
    "
  	\t_h_ : org next heading
  	\t_H_ : org prev heading
  	\t_tab_ : org folded <> open <> subtree
    	\t_j_ : ace jump
  	\t_s_ : swiper search
        \t_q_ : quit
  "
    ("h" #'org-next-visible-heading)
    ("H" #'org-previous-visible-heading)
    ("tab" #'org-cycle)
    ("j" #'ace-jump-mode :exit t)
    ("s" #'swiper :exit t)
    ("q" nil))

  (bind-keys :prefix-map i-hydra-map
	     :prefix "M-h"
	     ("j" . ijump-hydra/body)
	     ("e" . ierror-hydra/body)
	     ("o" . iorg-hydra/body)
	     ))

(bind-key "M-q" #'save-buffers-kill-terminal)
;;; C-x
(bind-keys :prefix-map window-map
	   :prefix "M-w"
	   ("1" . delete-other-windows)
	   ("2" . split-windows-below)
	   ("3" . split-window-right)
	   ("o" . other-window)
	   ("b" . ivy-switch-buffer))

;;;###autoload
(defun icopy ()
  (interactive)
  "Smart copy region or current line."
  (if (equal mark-active nil)
      (kill-ring-save (line-beginning-position) (line-end-position))
    (kill-ring-save (point) (mark))))

;;;TODO cut only when region is active
;;;###autoload
(defun icut ()
  (interactive)
  "Smart cut region or nothing."
  (if (equal mark-active nil)
      ()
    (kill-ring-save (point) (mark))))

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

(provide 'init-keybinds)
;;; init-keybinds.el ends here
