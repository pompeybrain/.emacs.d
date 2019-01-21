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

  (bind-keys :prefix-map i-hydra-map
	     :prefix "M-h"
	     ("j" . ijump-hydra/body)
	     ("e" . ierror-hydra/body))
  )

(bind-key "M-q" #'save-buffers-kill-terminal)
;;; C-x 
(bind-keys :prefix-map window-map
	   :prefix "M-w"
	   ("1" . delete-other-windows)
	   ("2" . split-windows-below)
	   ("3" . split-window-right)
	   ("o" . other-window)
	   ("b" . ivy-switch-buffer))

(bind-key "M-c" #'kill-ring-save)
(bind-key "M-n" #'next-line)
(bind-key "M-p" #'previous-line)
(bind-key "M-s" #'save-buffer)
(provide 'init-keybinds)
;;; init-keybinds.el ends here
