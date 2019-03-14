(require 'symbol-overlay)

(defhydra lookup (:hint nil :foreign-keys warn)
  "
  	\t lookup mode : highlight symbol jump replace
  	\t _i_ prev \t_k_next \t_h_first \t_;_end
    \t_r_replace
    \t_q_ : quit
  "
  ("i" #'symbol-overlay-jump-prev)
  ("k" #'symbol-overlay-jump-next)
  ("h" #'symbol-overlay-jump-first)
  (";" #'symbol-overlay-jump-last)
  ("r" #'symbol-overlay-query-replace)
  ("e" #'symbol-overlay-rename)
  ("q" #'symbol-overlay-remove-all :exit t))

(provide 'init-git-lisp)
