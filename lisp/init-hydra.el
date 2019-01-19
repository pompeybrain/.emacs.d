;;; package -- Summary
;;; Commentary:
;;; Code:

;; (message "debug: start init hydra")

(with-eval-after-load 'hydra

  (defhydra ierror-hydra (:color pink :hint nil :foreign-keys warn)
    "
	\tflymake error and warn lookup:
	\t_n_ : next error
	\t_p_ : prev error
	\t_q_ : quit
  "
    ("n" #'flymake-goto-next-error :color pink)
    ("p" #'flymake-goto-prev-error :color pink)
    ("q" nil))
  
  (global-set-key (kbd "C-c e") 'ierror-hydra/body)

  )

(provide 'init-hydra)
;;; init-hydra.el ends here
