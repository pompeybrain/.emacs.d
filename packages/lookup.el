;;; lookup.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; use isearch to lookup symbol or a word references.
;;; TODO:
;;; 1. isearch-yank's problem
;;; 2. interactively read input

;;; Code:

(require 'hydra)

(defhydra hydra-lookup
  (:hint nil :foreign-keys warn)
  "Lookup"
  ("i" isearch-repeat-backward "Previous")
  ("k" isearch-repeat-forward "Next")
  ("SPC" isearch-exit "Ok" :color blue)
  ("q" isearch-cancel "Cancel" :color blue))

(defun lookup ()
  (interactive)
  "Look up current word or symbol."
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-yank-string str))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (progn
            (when (< (car bounds) (point))
	          (goto-char (car bounds)))
            (isearch-yank-string
             (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (message "need read input.")
        (interactive
         (let ((str (read-string "iSearch: ")))
           (when str
             (isearch-yank-string str))))
        ;; (call-interactively 'isearch-forward)
        )))
  (hydra-lookup/body))

(provide 'lookup)
;;; lookup.el ends here
