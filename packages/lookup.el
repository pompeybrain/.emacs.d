;;; lookup.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; use isearch to lookup symbol or a word references.

;;; Code:

(require 'hydra)

(defvar lookup-current-str nil "lookup current string.")

(defhydra hydra-lookup
  (:hint nil :foreign-keys warn)
  "Lookup"
  ("i" isearch-repeat-backward "Previous")
  ("k" isearch-repeat-forward "Next")
  ("r" lookup-replace "Replace" :color blue)
  ("e" lookup-edit-search "Edit" :color blue)
  ("s" lookup-search-in-project "Search in project" :color blue)
  ("SPC" isearch-exit "Ok" :color blue)
  ("q" isearch-cancel "Cancel" :color blue))

(defun lookup ()
  (interactive)
  "Look up current word or symbol."
  (setq lookup-current-str nil)
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (setq lookup-current-str (buffer-substring-no-properties (region-beginning) (region-end))))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (progn
            (when (< (car bounds) (point))
	          (goto-char (car bounds)))
            (setq lookup-current-str
                  (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (setq lookup-current-str (read-string "Lookup: ")))))
  (when lookup-current-str
    (isearch-mode t)
    (isearch-yank-string lookup-current-str)
    (hydra-lookup/body)))

(defun lookup-edit-search ()
  (interactive)
  "Edit current search string then search"
  (isearch-exit)
  (setq lookup-current-str (read-string "Lookup: " lookup-current-str))
  (when lookup-current-str
    (isearch-mode t)
    (isearch-yank-string lookup-current-str)
    (hydra-lookup/body)))

(defun lookup-replace ()
  (interactive)
  "Replace something with current string."
  (isearch-repeat-backward)
  (isearch-exit)
  (let ((newStr (read-string "replace to: ")))
    (query-replace lookup-current-str newStr)))

(defun lookup-search-in-project ()
  (interactive)
  "Search current str in project."
  (isearch-exit)
  (counsel-rg lookup-current-str))

(provide 'lookup)
;;; lookup.el ends here
