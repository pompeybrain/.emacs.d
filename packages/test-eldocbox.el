;;; testeldoc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'eldoc-box)

(eldoc-box--display "Document:
This is test document.")

(eldoc-box--maybe-cleanup)
;; pos-tip
(defvar pos-tip-background-color "#121417")
(defvar pos-tip-foreground-color "#9dA5B4")

(provide 'testeldoc)
;;; testeldoc.el ends here
