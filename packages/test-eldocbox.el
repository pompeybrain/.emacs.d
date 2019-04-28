;;; testeldoc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'eldoc-box)

(eldoc-box--display "Document:
This is test document.")

(eldoc-box--maybe-cleanup)

(provide 'testeldoc)
;;; testeldoc.el ends here
