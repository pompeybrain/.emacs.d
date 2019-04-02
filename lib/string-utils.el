;;; string-utils ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun to-hyphen-case ()
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\." "-"
                              (downcase (replace-regexp-in-string "[[:upper:]]" "-\\&" str)))))

(defun to-camel-case ()
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\." "-"
                              (downcase (replace-regexp-in-string "[[:upper:]]" "-\\&" str)))))

(defun to-upper-case ()
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\." "-"
                              (downcase (replace-regexp-in-string "[[:upper:]]" "-\\&" str)))))

(provide 'string-utils)
;;; string-utils ends here
