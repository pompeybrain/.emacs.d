;;; compile.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; compile all .emacs.d els
;;; Code:

;;; 遍历文件夹内所有el文件，删除原elc，再compile
(defun compile-all-el ()
  "Compile all el in default-directory."
  (dolist (sub (directory-files user-emacs-directory t))
    (cond ((file-exists-p sub)
           (message "%S is file" sub))
          ((directory-name-p sub)
           (message "%S is directory" sub))
          (t (message "%S abnormal" sub)))))

(provide 'compile)
;;; compile.el ends here
