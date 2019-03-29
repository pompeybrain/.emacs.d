(defvar user-local-dir (concat user-emacs-directory ".local/"))

(defvar user-autoload-file (expand-file-name "autoloaddefs.el"))

(make-directory (file-name-directory user-autoload-file) t)

(defun generate-el-header (func)
  (goto-char (point-min))
  (insert ";; -*- lexical-binding:t -*-\n"
          ";; This file is autogenerated by `" (symbol-name func) "', DO NOT EDIT !!\n\n"))

(require 'autoload)
