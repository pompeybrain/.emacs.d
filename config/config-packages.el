;;; config ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'languages)
(require 'tools)
(require 'augment)
(require 'program)

;;; my packages
(require 'dashboard)
(require 'prettier)
(setq prettier-use-vscode-config t)
(add-hook 'after-init-hook (global-prettier-mode 1))

(provide 'config-packages)
;;; config ends here
