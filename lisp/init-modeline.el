;;; package -- Summary ;; -*- lexical-binding: t -*-
;;; Commentary: pretty mode-line
;;; Code:

(defface modeline-buffer-readonly
  '((t (:inherit (warnning bold) :background nil)))
  "Face used for the 'readonly' in mode-line.")

(defface modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' in mode-line.")

(defvar-local modeline-buffer-stat-name nil)

(defun modeline-update-buffer-stat ()
  "Update the buffer state in mode-line."
  (setq modeline-buffer-stat-name
	(cond ((buffer-read-only)
	       (propertize " %b " 'face 'modeline-buffer-readonly))
	      ((buffer-modified-p)
	       (propertize " %b " 'face 'modeline-buffer-modified)))))

(setq-default mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
 (vc-mode vc-mode)
 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
(provide 'init-modeline)
