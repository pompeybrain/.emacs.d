;;; fly-edit ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; a helper augment emacs edit used by fly-command-mode
;;; Code:

;;; selection

;;; ----------------------------------------------------------------------------

;;; text action

;;; ----------------------------------------------------------------------------

;;; command

(defun fly-format ()
  "Format current buffer, use different function according to major mode "
  (interactive)
  (cond
   ((memq major-mode prettier-support-modes) (prettier-format))
   ((eq major-mode 'emacs-lisp-mode) (indent-region (point-min) (point-max)))
   ((eq major-mode 'dart-mode) (dart-format))))

(defun fly-save-all-buffer ()
  (interactive)
  (save-some-buffers t))

(defun fly-js-dev ()
  "In current directory excute shell command : yarn dev, need core-lib shellcommand function."
  (interactive)
  (if (fboundp 'shellcommand)
      (shellcommand "yarn dev" "js-dev")
    (message "need shellcommand function")))

(defun copy-line-or-region ()
  "Copy current line, or text selection."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    ))

(defun cut-line-or-region ()
  "Cut current line, or text selection."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun comment-line-or-region ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

(defun fly-next-20-lines ()
  "Next 20 lines."
  (interactive)
  (scroll-up-command 20))

(defun fly-prev-20-lines ()
  "Prev 20 lines."
  (interactive)
  (scroll-down-command 20))

;;; ----------------------------------------------------------------------------

;;; interactive tools

;;; ----------------------------------------------------------------------------
(provide 'fly-edit)
;;; fly-edit ends here
