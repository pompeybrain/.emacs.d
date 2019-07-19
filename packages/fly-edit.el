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
   ((assoc major-mode prettier-support-modes) (prettier-format))
   ((eq major-mode 'emacs-lisp-mode) (indent-region (point-min) (point-max)))
   ((eq major-mode 'dart-mode) (dart-format))))

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

(defvar left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")

(defvar right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")

(defun goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))



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

;; TODO use custom   isearch navigating

(defun lookup-word-or-selection ()
  "Swiper if current region use current region."
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (swiper str))
    (swiper)))

;;; ----------------------------------------------------------------------------
(provide 'fly-edit)
;;; fly-edit ends here
