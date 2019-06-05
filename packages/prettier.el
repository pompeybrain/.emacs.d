;;; prettier.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Required: global installed prettier in path

;;; 1 project specific config file first.

;;; 2 vscode prettier setting to keep up with other co-worker

;;; 3 default config when don't find config file

;;; to support ignore-path should excute prettier command in project root directory

;;; add minor mode to switch  format before save in support major modes

;;; Code:

(require 'process-wrapper)
(require 'quiet)

(defvar prettier-support-modes
  '(js2-mode javascript-mode css-mode scss-mode less-mode json-mode web-mode
             yaml-mode typescript-mode markdown-mode) "prettier support major mode")

(defvar prettier-support-modes-alist
  '((js2-mode . "babylon") (javascript-mode . "babylon") (css-mode . "css")
    (scss-mode . "scss") (less-mode . "less") (json-mode . "json") (web-mode . "vue") (yaml-mode . "yaml")
    (typescript-mode . "typescript") (markdown-mode . "markdown"))
  "prettier support major mode parser alist")

(defgroup prettier nil
  "prettier a tool to format some front-end relate language."
  :group 'languages
  :prefix "prettier"
  :link '(url-link :tag "Repository" "https://github.com/prettier/prettier"))

;;; options reference : https://prettier.io/docs/en/options.html

(defcustom prettier-print-width "80"
  "Specify the line length that the printer will wrap on."
  :type 'string
  :group 'prettier)

(defcustom prettier-tab-width "2"
  "Specify the number of spaces per indentation-level."
  :type 'string
  :group 'prettier)

(defcustom prettier-use-tabs nil
  "Indent lines with tabs instead of spaces."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-semi t
  "Print semicolons at the ends of statements."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-single-quote t
  "Use single quotes instead of double quotes."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-trailing-comma "none"
  "Print trailing commas wherever possible when multi-line."
  :type 'string
  :options '("foo" "bar" "baz")
  :group 'prettier)

(defcustom prettier-bracket-space t
  "Print spaces between brackets in object literals."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-jsx-single-quote nil
  "Use single quotes instead of double quotes in JSX."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-jsx-bracket-same-line nil
  "Put the > of a multi-line JSX element at the end of the last line instead of being alone on the next line (does not apply to self closing elements)."
  :type 'boolean
  :group 'prettier)

(defcustom prettier-arrow-parens "avoid"
  "Include parentheses around a sole arrow function parameter."
  :type 'string
  :group 'prettier)

(defcustom prettier-prose-wrap "preserve"
  "By default, Prettier will wrap markdown text as-is since some services use a linebreak-sensitive renderer, e.g. GitHub comment and BitBucket. In some cases you may want to rely on editor/viewer soft wrapping instead, so this option allows you to opt out with never."
  :type 'string
  :group 'prettier)

(defcustom prettier-html-whitespace-sensitivity "css"
  "Specify the global whitespace sensitivity for HTML files"
  :type 'string
  :group 'prettier)

(defcustom prettier-end-of-line "auto"
  "end of line: auto lf crlf cr"
  :type 'string
  :group 'prettier)


;; TODO: add jsx options support

(defun prettier-default-options ()
  "Generate prettier options cli."
  (let ((options '()))
    (setq options(append options (list "--print-width" prettier-print-width)))
    (setq options (append options (list "--tab-width" prettier-tab-width)))
    (when prettier-use-tabs
      (push "--use-tabs" options))
    (unless prettier-semi
      (push "--no-semi" options))
    (when prettier-single-quote
      (push "--single-quote" options))
    (setq options (append options (list "--trailing-comma" prettier-trailing-comma)))
    (unless prettier-bracket-space
      (push "--no-bracket-spacing" options))
    (when prettier-jsx-single-quote
      (push "--jsx-single-quote" options))
    (when prettier-jsx-bracket-same-line
      (push "--jsx-bracket-same-line" options))
    (setq options (append options (list "--arrow-parens" prettier-arrow-parens)))
    (setq options (append options (list "--prose-wrap" prettier-prose-wrap)))
    (setq options (append options (list "--html-whitespace-sensitivity" prettier-html-whitespace-sensitivity)))
    (setq options (append options (list "--end-of-line" prettier-end-of-line)))
    options))


;;convert prettier.useTabs to prettier-use-tabs
(defun config-vscode-to-emacs (str)
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\." "-"
                              (downcase (replace-regexp-in-string "[[:upper:]]" "-\\&" str)))))

(defun prettier-vscode-config ()
  "aware current project .vscode config about prettier and set options use let bind variable."
  (let* ((exist-dir (locate-dominating-file (buffer-file-name) ".vscode"))
         (settings (expand-file-name ".vscode/settings.json" exist-dir))
         (vsconfig nil))
    (when (and exist-dir (not (equal exist-dir "~/")))
      (when (file-exists-p settings)
        (setq vsconfig
              (seq-filter (lambda (pair)
                            (string-match "prettier" (symbol-name (car pair))))
                          (json-read-file settings)))
        (when vsconfig
          (mapcar (lambda (pair)
                    (make-variable-buffer-local (intern (config-vscode-to-emacs (symbol-name (car pair)))))
                    (set (intern (config-vscode-to-emacs (symbol-name (car pair))))
                         (if (numberp (cdr pair))
                             (number-to-string (cdr pair)) ;number convert to string
                           (if (eq ':json-false (cdr pair)) ; :json-false is nil
                               nil
                             (cdr pair)))))
                  vsconfig)
          (prettier-default-options))))))

(defun prettier-options ()
  "Make up prettier options if find config use config file otherwise use default options."
  (let ((config-path (prettier-find-config))
        (vscode-config (prettier-vscode-config)))
    (if (string-empty-p config-path)
        (if vscode-config
            vscode-config
          (prettier-default-options))
      (list "--config" config-path))))


(defun prettier-command ()
  "Find the bin in dir node_modules or use global in execpath."
  (or
   (let ((exist-dir (locate-dominating-file buffer-file-name "node_modules")))
     (when exist-dir
       (let ((pretter-bin (concat exist-dir "node_modules/.bin/prettier")))
         (when (file-executable-p pretter-bin)
           pretter-bin))))
   (executable-find "prettier")
   (error "Couldn't find executable prettier. please install prettier locally or globally.")))

;;;###autoload
(defun prettier-find-config ()
  "Use prettier find current buffer file config path."
  (replace-regexp-in-string
   "\n\\'" ""
   (sync-process (prettier-command)
                 (list "--find-config-path" (buffer-file-name))
                 nil '(0) nil t)))

(defun goto-prev-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun prettier-apply-patch (patch-text)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (patch-buffer (get-buffer-create "prettier-patch-buffer"))
        (line-offset 0))
    (with-current-buffer patch-buffer
      (erase-buffer)
      (insert patch-text))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch in prettier-apply-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-prev-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch in prettier-apply-patch")))))))
    (kill-buffer patch-buffer)))

(defun prettier-diff-formated (formated-text)
  (quiet
    (let ((formated-file (make-temp-file "prettier-formated")))
      (unwind-protect
          (progn
            (with-temp-buffer
              (erase-buffer)
              (insert formated-text)
              (write-region nil nil formated-file))
            (sync-process "diff" (list "-n" "--strip-trailing-cr" "-" formated-file)
                          (current-buffer) '(0 1) #'prettier-apply-patch))
        (delete-file formated-file)))))

;; need more reasonable implementation
;; 1st .prettierignore root directory
;; 2nd node_modules root directory
;; 2nd projectl intergate
;; .git root directory
;; nothing change
(defun prettier-project-directory ()
  (let ((project-dir (locate-dominating-file (buffer-file-name) ".prettierignore")))
    (if project-dir
        project-dir
      default-directory)))

;;;###autoload
(defun prettier-format ()
  "Use prettier format current buffer file, just support file buffer now."
  (interactive)
  (let ((default-directory (prettier-project-directory))
        (parser (assoc-default major-mode prettier-support-modes-alist))
        (parser-or-path-args nil))
    (if parser
        (progn (if (buffer-file-name)
                   (setq parser-or-path-args  (list "--stdin-filepath" (buffer-file-name)))
                 (setq parser-or-path-args (list "--parser" parser)))
               (sync-process (prettier-command)
                             (append (prettier-options) parser-or-path-args)
                             (current-buffer)
                             '(0)
                             #'prettier-diff-formated))
      (message "prettier doesn't support current major mode: %s" major-mode))))

;; add save hook format
;; (defun add-save-format (mode)
;;   (add-hook (intern  (concat (symbol-name (car mode)) "-hook"))
;;             (lambda ()
;;               (add-hook 'before-save-hook 'prettier-format nil t))))

;; (mapcar 'add-save-format prettier-support-modes)

(defun prettier-add-format-hook ()
  "add format before save hook"
  (when (memq major-mode prettier-support-modes)
    (add-hook 'before-save-hook 'prettier-format nil 'local)))

;;;###autoload
(define-minor-mode global-prettier-mode
  "Runs prettier-format on support major modes before save when this mode is turned on"
  nil
  :lighter " Prettier"
  :global t
  (if global-prettier-mode
      (progn (add-hook 'after-change-major-mode-hook #'prettier-add-format-hook)
             (prettier-add-format-hook))
    (remove-hook 'after-change-major-mode-hook #'prettier-add-format-hook)
    (remove-hook 'before-save-hook 'prettier-format 'local)))

(provide 'prettier)
;;; prettier.el ends here
