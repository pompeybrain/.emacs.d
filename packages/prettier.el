;;; prettier ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; to react with prettier
;;; first a global installed prettier is required

;;; use project specific config file first.
;;; use default config when don't find config file

;;; use find-config-path if get a configpath use it replace of global options

;;; support vscode prettier setting to keep up with other co-worker

;;; Code:

(require 'process-wrapper)

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

(defvar prettier-support-modes
  '((js2-mode . "babylon") (javascript-mode . "babylon") (css-mode . "css")
    (scss-mode . "scss") (less-mode . "less") (json-mode . "json") (web-mode . "vue") (yaml-mode . "yaml")
    (typescript-mode . "typescript") (markdown-mode . "markdown"))
  "prettier support major mode")

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
  (let* ((project-dir (locate-dominating-file (buffer-file-name) ".vscode"))
         (settings (expand-file-name ".vscode/settings.json" project-dir))
         (vsconfig nil))
    (when (and project-dir (not (equal project-dir "~/")))
      (when (file-exists-p settings)
        (setq vsconfig
              (seq-filter (lambda (pair)
                            (string-match "prettier" (symbol-name (car pair))))
                          (json-read-file settings)))
        (when vsconfig
          ;; (message "%S" vsconfig)
          (mapcar (lambda (pair)
                    ;; (message "%S" (type-of (cdr pair)))
                    ;; (message (config-vscode-to-emacs (symbol-name (car pair))))
                    (make-variable-buffer-local (intern (config-vscode-to-emacs (symbol-name (car pair)))))
                    (set (intern (config-vscode-to-emacs (symbol-name (car pair))))
                         (if (numberp (cdr pair))
                             (number-to-string (cdr pair)) ;number convert to string
                           (if (eq ':json-false (cdr pair)) ; :json-false is nil
                               nil
                             ;; (message "%S" (eq ':json-false (cdr pair)))
                             (cdr pair)))))
                  vsconfig)
          ;; (message "%S" prettier-semi)
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
   (let ((project-dir (locate-dominating-file buffer-file-name "node_modules")))
     (when project-dir
       (let ((pretter-bin (concat project-dir "node_modules/.bin/prettier")))
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
                 nil nil t)))

(defun prettier-apply (formated-text)
  (let ((prev-point (point)))
    (erase-buffer)
    (insert formated-text)
    (goto-char prev-point)))

;;;###autoload
(defun prettier-format ()
  "Use prettier format current buffer file, just support file buffer now."
  (interactive)
  (let ((parser (assoc-default major-mode prettier-support-modes))
        (parser-or-path-args nil))
    (if parser
        (progn (if (buffer-file-name)
                   (setq parser-or-path-args  (list "--stdin-filepath" (buffer-file-name)))
                 (setq parser-or-path-args (list "--parser" parser)))
               (sync-process (prettier-command)
                             (append (prettier-options) parser-or-path-args)
                             (current-buffer)
                             #'prettier-apply))
      (message "prettier doesn't support current major mode: %s" major-mode))))

(defun prettier-test ()
  "test"
  (interactive)
  (message "%S" (prettier-vscode-config)))

;; add save hook format
(defun add-save-format (mode)
  (add-hook (intern  (concat (symbol-name (car mode)) "-hook"))
            (lambda ()
              (add-hook 'before-save-hook 'prettier-format nil t))))

(mapcar 'add-save-format prettier-support-modes)

(provide 'prettier)
;;; prettier ends here
