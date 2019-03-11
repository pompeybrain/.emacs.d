;;; init-prettier -*- lexical-binding: t -*-
;;; to react with prettier
;;; first a global installed prettier is required

;;; use project specific config file first.
;;; use default config when don't find config file

;;; use find-config-path if get a configpath use it replace of global options

;;; support vscode prettier setting to keep up with other co-worker

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
    options))

(defun prettier-options ()
  "Make up prettier options if find config use config file otherwise use default options."
  (let ((config-path (prettier-find-config)))
    (if (string-empty-p config-path)
        (prettier-default-options)
      (list "--config" config-path))))

(defvar prettier-support-modes
  '((js2-mode . "babylon") (javascript-mode . "babylon") (css-mode . "css")
    (json-mode . "json") (web-mode . "vue") (yaml-mode . "yaml")
    (typescript-mode . "typescript") (markdown-mode . "markdown"))
  "prettier support major mode")

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
  (sync-process (prettier-command)
                (list "--find-config-path" (buffer-file-name))
                nil nil t))

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
  (message "%S" (prettier-options))
  )

;; should call flycheck-handle-save more late
;; (call-process-region nil nil (prettier-command) nil "*prettier-output*" nil)
;; add save hook format
;; (defun add-save-format (mode)
;;   (add-hook (intern  (concat (symbol-name mode) "-hook"))
;;             (lambda ()
;;               (add-hook 'after-save-hook 'prettier-format t t))))

;; (mapcar 'add-save-format prettier-support-modes)

(provide 'init-prettier)
;;; init-prettier.el ends here.
