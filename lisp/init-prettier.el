;;; init-prettier -*- lexical-binding: t -*-
;;; to react with prettier
;;; first a global installed prettier is required

;;; use project specific config file first.
;;; use default config when don't find config file

;;; use find-config-path if get a configpath use it replace of global options

(require 'process-wrapper)

(defvar prettier-default-options '()
  "default config for prettier.")

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

(defun prettier-find-config ()
  "Use prettier find current buffer file config path."
  (interactive)
  (message "find config: %s"(sync-process (prettier-command)
                                          (list "--find-config-path" (buffer-file-name))
                                          nil)))

(defun prettier-apply (formated-text)
  (let ((prev-point (point)))
    (erase-buffer)
    ;; (insert-buffer-substring formated-buffer)
    (insert formated-text)
    (goto-char prev-point)))

;;;###autoload
(defun prettier-format ()
  "Use prettier format current buffer file, just support file buffer now."
  (interactive)
  (message "command: %s" (prettier-command))
  (let ((parser (assoc-default major-mode prettier-support-modes))
        (parser-or-path-args prettier-default-options))
    (if parser
        (progn (if (buffer-file-name)
                   (setq parser-or-path-args  (list "--stdin-filepath" (buffer-file-name)))
                 (setq parser-or-path-args (list "--parser" parser)))
               (sync-process (prettier-command)
                             (append prettier-default-options parser-or-path-args)
                             (current-buffer)
                             #'prettier-apply))
      (message "prettier doesn't support current major mode: %s" major-mode))))

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
