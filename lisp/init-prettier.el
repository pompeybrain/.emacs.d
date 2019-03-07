;;; to react with prettier
;;; first a global installed prettier is required
;;; use project specific config file first.
;;; use default config when don't find config file
;;; use prettier cli interface
;;; 1: use process-wrapper to excute cmd
;;; 2: a universal replace current buffer content function

(require 'process-wrapper)

(defvar prettier-default-options '("--single-quote")
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

(defun prettier-format ()
  "Use prettier format current buffer file, just support file buffer now."
  (interactive)
  (let ((parser (assoc-default major-mode prettier-support-modes)))
    (if parser
        ;; (progn
        (sync-process (prettier-command)
                      (append prettier-default-options (list "--parser" parser))
                      (current-buffer)
                      (lambda (formated-buffer)
                        (save-excursion ; TODO: other method to keep point
                          ;; (erase-buffer)
                          (delete-region (point-min) (point-max))
                          ;; (insert-buffer formated-buffer)
                          (let ((target (current-buffer)))
                            (with-current-buffer formated-buffer
                              (let ((formated-text (buffer-substring-no-properties (point-min) (point-max))))
                                (with-current-buffer target
                                  (insert formated-text))
                                )
                              ;; (copy-to-buffer target (point-min) (point-max))
                              ))
                          )))
      ;; (let ((code
      ;;        (apply 'call-process-region
      ;;               (append (list
      ;;                        (point-min) (point-max) (prettier-command) nil "pretter-output" nil)
      ;;                       prettier-default-options (list "--parser" parser)))))
      ;;   (message "prettier code: %d " code)
      ;;   (when (eq 0 code)
      ;;     ;; (revert-buffer t t t)
      ;;     ;; (when (member flycheck-mode minor-mode-list)
      ;;     ;;   (flycheck-handle-save))
      ;;     ))
      ;; )
      (message "prettier doesn't support current major mode: %s" major-mode))))

;; (test-prettier)
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
