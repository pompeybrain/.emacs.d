;;; to react with prettier
;;; first a global installed prettier is required
;;; use project specific config file first.
;;; use default config when don't find config file
;;; use prettier cli interface

(defvar prettier-default-options '("--single-quote" "--trailing-comma all")
  "default config for prettier.")

(defvar prettier-support-modes
  '(js2-mode css-mode json-mode web-mode
             yaml-mode typescript-mode markdown-mode)
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
  (progn
    ;; (apply 'call-process-region (append (list nil nil (prettier-command) nil "*prettier-output*" nil) prettier-default-options))
    (apply 'call-process-region (append (list nil nil (prettier-command) nil "*prettier-output*" nil) prettier-default-options (list "--write" buffer-file-name)))
    (revert-buffer t t t)
    (when (member flycheck-mode minor-mode-list)
      (flycheck-handle-save))))
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
