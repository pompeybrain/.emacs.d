;; (defconst emacs-d
;;   (file-name-directory
;;    (file-chase-links load-file-name)))

;; (setq package-user-dir
;;       (expand-file-name "elpa" emacs-d))

;; (package-initialize)

;; (setq package-archives
;;       '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; (package-refresh-contents)

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; ;; (load (expand-file-name "init-packages" emacs-d))
