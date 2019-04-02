;;; core-ui.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; set font
(set-face-attribute 'default nil :family "Fira Code")
;; (set-face-attribute 'default nil :family "Menlo")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :weight 'normal)
;;;(mac-auto-operator-composition-mode)	;macport for fira code
(setq-default
 line-spacing 3
 custom-safe-themes t      ;theme no warn
 overflow-newline-into-fringe nil
 indicate-empty-lines nil
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-scroll-output 'first-error
 cursor-in-non-selected-windows nil ; hide cursors in other windows
 enable-recursive-minibuffers nil
 fringe-indicator-alist
 (delq (assq 'continuation fringe-indicator-alist)
       fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 inhibit-compacting-font-caches t
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold 160       ; favor horizontal splits
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 ;; don't resize emacs in steps, it looks weird
 window-resize-pixelwise t
 frame-resize-pixelwise t
 display-line-numbers-current-absolute t
 display-line-numbers-width 3
 )

(fset #'yes-or-no-p #'y-or-n-p)
(fset #'display-startup-echo-area-message #'ignore)
(if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
(blink-cursor-mode -1)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 1
              window-divider-default-right-width 1)

;; (load-theme 'leuven)
(load-theme 'aod)
;; (load-theme 'zenburn)

;; 左右边界
(fringe-mode '(4 . 0))

;; set title
;;;defaults write org.gnu.Emacs HideDocumentIcon YES for macport version
;; (setq ns-use-proxy-icon nil)		;for 26
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

(global-hl-line-mode +1)

(add-hook 'minibuffer-setup-hook (lambda () (visual-line-mode -1)))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; how to set frame size and position in mac os very early
;;; defaults write org.gnu.Emacs Width 150
;;; defaults write org.gnu.Emacs Height 50
;;; defaults write org.gnu.Emacs Top 150
;;; defaults write org.gnu.Emacs Left 400

(provide 'core-ui)
;;; core-ui ends here
