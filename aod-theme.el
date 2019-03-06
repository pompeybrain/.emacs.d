;;; atom-one-dark-theme.el --- Atom One Dark color theme

;;; Commentary:

;; My Emacs version of the Atom One Dark theme from Atom.io.

;; candidate primary bold

;;; Code:

(deftheme aod
  "Atom One Dark - My version of the Atom One Dark theme from Atom.io.")

(defvar aod-colors-alist
  `(
    ("aod-bg"       . "#282C34")
    ("aod-bg-1"     . "#21252B")
    ("aod-bg-2"     . "#121417")
    ("aod-bg-hll"   . "#2C313A")
    ("aod-bg-hl"    . "#3E4451")
    ("aod-bg-focus" . "#454545")
    ("aod-builtin"  . "#56B6C2")
    ("aod-error"    . "#E06C75")
    ("aod-error-1"  . "#93262d")
    ("aod-fg+1"     . "#e3e6ea")
    ("aod-fg"       . "#ABB2BF")
    ("aod-fg-1"     . "#9DA5B4")
    ("aod-fg-2"     . "#74777a")
    ("aod-comment"  . "#5C6370")
    ("aod-gutter"   . "#4B5363")
    ("aod-keyword"  . "#C678DD")
    ("aod-mono-1"   . "#ABB2BF")
    ("aod-mono-2"   . "#828997")
    ("aod-mono-3"   . "#5C6370")
    ("aod-primary"  . "#61AFEF")
    ("aod-primary-1". "#447eaa")
    ("aod-string"   . "#98C379")
    ("aod-success"  . "#8FAC75")
    ("aod-success-1". "#768964")
    ("aod-warn"     . "#E5C07B")
    ("aod-type"     . "#D19A66")
    ("aod-cursor"   . "#528BFF")
    )
  "List of Atom One Dark colors.")

(defmacro aod-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    aod-colors-alist))
     ,@body))

(aod-with-color-variables
  (custom-theme-set-faces
   'aod

   `(default ((t (:foreground ,aod-fg :background ,aod-bg))))
   `(success ((t (:foreground ,aod-success))))
   `(warning ((t (:foreground ,aod-warn))))
   `(error ((t (:foreground ,aod-error :weight bold))))
   `(link ((t (:foreground ,aod-primary :underline t :weight bold))))
   `(link-visited ((t (:foreground ,aod-fg :underline t :weight normal))))
   `(cursor ((t (:background ,aod-cursor))))
   `(fringe ((t (:background ,aod-bg))))
   `(region ((t (:background ,aod-bg-hl))))
   `(highlight ((t (:background ,aod-bg-hl))))
   `(hl-line ((t (:background ,aod-bg-hll))))
   `(vertical-border ((t (:background ,aod-bg-2 :foreground ,aod-bg-2))))
   `(secondary-selection ((t (:background ,aod-bg-2))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,aod-keyword :background ,aod-bg))))

   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,aod-comment))))
   `(font-lock-constant-face ((t (:foreground ,aod-builtin))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,aod-primary))))
   `(font-lock-keyword-face ((t (:foreground ,aod-keyword :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,aod-mono-2))))
   `(font-lock-string-face ((t (:foreground ,aod-string))))
   `(font-lock-type-face ((t (:foreground ,aod-type))))
   `(font-lock-variable-name-face ((t (:foreground ,aod-error))))
   `(font-lock-warning-face ((t (:foreground ,aod-warn :bold t))))
   `(font-lock-builtin-face ((t (:foreground ,aod-builtin))))
   ;; mode-line
   `(mode-line ((t (:background ,aod-bg-1 :foreground ,aod-fg-2))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,aod-bg-1 :foreground ,aod-bg-1))))

   ;; doom-mode-line
   `(doom-modeline-bar ((t nil)))
   `(doom-modeline-inactive-bar ((t nil)))

   ;; window-divider
   `(window-divider ((t (:foreground ,aod-bg-2))))
   `(window-divider-first-pixel ((t (:foreground ,aod-bg-2))))
   `(window-divider-last-pixel ((t (:foreground ,aod-bg-2))))

   ;; ido
   `(ido-first-match ((t (:foreground ,aod-keyword :weight bold))))
   `(ido-only-match ((t (:foreground ,aod-error :weight bold))))
   `(ido-subdir ((t (:foreground ,aod-primary))))
   `(ido-virtual ((t (:foreground ,aod-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,aod-fg-2 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,aod-primary :background ,aod-bg :inverse-video nil))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,aod-fg :background ,aod-bg-2))))
   `(company-tooltip-annotation ((t (:foreground ,aod-mono-2 :background ,aod-bg-2))))
   `(company-tooltip-selection ((t (:foreground ,aod-fg :background ,aod-bg-hl))))
   `(company-tooltip-mouse ((t (:background ,aod-bg-hl))))
   `(company-tooltip-common ((t (:foreground ,aod-primary :weight bold  :background ,aod-bg-2))))
   `(company-tooltip-common-selection ((t (:foreground ,aod-primary :weight bold :background ,aod-bg-hl))))
   `(company-preview ((t (:background ,aod-bg))))
   `(company-preview-common ((t (:foreground ,aod-primary :background ,aod-bg))))
   `(company-scrollbar-fg ((t (:background ,aod-fg-2))))
   `(company-scrollbar-bg ((t (:background ,aod-bg-2))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,aod-error :style wave)))))
   `(flymake-note ((t (:underline (:color ,aod-success :style wave)))))
   `(flymake-warning ((t (:underline (:color ,aod-warn :style wave)))))

   ;; posframe
   `(flycheck-posframe-background-face ((t (:foreground ,aod-fg :background ,aod-bg-2))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color ,aod-error :style wave)))))
   `(flycheck-info ((t (:underline (:color ,aod-success :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,aod-warn :style wave)))))

   ;; compilation
   `(compilation-face ((t (:foreground ,aod-fg))))
   `(compilation-line-number ((t (:foreground ,aod-mono-2))))
   `(compilation-column-number ((t (:foreground ,aod-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,aod-bg :background ,aod-keyword))))
   `(isearch-fail ((t (:foreground ,aod-error :background nil))))
   `(lazy-highlight ((t (:foreground ,aod-keyword :underline ,aod-keyword))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   `(diff-hl-change ((t (:foreground ,aod-bg :background ,aod-primary-1))))
   `(diff-hl-delete ((t (:foreground ,aod-bg :background ,aod-error-1))))
   `(diff-hl-insert ((t (:foreground ,aod-bg :background ,aod-success-1))))

   ;; dired-mode
   `(dired-directory ((t (:inherit font-lock-keyword-face))))
   `(dired-flagged ((t (:inherit diff-hl-delete))))
   `(dired-symlink ((t (:foreground ,aod-keyword))))

   ;; helm
   `(helm-header ((t (:foreground ,aod-mono-2
                                  :background ,aod-bg
                                  :underline nil
                                  :box (:line-width 6 :color ,aod-bg)))))
   `(helm-source-header ((t (:foreground ,aod-type
                                         :background ,aod-bg
                                         :underline nil
                                         :weight bold
                                         :box (:line-width 6 :color ,aod-bg)))))
   `(helm-selection ((t (:background ,aod-bg-hl))))
   `(helm-selection-line ((t (:background ,aod-bg-hl))))
   `(helm-visible-mark ((t (:background ,aod-bg :foreground ,aod-type))))
   `(helm-candidate-number ((t (:foreground ,aod-success :background ,aod-bg-2))))
   `(helm-separator ((t (:background ,aod-bg :foreground ,aod-error))))
   `(helm-M-x-key ((t (:foreground ,aod-warn))))
   `(helm-bookmark-addressbook ((t (:foreground ,aod-warn))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,aod-keyword))))
   `(helm-bookmark-info ((t (:foreground ,aod-success))))
   `(helm-bookmark-man ((t (:foreground ,aod-type))))
   `(helm-bookmark-w3m ((t (:foreground ,aod-keyword))))
   `(helm-match ((t (:foreground ,aod-type))))
   `(helm-ff-directory ((t (:foreground ,aod-builtin :background ,aod-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,aod-fg :background ,aod-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,aod-success :background ,aod-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,aod-error :background ,aod-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,aod-type :background ,aod-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,aod-bg :background ,aod-type :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,aod-error))))
   `(helm-buffer-process ((t (:foreground ,aod-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,aod-fg))))
   `(helm-buffer-size ((t (:foreground ,aod-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,aod-keyword))))
   `(helm-grep-cmd-line ((t (:foreground ,aod-builtin))))
   `(helm-grep-file ((t (:foreground ,aod-fg))))
   `(helm-grep-finish ((t (:foreground ,aod-success))))
   `(helm-grep-lineno ((t (:foreground ,aod-mono-2))))
   `(helm-grep-finish ((t (:foreground ,aod-error))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,aod-mono-3 :foreground ,aod-bg-1))))
   `(helm-swoop-target-line-face ((t (:background ,aod-mono-3 :foreground ,aod-bg-1))))
   `(helm-swoop-target-word-face ((t (:background ,aod-keyword :foreground ,aod-fg))))
   `(helm-locate-finish ((t (:foreground ,aod-success))))
   `(info-menu-star ((t (:foreground ,aod-error))))

   ;; ivy
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,aod-success))))
   `(ivy-current-match ((t (:background ,aod-bg-focus :weight normal))))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,aod-error))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,aod-bg))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :foreground ,aod-primary))))

   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2
                                               :foreground ,aod-success :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2
                                               :foreground ,aod-type :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,aod-primary :background ,aod-bg))))
   `(ivy-modified-buffer ((t (:inherit default :foreground ,aod-warn))))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,aod-type :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,aod-success :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,aod-primary :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,aod-type :weight bold))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,aod-success :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,aod-error :weight bold))))
   `(git-gutter:modified ((t (:foreground ,aod-warn :weight bold))))

   ;; man
   `(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold ((t (:inherit font-lock-type-face :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face ((t (:inherit widget-button))))
   `(dictionary-reference-face ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; jabber
   `(jabber-roster-user-online ((t (:foreground ,aod-success))))
   `(jabber-roster-user-away ((t (:foreground ,aod-error))))
   `(jabber-roster-user-xa ((t (:foreground ,aod-error))))
   `(jabber-roster-user-dnd ((t (:foreground ,aod-keyword))))
   `(jabber-roster-user-chatty ((t (:foreground ,aod-type))))
   `(jabber-roster-user-error ((t (:foreground ,aod-error :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,aod-mono-3))))
   `(jabber-chat-prompt-local ((t (:foreground ,aod-primary))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,aod-type))))
   `(jabber-chat-prompt-system ((t (:foreground ,aod-mono-3))))
   `(jabber-chat-error ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face ((t (:foreground ,aod-builtin))))
   `(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; eww
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:background ,aod-bg-hl :foreground ,aod-fg
                                      :box (:line-width 2 :color ,aod-bg-2 :style released-button)))))
   `(eww-form-text ((t (:inherit widget-field :box (:line-width 1 :color ,aod-bg-2)))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,aod-error))))
   `(eww-valid-certificate ((t (:foreground ,aod-success))))

   ;; js2-mode
   `(js2-error ((t (:underline (:color ,aod-error :style wave)))))
   `(js2-external-variable ((t (:foreground ,aod-builtin))))
   `(js2-warning ((t (:underline (:color ,aod-warn :style wave)))))
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,aod-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,aod-keyword))))
   `(js2-jsdoc-type ((t (:foreground ,aod-type))))
   `(js2-jsdoc-value((t (:foreground ,aod-error))))
   `(js2-object-property ((t (:foreground ,aod-error))))

   ;; magit
   `(magit-section-highlight ((t (:background ,aod-bg-hl))))
   `(magit-section-heading ((t (:foreground ,aod-type :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,aod-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,aod-bg-hl :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,aod-type :background ,aod-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,aod-mono-2 :background ,aod-bg-hl))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,aod-mono-1 :background ,aod-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,aod-keyword :background ,aod-mono-3))))
   `(magit-diff-context ((t (:foreground ,aod-fg))))
   `(magit-diff-context-highlight ((t (:background ,aod-bg-2 :foreground ,aod-fg))))
   `(magit-diffstat-added ((t (:foreground ,aod-success))))
   `(magit-diffstat-removed ((t (:foreground ,aod-error))))
   `(magit-process-ok ((t (:foreground ,aod-success))))
   `(magit-process-ng ((t (:foreground ,aod-error))))
   `(magit-log-author ((t (:foreground ,aod-type))))
   `(magit-log-date ((t (:foreground ,aod-mono-2))))
   `(magit-log-graph ((t (:foreground ,aod-fg-1))))
   `(magit-sequence-pick ((t (:foreground ,aod-type))))
   `(magit-sequence-stop ((t (:foreground ,aod-success))))
   `(magit-sequence-part ((t (:foreground ,aod-warn))))
   `(magit-sequence-head ((t (:foreground ,aod-primary))))
   `(magit-sequence-drop ((t (:foreground ,aod-error))))
   `(magit-sequence-done ((t (:foreground ,aod-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,aod-mono-2))))
   `(magit-bisect-good ((t (:foreground ,aod-success))))
   `(magit-bisect-skip ((t (:foreground ,aod-warn))))
   `(magit-bisect-bad ((t (:foreground ,aod-error))))
   `(magit-blame-heading ((t (:background ,aod-bg-2 :foreground ,aod-mono-2))))
   `(magit-blame-hash ((t (:background ,aod-bg-2 :foreground ,aod-keyword))))
   `(magit-blame-name ((t (:background ,aod-bg-2 :foreground ,aod-type))))
   `(magit-blame-date ((t (:background ,aod-bg-2 :foreground ,aod-mono-3))))
   `(magit-blame-summary ((t (:background ,aod-bg-2 :foreground ,aod-mono-2))))
   `(magit-dimmed ((t (:foreground ,aod-mono-2))))
   `(magit-hash ((t (:foreground ,aod-keyword))))
   `(magit-tag  ((t (:foreground ,aod-warn :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,aod-success :weight bold))))
   `(magit-branch-local   ((t (:foreground ,aod-primary :weight bold))))
   `(magit-branch-current ((t (:foreground ,aod-primary :weight bold :box t))))
   `(magit-head           ((t (:foreground ,aod-primary :weight bold))))
   `(magit-refname        ((t (:background ,aod-bg :foreground ,aod-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,aod-bg :foreground ,aod-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,aod-bg :foreground ,aod-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,aod-success))))
   `(magit-signature-bad       ((t (:foreground ,aod-error))))
   `(magit-signature-untrusted ((t (:foreground ,aod-warn))))
   `(magit-cherry-unmatched    ((t (:foreground ,aod-builtin))))
   `(magit-cherry-equivalent   ((t (:foreground ,aod-keyword))))
   `(magit-reflog-commit       ((t (:foreground ,aod-success))))
   `(magit-reflog-amend        ((t (:foreground ,aod-keyword))))
   `(magit-reflog-merge        ((t (:foreground ,aod-success))))
   `(magit-reflog-checkout     ((t (:foreground ,aod-primary))))
   `(magit-reflog-reset        ((t (:foreground ,aod-error))))
   `(magit-reflog-rebase       ((t (:foreground ,aod-keyword))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,aod-success))))
   `(magit-reflog-remote       ((t (:foreground ,aod-builtin))))
   `(magit-reflog-other        ((t (:foreground ,aod-builtin))))

   ;; message
   `(message-cited-text ((t (:foreground ,aod-success))))
   `(message-header-cc ((t (:foreground ,aod-warn :weight bold))))
   `(message-header-name ((t (:foreground ,aod-keyword))))
   `(message-header-newsgroups ((t (:foreground ,aod-type :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,aod-error))))
   `(message-header-subject ((t (:foreground ,aod-primary))))
   `(message-header-to ((t (:foreground ,aod-type :weight bold))))
   `(message-header-xheader ((t (:foreground ,aod-fg-1))))
   `(message-mml ((t (:foreground ,aod-keyword))))
   `(message-separator ((t (:foreground ,aod-mono-3 :slant italic))))

   ;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,aod-keyword :background ,aod-bg-1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,aod-error :background ,aod-bg-1))))
   `(notmuch-crypto-signature-good ((t (:foreground ,aod-success :background ,aod-bg-1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,aod-success :background ,aod-bg-1))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,aod-warn :background ,aod-bg-1))))
   `(notmuch-hello-logo-background ((t (:inherit default))))
   `(notmuch-message-summary-face ((t (:background ,aod-bg-1))))
   `(notmuch-search-count ((t (:inherit default :foreground ,aod-fg-1))))
   `(notmuch-search-date ((t (:inherit default :foreground ,aod-keyword))))
   `(notmuch-search-matching-authors ((t (:inherit default :foreground ,aod-type))))
   `(notmuch-search-non-matching-authors ((t (:inherit font-lock-comment-face :slant italic))))
   `(notmuch-tag-added ((t (:underline ,aod-success))))
   `(notmuch-tag-deleted ((t (:strike-through ,aod-error))))
   `(notmuch-tag-face ((t (:foreground ,aod-success))))
   `(notmuch-tag-unread ((t (:foreground ,aod-error))))
   `(notmuch-tree-match-author-face ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face ((t (:weight semi-bold))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-face ((t (:slant italic :weight light :inherit font-lock-comment-face))))

   ;; elfeed
   `(elfeed-log-debug-level-face ((t (:background ,aod-bg-1 :foreground ,aod-success))))
   `(elfeed-log-error-level-face ((t (:background ,aod-bg-1 :foreground ,aod-error))))
   `(elfeed-log-info-level-face ((t (:background ,aod-bg-1 :foreground ,aod-primary))))
   `(elfeed-log-warn-level-face ((t (:background ,aod-bg-1 :foreground ,aod-warn))))
   `(elfeed-search-date-face ((t (:foreground ,aod-keyword))))
   `(elfeed-search-feed-face ((t (:foreground ,aod-type))))
   `(elfeed-search-tag-face ((t (:foreground ,aod-success))))
   `(elfeed-search-title-face ((t (:foreground ,aod-mono-1))))
   `(elfeed-search-unread-count-face ((t (:foreground ,aod-fg-1))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,aod-primary))))

   ;; powerline
   `(powerline-active1 ((,class (:background ,aod-bg-hl :foreground ,aod-keyword))))
   `(powerline-active2 ((,class (:background ,aod-bg-hl :foreground ,aod-keyword))))
   `(powerline-inactive1 ((,class (:background ,aod-bg :foreground ,aod-fg))))
   `(powerline-inactive2 ((,class (:background ,aod-bg :foreground ,aod-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,aod-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,aod-success))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aod-warn))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,aod-builtin))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,aod-keyword))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,aod-type))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,aod-primary))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,aod-success))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,aod-warn))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,aod-builtin))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,aod-keyword))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,aod-type))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,aod-error :weight bold))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,aod-success))))

   ;; elixir
   `(elixir-atom-face ((t (:foreground ,aod-builtin))))
   `(elixir-attribute-face ((t (:foreground ,aod-error))))

   ;;show-paren
   `(show-paren-mismatch ((t (:foreground ,aod-error :background ,aod-bg-hl :weight bold))))
   `(show-paren-match ((t (:background ,aod-gutter :weight bold))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,aod-error :background ,aod-bg-hl :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,aod-bg-hl :weight bold))))

   ;; spaceline
   `(spaceline-flycheck-error  ((,class (:foreground ,aod-error))))
   `(spaceline-flycheck-info   ((,class (:foreground ,aod-success))))
   `(spaceline-flycheck-warning((,class (:foreground ,aod-warn))))
   `(spaceline-python-venv ((,class (:foreground ,aod-keyword))))

   ;; web-mode
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face ((t (:background ,aod-bg-1 :foreground ,aod-error))))
   `(web-mode-html-attr-equal-face ((t (:inherit default))))
   `(web-mode-html-attr-name-face ((t (:foreground ,aod-warn))))
   `(web-mode-html-tag-bracket-face ((t (:inherit default))))
   `(web-mode-html-tag-face ((t (:foreground ,aod-error))))
   `(web-mode-symbol-face ((t (:foreground ,aod-warn))))
   `(web-mode-current-element-highlight-face ((t (:background ,aod-bg-focus :foreground ,aod-error))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,aod-primary))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,aod-fg :background ,aod-error))))
   `(rpm-spec-macro-face ((t (:foreground ,aod-type))))
   `(rpm-spec-var-face ((t (:foreground ,aod-error))))
   `(rpm-spec-doc-face ((t (:foreground ,aod-keyword))))
   `(rpm-spec-dir-face ((t (:foreground ,aod-builtin))))
   `(rpm-spec-package-face ((t (:foreground ,aod-error))))
   `(rpm-spec-ghost-face ((t (:foreground ,aod-error))))
   `(rpm-spec-section-face ((t (:foreground ,aod-type))))

   ;; guix
   `(guix-true ((t (:foreground ,aod-success :weight bold))))

   ;; gomoku
   `(gomoku-O ((t (:foreground ,aod-error :weight bold))))
   `(gomoku-X ((t (:foreground ,aod-success :weight bold))))

   ;; linum
   `(linum ((t (:foreground ,aod-gutter :background ,aod-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,aod-fg :background ,aod-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,aod-gutter :background ,aod-bg))))
   `(line-number-current-line ((t (:foreground ,aod-fg :background ,aod-bg))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,aod-bg-hl))))
   `(reb-match-1 ((t (:background ,aod-bg-1 :foreground ,aod-keyword :weight semi-bold))))
   `(reb-match-2 ((t (:background ,aod-bg-1 :foreground ,aod-success :weight semi-bold))))
   `(reb-match-3 ((t (:background ,aod-bg-1 :foreground ,aod-type :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face ((t (:underline (:color ,aod-error :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face ((t (:inherit default))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,aod-primary :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,aod-success :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,aod-success))))
   `(font-latex-warning-face ((t (:foreground ,aod-error))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,aod-builtin))))

   ;; org-mode
   `(org-date ((t (:foreground ,aod-builtin))))
   `(org-footnote ((t (:foreground ,aod-builtin))))
   `(org-sexp-date ((t (:foreground ,aod-builtin))))

   ;; calendar
   `(diary ((t (:inherit warning))))
   `(holiday ((t (:foreground ,aod-success))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,aod-warn))))
   `(breakpoint-enabled ((t (:foreground ,aod-error :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,aod-success))))
   `(realgud-overlay-arrow3        ((t (:foreground ,aod-warn))   `(realgud-overlay-arrow2        ((t (:foreground ,aod-type))))
                                    ))
   `(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,aod-error)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,aod-bg-hl)))))
   `(realgud-line-number           ((t (:foreground ,aod-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,aod-error))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,aod-warn))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,aod-builtin))))
   ))

(setq company-quickhelp-color-background "#121417")
(setq company-quickhelp-color-foreground "#9dA5B4")

(aod-with-color-variables
  (custom-theme-set-variables
   'aod
   ;; fill-column-indicator
   `(fci-rule-color ,aod-bg-hl)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `aod-type' |
   ;; | J         | `aod-primary'     |
   ;; | L         | `aod-warn' |
   ;; | Z         | `aod-error'    |
   ;; | S         | `aod-success'    |
   ;; | T         | `aod-keyword'   |
   ;; | I         | `aod-builtin'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,aod-bg-1 ,aod-error ,aod-success ,aod-type
                ,aod-primary ,aod-keyword ,aod-builtin ,aod-fg])
   ))

(defvar aod-theme-force-faces-for-mode t
  "If t, aod-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: `font-lock-constant-face', `font-lock-doc-face', `font-lock-variable-name-face'")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom One Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `aod-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
;; (defun aod-theme-change-faces-for-mode ()
;;   (interactive)
;;   (and (eq aod-theme-force-faces-for-mode t)
;;        (cond
;;         ((member major-mode '(js2-mode))
;;          ;; aod-warn
;;          (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
;;          (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
;;          ;; aod-mono-1
;;          (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
;;         )))

;; (add-hook 'after-change-major-mode-hook 'aod-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'aod)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; aod-theme.el ends here
