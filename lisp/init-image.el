(message "imagemagick surpport format:\n")
(mapcar (lambda (s)
	  (message "%S" s))
	(imagemagick-types))

(message "imagemagick surpport format:\n")
(mapcar (lambda (s)
	  (message "%S" s))
	(imagemagick-register-types))


(defvar test-image nil)
(defvar image-file (expand-file-name "icons/javascript.svg" emacs-d))
;; (defvar image-file (expand-file-name "icons/js.png" emacs-d))
(setq test-image
      (create-image
       image-file
       (intern (f-ext image-file))
       ;; (intern (f-ext "js.png"))
       ;; 'imagemagick
       nil :ascent 'center :width 40 :height 40 :size 20))
(image-size test-image)
(get-buffer-create "test")
(with-current-buffer "test"
  (setq max-image-size 100)
  ;; (insert (propertize " " 'display test-image) "\n\n")
  (insert-image test-image)
  )
;; can't resize or scale svg image because emacs convert it to bitmap
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34024
