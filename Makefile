emacs=emacs --quick --batch

test:
	$(emacs) --eval '(setq emacs-directory default-directory)'

install:
	$(emacs) -batch -l lisp/install-packages.el

profile:
	emacs -Q -l site-lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs
