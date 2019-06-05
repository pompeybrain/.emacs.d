emacs=emacs

debug:
	emacs --debug-init

install:
	$(emacs) --batch -l guard/install.el

compile:
	$(emacs) --batch -l guard/compile.el --eval '(compile-all-el)'

profile:
	emacs -Q -l site-lisp/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath init.el)\"))" -f profile-dotemacs
