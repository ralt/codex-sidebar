EMACS ?= emacs

.PHONY: check compile test
check: compile test

compile:
	$(EMACS) -Q --batch --eval '(progn (require (quote package)) (package-initialize))' -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile codex-sidebar.el

test:
	$(EMACS) -Q --batch --eval '(progn (require (quote package)) (package-initialize))' -L . -l test/codex-sidebar-test.el -f ert-run-tests-batch-and-exit
