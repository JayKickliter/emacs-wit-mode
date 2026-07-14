EMACS ?= emacs

.PHONY: compile test clean

compile:
	$(EMACS) -Q --batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile wit-mode.el

test: compile
	$(EMACS) -Q --batch -l wit-mode.el -l wit-mode-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f wit-mode.elc
