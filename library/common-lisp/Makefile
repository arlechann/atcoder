PARTS := $(sort $(wildcard src/*.lisp))

.PHONY: all template test clean

all: template

template: template.lisp

template.lisp: $(PARTS)
	cat $(PARTS) > $@

test:
	sbcl --noinform --disable-debugger \
		--eval "(ignore-errors (sb-ext:unlock-package :sb-di))" \
		--eval "(pushnew :swank *features*)" \
		--eval "(load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))" \
		--eval "(pushnew (truename \"$(CURDIR)/\") asdf:*central-registry* :test (function equal))" \
		--eval "(asdf:load-system \"atcoder-library-core\" :force t)" \
		--eval "(asdf:load-system \"atcoder-library-tests\" :force t)" \
		--eval "(rove:run \"atcoder-library-tests\")" \
		--quit

clean:
	rm -f template.lisp
