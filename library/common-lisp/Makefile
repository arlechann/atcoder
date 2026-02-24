PARTS := $(sort $(wildcard src/*.lisp))

.PHONY: all template test benchmark benchmark-match benchmark-pascal clean

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

benchmark: benchmark-match benchmark-pascal

benchmark-match:
	cd ../.. && sbcl --noinform --script library/common-lisp/tests/match-benchmark.lisp

benchmark-pascal:
	cd ../.. && sbcl --noinform --script library/common-lisp/tests/pascal-benchmark.lisp

clean:
	rm -f template.lisp
