PARTS := $(sort $(wildcard src/*.lisp))

.PHONY: all template clean

all: template

template: template.lisp

template.lisp: $(PARTS)
	cat $(PARTS) > $@

clean:
	rm -f template.lisp
