# -*- mode: makefile -*-
EMACS	?= emacs
__ALLSRC__	:= $(wildcard *.el)
ELFILES		:= $(__ALLSRC__:%v.el=)
ELCFILES	:= $(ELFILES:%.el=%.elc)

all: $(ELCFILES)

%.elc: %.el
	@$(EMACS) -q -no-site-file --batch -L . -f batch-byte-compile $<

clean:
	rm -f $(ELCFILES)

distclean: clean
