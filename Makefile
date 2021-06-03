EMACS ?= emacs

all: compile


compile:
	exec ${EMACS} -Q -batch -f batch-byte-compile diary-remind-period-after.el

clean-elc:
	rm -f diary-remind-period-after.elc

.PHONY:	all compile clean-elc
