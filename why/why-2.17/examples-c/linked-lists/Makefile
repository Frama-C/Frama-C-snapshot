
CADUCEUS=../../bin/caduceus.opt 
CADUCEUSLIB=$(shell pwd)/../../lib
EXECCADUCEUS=CADUCEUSLIB=$(CADUCEUSLIB) $(CADUCEUS)

FILES = swap reverse

check:
	for f in $(FILES); do caduceus $$f.c; done

all: swap reverse

reverse:
	$(EXECCADUCEUS) reverse.c
	make -f reverse.makefile simplify
	make -f reverse.makefile coq

swap:
	$(EXECCADUCEUS) swap.c
	make -f swap.makefile simplify
	make -f swap.makefile coq

simplify:
	for f in $(FILES); do \
	  $(EXECCADUCEUS) $$f.c; \
	  make -f $$f.makefile simplify; \
	done

%: %.c 
	$(EXECCADUCEUS) $*.c
	make -f $*.makefile simplify
	make -f $*.makefile coq

.PHONY: coq simplify cvcl

clean:
	make -f reverse.makefile clean
	make -f swap.makefile clean
	make -f has_cycle.makefile clean
	rm -f why/*.why
