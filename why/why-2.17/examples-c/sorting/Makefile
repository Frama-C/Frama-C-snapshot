
CADUCEUS=../../bin/caduceus.opt -why-opt -split-user-conj
CADUCEUSLIB=$(shell pwd)/../../lib
EXECCADUCEUS=CADUCEUSLIB=$(CADUCEUSLIB) $(CADUCEUS)

FILES = quicksort selection

TODO = selection

check:
	for f in $(FILES); do caduceus $$f.c; done

coq: $(FILES)

allcoq:
	for f in $(FILES); do \
	  $(EXECCADUCEUS) $$f.c; \
	  make -f $$f.makefile depend; \
	  make -f $$f.makefile coq; \
	done

simplify:
	for f in $(FILES); do \
	  $(EXECCADUCEUS) $$f.c; \
	  make -f $$f.makefile simplify; \
	done

gui:
	for f in $(FILES); do \
	  $(EXECCADUCEUS) $$f.c; \
	  make -f $$f.makefile gui; \
	done

cvcl:
	for f in $(FILES); do \
	  $(EXECCADUCEUS) $$f.c; \
	  make -f $$f.makefile cvcl; \
	done

.PHONY: coq simplify cvcl

why/%.why: %.c 
	rm -f coq/caduceus_spec_why.v
	$(EXECCADUCEUS) $<

%: %.c
	rm -f why/$*.why
	make why/$*.why
	make -f $*.makefile coq

clean:
	make -f selection.makefile clean
	rm -f why/*.why
