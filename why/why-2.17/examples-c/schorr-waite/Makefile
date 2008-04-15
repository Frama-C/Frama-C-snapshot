
check:
	caduceus schorr_waite.c

all: schorr_waite

transparent.ps: transparent.dvi
	dvips transparent.dvi -o transparent.ps

transparent.pdf: transparent.dvi
	dvipdf transparent.dvi 

transparent.dvi: transparent.tex
	latex transparent

%: %.c 
	caduceus $*.c
	make -f $*.makefile simplify
	make -f $*.makefile depend
	make -f $*.makefile coq

schorr_waite_split:
	caduceus -why-opt "--split" schorr_waite_split.c
	make -f schorr_waite_split.makefile simplify

clean:
	make -f schorr_waite.makefile clean
	rm -f why/*.why
