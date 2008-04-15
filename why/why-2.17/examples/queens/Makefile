
WHY=../../bin/why.opt
GWHY=../../bin/gwhy.opt
OPT=--no-arrays
VO=queens_valid.vo

gwhy:
	$(GWHY) $(OPT) queens.why

gwhy.split:
	$(GWHY) $(OPT) -split-user-conj queens.why

coq:
	$(WHY) $(OPT) --coq --coq-tactic "admit" queens.why
	coqide queens_why.v

coq.split:
	$(WHY) $(OPT) -split-user-conj --coq --coq-tactic "admit" queens.why
	coqide queens_why.v

FTP=$$HOME/WWW/why/queens/

export:
	why2html queens.why
	cp -f queens.why.html $(FTP)
	cp -f queens_why.v $(FTP)
	cp -f slides/slides-pp.pdf $(FTP)
	cp -f ../../bench/c/good/queens.c $(FTP)
	make -C report export


include ../Makefile.common
