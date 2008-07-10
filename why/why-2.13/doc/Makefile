HEVEA=hevea -fix

all: main.pdf manual.ps caduceus.ps $(KFILES) krakatoa.pdf

doc.dvi: doc.tex rules.tex macros.tex code.tex dep.ps

SRCFILES=../src/loc.mli ../src/logic.mli ../src/types.mli \
	 ../src/ptree.mli ../src/ltyping.mli ../src/ast.mli ../src/env.mli \
         ../src/typing.mli ../src/typing.ml ../src/wp.mli ../src/wp.ml \
	 ../src/monad.mli ../src/monad.ml ../src/mlize.mli ../src/mlize.ml

KFILES=Lesson1_max.pp \
	Lesson1_sqrt.pp Lesson1_sqrtbody.pp Lesson1_sqrtloopinv.pp \
	Lesson1_lemmas.pp Lesson1_pragma.pp Lesson1_sqrtdecr.pp \
	Arrays_findMax.pp Arrays_findMaxbody.pp \
	Arrays_findMax2.pp Arrays_shift.pp \
	Purse.pp Purse0.pp Purse_test1.pp Purse_test2.pp Purse2.pp \
	Purse_withdraw2.pp \
	Flag.pp Flag1.pp Flag_inv.pp Flag_isMonochrome.pp Flag_flag.pp \
	Flag_swap.pp Flag_body.pp \
	contracts.bnf lexpr.bnf \
	Gcd-nospec.pp Gcd-spec.pp Gcd.pp Gcd-lemmas.pp

dep.ps:
	(cd ../src; ocamldep *.ml* | ocamldot | dot -Tps) > dep.ps

code.tex: $(SRCFILES)
	ocamlweb --no-preamble -o $@ $(SRCFILES)

logo-why.pdf: logos.tex
	pdflatex logos.tex

logo-why.png: logo-why.pdf
	pdftoppm logos.pdf | pnmcrop | pnmtopng > logowhy.png

logo-why-small.png:

manual.ps: manual.tex version.tex
	latex manual
	latex manual
	makeindex manual
	latex manual
	dvips manual.dvi -o manual.ps

manual.html: manual.tex version.tex
	rm -f manual.aux
	$(HEVEA) manual.tex

caduceus.ps: caduceus.tex version.tex
	latex caduceus
	bibtex caduceus
	makeindex caduceus
	latex caduceus
	latex caduceus
	dvips caduceus.dvi -o caduceus.ps

caduceus.html: caduceus.tex version.tex
	rm -f caduceus.aux
	$(HEVEA) caduceus.tex

krakatoa.pdf: $(KFILES) krakatoa.tex version.tex
	pdflatex krakatoa.tex
	bibtex krakatoa
	makeindex krakatoa
	pdflatex krakatoa.tex
	pdflatex krakatoa.tex

february2008.pdf: $(KFILES) version.tex
	pdflatex february2008.tex

krakatoa.html: krakatoa.tex version.tex
	rm -f krakatoa.aux
	$(HEVEA) krakatoa.tex

main.html: main.tex version.tex
	rm -f main.aux
	$(HEVEA) main.tex


main.pdf: main.tex version.tex
	pdflatex main.tex


%.pp: %.tex pp
	./pp $< > $@

%.pp: %.c pp
	./pp -c $< > $@

%.pp: %.java pp
	./pp -java $< > $@

%.dvi: %.tex
	latex $< && latex $<

%.ps: %.dvi
	dvips -o $@ $<

%.eps: %.dia
	dia --filter=eps-builtin $^

%.pdf: %.eps
	epstopdf $^

%.tex: %.dia
	dia --filter=tex $^

%.png: %.dia
	dia --filter=png --size=2048,1536 $^

%.bnf: %.tex tex2bnf
	./tex2bnf < $< > $@

tex2bnf: tex2bnf.ml
	ocamlopt.opt -o $@ $^

pp: pp.ml
	ocamlopt.opt -o $@ $^

%.ml: %.mll
	ocamllex.opt $<

clean::
	rm -f *~ *.dvi *.log *.aux *.toc \
	dep.ps doc.ps manual.ps caduceus.ps krakatoa.pdf main.pdf 

