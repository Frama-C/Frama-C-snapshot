
ML=heapsort.mlw
V=$(ML:.mlw=_why.v) $(ML:.mlw=_valid.v)
VO=$(ML:.mlw=_valid.vo)

all: heap.vo Inftree.vo $(VO)

$(V): $(ML) $(WHY)
	$(WHY) $(WHYOPTIONS) $(ML)

heapsort_why.sx: swap.mlw downheap.mlw heapsort.mlw $(WHYBIN)
	$(WHY) --simplify $^

swap.check downheap.check heapsort.check: swap.mlw downheap.mlw heapsort.mlw $(WHYBIN)
	$(WHYBIN) -tc $^

heapsort_why.why: swap.mlw downheap.mlw heapsort.mlw $(WHYBIN)
	$(WHY) --why $^

include ../Makefile.common


