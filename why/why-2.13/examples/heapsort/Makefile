
ML=swap.mlw downheap.mlw heapsort.mlw
V=$(ML:.mlw=_why.v) $(ML:.mlw=_valid.v)
VO=$(ML:.mlw=_valid.vo)

all: heap.vo Inftree.vo $(VO)

$(V): $(ML) $(WHY)
	$(WHY) $(WHYOPTIONS) $(ML)

$(ML:.mlw=_why.sx): $(ML)
	$(WHY) --simplify $(ML)

include ../Makefile.common


