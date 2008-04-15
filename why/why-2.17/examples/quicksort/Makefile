
VO=quicksort_valid.vo

all: Partition.vo Quicksort.vo $(VO)

partition_why.v partition_valid.v quicksort_why.v quicksort_valid.v: partition.mlw quicksort.mlw $(WHY)
	$(WHY) $(WHYOPTIONS) partition.mlw quicksort.mlw

partition.check quicksort.check: partition.mlw quicksort.mlw $(WHYBIN)
	$(WHYBIN) -tc partition.mlw quicksort.mlw

quicksort_why.sx: partition.mlw quicksort.mlw $(WHYBIN)
	$(WHY) --simplify partition.mlw quicksort.mlw

quicksort_why.why: partition.mlw quicksort.mlw $(WHYBIN)
	$(WHY) --why partition.mlw quicksort.mlw

include ../Makefile.common
