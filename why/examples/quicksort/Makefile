
VO=partition_valid.vo quicksort_valid.vo

all: Partition.vo Quicksort.vo $(VO)

partition_why.v partition_valid.v quicksort_why.v quicksort_valid.v: partition.mlw quicksort.mlw $(WHY)
	$(WHY) $(WHYOPTIONS) partition.mlw quicksort.mlw

quicksort_why.sx: partition.mlw quicksort.mlw $(WHY)
	$(WHY) --simplify partition.mlw quicksort.mlw

include ../Makefile.common
