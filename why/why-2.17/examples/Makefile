
DIRS = misc binary-search bresenham quicksort edit-distance heapsort sqrt kmp \
       selectionsort find quicksort2 maximumsort algo-63-64-65 dijkstra \
       mergesort queens

check: 
	for d in $(DIRS); do make -C $$d check; done

all: 
	for d in $(DIRS); do make -C $$d; done

simplify:
	for d in $(DIRS); do make -C $$d simplify; done

ergo:
	for d in $(DIRS); do make -C $$d ergo; done

clean::
	rm -f */*~ */*.vo

depend::
	for d in $(DIRS); do make -C $$d depend; done
