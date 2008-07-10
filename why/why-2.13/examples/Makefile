
DIRS = misc binary-search bresenham quicksort edit-distance heapsort sqrt kmp \
       selectionsort find quicksort2 maximumsort 
# string-matching
# binary-search-c

all: 
	for d in $(DIRS); do make -C $$d; done

simplify:
	for d in $(DIRS); do make -C $$d simplify; done

clean::
	rm -f */*~ */*.vo

depend::
	for d in $(DIRS); do make -C $$d depend; done
