
DIRS = tutorial sorting g4 linked-lists schorr-waite

check:
	for d in $(DIRS); do make -C $$d check; done

all: 
	for d in $(DIRS); do \
		if make -C $$d coq; then \
			echo "Coq proofs OK in $$d" ; \
		else \
			echo "Coq proofs FAILED in $$d" ; \
			exit 1 ;\
		fi ; \
	done

bench:
	make -C tutorial allcoq
	for d in $(DIRS); do make -C $$d simplify; done

clean:
	for d in $(DIRS); do make -C $$d clean; done
