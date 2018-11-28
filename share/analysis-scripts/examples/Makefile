TARGETS=example example-multi example-slevel

.PHONY: all update-submodules clean $(TARGETS)

all: $(TARGETS)

update-submodules:
	git submodule update --init --recursive --remote

clean:
	@for f in $(TARGETS); \
	do \
	  $(MAKE) --no-print-directory --file $$f.mk clean; \
	done

$(TARGETS): %: %.mk
	@$(MAKE) --no-print-directory --file $<

