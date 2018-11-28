# This example is the same as example-multi.mk but pay attention to the
# following changes :
# 1. slevel is set inside SLEVEL variable instead of EVAFLAGS to allow
#    overriding when testing specific slevels
# 2. A percent (%) is used in example1.% and example2.% so that
#    options are used also for instance for example1.5000.val which
#    is the same target as example1.val but with 5000 slevel.
# 3. The all rule invoke the script

-include frama-c-path.mk
FRAMAC_CONFIG ?= frama-c-config
-include $(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/frama-c.mk

# Global parameters
CPPFLAGS     = -D__I586__
FCFLAGS     += -verbose 0
EVAFLAGS    += -plevel 100
EVABUILTINS += memset:Frama_C_memset memcpy:Frama_C_memcpy

export FRAMA_C_MEMORY_FOOTPRINT = 8

# Default targets
all:
	$(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/slevel-tweaker.sh -f example-slevel.mk example1 example2

# Clean
clean::
	$(RM) slevel-tweaker.log

# Input files
example1.parse example2.parse: example.c

# Project specific parameters
example1.parse: CPPFLAGS += -D__FRAMAC__
example1.%:     FCFLAGS  += -main my_main
example2.%:     SLEVEL   += -slevel 500
example2.%:     FCFLAGS  += -main main
