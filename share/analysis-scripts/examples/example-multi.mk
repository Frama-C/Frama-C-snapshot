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
all: example1.val example2.val

# Input files
example1.parse example2.parse: example.c

# Project specific parameters
example1.parse: CPPFLAGS += -D__FRAMAC__
example1.val:   FCFLAGS  += -main my_main
example2.val:   EVAFLAGS += -slevel 500
example2.val:   FCFLAGS  += -main main
