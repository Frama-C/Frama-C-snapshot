# frama-c-path.mk contains variables which are specific to each
# user and should not be versioned, such as the path to the
# frama-c binaries (e.g. FRAMAC and FRAMAC_GUI).
# It is an optional include, unnecessary if frama-c is in the PATH
-include frama-c-path.mk
# FRAMAC_CONFIG is defined in frama-c-path.mk when it is included, so the
# line below will be safely ignored if this is the case
FRAMAC_CONFIG ?= frama-c-config
# frama-c.mk should be included at the top of your Makefile, right below
# the inclusion of frama-c-path.mk
-include $(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/frama-c.mk

# Define global parameters
CPPFLAGS    += -D__I586__ -D__FRAMAC__
FCFLAGS     += -verbose 0 -main my_main
EVAFLAGS    += -plevel 611
EVABUILTINS += memset:Frama_C_memset memcpy:Frama_C_memcpy

# Export environment variable for Frama-C
export FRAMA_C_MEMORY_FOOTPRINT = 8

# Default target
all: example.val

# List input files
example.parse: example.c
