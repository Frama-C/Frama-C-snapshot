# TEMPLATE FOR MAKEFILE TO USE IN FRAMA-C/EVA CASE STUDIES

# DO NOT EDIT THE LINES BETWEEN THE '#'S

###############################################################################
# Improves analysis time, at the cost of extra memory usage
export FRAMA_C_MEMORY_FOOTPRINT = 8
#
# frama-c-path.mk contains variables which are specific to each
# user and should not be versioned, such as the path to the
# frama-c binaries (e.g. FRAMAC and FRAMAC_GUI).
# It is an optional include, unnecessary if frama-c is in the PATH
-include frama-c-path.mk
#
# FRAMAC_CONFIG is defined in frama-c-path.mk when it is included, so the
# line below will be safely ignored if this is the case
FRAMAC_CONFIG ?= frama-c-config
#
# frama-c.mk contains the main rules and targets
-include $(shell $(FRAMAC_CONFIG) -print-share-path)/analysis-scripts/frama-c.mk
#
###############################################################################

# EDIT VARIABLES AND TARGETS BELOW AS NEEDED
# The flags below are only suggestions to use with Eva, and can be removed

# (Optional) preprocessing flags, usually handled by -json-compilation-database
CPPFLAGS    +=

# (Optional) Frama-C general flags (parsing and kernel)
FCFLAGS     += \
  -kernel-warn-key annot:missing-spec=abort \
  -kernel-warn-key typing:implicit-function-declaration=abort \

# (Optional) Eva-specific flags
EVAFLAGS    += \
  -eva-warn-key builtins:missing-spec=abort \

# (MANDATORY) Name of the main target
MAIN_TARGET :=

# Remove these lines after defining the main target
ifeq ($(MAIN_TARGET),)
$(error MAIN_TARGET not defined in $(firstword $(MAKEFILE_LIST)))
endif

# Add other targets if needed
TARGETS = $(MAIN_TARGET).eva

# Default target
all: $(TARGETS)

# (MANDATORY) List of source files used by MAIN_TARGET.
# If there is a JSON compilation database,
# 'frama-c-script list-files' can help obtain it
$(MAIN_TARGET).parse:


# The following targets are optional and provided for convenience only
parse: $(TARGETS:%.eva=%.parse)
loop: $(TARGETS:%.eva=%.parse.loop) $(TARGETS:%=%.loop)
gui: $(MAIN_TARGET).eva.gui

# Run 'make <TARGET>.eva.loop' to obtain a .loop file, fine-tune it by hand,
# then rename it to <TARGET>.slevel to prevent it from being overwritten.
# If such file exists, use it to define per-function slevel values.
ifneq (,$(wildcard $(MAIN_TARGET).slevel))
$(MAIN_TARGET).eva: \
  EVAFLAGS += $(shell cat $(MAIN_TARGET).slevel | tr -d '\n\\')
endif
