##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2018                                               #
#    CEA (Commissariat a l'energie atomique et aux energies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# --------------------------------------------------------------------------
# ---  Frama-C Config
# --------------------------------------------------------------------------

ifndef FRAMAC_SHARE
FRAMAC_SHARE  :=$(shell frama-c -journal-disable -print-path)
endif
ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR :=$(shell frama-c -journal-disable -print-libpath)
endif
PLUGIN_DIR ?=.

ifneq ("$(FRAMAC_INTERNAL)","yes")
include $(FRAMAC_SHARE)/Makefile.config
endif

# --------------------------------------------------------------------------
# ---  Plugin Config
# --------------------------------------------------------------------------

PLUGIN_ENABLE:=yes
PLUGIN_DYNAMIC:=yes
PLUGIN_NAME:=Qed
PLUGIN_CMO:= \
	hcons \
	listmap listset \
	intmap intset \
	idxmap idxset \
	mergemap mergeset collection \
	partition cache \
	bvars logic \
	pool kind term \
	plib pretty engine export \
	export_whycore \
	export_altergo \
	export_why3 \
	export_coq \

PLUGIN_GUI_CMO:= QedGui

PLUGIN_DEPENDENCIES:=
PLUGIN_TESTS_DIRS:=
PLUGIN_BFLAGS:=
PLUGIN_OFLAGS:=
PLUGIN_GENERATED:= $(PLUGIN_DIR)/Qed.mli

PLUGIN_DISTRIBUTED:=$(PLUGIN_ENABLED)
PLUGIN_DISTRIB_EXTERNAL:= Makefile 

include $(FRAMAC_SHARE)/Makefile.dynamic

# --------------------------------------------------------------------------
# ---  Plugin API
# --------------------------------------------------------------------------

QED_API= \
	hcons.mli \
	listset.mli listmap.mli \
	intset.mli intmap.mli \
	idxset.mli idxmap.mli \
	mergemap.mli mergeset.mli collection.mli \
	partition.mli cache.mli \
	bvars.mli \
	logic.ml \
	pool.mli kind.mli term.mli \
	plib.mli pretty.mli engine.ml export.mli \
	export_whycore.mli \
	export_altergo.mli \
	export_why3.mli \
	export_coq.mli \

QED_MLI=$(addprefix $(Qed_DIR)/, $(QED_API))

define QED_capitalize
$(shell printf "%s%s" \
	$$($(ECHO) $(1) | cut -c 1 | tr '[:lower:]' '[:upper:]')
	$$($(ECHO) $(1) | cut -c 2-))
endef

define QED_export
$(ECHO) "module $(call QED_capitalize, $(basename $(notdir $(1)))) : sig" >> $(2);
$(CAT) $(1) >> $(2);
$(ECHO) "end" >> $(2);
endef

$(Qed_DIR)/Qed.mli: $(QED_MLI)
	$(PRINT_MAKING) $@
	$(RM) $@ $@.tmp
	$(ECHO) "(* This file is generated. Do not edit. *)" > $@.tmp
	$(ECHO) "(** {b Qed Public API} *)" > $@.tmp
	$(foreach file,$(QED_MLI),$(call QED_export,$(file),$@.tmp))
	$(CHMOD_RO) $@.tmp
	$(MV) $@.tmp $@

clean::
	$(RM) $(Qed_DIR)/Qed.mli

# --------------------------------------------------------------------------
