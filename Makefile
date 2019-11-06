##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2019                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
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

# This file is the main makefile of Frama-C.

FRAMAC_SRC=.
MAKECONFIG_DIR=share
PLUGIN_TESTS_LIST:=
include share/Makefile.common
include share/Makefile.dynamic_config.internal

#Check share/Makefile.config available
ifndef FRAMAC_ROOT_SRCDIR
$(error \
  "You should run ./configure first (or autoconf if there is no configure)")
endif

###################
# Frama-C Version #
###################

VERSION:=$(shell $(CAT) VERSION)
VERSION_CODENAME:=$(shell $(CAT) VERSION_CODENAME)

###########################
# Global plugin variables #
###########################

# the directory where compiled plugin non-GUI files are stored
PLUGIN_TOP_LIB_DIR= $(PLUGIN_LIB_DIR)/top

# the directory where compiled plugin GUI files are stored
PLUGIN_GUI_LIB_DIR= $(PLUGIN_LIB_DIR)/gui

# the directory where the other Makefiles are
FRAMAC_SHARE	= share

# Shared lists between Makefile.plugin and Makefile :
# initialized them as "simply extended variables" (with :=)
# for a correct behavior of += (see section 6.6 of GNU Make manual)
PLUGIN_LIST	:=
PLUGIN_GENERATED_LIST:=
PLUGIN_CMO_LIST	:=
PLUGIN_CMX_LIST	:=
PLUGIN_META_LIST :=
PLUGIN_DYN_CMI_LIST :=
PLUGIN_DYN_CMO_LIST :=
PLUGIN_DYN_CMX_LIST :=
PLUGIN_INTERNAL_CMO_LIST:=
PLUGIN_INTERNAL_CMX_LIST:=
PLUGIN_GUI_CMO_LIST:=
PLUGIN_GUI_CMX_LIST:=
PLUGIN_DYN_DEP_GUI_CMO_LIST:=
PLUGIN_DYN_DEP_GUI_CMX_LIST:=
PLUGIN_DYN_GUI_CMO_LIST :=
PLUGIN_DYN_GUI_CMX_LIST :=
PLUGIN_TYPES_CMO_LIST :=
PLUGIN_TYPES_CMX_LIST :=
PLUGIN_DEP_LIST:=
PLUGIN_DOC_LIST :=
PLUGIN_DOC_DIRS :=
PLUGIN_DISTRIBUTED_LIST:=
PLUGIN_DIST_TARGET_LIST:=
PLUGIN_DIST_DOC_LIST:=
PLUGIN_BIN_DOC_LIST:=
PLUGIN_DIST_EXTERNAL_LIST:=
PLUGIN_DIST_TESTS_LIST:=
PLUGIN_DISTRIBUTED_NAME_LIST:=
MERLIN_PACKAGES:=

PLUGIN_HEADER_SPEC_LIST :=
PLUGIN_HEADER_DIRS_LIST :=
PLUGIN_HEADER_EXCEPTIONS_LIST :=
PLUGIN_CEA_PROPRIETARY_HEADERS_LIST :=
PLUGIN_CEA_PROPRIETARY_FILES_LIST :=

# default value used for HEADER_SPEC and PLUGIN_HEADER_SPEC
DEFAULT_HEADER_SPEC := headers/header_spec.txt
# default value used for HEADER_DIRS and PLUGIN_HEADER_DIRS
DEFAULT_HEADER_DIRS := headers
# default value used for HEADER_EXCEPTIONS and PLUGIN_HEADER_EXCEPTIONS
DEFAULT_HEADER_EXCEPTIONS := configure
# default value used for CEA_PROPRIETARY_FILES and PLUGIN_CEA_PROPRIETARY_FILES
DEFAULT_CEA_PROPRIETARY_FILES := tests/non-free/%
# default value used for CEA_PROPRIETARY_HEADERS
# and PLUGIN_CEA_PROPRIETARY_HEADERS
DEFAULT_CEA_PROPRIETARY_HEADERS := CEA_PROPRIETARY

MERLIN_PACKAGES:=

###############################
# Additional global variables #
###############################

# Directories containing some source code
SRC_DIRS= ptests $(PLUGIN_LIB_DIR) $(FRAMAC_SRC_DIRS)

# Directory containing source code documentation
DOC_DIR	= doc/code

# Source files to document
MODULES_TODOC=

# Directories to include when compiling
INCLUDES=$(addprefix -I ,$(FRAMAC_SRC_DIRS)) -I $(PLUGIN_LIB_DIR)
ifneq ($(ENABLE_GUI),no)
GUI_INCLUDES = $(addprefix -package ,$(LIBRARY_NAMES_GUI))
else
GUI_INCLUDES =
endif

# Files for which dependencies must be computed.
# Other files are added later in this Makefile.
FILES_FOR_OCAMLDEP+=$(PLUGIN_LIB_DIR)/*.mli

BFLAGS	= $(PACKAGES) $(FLAGS) $(DEBUG) $(INCLUDES) \
		$(FRAMAC_USER_BFLAGS)
OFLAGS	= $(PACKAGES) $(FLAGS) $(DEBUG) $(INCLUDES) -compact \
		$(FRAMAC_USER_OFLAGS)

BLINKFLAGS += -linkpkg $(BFLAGS) -linkall -custom
OLINKFLAGS += -linkpkg $(OFLAGS) -linkall

DOC_FLAGS= -charset utf8 -colorize-code -stars -m A $(PACKAGES) $(INCLUDES) $(GUI_INCLUDES)

ifneq ($(VERBOSEMAKE),yes)
DOC_FLAGS+= -hide-warnings
endif

# Files that depend on external libraries -namely Zarith- whose interface
# has not been explicitely declared -opaque, hence would trigger warning 58.
# This can't be solved by Frama-C itself, we can only wait for an update of
# said library.

NON_OPAQUE_DEPS:=

# Libraries generated by Frama-C
GEN_BYTE_LIBS=
GEN_OPT_LIBS=

# Libraries used in Frama-C
EXTRA_OPT_LIBS:=
PACKAGES = $(addprefix -package ,$(LIBRARY_NAMES))
BYTE_LIBS = $(GEN_BYTE_LIBS)
OPT_LIBS = $(EXTRA_OPT_LIBS)

OPT_LIBS+= $(GEN_OPT_LIBS)

ICONS:= $(addprefix share/,\
		frama-c.ico frama-c.png unmark.png \
		switch-on.png switch-off.png)

THEME_ICON_NAMES:= \
		never_tried.png \
		unknown.png \
		surely_valid.png \
		surely_invalid.png \
		considered_valid.png \
		valid_under_hyp.png \
		invalid_under_hyp.png \
		invalid_but_dead.png \
		unknown_but_dead.png \
		valid_but_dead.png \
		inconsistent.png \
		fold.png unfold.png

THEME_ICONS_DEFAULT:= \
  $(addprefix share/theme/default/,$(THEME_ICON_NAMES))
THEME_ICONS_COLORBLIND:= \
  $(addprefix share/theme/colorblind/,$(THEME_ICON_NAMES))
THEME_ICONS_FLAT:= \
  $(addprefix share/theme/flat/,$(THEME_ICON_NAMES))

ROOT_LIBC_DIR:= share/libc
LIBC_SUBDIRS:= sys netinet net arpa
LIBC_DIR:= $(ROOT_LIBC_DIR) $(addprefix $(ROOT_LIBC_DIR)/,$(LIBC_SUBDIRS))
LIBC_FILES:= \
	$(wildcard share/*.h share/*.c) \
	$(wildcard $(addsuffix /*.h,$(LIBC_DIR))) \
	$(wildcard $(addsuffix /*.c,$(LIBC_DIR)))

# Checks that all .h can be included multiple times.
ALL_LIBC_HEADERS:=$(wildcard share/*.h $(addsuffix /*.h,$(LIBC_DIR)))

check-libc: bin/toplevel.$(OCAMLBEST)$(EXE)
	@echo "checking libc..."; \
	 EXIT_VALUE=0; \
	 for file in $(filter-out share/builtin.h,$(ALL_LIBC_HEADERS)); do \
	   echo "#include \"$$file\"" > check-libc.c; \
	   echo "#include \"$$file\"" >> check-libc.c; \
	   FRAMAC_SHARE=share bin/toplevel.$(OCAMLBEST)$(EXE) \
	   -cpp-extra-args="-Ishare/libc -nostdinc" check-libc.c \
	       > $$(basename $$file .h).log 2>&1; \
	   if test $$? -ne 0; then \
	     if grep -q -e '#error "Frama-C:' $$file; then : ; \
	     else \
	       echo "$$file cannot be included twice. \
Output is in $$(basename $$file .h).log"; \
	       EXIT_VALUE=1; \
	     fi; \
	   else \
	     rm $$(basename $$file .h).log; \
	   fi; \
	 done; \
	 rm check-libc.c; \
	 exit $$EXIT_VALUE

clean-check-libc:
	$(RM) *.log

# Kernel files to be included in the distribution.
# Plug-ins should use PLUGIN_DISTRIB_EXTERNAL if they export something else
# than *.ml* files in their directory.
# NB: configure for the distribution is generated in the distrib directory
# itself, rather than copied: otherwise, it could include references to
# non-distributed plug-ins.
DISTRIB_FILES:=\
      $(wildcard bin/migration_scripts/*2*.sh) bin/local_export.sh      \
      bin/frama-c bin/frama-c.byte bin/frama-c-gui bin/frama-c-gui.byte \
      bin/frama-c-config bin/frama-c-script                             \
      share/frama-c.WIN32.rc share/frama-c.Unix.rc                      \
      $(ICONS) $(THEME_ICONS_DEFAULT) $(THEME_ICONS_COLORBLIND)         \
      $(THEME_ICONS_FLAT)                                               \
      man/frama-c.1 doc/README						\
      doc/code/docgen.ml                                                \
      doc/code/*.css doc/code/intro_plugin.txt				\
      doc/code/intro_plugin_D_and_S.txt                                 \
      doc/code/intro_plugin_default.txt                                 \
      doc/code/intro_kernel_plugin.txt					\
      doc/code/toc_head.htm                                             \
      doc/code/toc_tail.htm                                             \
      doc/Makefile                                                      \
      $(filter-out ptests/ptests_config.ml,$(wildcard ptests/*.ml*))   \
      configure.in Makefile Makefile.generating				\
      Changelog config.h.in						\
      VERSION VERSION_CODENAME $(wildcard licenses/*)                   \
      $(LIBC_FILES)							\
      share/analysis-scripts/benchmark_database.py                      \
      share/analysis-scripts/cmd-dep.sh                                 \
      share/analysis-scripts/concat-csv.sh                              \
      share/analysis-scripts/clone.sh                                   \
      $(wildcard share/analysis-scripts/examples/*)                     \
      share/analysis-scripts/fc_stubs.c                                 \
      share/analysis-scripts/find_fun.py                                \
      share/analysis-scripts/flamegraph.pl                              \
      share/analysis-scripts/frama-c.mk                                 \
      share/analysis-scripts/frama_c_results.py                         \
      share/analysis-scripts/git_utils.py                               \
      share/analysis-scripts/list_files.py                              \
      share/analysis-scripts/make_template.py                           \
      share/analysis-scripts/make_wrapper.py                            \
      share/analysis-scripts/parse-coverage.sh                          \
      share/analysis-scripts/README.md                                  \
      share/analysis-scripts/results_display.py                         \
      share/analysis-scripts/summary.py                                 \
      share/analysis-scripts/template.mk                                \
      $(wildcard share/emacs/*.el) share/autocomplete_frama-c           \
      share/_frama-c                                                    \
      share/compliance/c11_functions.json                               \
      share/compliance/glibc_functions.json                             \
      share/compliance/nonstandard_identifiers.json                     \
      share/compliance/posix_identifiers.json                           \
      share/configure.ac                                                \
      share/Makefile.config.in share/Makefile.common                    \
      share/Makefile.generic						\
      share/Makefile.plugin.template share/Makefile.dynamic		\
      share/Makefile.dynamic_config.external				\
      share/Makefile.dynamic_config.internal				\
      share/META.frama-c                                                \
      $(filter-out src/kernel_internals/runtime/config.ml,              \
	  $(wildcard src/kernel_internals/runtime/*.ml*))               \
      $(wildcard src/kernel_services/abstract_interp/*.ml*)             \
      $(wildcard src/plugins/gui/*.ml*)                                 \
      $(wildcard src/libraries/stdlib/*.ml*)                            \
      $(wildcard src/libraries/utils/*.ml*)                             \
      $(wildcard src/libraries/utils/*.c)                               \
      $(wildcard src/libraries/project/*.ml*)                           \
      $(filter-out src/kernel_internals/parsing/check_logic_parser.ml,  \
          $(wildcard src/kernel_internals/parsing/*.ml*))               \
      $(wildcard src/kernel_internals/typing/*.ml*)                     \
      $(wildcard src/kernel_services/ast_data/*.ml*)                    \
      $(wildcard src/kernel_services/ast_queries/*.ml*)                 \
      $(wildcard src/kernel_services/ast_printing/*.ml*)                \
      $(wildcard src/kernel_services/cmdline_parameters/*.ml*)          \
      $(wildcard src/kernel_services/analysis/*.ml*)                    \
      $(wildcard src/kernel_services/ast_transformations/*.ml*)         \
      $(wildcard src/kernel_services/plugin_entry_points/*.ml*)         \
      $(wildcard src/kernel_services/visitors/*.ml*)                    \
      $(wildcard src/kernel_services/parsetree/*.ml*)                   \
      $(wildcard src/libraries/datatype/*.ml*)                          \
      bin/sed_get_make_major bin/sed_get_make_minor                     \
      INSTALL.md README.md .make-clean	                        \
      .make-clean-stamp .force-reconfigure 	\
      opam/opam opam/descr \

# Test files to be included in the distribution (without header checking).
# Plug-ins should use PLUGIN_DISTRIB_TESTS to export their test files. 
DISTRIB_TESTS=$(shell git ls-files \
                  tests \
                  src/plugins/aorai/tests \
                  src/plugins/report/tests \
                  src/plugins/wp/tests)


# files that are needed to compile API documentation of external plugins
DOC_GEN_FILES:=$(addprefix doc/code/,\
	*.css intro_plugin.txt intro_kernel_plugin.txt \
	intro_plugin_default.txt intro_plugin_D_and_S \
	kernel-doc.ocamldoc \
	docgen.ml docgen.cm* *.htm)

################
# Main targets #
################

# additional compilation targets for 'make all'.
# cannot be delayed after 'make all'
EXTRAS	= ptests bin/fc-config$(EXE)

ifneq ($(ENABLE_GUI),no)
ifeq ($(HAS_LABLGTK),yes)
EXTRAS	+= gui
endif
endif

all:: byte $(OCAMLBEST) $(EXTRAS) plugins_ptests_config

.PHONY: top opt byte dist bdist rebuild rebuild-branch

dist: clean
	$(QUIET_MAKE) all

clean-rebuild: clean
	$(QUIET_MAKE) all

rebuild: config.status
	$(MAKE) smartclean
	$(MAKE) depend $(FRAMAC_PARALLEL)
	$(MAKE) all $(FRAMAC_PARALLEL) || \
		(touch .force-reconfigure; \
		 $(MAKE) config.status && \
		 $(MAKE) depend $(FRAMAC_PARALLEL) && \
		 $(MAKE) all $(FRAMAC_PARALLEL))

sinclude .Makefile.user
# Should define FRAMAC_PARALLEL, FRAMAC_USER_FLAGS, FRAMAC_USER_MERLIN_FLAGS

#Create link in share for local execution if
.PHONY:create_share_link
create_share_link: share/.gitignore

# note: when using opam pin path in a cloned Frama-C git, the symbolic links
# become directories, so a different command is necessary for each situation

share/.gitignore: share/Makefile.config
	if test -f $@; then \
	  for link in $$(cat $@); do \
	    if test -L share$$link; then \
	      rm -f share$$link \
	    else \
	      rm -rf share$$link; \
	    fi; \
	  done; \
	fi
	$(RM) $@.tmp
	touch $@.tmp
	$(foreach dir,$(EXTERNAL_PLUGINS),\
		if test -d $(dir)/share; then \
			echo "Sharing $(dir)/link"; \
			ln -s $(realpath $(dir)/share) share/$(notdir $(dir)); \
			echo /$(notdir $(dir)) >> $@.tmp; \
		fi; )
	mv $@.tmp $@

ifeq ("$(DEVELOPMENT)","yes")
all:: share/.gitignore
endif

clean_share_link:
	if test -f share/.gitignore; then \
	  for link in $$(cat share/.gitignore); do \
	    if test -L share$$link; then \
	      rm -f share$$link \
	    else \
	      rm -rf share$$link; \
	    fi; \
	  done; \
	  rm share/.gitignore; \
	fi

clean:: clean_share_link

##############
# Ocamlgraph #
##############

# dgraph (included in ocamlgraph)
#[LC] Cf https://github.com/backtracking/ocamlgraph/pull/32
ifeq ($(HAS_GNOMECANVAS),yes)
ifneq ($(ENABLE_GUI),no)
GRAPH_GUICMO= dgraph.cmo
GRAPH_GUICMX= dgraph.cmx
GRAPH_GUIO= dgraph.o
HAS_DGRAPH=yes
else # enable_gui is no: disable dgraph
HAS_DGRAPH=no
endif
else # gnome_canvas is not yes: disable dgraph
HAS_DGRAPH=no
endif

##################
# Frama-C Kernel #
##################

# Libraries which could be compiled fully independently
#######################################################

VERY_FIRST_CMO = src/kernel_internals/runtime/frama_c_init.cmo
CMO	+= $(VERY_FIRST_CMO)

LIB_CMO =\
        src/libraries/stdlib/transitioning \
	src/libraries/stdlib/FCSet \
	src/libraries/stdlib/FCMap \
	src/libraries/stdlib/FCBuffer \
	src/libraries/stdlib/FCHashtbl \
	src/libraries/stdlib/extlib \
	src/libraries/datatype/unmarshal \
	src/libraries/datatype/unmarshal_z \
	src/libraries/datatype/structural_descr \
	src/libraries/datatype/type \
	src/libraries/datatype/descr \
	src/libraries/utils/filepath \
	src/libraries/utils/sanitizer \
	src/libraries/utils/pretty_utils \
	src/libraries/utils/hook \
	src/libraries/utils/bag \
	src/libraries/utils/wto \
	src/libraries/utils/vector \
	src/libraries/utils/indexer \
	src/libraries/utils/rgmap \
	src/libraries/utils/bitvector \
	src/libraries/utils/qstack \
	src/libraries/stdlib/integer \
	src/libraries/utils/json \
	src/libraries/utils/markdown \
	src/libraries/utils/rich_text \
	src/libraries/utils/dotgraph

NON_OPAQUE_DEPS+=\
  src/libraries/datatype/unmarshal_z \
  src/libraries/stdlib/integer

LIB_CMO:= $(addsuffix .cmo,$(LIB_CMO))
CMO	+= $(LIB_CMO)

# Very first files to be linked (most modules use them)
###############################

FIRST_CMO= src/kernel_internals/runtime/config \
	src/kernel_internals/runtime/gui_init \
	src/kernel_services/plugin_entry_points/log \
	src/kernel_services/cmdline_parameters/cmdline \
	src/libraries/project/project_skeleton \
	src/libraries/datatype/datatype \
	src/kernel_services/plugin_entry_points/journal

# project_skeleton requires log
# datatype requires project_skeleton
# rangemap requires datatype

FIRST_CMO:= $(addsuffix .cmo,$(FIRST_CMO))
CMO	+= $(FIRST_CMO)

#Project (Project_skeleton must be linked before Journal)
PROJECT_CMO= \
	state \
	state_dependency_graph \
	state_topological \
	state_selection \
	project \
	state_builder
PROJECT_CMO:= $(patsubst %,src/libraries/project/%.cmo,$(PROJECT_CMO))
CMO	+= $(PROJECT_CMO)

# kernel
########

KERNEL_CMO=\
	src/libraries/utils/utf8_logic.cmo                              \
	src/libraries/utils/binary_cache.cmo                            \
	src/libraries/utils/hptmap.cmo                                  \
	src/libraries/utils/hptset.cmo                                  \
	src/libraries/utils/escape.cmo                                  \
	src/kernel_services/ast_queries/cil_datatype.cmo             \
	src/kernel_services/cmdline_parameters/typed_parameter.cmo      \
	src/kernel_services/plugin_entry_points/dynamic.cmo             \
	src/kernel_services/cmdline_parameters/parameter_category.cmo   \
	src/kernel_services/cmdline_parameters/parameter_customize.cmo  \
	src/kernel_services/cmdline_parameters/parameter_state.cmo      \
	src/kernel_services/cmdline_parameters/parameter_builder.cmo    \
	src/kernel_services/plugin_entry_points/plugin.cmo              \
	src/kernel_services/plugin_entry_points/kernel.cmo              \
	src/libraries/utils/unicode.cmo                                 \
	src/kernel_services/plugin_entry_points/emitter.cmo             \
	src/libraries/utils/floating_point.cmo                          \
	src/libraries/utils/rangemap.cmo                                \
	src/kernel_services/ast_printing/cil_types_debug.cmo            \
	src/kernel_services/ast_printing/printer_builder.cmo            \
	src/libraries/utils/cilconfig.cmo                               \
	src/kernel_internals/typing/alpha.cmo                         \
	src/kernel_services/ast_queries/cil_state_builder.cmo        \
	src/kernel_internals/runtime/machdeps.cmo                       \
	src/kernel_services/ast_queries/cil_const.cmo                \
	src/kernel_services/ast_queries/logic_env.cmo                \
	src/kernel_services/ast_queries/logic_const.cmo              \
	src/kernel_services/visitors/visitor_behavior.cmo		\
	src/kernel_services/ast_queries/cil.cmo                      \
	src/kernel_internals/parsing/errorloc.cmo                      \
	src/kernel_services/ast_printing/cil_printer.cmo                \
	src/kernel_services/ast_printing/cil_descriptive_printer.cmo    \
	src/kernel_services/parsetree/cabs.cmo                               \
	src/kernel_services/parsetree/cabshelper.cmo                         \
	src/kernel_services/ast_printing/logic_print.cmo                \
	src/kernel_services/ast_queries/logic_utils.cmo              \
	src/kernel_internals/parsing/logic_parser.cmo                  \
	src/kernel_internals/parsing/logic_lexer.cmo                   \
	src/kernel_services/ast_queries/logic_typing.cmo             \
	src/kernel_services/ast_queries/ast_info.cmo                 \
	src/kernel_services/ast_data/ast.cmo                            \
	src/kernel_services/ast_printing/cprint.cmo                     \
	src/kernel_services/visitors/cabsvisit.cmo                      \
	src/kernel_internals/typing/cabs2cil.cmo                      \
	src/kernel_services/ast_data/globals.cmo                        \
	src/kernel_internals/typing/cfg.cmo                           \
	src/kernel_services/ast_data/kernel_function.cmo                \
	src/kernel_services/ast_data/property.cmo                       \
	src/kernel_services/ast_data/property_status.cmo                \
	src/kernel_services/ast_data/annotations.cmo                    \
	src/kernel_services/ast_printing/printer.cmo                    \
	src/kernel_internals/typing/logic_builtin.cmo                 \
	src/kernel_services/ast_printing/cabs_debug.cmo                 \
	src/kernel_internals/parsing/lexerhack.cmo                     \
	src/kernel_internals/parsing/clexer.cmo                        \
	src/kernel_internals/parsing/cparser.cmo                       \
	src/kernel_internals/parsing/logic_preprocess.cmo              \
	src/kernel_internals/typing/mergecil.cmo                      \
	src/kernel_internals/typing/rmtmps.cmo                        \
	src/kernel_internals/typing/oneret.cmo                        \
	src/kernel_internals/typing/frontc.cmo                        \
	src/kernel_services/analysis/ordered_stmt.cmo                   \
	src/kernel_services/analysis/wto_statement.cmo                  \
	src/kernel_services/analysis/dataflows.cmo                      \
	src/kernel_services/analysis/dataflow2.cmo                      \
	src/kernel_services/analysis/stmts_graph.cmo                    \
	src/kernel_services/analysis/dominators.cmo                     \
	src/kernel_services/analysis/service_graph.cmo                  \
	src/kernel_services/analysis/undefined_sequence.cmo             \
	src/kernel_services/analysis/interpreted_automata.cmo           \
	src/kernel_services/ast_data/alarms.cmo                         \
	src/kernel_services/ast_printing/description.cmo                \
	src/kernel_services/abstract_interp/lattice_messages.cmo        \
	src/kernel_services/abstract_interp/abstract_interp.cmo         \
	src/kernel_services/abstract_interp/bottom.cmo                  \
	src/kernel_services/abstract_interp/int_Base.cmo                \
	src/kernel_services/analysis/bit_utils.cmo                      \
	src/kernel_services/abstract_interp/fc_float.cmo                \
	src/kernel_services/abstract_interp/float_interval.cmo          \
	src/kernel_services/abstract_interp/fval.cmo                    \
	src/kernel_services/abstract_interp/ival.cmo                    \
	src/kernel_services/abstract_interp/base.cmo                    \
	src/kernel_services/abstract_interp/origin.cmo                  \
	src/kernel_services/abstract_interp/map_lattice.cmo             \
	src/kernel_services/abstract_interp/tr_offset.cmo               \
	src/kernel_services/abstract_interp/offsetmap.cmo               \
	src/kernel_services/abstract_interp/int_Intervals.cmo           \
	src/kernel_services/abstract_interp/locations.cmo               \
	src/kernel_services/abstract_interp/lmap.cmo                    \
	src/kernel_services/abstract_interp/lmap_bitwise.cmo            \
	src/kernel_services/visitors/visitor.cmo                        \
	src/kernel_services/ast_data/statuses_by_call.cmo               \
	src/kernel_services/ast_printing/printer_tag.cmo                \
	$(PLUGIN_TYPES_CMO_LIST)                                        \
	src/kernel_services/plugin_entry_points/db.cmo                  \
	src/libraries/utils/command.cmo                                 \
	src/libraries/utils/task.cmo                                    \
	src/kernel_services/ast_queries/filecheck.cmo                \
	src/kernel_services/ast_queries/json_compilation_database.cmo   \
	src/kernel_services/ast_queries/file.cmo                     \
	src/kernel_internals/typing/translate_lightweight.cmo         \
	src/kernel_internals/typing/allocates.cmo                     \
	src/kernel_internals/typing/unroll_loops.cmo                  \
	src/kernel_internals/typing/asm_contracts.cmo                 \
	src/kernel_services/analysis/loop.cmo                           \
	src/kernel_services/analysis/exn_flow.cmo                       \
        src/kernel_services/analysis/destructors.cmo                    \
	src/kernel_services/analysis/logic_interp.cmo                   \
	src/kernel_internals/typing/infer_annotations.cmo             \
	src/kernel_services/ast_transformations/clone.cmo                           \
	src/kernel_services/ast_transformations/filter.cmo                          \
	src/kernel_services/ast_transformations/inline.cmo              \
	src/kernel_internals/runtime/special_hooks.cmo                  \
	src/kernel_internals/runtime/messages.cmo

CMO	+= $(KERNEL_CMO)

MLI_ONLY+=\
	src/libraries/utils/hptmap_sig.mli                                   \
	src/kernel_services/cmdline_parameters/parameter_sig.mli             \
	src/kernel_services/ast_data/cil_types.mli                           \
	src/kernel_services/parsetree/logic_ptree.mli                             \
	src/kernel_services/ast_printing/printer_api.mli                     \
	src/kernel_services/abstract_interp/float_sig.mli                    \
	src/kernel_services/abstract_interp/float_interval_sig.mli           \
	src/kernel_services/abstract_interp/lattice_type.mli                 \
	src/kernel_services/abstract_interp/int_Intervals_sig.mli            \
	src/kernel_services/abstract_interp/offsetmap_lattice_with_isotropy.mli \
	src/kernel_services/abstract_interp/offsetmap_sig.mli                \
	src/kernel_services/abstract_interp/lmap_sig.mli                     \
	src/kernel_services/abstract_interp/offsetmap_bitwise_sig.mli

NO_MLI+= src/kernel_services/parsetree/cabs.mli                \
	src/kernel_internals/runtime/machdep_ppc_32.mli         \
	src/kernel_internals/runtime/machdep_x86_16.mli         \
	src/kernel_internals/runtime/machdep_x86_32.mli         \
	src/kernel_internals/runtime/machdep_x86_64.mli         \
	src/kernel_services/ast_printing/cabs_debug.mli        \
	src/kernel_internals/parsing/logic_lexer.mli           \
	src/kernel_internals/parsing/lexerhack.mli             \

MODULES_NODOC+= src/kernel_internals/runtime/machdep_ppc_32.ml \
	src/kernel_internals/runtime/machdep_x86_16.ml         \
	src/kernel_internals/runtime/machdep_x86_32.ml         \
	src/kernel_internals/runtime/machdep_x86_64.ml         \
	external/unmarshal_z.mli


GENERATED += $(addprefix src/kernel_internals/parsing/,\
		clexer.ml cparser.ml cparser.mli \
		logic_lexer.ml logic_parser.ml \
		logic_parser.mli logic_preprocess.ml)


.PHONY: check-logic-parser-wildcard
check-logic-parser-wildcard:
	cd src/kernel_internals/parsing && ocaml check_logic_parser.ml

NON_OPAQUE_DEPS+= src/kernel_services/plugin_entry_points/dynamic

# C Bindings
############

GEN_C_BINDINGS=src/libraries/utils/c_bindings.o
GEN_C_BINDINGS_FLAGS= -fPIC
GEN_BYTE_LIBS+= $(GEN_C_BINDINGS)
GEN_OPT_LIBS+= $(GEN_C_BINDINGS)

src/libraries/utils/c_bindings.o: src/libraries/utils/c_bindings.c
	$(PRINT_CC) $@
	$(CC) $(GEN_C_BINDINGS_FLAGS) -c -I$(call winpath,$(OCAMLLIB)) -O3 -Wall -o $@ $<

# Common startup module
# All link command should add it as last linked module and depend on it.
########################################################################

STARTUP_CMO=src/kernel_internals/runtime/boot.cmo
STARTUP_CMX=$(STARTUP_CMO:.cmo=.cmx)

# GUI modules
# See below for GUI compilation
##############################################################################

WTOOLKIT= \
	wutil widget wbox wfile wpane wpalette wtext wtable

ifeq ("$(LABLGTK_VERSION)","3")

src/plugins/gui/GSourceView.ml: src/plugins/gui/GSourceView3.ml.in
	$(CP) $< $@
	$(CHMOD_RO) $@

src/plugins/gui/GSourceView.mli: src/plugins/gui/GSourceView3.mli.in
	$(CP) $< $@
	$(CHMOD_RO) $@

else
src/plugins/gui/GSourceView.ml: src/plugins/gui/GSourceView2.ml.in
	$(CP) $< $@
	$(CHMOD_RO) $@

src/plugins/gui/GSourceView.mli: src/plugins/gui/GSourceView2.mli.in
	$(CP) $< $@
	$(CHMOD_RO) $@

endif

SOURCEVIEWCOMPAT:=GSourceView
GENERATED+=src/plugins/gui/GSourceView.ml src/plugins/gui/GSourceView.mli \
           src/plugins/gui/dgraph_helper.ml src/plugins/gui/gtk_compat.ml

ifeq ($(LABLGTK),lablgtk3)
src/plugins/gui/gtk_compat.ml: src/plugins/gui/gtk_compat.3.ml
	$(CP) $< $@
	$(CHMOD_RO) $@
else
src/plugins/gui/gtk_compat.ml: src/plugins/gui/gtk_compat.2.ml
	$(CP) $< $@
	$(CHMOD_RO) $@
endif
GENERATED+=src/plugins/gui/gtk_compat.ml

ifeq ($(HAS_DGRAPH),yes)
  DGRAPHFILES:=debug_manager
  src/plugins/gui/dgraph_helper.ml: src/plugins/gui/dgraph_helper.yes.ml
	$(CP) $< $@
	$(CHMOD_RO) $@
else
  DGRAPHFILES:=
  src/plugins/gui/dgraph_helper.ml: src/plugins/gui/dgraph_helper.no.ml
	$(CP) $< $@
	$(CHMOD_RO) $@
endif

SINGLE_GUI_CMO:= \
	wutil_once \
	gtk_compat \
	$(WTOOLKIT) \
	$(SOURCEVIEWCOMPAT) \
	gui_parameters \
	gtk_helper \
        dgraph_helper \
        gtk_form \
	source_viewer pretty_source source_manager book_manager \
	warning_manager \
	filetree \
	launcher \
	menu_manager \
	history \
	gui_printers \
	design \
	analyses_manager file_manager project_manager \
	help_manager \
        $(DGRAPHFILES) \
        property_navigator \

SINGLE_GUI_CMO:= $(patsubst %,src/plugins/gui/%.cmo,$(SINGLE_GUI_CMO))

###############################################################################
#                                                                             #
####################                                                          #
# Plug-in sections #                                                          #
####################                                                          #
#                                                                             #
# For 'internal' developers:                                                  #
# you can add your own plug-in here,                                          #
# but it is better to have your own separated Makefile                        #
###############################################################################

###########
# Metrics #
###########

PLUGIN_ENABLE:=$(ENABLE_METRICS)
PLUGIN_NAME:=Metrics
PLUGIN_DISTRIBUTED:=yes
PLUGIN_DIR:=src/plugins/metrics
PLUGIN_CMO:= metrics_parameters css_html metrics_base metrics_acsl \
	     metrics_cabs metrics_cilast metrics_coverage \
	     register
PLUGIN_GUI_CMO:= metrics_gui register_gui
PLUGIN_DEPENDENCIES:=Eva
PLUGIN_INTERNAL_TEST:=yes
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

#############
# Callgraph #
#############

PLUGIN_ENABLE:=$(ENABLE_CALLGRAPH)
PLUGIN_NAME:=Callgraph
PLUGIN_DISTRIBUTED:=yes
PLUGIN_DIR:=src/plugins/callgraph
PLUGIN_CMO:= options journalize subgraph cg services uses register
ifeq ($(HAS_DGRAPH),yes)
PLUGIN_GUI_CMO:=cg_viewer
else
PLUGIN_GUI_CMO:=
PLUGIN_DISTRIB_EXTERNAL:=cg_viewer.ml
endif
PLUGIN_CMI:= callgraph_api
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_TESTS_DIRS:=callgraph
PLUGIN_TESTS_LIB:=tests/callgraph/function_pointer.ml
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))


##################
# Evolved Value Analysis #
##################

PLUGIN_ENABLE:=$(ENABLE_EVA)
PLUGIN_NAME:=Eva
PLUGIN_DIR:=src/plugins/value
PLUGIN_EXTRA_DIRS:=engine values domains domains/cvalue domains/apron \
	domains/gauges domains/equality legacy partitioning utils gui_files \
	values/numerors domains/numerors
PLUGIN_TESTS_DIRS+=value/traces

# Files for the binding to Apron domains. Only available if Apron is available.
ifeq ($(HAS_APRON),yes)
PLUGIN_REQUIRES+= apron.octMPQ apron.boxMPQ apron.polkaMPQ apron.apron gmp
APRON_CMO:= domains/apron/apron_domain
else
APRON_CMO:=
PLUGIN_DISTRIB_EXTERNAL+= \
	domains/apron/apron_domain.ml domains/apron/apron_domain.mli
endif

# Files for the numerors domain. Only available is MPFR is available.
NUMERORS_FILES:= \
	values/numerors/numerors_utils values/numerors/numerors_float \
	values/numerors/numerors_interval values/numerors/numerors_arithmetics \
	values/numerors/numerors_value domains/numerors/numerors_domain

ifeq ($(HAS_MPFR),yes)
PLUGIN_REQUIRES+= gmp
PLUGIN_TESTS_DIRS+=value/numerors
NUMERORS_CMO:= $(NUMERORS_FILES)
else
# Do not compile numerors files, but include them in the distributed files.
NUMERORS_CMO:=
PLUGIN_DISTRIB_EXTERNAL+= $(addsuffix .ml,$(NUMERORS_FILES))
PLUGIN_DISTRIB_EXTERNAL+= $(addsuffix .mli,$(NUMERORS_FILES))
endif

# General rules for ordering files within PLUGIN_CMO:
# - try to keep the legacy Value before Eva
PLUGIN_CMO:= partitioning/split_strategy value_parameters \
	utils/value_perf utils/value_util utils/red_statuses \
	utils/mark_noresults \
	utils/widen_hints_ext utils/widen utils/partitioning_annots \
	partitioning/split_return \
	partitioning/per_stmt_slevel \
	utils/library_functions \
	utils/eval_typ utils/backward_formals \
	alarmset eval utils/structure utils/abstract \
	values/value_product values/location_lift \
	values/cvalue_forward values/cvalue_backward \
	values/main_values values/main_locations \
	values/offsm_value values/sign_value \
	legacy/eval_op legacy/function_args \
	domains/domain_store domains/domain_builder \
	domains/domain_product domains/domain_lift domains/unit_domain \
	domains/printer_domain \
	domains/traces_domain \
	domains/simple_memory \
	domains/octagons \
	domains/gauges/gauges_domain \
	domains/hcexprs \
	domains/equality/equality domains/equality/equality_domain \
	domains/offsm_domain \
	domains/symbolic_locs \
	domains/sign_domain \
	domains/cvalue/warn domains/cvalue/locals_scoping \
	domains/cvalue/cvalue_offsetmap \
	utils/value_results \
	domains/cvalue/builtins domains/cvalue/builtins_malloc \
	domains/cvalue/builtins_string domains/cvalue/builtins_misc \
	domains/cvalue/builtins_memory domains/cvalue/builtins_print_c \
	domains/cvalue/builtins_watchpoint \
	domains/cvalue/builtins_float domains/cvalue/builtins_split \
	domains/inout_domain \
	utils/state_import \
	legacy/eval_terms legacy/eval_annots \
	domains/powerset engine/transfer_logic \
	domains/cvalue/cvalue_transfer domains/cvalue/cvalue_init \
	domains/cvalue/cvalue_specification \
	domains/cvalue/cvalue_domain \
	engine/subdivided_evaluation engine/evaluation engine/abstractions \
	engine/recursion engine/transfer_stmt engine/transfer_specification \
	partitioning/auto_loop_unroll \
	partitioning/partition partitioning/partitioning_parameters \
	partitioning/partitioning_index partitioning/trace_partitioning \
	engine/mem_exec engine/iterator engine/initialization \
	engine/compute_functions engine/analysis register \
	$(APRON_CMO) $(NUMERORS_CMO)
PLUGIN_CMI:= values/abstract_value values/abstract_location \
	domains/abstract_domain domains/simpler_domains
PLUGIN_DEPENDENCIES:=Callgraph LoopAnalysis RteGen

# These files are used by the GUI, but do not depend on Lablgtk
VALUE_GUI_AUX:=gui_files/gui_types gui_files/gui_eval \
		gui_files/gui_callstacks_filters
PLUGIN_GUI_CMO:=$(VALUE_GUI_AUX) gui_files/gui_callstacks_manager \
		gui_files/gui_red gui_files/register_gui

PLUGIN_INTERNAL_TEST:= yes
PLUGIN_TESTS_LIB=tests/float/fval_test.ml
PLUGIN_DISTRIBUTED:=yes
VALUE_TYPES:=$(addprefix src/plugins/value_types/,\
		cilE cvalue precise_locs value_types widen_type)
PLUGIN_TYPES_CMO:=$(VALUE_TYPES)
PLUGIN_TYPES_TODOC:=$(addsuffix .mli,$(VALUE_TYPES))

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

##################
# Occurrence     #
##################

PLUGIN_ENABLE:=$(ENABLE_OCCURRENCE)
PLUGIN_NAME:=Occurrence
PLUGIN_DISTRIBUTED:=yes
PLUGIN_DIR:=src/plugins/occurrence
PLUGIN_CMO:= options register
PLUGIN_GUI_CMO:=register_gui
PLUGIN_INTRO:=doc/code/intro_occurrence.txt
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Eva
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

################################################
# Runtime Error Annotation Generation analysis #
################################################

PLUGIN_ENABLE:=$(ENABLE_RTEGEN)
PLUGIN_NAME:=RteGen
PLUGIN_DIR:=src/plugins/rte
PLUGIN_CMO:= options generator rte flags visit register
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_TESTS_DIRS:=rte rte_manual
PLUGIN_TESTS_LIB:=\
  tests/rte/my_annotation/my_annotation.ml \
  tests/rte/rte_api/rte_get_annot.ml \
  tests/rte/compute_annot/compute_annot.ml \
  tests/rte/my_annot_proxy/my_annot_proxy.ml
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

#################
# From analysis #
#################

PLUGIN_ENABLE:=$(ENABLE_FROM_ANALYSIS)
PLUGIN_NAME:=From
PLUGIN_DIR:=src/plugins/from
PLUGIN_CMO:= from_parameters from_compute \
	functionwise callwise from_register
PLUGIN_GUI_CMO:=from_register_gui
PLUGIN_TESTS_DIRS:=idct test float
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
FROM_TYPES:=src/plugins/value_types/function_Froms
PLUGIN_TYPES_CMO:=$(FROM_TYPES)
PLUGIN_TYPES_TODOC:=$(addsuffix .mli,$(FROM_TYPES))
PLUGIN_DEPENDENCIES:=Callgraph Eva Postdominators

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

##################
# Users analysis #
##################

PLUGIN_ENABLE:=$(ENABLE_USERS)
PLUGIN_NAME:=Users
PLUGIN_DIR:=src/plugins/users
PLUGIN_CMO:= users_register
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Callgraph Eva

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

########################
# Constant propagation #
########################

PLUGIN_ENABLE:=$(ENABLE_CONSTANT_PROPAGATION)
PLUGIN_NAME:=Constant_Propagation
PLUGIN_DIR:=src/plugins/constant_propagation
PLUGIN_TESTS_LIB:=tests/constant_propagation/introduction_of_non_explicit_cast.ml
PLUGIN_CMO:= propagationParameters \
	api
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Eva

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

###################
# Post-dominators #
###################

PLUGIN_ENABLE:=$(ENABLE_POSTDOMINATORS)
PLUGIN_NAME:=Postdominators
PLUGIN_DIR:=src/plugins/postdominators
PLUGIN_CMO:= postdominators_parameters print compute
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

#########
# inout #
#########

PLUGIN_ENABLE:=$(ENABLE_INOUT)
PLUGIN_NAME:=Inout
PLUGIN_DIR:=src/plugins/inout
PLUGIN_CMO:= inout_parameters cumulative_analysis \
	     operational_inputs outputs inputs derefs register
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
INOUT_TYPES:=src/plugins/value_types/inout_type
PLUGIN_TYPES_CMO:=$(INOUT_TYPES)
PLUGIN_TYPES_TODOC:=$(addsuffix .mli,$(INOUT_TYPES))
PLUGIN_DEPENDENCIES:=Callgraph Eva From

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

###################
# Impact analysis #
###################

PLUGIN_ENABLE:=$(ENABLE_IMPACT)
PLUGIN_NAME:=Impact
PLUGIN_DIR:=src/plugins/impact
PLUGIN_CMO:= options pdg_aux reason_graph compute_impact register
PLUGIN_GUI_CMO:= register_gui
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Inout Eva Pdg Slicing

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

##################################
# PDG : program dependence graph #
##################################

PLUGIN_ENABLE:=$(ENABLE_PDG)
PLUGIN_NAME:=Pdg
PLUGIN_DIR:=src/plugins/pdg
PLUGIN_TESTS_LIB:=tests/pdg/dyn_dpds.ml \
                  tests/pdg/sets.ml
PLUGIN_TESTS_DIRS:=pdg
PLUGIN_CMO:= pdg_parameters \
	    ctrlDpds \
	    pdg_state \
	    build \
	    sets \
	    annot \
	    marks \
	    register

PDG_TYPES:=pdgIndex pdgTypes pdgMarks
PDG_TYPES:=$(addprefix src/plugins/pdg_types/,$(PDG_TYPES))
PLUGIN_TYPES_CMO:=$(PDG_TYPES)

PLUGIN_INTRO:=doc/code/intro_pdg.txt
PLUGIN_TYPES_TODOC:=$(addsuffix .mli,$(PDG_TYPES))
PLUGIN_DEPENDENCIES:=Callgraph Eva From
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

################################################
# Scope : show different kinds of dependencies #
################################################

PLUGIN_ENABLE:=$(ENABLE_SCOPE)
PLUGIN_NAME:=Scope
PLUGIN_DIR:=src/plugins/scope
PLUGIN_TESTS_LIB:=tests/scope/bts971.ml \
                  tests/scope/zones.ml
PLUGIN_CMO:= datascope zones defs
PLUGIN_GUI_CMO:=dpds_gui
PLUGIN_DEPENDENCIES:=Eva Inout
PLUGIN_INTRO:=doc/code/intro_scope.txt
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

#####################################
# Sparecode : unused code detection #
#####################################

PLUGIN_ENABLE:=$(ENABLE_SPARECODE)
PLUGIN_NAME:=Sparecode
PLUGIN_DIR:=src/plugins/sparecode
PLUGIN_CMO:= sparecode_params globs spare_marks transform register
PLUGIN_INTRO:=doc/code/intro_sparecode.txt
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Pdg Eva Users

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

###########
# Slicing #
###########

PLUGIN_ENABLE:=$(ENABLE_SLICING)
PLUGIN_NAME:=Slicing
PLUGIN_DIR:=src/plugins/slicing
PLUGIN_CMO:= slicingInternals \
	    slicingTypes \
	    slicingParameters \
	    slicingState \
	    slicingMacros \
	    slicingMarks \
	    slicingActions \
	    fct_slice \
	    printSlice \
	    slicingProject \
	    slicingTransform \
	    slicingSelect \
	    slicingCmds \
	    api \
	    register

PLUGIN_GUI_CMO:=register_gui

PLUGIN_INTRO:=doc/code/intro_slicing.txt
PLUGIN_UNDOC:=register.ml # slicing_gui.ml

PLUGIN_TESTS_DIRS:= slicing
PLUGIN_TESTS_LIB:= tests/slicing/libSelect.ml tests/slicing/libAnim.ml \
	tests/slicing/simple_intra_slice.ml tests/slicing/combine.ml \
	tests/slicing/ex_spec_interproc.ml tests/slicing/horwitz.ml \
	tests/slicing/mark_all_slices.ml tests/slicing/merge.ml \
	tests/slicing/min_call.ml tests/slicing/select_by_annot.ml \
	tests/slicing/select_simple.ml tests/slicing/simple_intra_slice.ml \
	tests/slicing/slice_no_body.ml tests/slicing/switch.ml \
	tests/slicing/adpcm.ml
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
PLUGIN_DEPENDENCIES:=Pdg Callgraph Eva Sparecode

$(eval $(call include_generic_plugin_Makefile,$(PLUGIN_NAME)))

#####################
# External plug-ins #
#####################

define INCLUDE_PLUGIN
FRAMAC_SHARE:=$(FRAMAC_ROOT_SRCDIR)/share
FRAMAC_PLUGIN:=$(FRAMAC_ROOT_SRCDIR)/lib/plugins
FRAMAC_PLUGIN_GUI:=$(FRAMAC_ROOT_SRCDIR)/lib/plugins/gui
PLUGIN_DIR:=$(1)
include $(1)/Makefile
endef

$(foreach p,$(EXTERNAL_PLUGINS),$(eval $(call INCLUDE_PLUGIN,$p)))

###############################################################################
#                                                                             #
###########################                                                   #
# End of plug-in sections #                                                   #
###########################                                                   #
#                                                                             #
###############################################################################

#####################
# Generic variables #
#####################

CMX	= $(CMO:.cmo=.cmx)
CMI	= $(CMO:.cmo=.cmi)

ALL_CMO	= $(CMO) $(PLUGIN_CMO_LIST) $(STARTUP_CMO)
ALL_CMX	= $(CMX) $(PLUGIN_CMX_LIST) $(STARTUP_CMX)

FILES_FOR_OCAMLDEP+= $(addsuffix /*.mli,$(FRAMAC_SRC_DIRS)) \
	$(addsuffix /*.ml,$(FRAMAC_SRC_DIRS))

MODULES_TODOC+=$(filter-out $(MODULES_NODOC),\
	$(MLI_ONLY) $(NO_MLI:.mli=.ml) \
	$(filter-out $(NO_MLI),\
	$(filter-out $(PLUGIN_TYPES_CMO_LIST:.cmo=.mli),$(CMO:.cmo=.mli))))

################################
# toplevel.{byte,opt} binaries #
################################

ALL_BATCH_CMO= $(filter-out src/kernel_internals/runtime/gui_init.cmo,\
	$(ALL_CMO))
ALL_BATCH_CMX= $(filter-out src/kernel_internals/runtime/gui_init.cmx,\
	$(ALL_CMX))

bin/toplevel.byte$(EXE): $(ALL_BATCH_CMO) $(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLC) $(BLINKFLAGS) -o $@ $(BYTE_LIBS) $(ALL_BATCH_CMO)

#Profiling version of toplevel.byte using ocamlprof
bin/toplevel.prof$(EXE): $(ALL_BATCH_CMO) $(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST)
	$(PRINT_OCAMLCP) $@
	$(OCAMLCP) $(BLINKFLAGS) -o $@ $(BYTE_LIBS) $(ALL_BATCH_CMO)

bin/toplevel.opt$(EXE): $(ALL_BATCH_CMX) $(GEN_OPT_LIBS) \
			$(PLUGIN_DYN_CMX_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) $(OLINKFLAGS) -o $@ $(OPT_LIBS) $(ALL_BATCH_CMX)

LIB_KERNEL_CMO= $(filter-out src/kernel_internals/runtime/gui_init.cmo, $(CMO))
LIB_KERNEL_CMX= $(filter-out src/kernel_internals/runtime/gui_init.cmx, $(CMX))

lib/fc/frama-c.cma: $(LIB_KERNEL_CMO) $(GEN_BYTE_LIBS) lib/fc/META.frama-c
	$(PRINT_LINKING) $@
	$(MKDIR) $(FRAMAC_LIB)
	$(OCAMLMKLIB) -o lib/fc/frama-c $(BYTE_LIBS) $(LIB_KERNEL_CMO)

lib/fc/frama-c.cmxa: lib/fc/frama-c.cma $(GEN_OPT_LIBS) $(LIB_KERNEL_CMX)
	$(MKDIR) $(FRAMAC_LIB)
	$(PRINT_LINKING) $@
	$(OCAMLMKLIB) -o lib/fc/frama-c $(OPT_LIBS) $(LIB_KERNEL_CMX)

####################
# (Ocaml) Toplevel #
####################

bin/toplevel.top$(EXE): $(filter-out src/kernel_internals/runtime/boot.ml,$(ALL_BATCH_CMO)) \
			src/kernel_internals/runtime/toplevel_config.cmo \
			$(GEN_BYTE_LIBS) $(PLUGIN_DYN_CMO_LIST)
	$(PRINT_OCAMLMKTOP) $@
	$(OCAMLMKTOP) $(BFLAGS) -warn-error -31 -custom -o $@ \
	  -linkpkg $(BYTE_LIBS) $(ALL_BATCH_CMO) \
	   src/kernel_internals/runtime/toplevel_config.cmo

#######
# GUI #
#######

ifneq ($(ENABLE_GUI),no)

SINGLE_GUI_CMI = $(SINGLE_GUI_CMO:.cmo=.cmi)
SINGLE_GUI_CMX = $(SINGLE_GUI_CMO:.cmo=.cmx)

GUICMO += $(SINGLE_GUI_CMO) $(PLUGIN_GUI_CMO_LIST)

MODULES_TODOC+= $(filter-out src/plugins/gui/book_manager.mli,\
	$(SINGLE_GUI_CMO:.cmo=.mli))

GUICMI = $(GUICMO:.cmo=.cmi)
GUICMX = $(SINGLE_GUI_CMX) $(PLUGIN_GUI_CMX_LIST)

$(GUICMI) $(GUICMO) bin/viewer.byte$(EXE): BFLAGS+= $(GUI_INCLUDES)
$(GUICMX) bin/viewer.opt$(EXE): OFLAGS+= $(GUI_INCLUDES)

$(PLUGIN_DYN_DEP_GUI_CMO_LIST): BFLAGS+= $(GUI_INCLUDES)
$(PLUGIN_DYN_DEP_GUI_CMX_LIST): OFLAGS+= $(GUI_INCLUDES)

.PHONY:gui

gui-byte:: bin/viewer.byte$(EXE) share/Makefile.dynamic_config \
           $(PLUGIN_META_LIST)

gui-opt:: gui-byte bin/viewer.opt$(EXE)

gui: gui-$(OCAMLBEST)

ALL_GUI_CMO= $(ALL_CMO) $(GRAPH_GUICMO) $(GUICMO)
ALL_GUI_CMX= $(patsubst %.cma,%.cmxa,$(ALL_GUI_CMO:.cmo=.cmx))

bin/viewer.byte$(EXE): BYTE_LIBS+= $(GRAPH_GUICMO)
bin/viewer.byte$(EXE): $(filter-out $(GRAPH_GUICMO),$(ALL_GUI_CMO)) \
			$(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST) $(PLUGIN_DYN_GUI_CMO_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLC) $(BLINKFLAGS) $(THREAD) -o $@ $(BYTE_LIBS) \
	  $(CMO) \
	  $(filter-out \
	    $(patsubst $(PLUGIN_GUI_LIB_DIR)/%,$(PLUGIN_LIB_DIR)/%,\
		$(PLUGIN_GUI_CMO_LIST)),\
	    $(PLUGIN_CMO_LIST)) \
	  $(GUICMO) $(STARTUP_CMO)

bin/viewer.opt$(EXE): OPT_LIBS+= $(GRAPH_GUICMX)
bin/viewer.opt$(EXE): $(filter-out $(GRAPH_GUICMX),$(ALL_GUI_CMX)) \
			$(GEN_OPT_LIBS) \
			$(PLUGIN_DYN_CMX_LIST) $(PLUGIN_DYN_GUI_CMX_LIST) \
			$(PLUGIN_CMX_LIST) $(PLUGIN_GUI_CMX_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) $(OLINKFLAGS) $(THREAD) -o $@ $(OPT_LIBS) \
	  $(CMX) \
	  $(filter-out \
	    $(patsubst $(PLUGIN_GUI_LIB_DIR)/%,$(PLUGIN_LIB_DIR)/%,\
	      $(PLUGIN_GUI_CMX_LIST)),\
	    $(PLUGIN_CMX_LIST)) \
	  $(GUICMX) $(STARTUP_CMX)
endif

#####################
# Config Ocaml File #
#####################

CONFIG_DIR=src/kernel_internals/runtime
CONFIG_FILE=$(CONFIG_DIR)/config.ml
CONFIG_CMO=$(CONFIG_DIR)/config.cmo
GENERATED +=$(CONFIG_FILE)
#Generated in Makefile.generating

empty:=
space:=$(empty) $(empty)

ifeq ($(ENABLE_GUI),no)
CONFIG_CMO=$(ALL_CMO)
CONFIG_PLUGIN_CMO=$(PLUGIN_CMO_LIST)
else
CONFIG_CMO=$(ALL_GUI_CMO)
CONFIG_PLUGIN_CMO=$(PLUGIN_GUI_CMO_LIST)
endif

ifeq ($(HAS_DOT),yes)
OPTDOT=Some \"$(DOT)\"
else
OPTDOT=None
endif

COMPILATION_UNITS=\
  $(foreach p,$(CONFIG_CMO),\"$(notdir $(patsubst %.cmo,%,$p))\"; )

###################
# Generating part #
###################
# It is in another file in order to have a dependency only on Makefile.generating.
# It must be before `.depend` definition because it modifies $GENERATED.

include Makefile.generating

#########
# Tests #
#########

ifeq ($(OCAMLBEST),opt)
PTESTS_FILES=ptests_config.cmi ptests_config.cmx ptests_config.o
else
PTESTS_FILES=ptests_config.cmi ptests_config.cmo
endif

.PHONY: tests oracles btests tests_dist libc_tests plugins_ptests_config external_tests \
	update_external_tests

tests:: byte opt ptests
	$(PRINT_EXEC) ptests
	time -p $(PTESTS) $(PTESTS_OPTS) $(FRAMAC_PARALLEL) \
		-make "$(MAKE)" $(PLUGIN_TESTS_LIST)

external_tests: byte opt ptests
tests:: external_tests

update_external_tests: PTESTS_OPTS="-update"
update_external_tests: external_tests

oracles: byte opt ptests
	$(PRINT_MAKING) oracles
	./bin/ptests.$(OCAMLBEST)$(EXE) -make "$(MAKE)" $(PLUGIN_TESTS_LIST) \
		> /dev/null 2>&1
	./bin/ptests.$(OCAMLBEST)$(EXE) -make "$(MAKE)" -update \
		$(PLUGIN_TESTS_LIST)

btests: byte ./bin/ptests.byte$(EXE)
	$(PRINT_EXEC) ptests -byte
	time -p ./bin/ptests.byte$(EXE) -make "$(MAKE)" -byte \
		$(PLUGIN_TESTS_LIST)

tests_dist: dist ptests
	$(PRINT_EXEC) ptests
	time -p ./bin/ptests.$(OCAMLBEST)$(EXE) -make "$(MAKE)" \
		$(PLUGIN_TESTS_LIST)

# test only one test suite : make suite_tests
%_tests: opt ptests
	$(PRINT_EXEC) ptests
	./bin/ptests.$(OCAMLBEST)$(EXE) -make "$(MAKE)" $($*_TESTS_OPTS) $*

# full test suite
wp_TESTS_OPTS=-j 1
fulltests: tests wp_tests

acsl_tests: byte
	$(PRINT_EXEC) acsl_tests
	find doc/speclang -name \*.c -exec ./bin/toplevel.byte$(EXE) {} \; > /dev/null

LONELY_TESTS_ML_FILES:=\
  $(sort $(shell find $(TEST_DIRS_AS_PLUGIN:%=tests/%) -not -path '*/\.*' -name '*.ml'))
$(foreach file,$(LONELY_TESTS_ML_FILES),\
  $(eval $(file:%.ml=%.cmo): BFLAGS+=-I $(dir $(file))))
$(foreach file,$(LONELY_TESTS_ML_FILES),\
  $(eval $(file:%.ml=%.cmx): OFLAGS+=-I $(dir $(file))))
$(foreach file,$(LONELY_TESTS_ML_FILES),\
  $(eval $(file:%.ml=%.cmxs): OFLAGS+=-I $(dir $(file))))
.PRECIOUS: $(LONELY_TESTS_ML_FILES:%.ml=%.cmx) \
           $(LONELY_TESTS_ML_FILES:%.ml=%.cmxs) \
           $(LONELY_TESTS_ML_FILES:%.ml=%.cmo) \
           $(LONELY_TESTS_ML_FILES:%.ml=%.cmi)

bin/ocamldep_transitive_closure: devel_tools/ocamldep_transitive_closure.ml
	$(OCAMLOPT) -package ocamlgraph -package str -linkpkg -o $@ $<

tests/crowbar/.%.depend: tests/crowbar/%.ml
	$(OCAMLDEP) $(INCLUDES) $< > $@

tests/crowbar/%: tests/crowbar/%.ml tests/crowbar/.%.depend .depend \
                 bin/ocamldep_transitive_closure bin/toplevel.opt
	$(OCAMLOPT) $(OLINKFLAGS) -w -42 -package crowbar -o $@ \
        $(GEN_C_BINDINGS) \
        $$(bin/ocamldep_transitive_closure -root tests/crowbar/$*.cmx \
             -deps tests/crowbar/.$*.depend -deps .depend) \
        $<

crowbar-%: tests/crowbar/%
	$<

crowbar-afl-%: tests/crowbar/%
	$(MKDIR) tests/crowbar/output-$*
	afl-fuzz -i tests/crowbar/input -o tests/crowbar/output-$* $< @@

##############
# Emacs tags #
##############

.PHONY: tags
# otags gives a better tagging of ocaml files than etags
ifdef OTAGS
tags:
	$(OTAGS) -r src lib
vtags:
	$(OTAGS) -vi -r src lib
else
tags:
	find . -name "*.ml[ily]" -o -name "*.ml" | sort -r | xargs \
	etags "--regex=/[ \t]*let[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*and[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*type[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*exception[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*val[ \t]+\([^ \t]+\)/\1/" \
	      "--regex=/[ \t]*module[ \t]+\([^ \t]+\)/\1/"
endif

#################
# Documentation #
#################

.PHONY: doc doc-distrib

# private targets, useful for recompiling the doc without dependencies
# (too long!)
.PHONY: doc-kernel doc-index plugins-doc doc-update doc-tgz

DOC_DEPEND=$(MODULES_TODOC) byte $(DOC_PLUGIN)
ifneq ($(ENABLE_GUI),no)
DOC_DEPEND+=bin/viewer.byte$(EXE)
endif

$(DOC_DIR)/docgen.cmo: $(DOC_DIR)/docgen.ml
	$(PRINT_OCAMLC) $@
	$(OCAMLC) -c -I +ocamldoc -I $(CONFIG_DIR) $(DOC_DIR)/docgen.ml

$(DOC_DIR)/docgen.cmxs: $(DOC_DIR)/docgen.ml
	$(PRINT_PACKING) $@
	$(OCAMLOPT) -o $@ -shared -I +ocamldoc -I $(CONFIG_DIR) \
	  $(DOC_DIR)/docgen.ml

clean-doc::
	$(PRINT_RM) "documentation generator"
	$(RM) $(DOC_DIR)/docgen.cm*

DOC_NOT_FOR_DISTRIB=yes
plugins-doc:
	$(QUIET_MAKE) \
	 $(if $(DOC_NOT_FOR_DISTRIB),$(PLUGIN_DOC_LIST),\
	   $(filter \
	     $(addsuffix _DOC,$(PLUGIN_DISTRIBUTED_NAME_LIST)),\
	     $(PLUGIN_DOC_LIST)))

.PHONY: server-doc-md server-doc-html server-doc

server-doc-md: byte
	$(PRINT) 'Generating Markdown server documentation'
	@rm -fr doc/server
	@mkdir -p doc/server
	./bin/frama-c.byte -server-doc doc/server

server-doc-html: server-doc-md
	$(PRINT) 'Generating HTML server documentation'
	@find doc/server -name "*.md" -print -exec pandoc \
			--standalone --toc --toc-depth=2 --to html \
			--template doc/pandoc/template.html \
			--metadata-file {}.json \
			--lua-filter doc/pandoc/href.lua \
			{} -o {}.html \;
	@cp -f doc/pandoc/style.css doc/server/
	$(PRINT) 'HTML server documentation ready:'
	$(PRINT) '  open doc/server/readme.md.html'

server-doc: server-doc-html

# to make the documentation for one plugin only,
# the name of the plugin should begin with a capital letter :
# Example for the pdg doc : make Pdg_DOC
# While working on the documentation of a plugin, it can also be useful
# to use : make -o doc/code/kernel-doc.ocamldoc Plugin_DOC
# to avoid redoing the global documentation each time.

STDLIB_FILES:=\
	array \
	buffer \
        bytes \
	char \
	format \
	hashtbl \
	int64 \
	list \
	map \
	marshal \
	obj \
        parsing \
	printf \
	queue \
	scanf \
	set \
	stack \
	string \
	sys \
	weak \
	ephemeron

ifeq ($(HAS_OCAML407),no)
  STDLIB_FILES+=pervasives
endif

STDLIB_FILES:=$(patsubst %,$(OCAMLLIB)/%.mli,$(STDLIB_FILES))

.PHONY: doc-kernel
doc-kernel: $(DOC_DIR)/kernel-doc.ocamldoc

$(DOC_DIR)/kernel-doc.ocamldoc: $(DOC_DEPEND)
	$(PRINT_DOC) Kernel Documentation
	$(MKDIR) $(DOC_DIR)/html
	$(RM) $(DOC_DIR)/html/*.html
	$(OCAMLDOC) $(DOC_FLAGS) \
	  $(addprefix -passopt -stdlib ,$(STDLIB_FILES)) \
	  -t "Frama-C Kernel" \
	  -sort -css-style ../style.css \
	  -g $(DOC_PLUGIN) \
	  -d $(DOC_DIR)/html -dump $@ \
	  $(MODULES_TODOC); \
	  RES=$$?; \
	  if test $$RES -ne 0; then \
	    $(RM) $@; \
	    exit $$RES; \
	  fi

DYN_MLI_DIR := src/plugins/print_api

.PHONY: doc-dynamic
doc-dynamic: doc-kernel
	$(RM) $(DYN_MLI_DIR)/dynamic_plugins.mli
	./bin/frama-c.byte \
		-print_api $(call winpath,$(FRAMAC_ROOT_SRCDIR)/$(DYN_MLI_DIR))
	$(PRINT_DOC) Dynamically registered plugins Documentation
	$(MKDIR) $(DOC_DIR)/dynamic_plugins
	$(RM) $(DOC_DIR)/dynamic_plugins/*.html
	$(OCAMLDOC) $(DOC_FLAGS) -I $(FRAMAC_LIB) -I $(OCAMLLIB) \
	  -passopt -docpath $(DOC_DIR)/html \
	  -sort -css-style ../style.css \
	  -load $(DOC_DIR)/kernel-doc.ocamldoc \
	  -t " Dynamically registered plugins" \
	  -g $(DOC_PLUGIN) \
	  -d $(DOC_DIR)/dynamic_plugins \
	  $(DYN_MLI_DIR)/dynamic_plugins.mli
	$(ECHO) '<li><a href="dynamic_plugins/Dynamic_plugins.html">Dynamically registered plugins</a </li>' > $(DOC_DIR)/dynamic_plugins.toc

doc-index: doc-kernel doc-dynamic plugins-doc
	$(PRINT_MAKING) doc/code/index.html
	$(CAT)  $(DOC_DIR)/toc_head.htm $(DOC_DIR)/*.toc \
		$(DOC_DIR)/toc_tail.htm > $(DOC_DIR)/index.html

doc-update: doc-kernel doc-dynamic plugins-doc doc-index

doc:: doc-kernel doc-dynamic plugins-doc doc-index

doc-kernel doc-dynamic plugins-doc doc-index: $(DOC_DEPEND)

doc-tgz:
	$(PRINT_MAKING) frama-c-api.tar.gz
	cd $(DOC_DIR); \
	  $(TAR) zcf tmp.tgz index.html *.txt \
	   $(notdir $(wildcard $(DOC_DIR)/*.css $(DOC_DIR)/*.png \
			       $(DOC_DIR)/dynamic_plugins*)) \
	   html \
	   $(foreach p,$(PLUGIN_DISTRIBUTED_NAME_LIST),\
	     $(notdir $($(p)_DOC_DIR)))
	$(MKDIR) frama-c-api
	$(RM) -r frama-c-api/*
	cd frama-c-api; $(TAR) zxf ../$(DOC_DIR)/tmp.tgz
	$(TAR) zcf frama-c-api.tar.gz frama-c-api
	$(RM) -r frama-c-api $(DOC_DIR)/tmp.tgz

doc-distrib:
	$(QUIET_MAKE) clean-doc
	$(QUIET_MAKE) doc DOC_NOT_FOR_DISTRIB=
	$(QUIET_MAKE) doc-tgz

#find src -name "*.ml[i]" -o -name "*.ml" -maxdepth 3 | sort -r | xargs
dots: $(ALL_CMO)
	$(PRINT_DOC) callgraph
	$(OCAMLDOC) $(DOC_FLAGS) $(INCLUDES) -o doc/call_graph.dot \
	  -dot -dot-include-all -dot-reduce $(MODULES_TODOC)
	$(QUIET_MAKE) doc/call_graph.svg
	$(QUIET_MAKE) doc/call_graph.ps

# pandoc is required to regenerate the manpage
man/frama-c.1: man/frama-c.1.header man/frama-c.1.md
	$(PRINT) 'generating $@'
	$(RM) $@
	pandoc -s -t man -H $^ | tail -n +5 > man/frama-c.1
	$(CHMOD_RO) $@

# Checking consistency with the current implementation
######################################################

DOC_DEV_DIR = doc/developer
CHECK_API_DIR=$(DOC_DEV_DIR)/check_api

$(CHECK_API_DIR)/check_code.cmo: $(CHECK_API_DIR)/check_code.ml
	$(PRINT_OCAMLC) $@
	$(OCAMLC) -c -I +ocamldoc str.cma $(CHECK_API_DIR)/check_code.ml

$(CHECK_API_DIR)/check_code.cmxs: $(CHECK_API_DIR)/check_code.ml
	$(PRINT_PACKING) $@
	$(OCAMLOPT) -o $@ -shared -I +ocamldoc \
		str.cmxa $(CHECK_API_DIR)/check_code.ml

CHECK_CODE=$(CHECK_API_DIR)/check_code.cmxs

.PHONY: check-devguide
check-devguide: $(CHECK_CODE) $(DOC_DEPEND) $(DOC_DIR)/kernel-doc.ocamldoc
	$(PRINT) 'Checking     developer guide consistency'
	$(MKDIR) $(CHECK_API_DIR)/html
	$(OCAMLDOC) $(DOC_FLAGS) -I $(OCAMLLIB) \
	  -g $(CHECK_CODE) \
	  -passopt -docdevpath -passopt "`pwd`/$(CHECK_API_DIR)" \
	  -load $(DOC_DIR)/kernel-doc.ocamldoc \
	  -d $(CHECK_API_DIR)/html
	$(RM) -r  $(CHECK_API_DIR)/html
	$(MAKE) --silent -C $(CHECK_API_DIR) main.idx
	$(MAKE) --silent -C $(CHECK_API_DIR) >$(CHECK_API_DIR)/summary.txt
	$(ECHO) see all the information displayed here \
		in $(CHECK_API_DIR)/summary.txt
	$(RM) code_file
################################
# Code prettyfication and lint #
################################

# We're interested by any .ml[i]? file in src, except for scripts in test
# directories, and generated files (in particular lexers and parsers)
# Note: the find command below is *very* ugly, but it should be POSIX-compliant.

ALL_ML_FILES:=$(shell find src -name '*.ml' -print -o -name '*.mli' -print -o -path '*/tests' -prune '!' -name '*')
MANUAL_ML_FILES:=$(filter-out $(GENERATED) $(PLUGIN_GENERATED_LIST), $(ALL_ML_FILES))

# Allow control of files to be linted/fixed by external sources
# (e.g. pre-commit hook that will concentrate on files which have changed)

sinclude .Makefile.lint

HAS_GIT_FILE:=$(wildcard .git/HEAD)

ifeq ("$(HAS_GIT_FILE)","")
LINT_OTHER_SOURCES:=
else
LINT_OTHER_SOURCES:=\
  $(filter-out \
    $(shell git ls-tree --name-only HEAD src/plugins/*), \
    $(wildcard src/plugins/*))
endif

$(foreach dir,$(LINT_OTHER_SOURCES),$(eval sinclude $(dir)/.Makefile.lint))

ML_LINT_MISSING:=$(filter-out $(MANUAL_ML_FILES), $(ML_LINT_KO))

# By default, also checks files with unknown status:
# this requires new files to pass lint checker from the beginning

ML_LINT_CHECK?=$(filter-out $(ML_LINT_KO), $(MANUAL_ML_FILES))

# this NEWLINE variable containing a literal newline character is used to avoid
# the error "argument list too long", in some instances of "foreach".
# For details, see https://stackoverflow.com/questions/7039811
define NEWLINE


endef

# pre-requisite intentionally left blank: this target should only be used
# if the file is not present to generate it once and forall,
# and be edited manually afterwards
# double colon here tells make not to attempt updating the .Makefile.lint
# if it does not exist, but just to ignore it.
.Makefile.lint::
	echo "ML_LINT_KO:=" >> $@
	$(foreach file,$(sort $(MANUAL_ML_FILES)), \
            if ! $(MAKE) ML_LINT_CHECK=$(file) lint; \
            then echo "ML_LINT_KO+=$(file)" >> $@; fi;$(NEWLINE) )

$(foreach dir,$(LINT_OTHER_SOURCES),\
  $(eval $(dir)/.Makefile.lint:: ; \
     $(foreach file, $(sort $(filter $(dir)/%, $(MANUAL_ML_FILES))), \
            if ! $$(MAKE) ML_LINT_CHECK=$(file) lint; \
            then echo "ML_LINT_KO+=$(file)" >> $$@; fi; )))

.PHONY: stats-lint
stats-lint:
	echo \
         "scale = 2; bad = $(words $(ML_LINT_MISSING)); \
          all = $(words $(sort $(MANUAL_ML_FILES))); \
	  fail = $(words $(ML_LINT_KO)); \
          \"lint coverage: \"; \
          ((all - fail) * 100) / all; " | bc
	echo "number of files supposed to pass lint: $(words $(ML_LINT_CHECK))"
	echo "number of files supposed to fail lint: $(words $(ML_LINT_KO))"
ifneq ($(strip $(ML_LINT_MISSING)),)
	echo "number of files missing from src/ : $(words $(ML_LINT_MISSING))"
	$(foreach file, $(ML_LINT_MISSING), echo $(file);)
	exit 1
endif

INDENT_TARGET= $(patsubst %,%.indent,$(ML_LINT_CHECK))

LINT_TARGET= $(patsubst %,%.lint,$(ML_LINT_CHECK))

FIX_SYNTAX_TARGET=$(patsubst %,%.fix-syntax,$(ML_LINT_CHECK))

.PHONY: $(INDENT_TARGET) $(LINT_TARGET) $(FIX_SYNTAX_TARGET) \
        indent lint fix-syntax

indent: $(INDENT_TARGET)

lint: $(LINT_TARGET)

check-ocp-indent-version:
	if command -v ocp-indent >/dev/null; then \
		$(eval ocp_version_major := $(shell ocp-indent --version | $(SED) -E "s/^([0-9]+)\.[0-9]+\..*/\1/")) \
		$(eval ocp_version_minor := $(shell ocp-indent --version | $(SED) -E "s/^[0-9]+\.([0-9]+)\..*/\1/")) \
		if [ "$(ocp_version_major)" -lt 1 -o "$(ocp_version_minor)" -lt 7 ]; then \
			echo "error: ocp-indent >=1.7.0 required for linting (got $(ocp_version_major).$(ocp_version_minor))"; \
			exit 1; \
		fi; \
	else \
		exit 1; \
	fi;

fix-syntax: $(FIX_SYNTAX_TARGET)

$(INDENT_TARGET): %.indent: % check-ocp-indent-version
	ocp-indent -i $<

$(LINT_TARGET): %.lint: % check-ocp-indent-version
	# See SO 1825552 on mixing grep and \t (and cry)
	# For OK_NL, we have three cases:
	# - for empty files, the computation boils down to 0 - 0 == 0
	# - for non-empty files with a proper \n at the end, to 1 - 1 == 0
	# - for empty files without \n, to 1 - 0 == 1 that will be catched
	OK_TAB=$$(grep -c -e "$$(printf '^ *\t')" $<) ; \
        OK_SPACE=$$(grep -c -e '[[:blank:]]$$' $<) ; \
        OK_NL=$$(($$(tail -c -1 $< | wc -c) - $$(tail -n -1 $< | wc -l))) ; \
        OK_EMPTY=$$(tail -n -1 $< | grep -c -e '^[[:blank:]]*$$') ; \
        ERROR=$$(($$OK_TAB + $$OK_SPACE + $$OK_NL + $$OK_EMPTY)) ; \
        if test $$ERROR -gt 0; then \
          echo "File $< does not pass syntactic checks:"; \
          echo "$$OK_TAB lines indented with tabulation instead of spaces"; \
          echo "$$OK_SPACE lines with spaces at end of line"; \
          test $$OK_NL -eq 0 || echo "No newline at end of file"; \
          test $$OK_EMPTY -eq 0 || echo "Empty line(s) at end of file"; \
          echo "Please run make ML_LINT_CHECK=$< fix-syntax"; \
          exit 1 ; \
        fi
	ocp-indent $< > $<.tmp;
	if cmp -s $< $<.tmp; \
        then rm -f $<.tmp; \
        else \
          echo "File $< is not indented correctly."; \
          echo "Please run make ML_LINT_CHECK=$< indent";\
          rm $<.tmp; \
          exit 1; \
        fi

$(FIX_SYNTAX_TARGET): %.fix-syntax: %
	$(ISED) -e 's/^ *\t\+/ /' $<
	$(ISED) -e 's/\(.*[^[:blank:]]\)[[:blank:]]\+$$/\1/' $<
	$(ISED) -e 's/^[ \t]\+$$//' $<
	if test \( $$(tail -n -1 $< | wc -l) -eq 0 \) -a \( $$(wc -c $< | cut -d " " -f 1) -gt 0 \) ; then \
          echo "" >> $<; \
        else \
          while tail -n -1 $< | grep -l -e '^[ \t]*$$'; do \
            head -n -1 $< > $<.tmp; \
            mv $<.tmp $<; \
          done; \
        fi

# Avoid a UTF-8 locale at all cost: in such setting, sed does not work
# reliably if you happen to have latin-1 encoding somewhere,
# which is still unfortunately the case in some dark corners of the platform
%.fix-syntax: LC_ALL = C

################
# Installation #
################

#       line below does not work if INCLUDES contains twice the same directory
#       Do not attempt to copy gui interfaces if gui is disabled
#Byte
ALL_BATCH_CMO_FIXED=$(filter-out src/kernel_internals/runtime/gui_init.cmo,$(CMO) $(STARTUP_CMO))
LIB_BYTE_TO_INSTALL=\
	$(MLI_ONLY:.mli=.cmi) \
	$(ALL_BATCH_CMO_FIXED:.cmo=.cmi) \
	$(ALL_BATCH_CMO_FIXED) \
	$(filter-out %.o,$(GEN_BYTE_LIBS:.cmo=.cmi)) \
	$(GEN_BYTE_LIBS)

#Byte GUI
ifneq ("$(ENABLE_GUI)","no")
LIB_BYTE_TO_INSTALL+=$(SINGLE_GUI_CMI) $(SINGLE_GUI_CMO)
endif

#Opt
ifeq ("$(OCAMLBEST)","opt")
ALL_BATCH_CMX_FIXED= $(filter-out src/kernel_internals/runtime/gui_init.cmx,\
	$(CMX) $(STARTUP_CMX))
LIB_OPT_TO_INSTALL +=\
	$(ALL_BATCH_CMX) \
	$(filter %.a,$(ALL_BATCH_CMX_FIXED:.cmxa=.a)) \
	$(filter %.o,$(ALL_BATCH_CMX_FIXED:.cmx=.o)) \
	$(filter-out %.o,$(GEN_OPT_LIBS)) \
	$(filter-out $(GEN_BYTE_LIBS),$(filter %.o,$(GEN_OPT_LIBS:.cmx=.o)))

#Opt GUI
ifneq ("$(ENABLE_GUI)","no")
LIB_OPT_TO_INSTALL += $(SINGLE_GUI_CMX) $(SINGLE_GUI_CMX:.cmx=.o)
endif

endif

clean-install:
	$(PRINT_RM) "Installation directory"
	$(RM) -r $(FRAMAC_LIBDIR)

install-lib-byte: clean-install
	$(PRINT_INSTALL) kernel API
	$(MKDIR) $(FRAMAC_LIBDIR)
	$(CP) $(LIB_BYTE_TO_INSTALL) $(FRAMAC_LIBDIR)
	$(CP) $(addprefix lib/fc/,dllframa-c.so libframa-c.a frama-c.cma META.frama-c) $(FRAMAC_LIBDIR)

install-lib-opt: install-lib-byte
	$(CP) $(LIB_OPT_TO_INSTALL) $(FRAMAC_LIBDIR)
	$(CP) $(addprefix lib/fc/,frama-c.a frama-c.cmxa) $(FRAMAC_LIBDIR)

install-doc-code:
	$(PRINT_INSTALL) API documentation
	$(MKDIR) $(FRAMAC_DATADIR)/doc/code
	(cd doc ; tar cf - --exclude='.svn' --exclude='*.toc' \
			--exclude='*.htm' --exclude='*.txt' \
			--exclude='*.ml' \
			code \
		| (cd $(FRAMAC_DATADIR)/doc ; tar xf -))

.PHONY: install
install:: install-lib-$(OCAMLBEST)
	$(PRINT_MAKING) destination directories
	$(MKDIR) $(BINDIR)
	$(MKDIR) $(MANDIR)/man1
	$(MKDIR) $(FRAMAC_PLUGINDIR)/top
	$(MKDIR) $(FRAMAC_PLUGINDIR)/gui
	$(MKDIR) $(FRAMAC_DATADIR)/theme/default
	$(MKDIR) $(FRAMAC_DATADIR)/theme/colorblind
	$(MKDIR) $(FRAMAC_DATADIR)/theme/flat
	$(MKDIR) $(FRAMAC_DATADIR)/libc/sys
	$(MKDIR) $(FRAMAC_DATADIR)/libc/netinet
	$(MKDIR) $(FRAMAC_DATADIR)/libc/net
	$(MKDIR) $(FRAMAC_DATADIR)/libc/arpa
	$(PRINT_INSTALL) shared files
	$(CP) \
	  $(wildcard share/*.c share/*.h) \
	  share/Makefile.dynamic share/Makefile.plugin.template \
	  share/Makefile.config share/Makefile.common share/Makefile.generic \
	  share/configure.ac share/autocomplete_frama-c share/_frama-c \
	  $(FRAMAC_DATADIR)
	$(MKDIR) $(FRAMAC_DATADIR)/analysis-scripts
	$(CP) share/analysis-scripts/benchmark_database.py \
	  share/analysis-scripts/cmd-dep.sh \
	  share/analysis-scripts/concat-csv.sh \
	  share/analysis-scripts/clone.sh \
	  share/analysis-scripts/fc_stubs.c \
	  share/analysis-scripts/find_fun.py \
	  share/analysis-scripts/flamegraph.pl \
	  share/analysis-scripts/frama-c.mk \
	  share/analysis-scripts/frama_c_results.py \
	  share/analysis-scripts/git_utils.py \
	  share/analysis-scripts/list_files.py \
	  share/analysis-scripts/make_template.py \
	  share/analysis-scripts/make_wrapper.py \
	  share/analysis-scripts/parse-coverage.sh \
	  share/analysis-scripts/README.md \
	  share/analysis-scripts/results_display.py \
	  share/analysis-scripts/summary.py \
	  share/analysis-scripts/template.mk \
	  $(FRAMAC_DATADIR)/analysis-scripts
	$(MKDIR) $(FRAMAC_DATADIR)/analysis-scripts/examples
	$(CP) share/analysis-scripts/examples/* \
	  $(FRAMAC_DATADIR)/analysis-scripts/examples
	$(MKDIR) $(FRAMAC_DATADIR)/compliance
	$(CP) share/compliance/c11_functions.json \
	  share/compliance/glibc_functions.json \
	  share/compliance/nonstandard_identifiers.json \
	  share/compliance/posix_identifiers.json \
	  $(FRAMAC_DATADIR)/compliance
	$(MKDIR) $(FRAMAC_DATADIR)/emacs
	$(CP) $(wildcard share/emacs/*.el) $(FRAMAC_DATADIR)/emacs
	$(CP) share/frama-c.rc $(ICONS) $(FRAMAC_DATADIR)
	$(CP) $(THEME_ICONS_DEFAULT) $(FRAMAC_DATADIR)/theme/default
	$(CP) $(THEME_ICONS_COLORBLIND) $(FRAMAC_DATADIR)/theme/colorblind
	$(CP) $(THEME_ICONS_FLAT) $(FRAMAC_DATADIR)/theme/flat
	if [ -d $(EMACS_DATADIR) ]; then \
	  $(CP) $(wildcard share/emacs/*.el) $(EMACS_DATADIR); \
	fi
	$(CP) share/Makefile.dynamic_config.external \
	      $(FRAMAC_DATADIR)/Makefile.dynamic_config
	$(PRINT_INSTALL) C standard library
	$(CP) $(wildcard share/libc/*.c share/libc/*.i share/libc/*.h) \
	      $(FRAMAC_DATADIR)/libc
	$(CP) share/libc/sys/*.[ch] $(FRAMAC_DATADIR)/libc/sys
	$(CP) share/libc/arpa/*.[ch] $(FRAMAC_DATADIR)/libc/arpa
	$(CP) share/libc/net/*.[ch] $(FRAMAC_DATADIR)/libc/net
	$(CP) share/libc/netinet/*.[ch] $(FRAMAC_DATADIR)/libc/netinet
	$(PRINT_INSTALL) binaries
	$(CP) bin/toplevel.$(OCAMLBEST) $(BINDIR)/frama-c$(EXE)
	$(CP) bin/toplevel.byte$(EXE) $(BINDIR)/frama-c.byte$(EXE)
	if [ -x bin/toplevel.top ] ; then \
	  $(CP) bin/toplevel.top $(BINDIR)/frama-c.toplevel$(EXE); \
	fi
	if [ -x bin/viewer.$(OCAMLBEST) ] ; then \
	  $(CP) bin/viewer.$(OCAMLBEST) $(BINDIR)/frama-c-gui$(EXE);\
	fi
	if [ -x bin/viewer.byte$(EXE) ] ; then \
	  $(CP) bin/viewer.byte$(EXE) $(BINDIR)/frama-c-gui.byte$(EXE); \
	fi
	$(CP) bin/ptests.$(OCAMLBEST)$(EXE) \
	      $(BINDIR)/ptests.$(OCAMLBEST)$(EXE)
	if [ -x bin/fc-config$(EXE) ] ; then \
		$(CP) bin/fc-config$(EXE) $(BINDIR)/frama-c-config$(EXE); \
	fi
	if [ -x bin/frama-c-script ] ; then \
		$(CP) bin/frama-c-script $(BINDIR)/frama-c-script; \
	fi
	$(PRINT_INSTALL) config files
	$(CP) $(addprefix ptests/,$(PTESTS_FILES)) $(FRAMAC_LIBDIR)
	$(PRINT_INSTALL) API documentation
	$(MKDIR) $(FRAMAC_DATADIR)/doc/code
	$(CP) $(wildcard $(DOC_GEN_FILES)) $(FRAMAC_DATADIR)/doc/code
	$(PRINT_INSTALL) plug-ins
	if [ -d "$(FRAMAC_PLUGIN)" ]; then \
	  $(CP)  $(PLUGIN_DYN_CMI_LIST) $(PLUGIN_META_LIST) \
		 $(FRAMAC_PLUGINDIR); \
	  $(CP)  $(PLUGIN_DYN_CMO_LIST) $(FRAMAC_PLUGINDIR)/top; \
	  if [ "$(OCAMLBEST)" = "opt" ]; then \
	    $(CP) $(PLUGIN_DYN_CMX_LIST) $(FRAMAC_PLUGINDIR)/top; \
	  fi; \
	fi
	$(PRINT_INSTALL) gui plug-ins
	if [ -d "$(FRAMAC_PLUGIN_GUI)" -a "$(PLUGIN_DYN_GUI_EXISTS)" = "yes" ]; \
	then \
	  $(CP) $(patsubst %.cma,%.cmi,$(PLUGIN_DYN_GUI_CMO_LIST:.cmo=.cmi)) \
		$(PLUGIN_DYN_GUI_CMO_LIST) $(FRAMAC_PLUGINDIR)/gui; \
	  if [ "$(OCAMLBEST)" = "opt" ]; then \
	    $(CP) $(PLUGIN_DYN_GUI_CMX_LIST) $(FRAMAC_PLUGINDIR)/gui; \
	  fi; \
	fi
	$(PRINT_INSTALL) man pages
	$(CP) man/frama-c.1 $(MANDIR)/man1/frama-c.1
	$(CP) man/frama-c.1 $(MANDIR)/man1/frama-c-gui.1

.PHONY: uninstall
uninstall::
	$(PRINT_RM) installed binaries
	$(RM) $(BINDIR)/frama-c* $(BINDIR)/ptests.$(OCAMLBEST)$(EXE)
	$(PRINT_RM) installed shared files
	$(RM) -R $(FRAMAC_DATADIR)
	$(PRINT_RM) installed libraries
	$(RM) -R $(FRAMAC_LIBDIR) $(FRAMAC_PLUGINDIR)
	$(PRINT_RM) installed man files
	$(RM) $(MANDIR)/man1/frama-c.1 $(MANDIR)/man1/frama-c-gui.1

################################
# File headers: license policy #
################################


# Generating headers
####################

# Default header specification files
HEADER_SPEC := $(DEFAULT_HEADER_SPEC)
# The list can be extended by external plugins using PLUGIN_HEADER_SPEC variable
HEADER_SPEC += $(PLUGIN_HEADER_SPEC_LIST)
# Default list of header specification files can be overloaded.
HEADER_SPEC_FILE?=$(HEADER_SPEC)

# Default directory (containing subdirectories open-source and close-source)
HEADER_DIRS := $(DEFAULT_HEADER_DIRS)
# The list can be extended by external plugins using PLUGIN_HEADER_DIRS variable
HEADER_DIRS += $(PLUGIN_HEADER_DIRS_LIST)
# Takes into account the kind of distribution (open-souce/close-source)
DISTRIB_HEADER_DIRS?=$(addsuffix /$(DISTRIB_HEADERS),$(HEADER_DIRS))

# List of distributed files allowed to have no entry into the HEADER_SPEC_FILE
HEADER_EXCEPTIONS := $(DEFAULT_HEADER_EXCEPTIONS)
HEADER_EXCEPTIONS += opam/files $(wildcard $(PLUGIN_HEADER_EXCEPTIONS_LIST))

# List of headers that cannot be part of an open-source distribution
CEA_PROPRIETARY_HEADERS := $(DEFAULT_CEA_PROPRIETARY_HEADERS)
CEA_PROPRIETARY_HEADERS += $(PLUGIN_CEA_PROPRIETARY_HEADERS_LIST)

# List of files that cannot be part of an open-source distribution
CEA_PROPRIETARY_FILES := $(DEFAULT_CEA_PROPRIETARY_FILES)
CEA_PROPRIETARY_FILES += $(PLUGIN_CEA_PROPRIETARY_FILES_LIST)

HDRCK=./headers/hdrck$(EXE)

HDRCK_EXTRA?=$(STRICT_HEADERS)
# Can be set to "-exit-on-warning"
ifeq ($(HDRCK_EXTRA),no)
HDRCK_EXTRA:=""
else
ifeq ($(HDRCK_EXTRA),yes)
HDRCK_EXTRA:="-exit-on-warning"
endif
endif

.PHONY: headers

# OPEN_SOURCE: set it to 'no' if you want to apply close source headers.
# STRICT_HEADERS: set it to 'yes' if you want to consider warnings as errors
headers:: $(HDRCK)
	$(PRINT) "|$(OPEN_SOURCE)|$(SPECIFIED_OPEN_SOURCE)|"
	$(PRINT) "Applying $(HDRCK_DISTRIB_HEADERS) headers (OPEN_SOURCE=$(HDRCK_OPEN_SOURCE))..."
	$(PRINT) "- HEADER_SPEC_FILE=$(HEADER_SPEC_FILE)"
	$(PRINT) "- DISTRIB_HEADER_DIRS=$(HDRCK_DISTRIB_HEADER_DIRS)"
	$(HDRCK) \
		$(HDRCK_EXTRA) \
		-update -C . \
		$(addprefix -header-dirs ,$(HDRCK_DISTRIB_HEADER_DIRS)) \
		-headache-config-file ./headers/headache_config.txt \
		$(HEADER_SPEC_FILE)

hdrck: $(HDRCK)

$(HDRCK): headers/hdrck.ml
	$(PRINT_MAKING)	$@
ifeq ($(OCAMLBEST),opt)
	$(OCAMLOPT) str.cmxa unix.cmxa $< -o $@
else
	$(OCAMLC) str.cma unix.cma $< -o $@
endif

hdrck-clean:
	$(RM) headers/hdrck headers/hdrck.o
	$(RM) headers/hdrck.cmx headers/hdrck.cmi headers/hdrck.cmp

clean:: hdrck-clean

CURRENT_HEADERS?=open-source
CURRENT_HEADER_DIRS?=$(addsuffix /$(CURRENT_HEADERS),$(HEADER_DIRS))

# OPEN_SOURCE: set it to 'yes' if you want to check open source headers
# STRICT_HEADERS: set it to 'yes' if you want to consider warnings as errors
# The target check-headers does the following checks:
# 1. Checks entries of HEADER_SPEC_FILE
# 2. Checks that every DISTRIB_FILES (except HEADER_EXCEPTIONS) have an entry
#    inside HEADER_SPEC_FILE
# 3. Checks that all these files are not under DISTRIB_PROPRIETARY_HEADERS
#    licences
# Also check that distributed files are not encoded in ISO-8859. Do this first,
# because identical headers but with different encodings are not exactly
# easy to distinguish
.PHONY: check-headers
check-headers: $(HDRCK)
	$(PRINT) "Checking $(DISTRIB_HEADERS) headers (OPEN_SOURCE=$(OPEN_SOURCE), CURRENT_HEADERS=$(CURRENT_HEADERS))..."
	$(PRINT) "- HEADER_SPEC_FILE=$(HEADER_SPEC_FILE)"
	$(PRINT) "- CURRENT_HEADER_DIRS=$(CURRENT_HEADER_DIRS)"
	$(PRINT) "- FORBIDDEN_HEADERS=$(DISTRIB_PROPRIETARY_HEADERS)"
	 # Workaround to avoid "argument list too long" in make 3.82+ without
	 # using 'file' built-in, only available on make 4.0+
	 # for make 4.0+, using the 'file' function could be a better solution,
	 # although it seems to segfault in 4.0 (but not in 4.1)
	$(RM) file_list_to_check.tmp file_list_exceptions.tmp
	@$(foreach file,$(DISTRIB_FILES),\
			echo $(file) >> file_list_to_check.tmp$(NEWLINE))
	@$(foreach file,$(HEADER_EXCEPTIONS),\
			echo $(file) >> file_list_exceptions.tmp$(NEWLINE))

	@if command -v file >/dev/null 2>/dev/null; then \
		echo "Checking that distributed files do not use iso-8859..."; \
		file --mime-encoding -f file_list_to_check.tmp | \
			grep "iso-8859" \
			| $(SED) "s/^/error: invalid encoding in /" \
			| ( ! grep "error: invalid encoding" ); \
	else echo "command 'file' not found, skipping encoding checks"; \
	fi
	$(HDRCK) \
		$(HDRCK_EXTRA) \
		$(addprefix -header-dirs ,$(CURRENT_HEADER_DIRS)) \
		$(addprefix -forbidden-headers ,$(DISTRIB_PROPRIETARY_HEADERS)) \
		-headache-config-file ./headers/headache_config.txt \
		-distrib-file file_list_to_check.tmp \
		-header-except-file file_list_exceptions.tmp \
		$(HEADER_SPEC_FILE)
	$(RM) file_list_to_check.tmp file_list_exceptions.tmp

########################################################################
# Makefile is rebuilt whenever Makefile.in or configure.in is modified #
########################################################################

share/Makefile.config: share/Makefile.config.in config.status
	$(PRINT_MAKING) $@
	./config.status --file $@

share/Makefile.dynamic_config: share/Makefile.dynamic_config.internal
	$(PRINT_MAKING) $@
	$(RM) $@
	$(CP) $< $@
	$(CHMOD_RO) $@

config.status: configure
	$(PRINT_MAKING) $@
	./config.status --recheck

configure: configure.in .force-reconfigure
	$(PRINT_MAKING) $@
	autoconf -f

# If 'make clean' has to be performed after 'svn update':
# change '.make-clean-stamp' before 'svn commit'
.make-clean: .make-clean-stamp
	$(TOUCH) $@
	$(QUIET_MAKE) clean

include .make-clean

# force "make clean" to be executed for all users of SVN
force-clean:
	expr `$(CAT) .make-clean-stamp` + 1 > .make-clean-stamp

# force a reconfiguration for all svn users
force-reconfigure:
	expr `$(CAT) .force-reconfigure` + 1 > .force-reconfigure

.PHONY: force-clean force-reconfigure

############
# cleaning #
############

clean-journal:
	$(PRINT_RM) journal
	$(RM) frama_c_journal*

clean-tests:
	$(PRINT_RM) tests
	$(RM) tests/*/*.byte$(EXE) tests/*/*.opt$(EXE) tests/*/*.cm* \
		tests/dynamic/.cm* tests/*/*~ tests/*/#*
	$(RM) tests/*/result/*.*

clean-doc:: $(PLUGIN_LIST:=_CLEAN_DOC)
	$(PRINT_RM) documentation
	$(RM) -r $(DOC_DIR)/html
	$(RM) $(DOC_DIR)/docgen.cm* $(DOC_DIR)/*~
	$(RM) doc/db/*~ doc/db/ocamldoc.sty doc/db/db.tex
	$(RM) doc/training/*/*.cm*
	if [ -f doc/developer/Makefile ]; then \
	  $(MAKE) --silent -C doc/developer clean; \
	fi

clean-gui::
	$(PRINT_RM) gui
	$(RM) src/*/*/*_gui.cm* src/*/*/*_gui.o \
	      src/plugins/gui/*.cm* src/plugins/gui/*.o

clean:: $(PLUGIN_LIST:=_CLEAN) \
		clean-tests clean-journal clean-check-libc
	$(PRINT_RM) lib/plugins
	$(RM) $(addprefix $(PLUGIN_LIB_DIR)/,*.mli *.cm* *.o META.*)
	$(RM) $(addprefix $(PLUGIN_TOP_LIB_DIR)/,*.mli *.cm* *.o *.a)
	$(RM) $(addprefix $(PLUGIN_GUI_LIB_DIR)/,*.mli *.cm* *.o *.a)
	$(PRINT_RM) local installation
	$(RM) lib/*.cm* lib/*.o lib/fc/*.cm* lib/fc/*.o lib/gui/*.cm* lib/*.cm*
	$(PRINT_RM) other sources
	for d in . $(SRC_DIRS) src/plugins/gui share; do \
	  $(RM) $$d/*.cm* $$d/*.o $$d/*.a $$d/*.annot $$d/*~ $$d/*.output \
		$$d/*.annot $$d/\#*; \
	done
	$(PRINT_RM) generated files
	$(RM) $(GENERATED)
	$(PRINT_RM) binaries
	$(RM) bin/toplevel.byte$(EXE) bin/viewer.byte$(EXE) \
		bin/ptests.byte$(EXE) bin/*.opt$(EXE) bin/toplevel.top$(EXE)
	$(RM) bin/fc-config$(EXE)

smartclean:
	$(MAKE) -f share/Makefile.clean smartclean

# Do NOT use :: for this rule: it is mandatory to remove share/Makefile.config
# as the very last step performed by make (who'll otherwise try to regenerate
# it in the middle of cleaning)
dist-clean distclean: clean clean-doc \
	              $(PLUGIN_LIST:=_DIST_CLEAN)
	$(PRINT_RM) config
	$(RM) share/Makefile.config
	$(RM) config.cache config.log config.h
	$(RM) -r autom4te.cache
	$(PRINT_RM) documentation
	$(RM) $(DOC_DIR)/kernel-doc.ocamldoc
	$(PRINT_RM) dummy plug-ins
	$(RM) src/dummy/*/*.cm* src/dummy/*/*.o src/dummy/*/*.a \
		src/dummy/*/*.annot src/dummy/*/*~ src/dummy/*/*.output \
		src/dummy/*/*.annot src/dummy/*/\#*


ifeq ($(OCAMLWIN32),yes)
# Use Win32 typical resources
share/frama-c.rc: share/frama-c.WIN32.rc
	$(PRINT_MAKING) $@
	$(CP) $^ $@
else
# Use Unix typical resources
share/frama-c.rc: share/frama-c.Unix.rc
	$(PRINT_MAKING) $@
	$(CP) $^ $@
endif

GENERATED+=share/frama-c.rc

##########
# Depend #
##########

PLUGIN_DEP_LIST:=$(PLUGIN_LIST)

.PHONY: depend

# in case .depend is absent, we will make it. Otherwise, it will be left
# untouched. Only make depend will force a recomputation of dependencies
.depend: $(GENERATED) share/Makefile.dynamic_config
	$(MAKE) depend

depend:: $(GENERATED) share/Makefile.dynamic_config
	$(PRINT_MAKING) .depend
	$(RM) .depend
	$(OCAMLDEP) $(INCLUDES) $(FILES_FOR_OCAMLDEP) > .depend
	$(OCAMLDEP) $(INCLUDES) $(TEST_DIRS_AS_PLUGIN:%=-I tests/%) \
          $(LONELY_TESTS_ML_FILES) >> .depend
	$(CHMOD_RO) .depend

#Used by internal plugins to wait until the *.mli of all the plugins are in
# $(PLUGIN_LIB_DIR) before computing their .depend. Otherwise ocamldep doesn't
# mark inter-plugin dependencies
$(PLUGIN_LIB_DIR)/.placeholders_ready:
	touch $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),smartclean)
ifneq ($(MAKECMDGOALS),depend)
sinclude .depend
endif
endif
endif
endif

#####################
# ptest development #
#####################

.PHONY: ptests

PTESTS_SRC=ptests/ptests_config.ml ptests/ptests.ml

# Do not generate tests/ptests_config if we are compiling a distribution
# that does not contain a 'tests' dir
PTESTS_CONFIG:= $(shell if test -d tests; then echo tests/ptests_config; fi)

ptests: bin/ptests.$(OCAMLBEST)$(EXE) $(PTESTS_CONFIG)

bin/ptests.byte$(EXE): $(PTESTS_SRC)
	$(PRINT_LINKING) $@
	$(OCAMLC) -I ptests -dtypes -thread -g -o $@ \
	    unix.cma threads.cma str.cma dynlink.cma $^

bin/ptests.opt$(EXE): $(PTESTS_SRC)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) -I ptests -dtypes -thread -o $@ \
	    unix.cmxa threads.cmxa str.cmxa dynlink.cmxa $^

GENERATED+=ptests/ptests_config.ml tests/ptests_config

#######################
# Source distribution #
#######################

.PHONY: src-distrib

STANDALONE_PLUGINS_FILES = \
	$(addprefix src/dummy/hello_world/,hello_world.ml Makefile) \
	$(addprefix src/dummy/untyped_metrics/,count_for.ml Makefile)

DISTRIB_FILES += $(wildcard $(PLUGIN_DISTRIBUTED_LIST)                   \
                    $(PLUGIN_DIST_EXTERNAL_LIST)                         \
                    $(PLUGIN_DIST_DOC_LIST) $(STANDALONE_PLUGINS_FILES))
DISTRIB_FILES:=$(filter-out $(GENERATED) $(PLUGIN_GENERATED_LIST),\
                  $(DISTRIB_FILES))

DISTRIB_TESTS += $(wildcard $(PLUGIN_DIST_TESTS_LIST)) 


SPECIFIED_OPEN_SOURCE:=$(OPEN_SOURCE)
OPEN_SOURCE  ?= no

ifneq ($(OPEN_SOURCE),yes)
# close source version
DISTRIB_HEADERS:=close-source
DISTRIB_PROPRIETARY_HEADERS:=
else
# open source version
DISTRIB_HEADERS:=open-source
# for checking that distributed files aren't under proprietary licence.
DISTRIB_PROPRIETARY_HEADERS:=$(CEA_PROPRIETARY_HEADERS)
# DISTRIB_TESTS contents files that can be distributed without header checking
DISTRIB_TESTS:=$(filter-out $(CEA_PROPRIETARY_FILES) ,\
                  $(DISTRIB_TESTS))
# DISTRIB_FILES contents files that can be distributed with header checking
DISTRIB_FILES:=$(filter-out $(CEA_PROPRIETARY_FILES) ,\
                  $(DISTRIB_FILES))
endif

# Set some variables for `headers`target.
ifeq ($(OPEN_SOURCE),$(SPECIFIED_OPEN_SOURCE))
# The OPEN_SOURCE variable is specified. So, use it for `make headers`
HDRCK_OPEN_SOURCE=$(SPECIFIED_OPEN_SOURCE)
HDRCK_DISTRIB_HEADERS=$(DISTRIB_HEADERS)
HDRCK_DISTRIB_HEADER_DIRS=$(DISTRIB_HEADER_DIRS)
else
# The OPEN_SOURCE variable is unspecified. So, use open-source default for `make headers`
HDRCK_OPEN_SOURCE=unspecified
HDRCK_DISTRIB_HEADERS=open-source
HDRCK_DISTRIB_HEADER_DIRS?=$(addsuffix /$(HDRCK_DISTRIB_HEADERS),$(HEADER_DIRS))
endif

# Variables governing the name of the generated .tar.gz.
# Optionally define them as empty to silence warnings about undefined variables
CLIENT ?=

DISTRIB_DIR=tmp
ifeq ("$(CLIENT)","")
VERSION_NAME:=$(VERSION)
else
VERSION_NAME:=$(VERSION)-$(CLIENT)
endif

DISTRIB?=frama-c-$(VERSION_NAME)-$(VERSION_CODENAME)
CLIENT_DIR=$(DISTRIB_DIR)/$(DISTRIB)


# useful parameters:
# CLIENT: name of the client (in the version number, the archive name, etc)
# DISTRIB: name of the generated tarball and of the root tarball directory
# OPEN_SOURCE: set it to 'yes' if you want to exclude close source files
# note: make headers has to be applied...
src-distrib: $(HDRCK) check-headers
ifeq ("$(CLIENT)","")
	$(PRINT_BUILD) "$(DISTRIB_HEADERS) tarball $(DISTRIB) (OPEN_SOURCE=$(OPEN_SOURCE))"
else
	$(PRINT_BUILD) "$(DISTRIB_HEADERS) tarball $(DISTRIB) for $(CLIENT) (OPEN_SOURCE=$(OPEN_SOURCE))"
endif
	$(RM) -r $(CLIENT_DIR)
	$(MKDIR) -p $(CLIENT_DIR)
	@#Workaround to avoid "argument list too long" in make 3.82+ without
	@#using 'file' built-in, only available on make 4.0+
	@#for make 4.0+, using the 'file' function could be a better solution,
	@#although it seems to segfault in 4.0 (but not in 4.1)
	$(RM) file_list_to_archive.tmp
	@$(foreach file,$(DISTRIB_FILES) $(DISTRIB_TESTS),\
			echo $(file) >> file_list_to_archive.tmp$(NEWLINE))
	$(TAR) -cf - --files-from file_list_to_archive.tmp | $(TAR) -C $(CLIENT_DIR) -xf -
	$(RM) file_list_to_archive.tmp
	$(PRINT_MAKING) files
	(cd $(CLIENT_DIR) ; \
	   echo "$(VERSION_NAME)" > VERSION && \
	   DISTRIB_CONF=yes autoconf > ../../.log.autoconf 2>&1)
	$(MKDIR) $(CLIENT_DIR)/bin
	$(MKDIR) $(CLIENT_DIR)/lib/plugins
	$(MKDIR) $(CLIENT_DIR)/lib/gui
	$(RM) ../$(DISTRIB).tar.gz
	$(PRINT) "Updating files to archive with $(DISTRIB_HEADERS) headers"
	$(HDRCK) \
		$(HDRCK_EXTRA) \
		-update -C $(CLIENT_DIR) \
		$(addprefix -header-dirs ,$(DISTRIB_HEADER_DIRS)) \
		-headache-config-file ./headers/headache_config.txt \
		$(HEADER_SPEC_FILE)
	$(PRINT_TAR) $(DISTRIB).tar.gz
	(cd $(DISTRIB_DIR); $(TAR) cf - \
			--numeric-owner --owner=0 --group=0 --sort=name \
			--mtime="$$(date +"%F") Z" --mode='a+rw' \
			--exclude "*autom4te.cache*" \
			$(DISTRIB) | gzip -9 -n > ../$(DISTRIB).tar.gz \
	)
	$(PRINT_RM) $(DISTRIB_DIR)
	$(RM) -r $(DISTRIB_DIR)

doc-companions:
	$(MAKE) -C doc/developer archives VERSION=$(VERSION)-$(VERSION_CODENAME)
	$(MV) doc/developer/hello-$(VERSION)-$(VERSION_CODENAME).tar.gz hello-$(VERSION)-$(VERSION_CODENAME).tar.gz
	$(ECHO) "The documentation companion hello-$(VERSION)-$(VERSION_CODENAME).tar.gz has been generated."

clean-distrib: dist-clean
	$(PRINT_RM) distrib
	$(RM) -r $(DISTRIB_DIR) $(DISTRIB).tar.gz

create_lib_to_install_list = $(addprefix $(FRAMAC_LIB)/,$(call map,notdir,$(1)))

byte:: bin/toplevel.byte$(EXE) lib/fc/frama-c.cma share/Makefile.dynamic_config \
	$(call create_lib_to_install_list,$(LIB_BYTE_TO_INSTALL)) \
      $(PLUGIN_META_LIST) lib/fc/META.frama-c

opt:: bin/toplevel.opt$(EXE) lib/fc/frama-c.cmxa share/Makefile.dynamic_config \
	$(call create_lib_to_install_list,$(LIB_OPT_TO_INSTALL)) \
	$(filter %.o %.cmi,\
	   $(call create_lib_to_install_list,$(LIB_BYTE_TO_INSTALL))) \
      $(PLUGIN_META_LIST) lib/fc/META.frama-c

top: bin/toplevel.top$(EXE) \
	$(call create_lib_to_install_list,$(LIB_BYTE_TO_INSTALL)) \
      $(PLUGIN_META_LIST)

##################
# Copy in lib/fc #
##################

define copy_in_lib
$(FRAMAC_LIB)/$(notdir $(1)): $(1)
	$(MKDIR) $(FRAMAC_LIB)
	$(CP) $$< $$@

endef
$(eval $(foreach file,$(LIB_BYTE_TO_INSTALL),$(call copy_in_lib,$(file))))
$(eval $(foreach file,$(LIB_OPT_TO_INSTALL),$(call copy_in_lib,$(file))))

################
# Generic part #
################

$(NON_OPAQUE_DEPS:%=%.cmx): OFLAGS := $(OFLAGS) -w -58

$(CROWBAR_AFL_TARGET:%=%.cmx): OFLAGS:=$(OFLAGS) -afl-instrument

include share/Makefile.generic

###############################################################################
# Local Variables:
# compile-command: "make"
# End:
