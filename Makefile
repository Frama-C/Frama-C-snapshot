##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2014                                               #
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

include share/Makefile.common

###################
# Frama-C Version #
###################

VERSION=$(shell $(SED) -e 's/\\(.*\\)/\\1/' VERSION)

ifeq ($(findstring +dev,$(VERSION)),+dev)
DEVELOPMENT=yes
else
DEVELOPMENT=no
endif

###########################
# Global plugin variables #
###########################

# the directory where compiled plugin files are stored
PLUGIN_LIB_DIR	= lib/plugins
PLUGIN_GUI_LIB_DIR= lib/plugins/gui

# the directory where the other Makefiles are
FRAMAC_SHARE	= share

# set it to yes to tell Makefile.dynamic than we come from here
FRAMAC_MAKE	=yes

# Shared lists between Makefile.plugin and Makefile :
# initialized them as "simply extended variables" (with :=)
# for a correct behavior of += (see section 6.6 of GNU Make manual)
PLUGIN_LIST	:=
PLUGIN_DYN_EXISTS:="no"
PLUGIN_DYN_LIST :=
PLUGIN_CMO_LIST	:=
PLUGIN_CMX_LIST	:=
PLUGIN_DYN_CMO_LIST :=
PLUGIN_DYN_CMX_LIST :=
PLUGIN_INTERNAL_CMO_LIST:=
PLUGIN_INTERNAL_CMX_LIST:=
PLUGIN_DEP_GUI_CMO_LIST:=
PLUGIN_DEP_GUI_CMX_LIST:=
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
PLUGIN_TESTS_LIST:=
PLUGIN_DISTRIBUTED_NAME_LIST:=

CEA_WP:=
UNMODIFIED_WHY3:=
MODIFIED_WHY3:=

###############################
# Additional global variables #
###############################

# put here any config option for the binary distribution outside of
# plugins
CONFIG_DISTRIB_BIN:=

# additional compilation targets for 'make all'.
# cannot be delayed after 'make all'
EXTRAS	= ptests

# Directories containing some source code
SRC_DIRS= ptests $(PLUGIN_LIB_DIR)

# Directory containing source code documentation
DOC_DIR	= doc/code

# Source files to document
MODULES_TODOC=

# Directories containing some source code
SRC_DIRS+= $(FRAMAC_SRC_DIRS)

# Directories to include when compiling
INCLUDES=$(addprefix -I , $(FRAMAC_SRC_DIRS)) -I $(PLUGIN_LIB_DIR) -I lib

# Directories to include for ocamldep
# Remove -I +.* and -I C:/absolute/win/path 
INCLUDES_FOR_OCAMLDEP=$(addprefix -I , $(FRAMAC_SRC_DIRS)) \
                      -I $(PLUGIN_LIB_DIR) -I lib

# Ocamldep flags
DEP_FLAGS= $(shell echo $(INCLUDES_FOR_OCAMLDEP) \
             | $(SED) -e "s/-I *.:[^ ]*//g" -e "s/-I *+[^ ]*//g")

# Files for which dependencies are computed
FILES_FOR_OCAMLDEP+=$(PLUGIN_LIB_DIR)/*.mli \
		$(addsuffix /*.mli, $(UNPACKED_DIRS)) \
		$(addsuffix /*.ml, $(UNPACKED_DIRS))

# Flags to use by ocamlc and ocamlopt
ifeq ($(DEVELOPMENT),yes)
ifeq ($(WARN_ERROR_ALL),yes)
DEV_FLAGS=$(FLAGS) -warn-error +a
else
DEV_FLAGS=$(FLAGS) -warn-error +a-32-33-34-35-36-37-38-39
endif #WARN_ERROR_ALL
else
DEV_FLAGS=$(FLAGS)
endif #DEVELOPMENT

BFLAGS	= $(DEV_FLAGS) $(DEBUG) $(INCLUDES) $(COVERAGE_COMPILER_BYTE) \
	  $(OCAMLVIZ_COMPILER_BYTE) $(OUNIT_COMPILER_BYTE)
OFLAGS	= $(DEV_FLAGS) $(DEBUG) $(INCLUDES) $(COVERAGE_COMPILER_OPT) \
	  $(GPROFOPT) $(OCAMLVIZ_COMPILER_OPT) $(OUNIT_COMPILER_OPT) -compact

BLINKFLAGS += $(BFLAGS) -linkall -custom
OLINKFLAGS += $(OFLAGS) -linkall

DOC_FLAGS= -colorize-code -stars -inv-merge-ml-mli -m A -hide-warnings \
	$(INCLUDES) $(GUI_INCLUDES)

# Libraries generated by Frama-C
GEN_BYTE_LIBS=
GEN_OPT_LIBS=

# Libraries used in Frama-C
EXTRA_OPT_LIBS:=
BYTE_LIBS = nums.cma unix.cma bigarray.cma str.cma dynlink.cma \
	$(GEN_BYTE_LIBS)
OPT_LIBS = nums.cmxa unix.cmxa bigarray.cmxa str.cmxa $(EXTRA_OPT_LIBS)

ifeq ("$(NATIVE_DYNLINK)","yes")
OPT_LIBS+= dynlink.cmxa
endif

OPT_LIBS+= $(GEN_OPT_LIBS)

ICONS:= $(addprefix share/, \
		frama-c.ico frama-c.gif unmark.png )

FEEDBACK_ICONS:= $(addprefix share/feedback/, \
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
		switch-on.png \
		switch-off.png )

ROOT_LIBC_DIR:= share/libc
LIBC_SUBDIRS:= . sys netinet linux net arpa
LIBC_DIR:= $(addprefix $(ROOT_LIBC_DIR)/, $(LIBC_SUBDIRS))
FREE_LIBC:= \
	share/*.h share/*.c \
	$(addsuffix /*.h, $(LIBC_DIR)) \
	$(ROOT_LIBC_DIR)/./__fc_builtin_for_normalization.i

NONFREE_LIBC:= $(addsuffix /*.[ci], $(LIBC_DIR))


# Checks that all .h can be included multiple times.
ALL_LIBC_HEADERS:=$(wildcard share/*.h $(addsuffix /*.h, $(LIBC_DIR)))

check-libc: bin/toplevel.$(OCAMLBEST)$(EXE)
	@echo "checking libc..."; \
	 EXIT_VALUE=0; \
	 for file in $(ALL_LIBC_HEADERS); do \
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
DISTRIB_FILES:= cil/*/*.ml* cil/*/*.in        				\
      $(filter-out cil/src/frontc/cparser.ml cil/src/frontc/cparser.mli \
          cil/src/logic/logic_lexer.ml cil/src/logic/logic_parser.mli   \
          cil/src/logic/logic_parser.ml cil/src/frontc/clexer.ml        \
          cil/src/logic/logic_preprocess.ml,                            \
	  $(wildcard cil/src/*/*.ml*))                                  \
      bin/*2*.sh							\
      share/frama-c.WIN32.rc share/frama-c.Unix.rc                      \
      $(ICONS) $(FEEDBACK_ICONS)			 		\
      man/frama-c.1 doc/manuals/*.pdf doc/README			\
      doc/code/docgen_*.ml						\
      doc/code/*.css doc/code/intro_plugin.txt				\
      doc/code/intro_plugin_D_and_S.txt                                 \
      doc/code/intro_plugin_default.txt                                 \
      doc/code/intro_kernel_plugin.txt doc/code/intro_occurrence.txt	\
      doc/code/intro_pdg.txt doc/code/intro_scope.txt			\
      doc/code/intro_slicing.txt doc/code/intro_sparecode.txt		\
      doc/code/intro_wp.txt doc/code/toc_head.htm			\
      doc/code/toc_tail.htm                                             \
      doc/code/print_api/*.ml* doc/code/print_api/Makefile              \
      tests/*/*.c tests/*/*.i tests/*/*.h tests/*/*.ml			\
      $(filter-out ptests/ptests_config.ml, $(wildcard ptests/*.ml*))   \
      configure.in Makefile                      			\
      share/Makefile.plugin share/Makefile.dynamic			\
      share/Makefile.dynamic_config.internal				\
      share/Makefile.dynamic_config.external Changelog config.h.in	\
      VERSION licenses/* 						\
      $(FREE_LIBC)                                                      \
      share/acsl.el share/configure.ac					\
      share/Makefile.config.in share/Makefile.common                    \
      share/Makefile.generic			                        \
      share/Makefile.plugin share/Makefile.dynamic			\
      share/Makefile.dynamic_config.external				\
      share/Makefile.dynamic_config.internal		   		\
      $(filter-out src/kernel/config.ml, $(wildcard src/kernel/*.ml*))  \
      external/hptmap.ml* external/unmarshal*.ml* external/unz.ml*      \
      external/sysutil.ml* 			\
      src/ai/*.ml* src/buckx/*.ml*				 	\
      src/buckx/*.c src/gui/*.ml* src/logic/*.ml*                       \
      $(filter-out src/lib/integer.ml                                   \
         src/lib/dynlink_common_interface.ml,				\
	$(wildcard src/lib/*.ml*)) 					\
      src/memory_state/*.ml* src/misc/*.ml* src/project/*.ml*		\
      src/printer/*.ml* src/toplevel/toplevel_config.ml src/type/*.ml*  \
      bin/sed_get_make_major bin/sed_get_make_minor                     \
      INSTALL INSTALL_WITH_WHY .make-clean	                        \
      .make-clean-stamp .make-ocamlgraph-stamp .force-reconfigure

# files that are needed to compile API documentation of external plugins
DOC_GEN_FILES:=$(addprefix doc/code/, \
	*.css intro_plugin.txt intro_kernel_plugin.txt \
	intro_plugin_default.txt intro_plugin_D_and_S \
	kernel-doc.ocamldoc \
	docgen_*.ml docgen.cm* *.htm)

################
# Main targets #
################

ifneq ($(ENABLE_GUI),no)
ifeq ($(HAS_LABLGTK),yes)
EXTRAS	+= gui
endif
endif

all:: byte $(OCAMLBEST) $(EXTRAS)

.PHONY: top opt byte dist bdist archclean rebuild
top: bin/toplevel.top$(EXE)
	$(MAKE) install-kernel-byte FRAMAC_LIBDIR=lib/fc

byte:: bin/toplevel.byte$(EXE) \
      share/Makefile.dynamic_config share/Makefile.kernel
	$(MAKE) install-kernel-byte FRAMAC_LIBDIR=lib/fc

opt:: bin/toplevel.opt$(EXE) \
     share/Makefile.dynamic_config share/Makefile.kernel
	$(MAKE) install-kernel-opt FRAMAC_LIBDIR=lib/fc

dist: clean
	$(QUIET_MAKE) OPTIM="-unsafe -noassert" DEBUG="" all

bdist: clean
	$(QUIET_MAKE) OPTIM="-unsafe -noassert" DEBUG="" byte

ifneq ("$(OCAMLGRAPH_LOCAL)","")
archclean: clean
	$(MAKE) -C $(OCAMLGRAPH_LOCAL) distclean
	cd $(OCAMLGRAPH_LOCAL) ; ./configure

rebuild: archclean
	$(MAKE) -C $(OCAMLGRAPH_LOCAL)
	$(QUIET_MAKE) all

OCAMLGRAPH_MERLIN="S `readlink -f $(OCAMLGRAPH_LOCAL)`\\nB `readlink -f $(OCAMLGRAPH_LOCAL)`"
else
archclean: clean

rebuild: archclean
	$(QUIET_MAKE) all

OCAMLGRAPH_MERLIN="PKG ocamlgraph"
endif

.PHONY:merlin
merlin:
#create Merlin file
	find `echo "src cil external" | xargs -n 1 -d ' ' readlink -f` \( -name .svn -name tests -o -name doc -o -name result -o -name -o -name oracle -o -name "*.cache" \) -prune -o \( -type d -printf "B %p\nS %p\n"  \) > .merlin
	echo $(OCAMLGRAPH_MERLIN) >> .merlin
	echo "PKG zarith" >> .merlin

############
# Coverage #
############

USE_COVERAGE_TOOL=no
ifeq ($(USE_COVERAGE_TOOL),yes)
COVERAGE_PATH=.
COVERAGE_PREPRO=camlp4o -no_quot -filter $(COVERAGE_PATH)/coverage_filter.cmo
COVERAGE_COMPILER_BYTE=-I $(COVERAGE_PATH) -pp "$(COVERAGE_PREPRO)"
COVERAGE_COMPILER_OPT=-I $(COVERAGE_PATH) -pp "$(COVERAGE_PREPRO)"
COVERAGE_LIB_BYTE=coverage.cma
COVERAGE_LIB_OPT=coverage.cmxa
endif

INCLUDES+=$(COVERAGE_COMPILER_BYTE)
INCLUDES_FOR_OCAMLDEP+=$(COVERAGE_COMPILER_BYTE)
GEN_BYTE_LIBS+=$(COVERAGE_LIB_BYTE)
GEN_OPT_LIBS+=$(COVERAGE_LIB_OPT)
SRC_DIRS+=$(COVERAGE_PATH)

########################
# Ocamlviz (profiling) #
########################

# To use OCamlviz you need to fix its makefile :-(
# In $(OCAMLVIZ_PATH)/Makefile.in change the line
# OCAMLVIZCMO = $(PROTOCOLCMO) src/monitor_impl.cmo src/ocamlviz.cmo src/ocamlviz_threads.cmo
# into
# OCAMLVIZCMO = $(PROTOCOLCMO) src/monitor_impl.cmo src/ocamlviz.cmo
# and
# cp -f src/ocamlviz.mli src/ocamlviz.cmi src/ocamlviz_threads.cmi $(OCAMLLIB)
# into
# cp -f src/ocamlviz.mli src/ocamlviz.cmi $(OCAMLLIB)
#
# Then run "./configure && make && make install" in $(OCAMLVIZ_PATH)
# Only one instance of Frama-C can be launched at a time when Ocamlviz is on.
USE_OCAMLVIZ_TOOL=no
ifeq ($(USE_OCAMLVIZ_TOOL),yes)
OCAMLVIZ_PATH=~/src/ocamlviz
OCAMLVIZ_COMPILER_BYTE=-I $(OCAMLVIZ_PATH)/src # -pp "camlp4 pa_o.cmo str.cma $(OCAMLVIZ_PATH)/camlp4/pa_ocamlviz.cmo pr_o.cmo"
# Seems really broken and generates fatal warnings
OCAMLVIZ_COMPILER_OPT=-I $(OCAMLVIZ_PATH)/src
OCAMLVIZ_LIB_BYTE=~/lib/ocaml/libocamlviz.cma
OCAMLVIZ_LIB_OPT=~/lib/ocaml/libocamlviz.cmxa
endif

BYTE_LIBS+=$(OCAMLVIZ_LIB_BYTE)
OPT_LIBS+=$(OCAMLVIZ_LIB_OPT)

#########
# OUnit #
#########

USE_OUNIT_TOOL=no
ifeq ($(USE_OUNIT_TOOL),yes)
  OCAML_LIBDIR :=$(shell ocamlc -where)
  OUNIT_PATH=$(OCAML_LIBDIR)/../pkg-lib/oUnit
  OUNIT_COMPILER_BYTE=-I $(OUNIT_PATH)
  OUNIT_COMPILER_OPT=-I $(OUNIT_PATH)
  OUNIT_LIB_BYTE=$(OUNIT_PATH)/oUnit.cma
  OUNIT_LIB_OPT=$(OUNIT_PATH)/oUnit.cmxa
endif

BYTE_LIBS+=$(OUNIT_LIB_BYTE)
OPT_LIBS+=$(OUNIT_LIB_OPT)

##############
# Ocamlgraph #
##############

ifneq ("$(OCAMLGRAPH_LOCAL)","")

GRAPH_FILES=graph.cmo
ifeq ($(OCAMLBEST),opt)
GRAPH_FILES+=graph.cmx
endif

lib/graph.cmi: .make-ocamlgraph $(wildcard $(OCAMLGRAPH_LOCAL)/src/*.ml*) \
		$(OCAMLGRAPH_LOCAL)/Makefile
	$(PRINT_BUILD) ocamlgraph
	$(MAKE) -C $(OCAMLGRAPH_LOCAL) $(GRAPH_FILES)
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/%,$@) $@

lib/graph.cmo: lib/graph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/%,$@) $@

lib/graph.cmx: lib/graph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/%,$@) $@

lib/graph.o: lib/graph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/%,$@) $@

GRAPH_LIB+= lib/graph.cmo lib/graph.cmi
ifneq ($(OCAMLOPT),no)
GRAPH_LIB+= lib/graph.cmx lib/graph.o
endif

GRAPH_BYTE_LIBS=lib/graph.cmo
GRAPH_OPT_LIBS=lib/graph.cmx
GEN_BYTE_LIBS+=$(GRAPH_BYTE_LIBS)
GEN_OPT_LIBS+=$(GRAPH_OPT_LIBS)

.PRECIOUS: .cmo .cmi .cmx .o .cmxa .cma

# dgraph (included in ocamlgraph)
ifeq ($(HAS_GNOMECANVAS),yes)
ifneq ($(ENABLE_GUI),no)

DGRAPH_FILES=dgraph/dgraph.cmo
ifeq ($(OCAMLBEST),opt)
DGRAPH_FILES+=dgraph/dgraph.cmx
endif

lib/dgraph.cmi: lib/graph.cmi
	$(PRINT_BUILD) ocamlgraph GUI
	$(MAKE) -C $(OCAMLGRAPH_LOCAL) $(DGRAPH_FILES)
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/dgraph/%,$@) $@

lib/dgraph.cmo: lib/dgraph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/dgraph/%,$@) $@

lib/dgraph.cmx: lib/dgraph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/dgraph/%,$@) $@

lib/dgraph.o: lib/dgraph.cmi
	$(PRINT_CP) $@
	$(CP) $(patsubst lib/%,$(OCAMLGRAPH_LOCAL)/dgraph/%,$@) $@

GRAPH_GUICMO= lib/dgraph.cmo
GRAPH_GUICMI= $(GRAPH_GUICMO:.cmo=.cmi)
GRAPH_GUICMX= $(GRAPH_GUICMO:.cmo=.cmx)
GRAPH_GUIO=  $(GRAPH_GUICMO:.cmo=.o)
GRAPH_LIB+= $(GRAPH_GUICMI) $(GRAPH_GUICMO)
ifneq ($(OCAMLOPT),no)
GRAPH_LIB+= $(GRAPH_GUICMX) $(GRAPH_GUIO)
endif

GEN_BYTE_GUI_LIBS+=$(GRAPH_GUICMO)
GEN_OPT_GUI_LIBS+=$(GRAPH_GUICMX)

HAS_DGRAPH=yes

else # enable_gui is no: disable dgraph
HAS_DGRAPH=no
endif
else # gnome_canvas is not yes: disable dgraph
HAS_DGRAPH=no
endif

else # does not use ocamlgraph local version

INCLUDES+=$(OCAMLGRAPH_INCLUDE)
BYTE_LIBS+= graph.cma
OPT_LIBS+= graph.cmxa

# and dgraph (included in ocamlgraph)
ifeq ($(HAS_GNOMECANVAS),yes)
ifneq ($(ENABLE_GUI),no)
GRAPH_GUICMO_BASE= dgraph.cmo
GRAPH_GUICMO=$(GRAPH_GUICMO_BASE:%=$(OCAMLGRAPH_HOME)/%)
GRAPH_GUICMX= $(GRAPH_GUICMO:.cmo=.cmx)
GRAPH_GUIO= $(GRAPH_GUICMO:.cmo=.o)
HAS_DGRAPH=yes
else # enable_gui is no: disable dgraph
HAS_DGRAPH=no
endif
else # gnome_canvas is not yes: disable dgraph
HAS_DGRAPH=no
endif

endif # testing ocamlgraph is local

GENERATED+=$(GRAPH_LIB)

# Redoing ocamlgraph on need
############################

# If 'make untar-ocamlgraph' have to be performed after 'svn update':
# change '.make-ocamlgraph-stamp' before 'cvs commit'
.make-ocamlgraph: .make-ocamlgraph-stamp
	$(TOUCH) $@
ifneq ("$(OCAMLGRAPH_LOCAL)","")
# Inline the rules of "untar-ocamlgraph" here
# because calling a recursive make does not work
	$(PRINT_UNTAR) ocamlgraph
	$(RM) -r $(OCAMLGRAPH_LOCAL)
	$(TAR) xzf ocamlgraph.tar.gz
	cd $(OCAMLGRAPH_LOCAL) && ./configure
	$(MAKE) clean
endif

include .make-ocamlgraph
DISTRIB_FILES += .make-ocamlgraph

# force "make untar-ocamlgraph" to be executed for all SVN users
force-ocamlgraph:
	expr `$(CAT) .make-ocamlgraph-stamp` + 1 > .make-ocamlgraph-stamp

.PHONY: untar-ocamlgraph
untar-ocamlgraph:
	$(PRINT_UNTAR) $@
	$(RM) -r $(OCAMLGRAPH_LOCAL)
	$(TAR) xzf ocamlgraph.tar.gz
	cd $(OCAMLGRAPH_LOCAL) && ./configure
	$(MAKE) clean

##########
# Zarith #
##########

ifeq ($(HAS_ZARITH),yes)
BYTE_LIBS+= zarith.cma
OPT_LIBS+= zarith.cmxa
INCLUDES+= -I $(ZARITH_PATH)
src/lib/integer.ml: src/lib/integer.ml.zarith \
		share/Makefile.config.in Makefile
	$(PRINT_CP) $@
	$(CP) $< $@
	$(CHMOD_RO) $@
else
src/lib/integer.ml: src/lib/integer.ml.bigint \
		share/Makefile.config.in Makefile
	$(PRINT_CP) $@
	$(CP) $< $@
	$(CHMOD_RO) $@
endif
GENERATED += src/lib/integer.ml
DISTRIB_FILES+= src/lib/integer.ml.zarith src/lib/integer.ml.bigint

##################
# Frama-C Kernel #
##################

# Dynlink library
#################

GENERATED += src/lib/dynlink_common_interface.ml

ifeq ($(USABLE_NATIVE_DYNLINK),yes) # native dynlink works

src/lib/dynlink_common_interface.ml: src/lib/dynlink_311_or_higher.ml \
		share/Makefile.config Makefile
	$(PRINT_MAKING) $@
	$(CP) $< $@
	$(CHMOD_RO) $@

else # native dynlink doesn't work

ifeq ($(NATIVE_DYNLINK),yes) # native dynlink does exist but doesn't work
src/lib/dynlink_common_interface.ml: src/lib/bad_dynlink_311_or_higher.ml \
		share/Makefile.config Makefile
	$(PRINT_MAKING) $@
	$(CP) $< $@
	$(CHMOD_RO) $@

else # no dynlink at all (for instance no native compiler)

# Just for ocamldep
src/lib/dynlink_common_interface.ml: src/lib/dynlink_311_or_higher.ml \
		share/Makefile.config Makefile
	$(PRINT_MAKING) $@
	$(CP) $< $@
	$(CHMOD_RO) $@

# Add two different rules for bytecode and native since
# the file dynlink_common_interface.ml does not provide from the same file
# in these cases.

src/lib/dynlink_common_interface.cmo: src/lib/dynlink_311_or_higher.ml \
		share/Makefile.config Makefile
	$(PRINT_MAKING) src/lib/dynlink_common_interface.ml
	$(CP) $< src/lib/dynlink_common_interface.ml
	$(CHMOD_RO) src/lib/dynlink_common_interface.ml
	$(PRINT_OCAMLC) $@
	$(OCAMLC) -c $(BFLAGS) src/lib/dynlink_common_interface.ml

src/lib/dynlink_common_interface.cmx: src/lib/no_dynlink_opt.ml \
		share/Makefile.config \
		Makefile
	$(PRINT_MAKING) src/lib/dynlink_common_interface.ml
	$(CP) $< src/lib/dynlink_common_interface.ml
	$(CHMOD_RO) src/lib/dynlink_common_interface.ml
	$(PRINT_OCAMLOPT) $@
	$(OCAMLOPT) -c $(OFLAGS) src/lib/dynlink_common_interface.ml

# force dependency order between these two files in order to not generate them
# in parallel since each of them generates the same .ml file
src/lib/dynlink_common_interface.cmx: src/lib/dynlink_common_interface.cmo
src/lib/dynlink_common_interface.o: src/lib/dynlink_common_interface.cmx

endif
endif


# Libraries which could be compiled fully independently
#######################################################

EXTERNAL_LIB_CMO = unmarshal unmarshal_nums sysutil

# Zarith
ifeq ($(HAS_ZARITH),yes)
EXTERNAL_LIB_CMO+= unz
MODULES_NODOC+=external/unz.mli
endif

EXTERNAL_LIB_CMO:= $(patsubst %, external/%.cmo, $(EXTERNAL_LIB_CMO))
CMO	+= $(EXTERNAL_LIB_CMO)

LIB_CMO = \
	src/lib/dynlink_common_interface \
	src/type/structural_descr \
	src/type/type \
	src/type/descr \
	src/lib/FCSet \
	src/lib/FCMap \
	src/lib/FCHashtbl \
	src/lib/extlib \
	src/lib/pretty_utils \
	src/lib/hook \
	src/lib/bag \
	src/lib/indexer \
	src/lib/vector \
	src/lib/bitvector \
	src/lib/qstack \
	src/lib/integer \
	src/lib/filepath

LIB_CMO:= $(addsuffix .cmo, $(LIB_CMO))
CMO	+= $(LIB_CMO)

# Very first files to be linked (most modules use them)
###############################

FIRST_CMO= src/kernel/config \
	src/kernel/gui_init \
	src/kernel/log \
	src/kernel/cmdline \
	src/project/project_skeleton \
	src/type/datatype \
	src/kernel/journal

# project_skeleton requires log
# datatype requires project_skeleton
# rangemap requires datatype

FIRST_CMO:= $(addsuffix .cmo, $(FIRST_CMO))
CMO	+= $(FIRST_CMO)

#Project (Project_skeleton must be linked before Journal)
PROJECT_CMO= \
	state \
	state_dependency_graph \
	state_topological \
	state_selection \
	project \
	state_builder
PROJECT_CMO:= $(patsubst %, src/project/%.cmo, $(PROJECT_CMO))
CMO	+= $(PROJECT_CMO)

# Kernel files usable by Cil
PRE_KERNEL_CMO= \
	src/kernel/typed_parameter \
	src/kernel/dynamic \
	src/kernel/parameter_customize \
	src/kernel/parameter_state \
	src/kernel/parameter_builder \
	src/kernel/plugin \
	src/kernel/kernel \
	src/kernel/emitter \
	src/lib/floating_point \
	src/lib/rangemap \
	src/lib/binary_cache \
	external/hptmap \
	src/lib/hptset \
	src/printer/printer_builder

PRE_KERNEL_CMO:= $(patsubst %, %.cmo, $(PRE_KERNEL_CMO))
CMO	+= $(PRE_KERNEL_CMO)

MLI_ONLY +=src/kernel/parameter_sig.mli

# Cil
#####

ifeq ("$(LOCAL_MACHDEP)","yes")

# Create the machine dependency module
# If the cl command cannot be run then the MSVC part will be identical to GCC
.PHONY : machdep $(CIL_PATH)/local_machdep.ml
machdep: $(CIL_PATH)/local_machdep.ml
bin/machdep.exe: machdep

$(CIL_PATH)/local_machdep.ml : cil/src/machdep.c configure.in Makefile
	$(PRINT_MAKING) $@
	$(RM) $@
	$(ECHO) "(* This module was generated automatically by code in Makefile and machdep.c *)" >$@
# Now generate the type definition
	$(ECHO) "open Cil_types" >> $@
	if gcc -D_GNUCC $< -o bin/machdep.exe ;then \
	    $(ECHO) "machdep.exe created succesfully."; \
	else \
	    $(RM) $@; exit 1; \
	fi
	$(ECHO) "let gcc = {" >>$@
	./bin/machdep.exe >>$@
	$(ECHO) "	 underscore_name = $(UNDERSCORE_NAME) ;" >> $@
	$(ECHO) "}"          >>$@
	if cl /D_MSVC $< /Febin/machdep.exe /Fobin/machdep.obj ;then \
	   $(ECHO) "let hasMSVC = true" >>$@; \
	else \
	   $(ECHO) "let hasMSVC = false" >>$@; \
	fi
	$(ECHO) "let msvc = {" >>$@
	./bin/machdep.exe >>$@
	$(ECHO) "	 underscore_name = true ;" >> $@
	$(ECHO) "}"          >>$@
	$(ECHO) \
	  "let gccHas__builtin_va_list = $(HAVE_BUILTIN_VA_LIST)" >>$@
	$(ECHO) "let __thread_is_keyword = $(THREAD_IS_KEYWORD)"  >>$@
	$(ECHO) \
	  "$@ generated. You may have this file merged into Frama-C by developers."
	$(CHMOD_RO) $@

endif

# .cmo files of cil
CIL_CMO = cil/src/cilmsg.cmo cil/ocamlutil/alpha.cmo			\
	cil/ocamlutil/cilconfig.cmo 					\
	$(addprefix $(CIL_PATH)/,                                       \
		cil_datatype.cmo	                                \
		cil_state_builder.cmo 					\
		logic/utf8_logic.cmo				        \
		machdep_x86_16.cmo machdep_x86_32.cmo			\
		machdep_x86_64.cmo machdep_ppc_32.cmo	        	\
		cil_const.cmo			                        \
		logic/logic_env.cmo escape.cmo				\
		logic/logic_const.cmo cil.cmo)		                \
        src/printer/cil_printer.cmo                                     \
        src/printer/cil_descriptive_printer.cmo                         \
	$(addprefix $(CIL_PATH)/,                                       \
		    frontc/errorloc.cmo	                                \
		    frontc/cabs.cmo ext/expcompare.cmo			\
		    frontc/cabs_debug.cmo				\
		    frontc/cabshelper.cmo 				\
		    logic/logic_utils.cmo logic/logic_builtin.cmo	\
		    logic/logic_print.cmo logic/logic_parser.cmo	\
		    logic/logic_lexer.cmo frontc/lexerhack.cmo		\
		    mergecil.cmo rmtmps.cmo logic/logic_typing.cmo	\
		    frontc/cprint.cmo frontc/cabscond.cmo		\
		    frontc/cabsvisit.cmo frontc/cabs2cil.cmo		\
		    frontc/clexer.cmo frontc/cparser.cmo		\
		    logic/logic_preprocess.cmo				\
		    frontc/frontc.cmo					\
		    ext/callgraph.cmo					\
		    ext/dataflow.cmo 					\
		    ext/oneret.cmo \
		    ext/cfg.cmo \
	) # end of addprefix

CMO	+= $(CIL_CMO)
MLI_ONLY+= $(CIL_PATH)/cil_types.mli $(CIL_PATH)/logic/logic_ptree.mli \
	src/printer/printer_api.mli
NO_MLI+= \
	cil/src/machdep_ppc_32.mli \
	cil/src/machdep_x86_16.mli \
	cil/src/machdep_x86_32.mli \
	cil/src/machdep_x86_64.mli \
	cil/src/frontc/cabs.mli \
	cil/src/frontc/cabs_debug.mli \
	cil/src/ext/expcompare.mli \
	cil/src/logic/logic_lexer.mli \
	cil/src/frontc/lexerhack.mli \
	cil/src/ext/usedef.mli \
	cil/src/ext/liveness.mli \
	cil/src/ext/reachingdefs.mli \
	cil/src/ext/availexpslv.mli \
	cil/src/ext/rmciltmps.mli
MODULES_NODOC+=	cil/src/machdep_ppc_32.ml \
	cil/src/machdep_x86_16.ml \
	cil/src/machdep_x86_32.ml \
	cil/src/machdep_x86_64.ml \

GENERATED += $(addprefix $(CIL_PATH)/, \
		frontc/clexer.ml frontc/cparser.ml frontc/cparser.mli \
		logic/logic_lexer.ml logic/logic_parser.ml \
		logic/logic_parser.mli logic/logic_preprocess.ml)

.PHONY: check-logic-parser-wildcard
check-logic-parser-wildcard:
	cd $(CIL_PATH)/logic && ocaml check_logic_parser.ml

# Buckx
#######

CMO	+= src/buckx/buckx.cmo

GEN_BUCKX=src/buckx/buckx_c.o
GEN_BYTE_LIBS+= $(GEN_BUCKX)
GEN_OPT_LIBS+= $(GEN_BUCKX)

src/buckx/buckx_c.o: src/buckx/buckx_c.c
	$(PRINT_OCAMLC) $@
	$(OCAMLC) $(BFLAGS) -ccopt "-O3 -fno-pic -fomit-frame-pointer -o $@" $<

# Main part of the kernel
#########################

# cannot use $(CONFIG_CMO) here :-(
KERNEL_CMO= \
	src/kernel/ast_info.cmo \
	src/kernel/ast.cmo \
	src/kernel/globals.cmo \
	src/kernel/kernel_function.cmo \
	src/logic/property.cmo \
	src/logic/property_status.cmo \
	src/logic/annotations.cmo \
	src/printer/printer.cmo \
	src/kernel/stmts_graph.cmo \
	cil/src/ext/ordered_stmt.cmo \
	cil/src/ext/dataflows.cmo \
	cil/src/ext/dataflow2.cmo \
	cil/src/ext/usedef.cmo \
	cil/src/ext/liveness.cmo \
	cil/src/ext/reachingdefs.cmo \
	cil/src/ext/availexpslv.cmo \
	cil/src/ext/rmciltmps.cmo \
	cil/src/ext/deadcodeelim.cmo \
	src/kernel/dominators.cmo \
	src/logic/description.cmo \
	src/logic/statuses_by_call.cmo \
	src/kernel/alarms.cmo \
	src/kernel/messages.cmo \
	src/ai/abstract_interp.cmo \
	src/ai/int_Base.cmo \
	src/kernel/unicode.cmo \
	src/misc/service_graph.cmo \
	src/ai/ival.cmo \
	src/misc/bit_utils.cmo \
	src/ai/lattice_Interval_Set.cmo \
	src/ai/base.cmo \
	src/ai/origin.cmo \
	src/ai/map_Lattice.cmo \
	src/ai/trace.cmo \
	src/memory_state/locations.cmo \
	src/memory_state/value_messages.cmo \
	src/kernel/cilE.cmo \
	src/memory_state/int_Interv.cmo \
	src/memory_state/int_Interv_Map.cmo \
	src/memory_state/tr_offset.cmo \
	src/memory_state/offsetmap.cmo \
	src/memory_state/offsetmap_bitwise.cmo \
	src/memory_state/lmap.cmo \
	src/memory_state/lmap_bitwise.cmo \
	src/memory_state/function_Froms.cmo \
	src/memory_state/cvalue.cmo \
	src/memory_state/widen_type.cmo \
	src/kernel/visitor.cmo \
	cil/src/frontc/cabsbranches.cmo \
	src/kernel/loop.cmo \
	$(PLUGIN_TYPES_CMO_LIST) \
	src/memory_state/value_types.cmo \
	src/kernel/db.cmo  \
	src/kernel/command.cmo \
	src/kernel/task.cmo \
	src/kernel/file.cmo \
	src/logic/translate_lightweight.cmo \
	src/kernel/unroll_loops.cmo \
	src/misc/filter.cmo \
	src/kernel/special_hooks.cmo \
	src/logic/logic_interp.cmo \
	src/logic/infer_annotations.cmo \
	src/logic/allocates.cmo

CMO	+= $(KERNEL_CMO)

MLI_ONLY+= src/ai/lattice_type.mli \
	src/memory_state/offsetmap_lattice_with_isotropy.mli \
	src/memory_state/offsetmap_sig.mli src/memory_state/lmap_sig.mli

NO_MLI+= src/ai/map_Lattice.mli src/memory_state/value_messages.mli \
	src/memory_state/int_Interv_Map.mli

# Common startup module
# All link command should add it as last linked module and depend on it.
########################################################################

STARTUP_CMO=src/kernel/boot.cmo
STARTUP_CMX=$(STARTUP_CMO:.cmo=.cmx)

# GUI modules
# See below for GUI compilation
##############################################################################

SINGLE_GUI_CMO:= gui_parameters \
	gtk_helper gtk_form toolbox \
	source_viewer pretty_source source_manager book_manager \
	warning_manager \
	filetree \
	launcher \
	menu_manager \
	history \
	design \
	analyses_manager file_manager project_manager debug_manager \
	help_manager \
	property_navigator

SINGLE_GUI_CMO:= $(patsubst %, src/gui/%.cmo, $(SINGLE_GUI_CMO))

###############################################################################
#                                                                             #
####################                                                          #
# Plug-in sections #                                                          #
####################                                                          #
#                                                                             #
# For 'internal' developpers:                                                 #
# you can add your own plug-in here,                                          #
# but it is better to have your own separated Makefile                        #
###############################################################################

###########
# Metrics #
###########

PLUGIN_ENABLE:=$(ENABLE_METRICS)
PLUGIN_NAME:=Metrics
PLUGIN_DISTRIBUTED:=yes
PLUGIN_HAS_MLI:=yes
PLUGIN_DIR:=src/metrics
PLUGIN_CMO:= metrics_parameters css_html metrics_base metrics_acsl \
	     metrics_cabs metrics_cilast metrics_coverage \
	     register
PLUGIN_GUI_CMO:= metrics_gui register_gui
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

#######################
# Syntactic callgraph #
#######################

# Extension of the GUI for syntactic callgraph is compilable
# only if dgraph is available
ifeq ($(HAS_DGRAPH),yes)
PLUGIN_GUI_CMO:=cg_viewer
else
PLUGIN_UNDOC:=cg_viewer.ml
endif

PLUGIN_ENABLE:=$(ENABLE_SYNTACTIC_CALLGRAPH)
PLUGIN_NAME:=Syntactic_callgraph
PLUGIN_DISTRIBUTED:=yes
PLUGIN_HAS_MLI:=yes
PLUGIN_DIR:=src/syntactic_callgraph
PLUGIN_CMO:= options register
PLUGIN_NO_TEST:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

##################
# Value analysis #
##################

PLUGIN_ENABLE:=$(ENABLE_VALUE_ANALYSIS)
PLUGIN_NAME:=Value
PLUGIN_DIR:=src/value
PLUGIN_CMO:= kf_state split_strategy value_parameters \
        stop_at_nth value_perf value_util \
	library_functions mark_noresults separate \
	state_set state_imp value_results current_table widen warn \
	precise_locs eval_op eval_exprs non_linear initial_state \
	locals_scoping  builtins \
	eval_terms eval_annots mem_exec function_args \
	split_return eval_stmt per_stmt_slevel eval_slevel \
	$(sort $(patsubst src/value/%.ml,%,\
	           $(wildcard src/value/builtins_nonfree*.ml))) \
	eval_funs register
PLUGIN_GUI_CMO:=register_gui
PLUGIN_HAS_MLI:=yes
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
include share/Makefile.plugin

##################
# Occurrence     #
##################

PLUGIN_ENABLE:=$(ENABLE_OCCURRENCE)
PLUGIN_NAME:=Occurrence
PLUGIN_DISTRIBUTED:=yes
PLUGIN_HAS_MLI:=yes
PLUGIN_DIR:=src/occurrence
PLUGIN_CMO:= options register
PLUGIN_GUI_CMO:=register_gui
PLUGIN_INTRO:=doc/code/intro_occurrence.txt
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

################################################
# Runtime Error Annotation Generation analysis #
################################################

PLUGIN_ENABLE:=$(ENABLE_RTE_ANNOTATION)
PLUGIN_NAME:=RteGen
PLUGIN_DIR:=src/rte
PLUGIN_CMO:= options generator rte visit register
PLUGIN_HAS_MLI:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

#################
# From analysis #
#################

PLUGIN_ENABLE:=$(ENABLE_FROM_ANALYSIS)
PLUGIN_NAME:=From
PLUGIN_DIR:=src/from
PLUGIN_CMO:= from_parameters from_compute \
	functionwise callwise path_dependencies mem_dependencies from_register
PLUGIN_GUI_CMO:=from_register_gui
PLUGIN_HAS_MLI:=yes
PLUGIN_TESTS_DIRS:=idct test float
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

##################
# Users analysis #
##################

PLUGIN_ENABLE:=$(ENABLE_USERS)
PLUGIN_NAME:=Users
PLUGIN_DIR:=src/users
PLUGIN_CMO:= users_register
PLUGIN_HAS_MLI:=yes
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

########################
# Constant propagation #
########################

PLUGIN_ENABLE:=$(ENABLE_SEMANTIC_CONSTANT_FOLDING)
PLUGIN_NAME:=Constant_Propagation
PLUGIN_DIR:=src/constant_propagation
PLUGIN_CMO:= propagationParameters \
	register
PLUGIN_HAS_MLI:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

###################
# Post-dominators #
###################

PLUGIN_ENABLE:=$(ENABLE_POSTDOMINATORS)
PLUGIN_NAME:=Postdominators
PLUGIN_DIR:=src/postdominators
PLUGIN_CMO:= postdominators_parameters print compute
PLUGIN_HAS_MLI:=yes
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

#########
# inout #
#########

PLUGIN_ENABLE:=$(ENABLE_INOUT)
PLUGIN_NAME:=Inout
PLUGIN_DIR:=src/inout
PLUGIN_CMO:= inout_parameters cumulative_analysis \
	     operational_inputs outputs inputs derefs access_path register
PLUGIN_TYPES_CMO:=src/memory_state/inout_type
PLUGIN_HAS_MLI:=yes
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

######################
# Semantic callgraph #
######################

PLUGIN_ENABLE:=$(ENABLE_SEMANTIC_CALLGRAPH)
PLUGIN_NAME:=Semantic_callgraph
PLUGIN_DIR:=src/semantic_callgraph
PLUGIN_CMO:= options register
PLUGIN_HAS_MLI:=yes
PLUGIN_NO_TEST:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

###################
# Impact analysis #
###################

PLUGIN_ENABLE:=$(ENABLE_IMPACT)
PLUGIN_NAME:=Impact
PLUGIN_DIR:=src/impact
PLUGIN_CMO:= options pdg_aux reason_graph compute_impact register
PLUGIN_GUI_CMO:= register_gui
PLUGIN_HAS_MLI:=yes
PLUGIN_DISTRIBUTED:=yes
# PLUGIN_UNDOC:=impact_gui.ml
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

##################################
# PDG : program dependence graph #
##################################

PLUGIN_ENABLE:=$(ENABLE_PDG)
PLUGIN_NAME:=Pdg
PLUGIN_DIR:=src/pdg
PLUGIN_HAS_MLI:=yes
PLUGIN_CMO:= pdg_parameters \
	    ctrlDpds \
	    pdg_state \
	    build \
	    sets \
	    annot \
	    marks \
	    register

PDG_TYPES:=pdgIndex pdgTypes pdgMarks
PDG_TYPES:=$(addprefix src/pdg_types/, $(PDG_TYPES))
PLUGIN_TYPES_CMO:=$(PDG_TYPES)

PLUGIN_INTRO:=doc/code/intro_pdg.txt
PLUGIN_TYPES_TODOC:=$(addsuffix .mli, $(PDG_TYPES))

PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

################################################
# Scope : show different kinds of dependencies #
################################################

PLUGIN_ENABLE:=$(ENABLE_SCOPE)
PLUGIN_NAME:=Scope
PLUGIN_DIR:=src/scope
PLUGIN_CMO:= datascope zones defs
PLUGIN_HAS_MLI:=yes
PLUGIN_GUI_CMO:=dpds_gui
PLUGIN_INTRO:=doc/code/intro_scope.txt
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

#####################################
# Sparecode : unused code detection #
#####################################

PLUGIN_ENABLE:=$(ENABLE_SPARECODE)
PLUGIN_NAME:=Sparecode
PLUGIN_DIR:=src/sparecode
PLUGIN_CMO:= sparecode_params globs spare_marks transform register
PLUGIN_HAS_MLI:=yes
PLUGIN_DEPENDS:=Pdg
PLUGIN_INTRO:=doc/code/intro_sparecode.txt
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

###########
# Slicing #
###########

PLUGIN_ENABLE:=$(ENABLE_SLICING)
PLUGIN_NAME:=Slicing
PLUGIN_DIR:=src/slicing
PLUGIN_CMO:= slicingParameters \
	    slicingMacros \
	    slicingMarks \
	    slicingActions \
	    fct_slice \
	    printSlice \
	    slicingProject \
	    slicingTransform \
	    slicingCmds \
	    register
SLICING_TYPES:=slicingInternals slicingTypes
SLICING_TYPES:=$(addprefix src/slicing_types/, $(SLICING_TYPES))
PLUGIN_TYPES_CMO:=$(SLICING_TYPES)

PLUGIN_GUI_CMO:=register_gui

PLUGIN_INTRO:=doc/code/intro_slicing.txt
PLUGIN_TYPES_TODOC:= $(addsuffix .ml, $(SLICING_TYPES))
PLUGIN_UNDOC:=register.ml # slicing_gui.ml

PLUGIN_TESTS_DIRS:= slicing slicing2
#PLUGIN_TESTS_DIRS_DEFAULT:=slicing
PLUGIN_TESTS_LIB:= tests/slicing/libSelect tests/slicing/libAnim
PLUGIN_DEPENDS:=Pdg
PLUGIN_DISTRIBUTED:=yes
PLUGIN_INTERNAL_TEST:=yes
include share/Makefile.plugin

FILES_FOR_OCAMLDEP+=$(TEST_SLICING_ML)

#####################
# External plug-ins #
#####################

define INCLUDE_PLUGIN
FRAMAC_INTERNAL:=yes
FRAMAC_MAKE:=yes
FRAMAC_SHARE:=./share
FRAMAC_PLUGIN:=lib/plugins
FRAMAC_PLUGIN_GUI:=lib/plugins/gui
PLUGIN_DIR:=$(1)
FRAMAC_LIB:=lib/fc
include $(1)/Makefile
endef

$(foreach p, $(EXTERNAL_PLUGINS), $(eval $(call INCLUDE_PLUGIN,$p)))

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

FILES_FOR_OCAMLDEP+= $(addsuffix /*.mli, $(FRAMAC_SRC_DIRS)) \
	$(addsuffix /*.ml, $(FRAMAC_SRC_DIRS))

MODULES_TODOC+=$(filter-out $(MODULES_NODOC), \
	$(MLI_ONLY) $(NO_MLI:.mli=.ml) \
	$(filter-out $(NO_MLI), \
	$(filter-out $(PLUGIN_TYPES_CMO_LIST:.cmo=.mli), $(CMO:.cmo=.mli))))

############
# Toplevel #
############

ALL_BATCH_CMO= $(filter-out src/kernel/gui_init.cmo, $(ALL_CMO))
# ALL_BATCH_CMX is not a translation of ALL_BATCH_CMO with cmo -> cmx
# in case native dynlink is not available: dynamic plugin are linked
# dynamically in bytecode and statically in native code...
ALL_BATCH_CMX= $(filter-out src/kernel/gui_init.cmx, $(ALL_CMX))

bin/toplevel.byte$(EXE): $(ALL_BATCH_CMO) $(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLC) $(BLINKFLAGS) -o $@ $(BYTE_LIBS) $(ALL_BATCH_CMO)

bin/toplevel.prof$(EXE): $(ALL_BATCH_CMO) $(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST)
	$(PRINT_OCAMLCP) $@
	$(OCAMLCP) $(BFLAGS) -o $@ $(BYTE_LIBS) $(ALL_BATCH_CMO)

src/toplevel/toplevel_boot.ml: src/toplevel/toplevel_config.ml \
			       src/kernel/boot.ml Makefile
	cp src/toplevel/toplevel_config.ml $@
	sed -e "s/~quit:true/~quit:false/" src/kernel/boot.ml >> $@

GENERATED+= src/toplevel/toplevel_boot.ml

bin/toplevel.top$(EXE): $(filter-out src/kernel/boot.ml, $(ALL_BATCH_CMO)) \
			src/toplevel/toplevel_boot.cmo \
			$(GEN_BYTE_LIBS) $(PLUGIN_DYN_CMO_LIST)
	$(PRINT_OCAMLMKTOP) $@
	$(OCAMLMKTOP) $(BFLAGS) -custom -o $@ $(BYTE_LIBS) \
	  $(patsubst src/kernel/boot.cmo, src/toplevel/toplevel_boot.cmo, \
		 $(ALL_BATCH_CMO))	  

bin/toplevel.opt$(EXE): $(ALL_BATCH_CMX) $(GEN_OPT_LIBS) \
			$(PLUGIN_DYN_CMX_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) $(OLINKFLAGS) -o $@ $(OPT_LIBS) $(ALL_BATCH_CMX)

share/Makefile.kernel: Makefile share/Makefile.config share/Makefile.common
	$(PRINT_MAKING) $@
	$(RM) $@
	$(ECHO) "# This makefile was automatically generated." > $@
	$(ECHO) "# Do not modify." >> $@
	$(ECHO) "ifeq (\$$(FRAMAC_INTERNAL),yes)" >> $@
	$(ECHO) "DYN_BLINKFLAGS=$(filter-out $(INCLUDES), $(BLINKFLAGS)) $(foreach d, $(INCLUDES:-I%=%), -I $(FRAMAC_TOP_SRCDIR)/$(d))" >> $@
	$(ECHO) "DYN_GEN_BYTE_LIBS=$(addprefix $(FRAMAC_TOP_SRCDIR)/, $(GEN_BYTE_LIBS))" >> $@
	$(ECHO) "DYN_BYTE_LIBS=$(filter-out $(GEN_BYTE_LIBS), $(BYTE_LIBS))" >> $@
	$(ECHO) "DYN_ALL_BATCH_CMO=$(addprefix $(FRAMAC_TOP_SRCDIR)/, $(notdir $(ALL_BATCH_CMO)))" >> $@
	$(ECHO) "DYN_OLINKFLAGS=$(filter-out $(INCLUDES), $(OLINKFLAGS)) $(foreach d, $(INCLUDES:-I%=%), -I $(FRAMAC_TOP_SRCDIR)/$(d))" >> $@
	$(ECHO) "DYN_GEN_OPT_LIBS=$(addprefix $(FRAMAC_TOP_SRCDIR)/, $(GEN_OPT_LIBS))" >> $@
	$(ECHO) "DYN_OPT_LIBS=$(filter-out $(GEN_OPT_LIBS), $(OPT_LIBS))" >> $@
	$(ECHO) "DYN_ALL_BATCH_CMX=$(addprefix $(FRAMAC_TOP_SRCDIR)/, $(ALL_BATCH_CMX))" >> $@
	$(ECHO) "else" >> $@
	$(ECHO) "DYN_BLINKFLAGS=$(filter-out $(INCLUDES), $(BLINKFLAGS)) $(addprefix -I ,$(filter +%,$(INCLUDES)))" >> $@
	$(ECHO) "DYN_GEN_BYTE_LIBS=$(addprefix $(FRAMAC_LIBDIR)/, $(notdir $(GEN_BYTE_LIBS)))" >> $@
	$(ECHO) "DYN_BYTE_LIBS=$(filter-out $(GEN_BYTE_LIBS), $(BYTE_LIBS))" >> $@
	$(ECHO) "DYN_ALL_BATCH_CMO=$(addprefix $(FRAMAC_LIBDIR)/, $(notdir $(ALL_BATCH_CMO)))" >> $@
	$(ECHO) "DYN_OLINKFLAGS=$(filter-out $(INCLUDES), $(OLINKFLAGS))  $(addprefix -I ,$(filter +%,$(INCLUDES)))" >> $@
	$(ECHO) "DYN_GEN_OPT_LIBS=$(addprefix $(FRAMAC_LIBDIR)/, $(notdir $(GEN_OPT_LIBS)))" >> $@
	$(ECHO) "DYN_OPT_LIBS=$(filter-out $(GEN_OPT_LIBS), $(OPT_LIBS))" >> $@
	$(ECHO) "DYN_ALL_BATCH_CMX=$(addprefix $(FRAMAC_LIBDIR)/, $(notdir $(ALL_BATCH_CMX)))" >> $@
	$(ECHO) "endif" >> $@
	$(CHMOD_RO) $@

#######
# GUI #
#######

ifneq ($(ENABLE_GUI),no)
GUI_INCLUDES = -I src/gui -I $(LABLGTK_PATH)
INCLUDES_FOR_OCAMLDEP+=-I src/gui
BYTE_GUI_LIBS+= lablgtk.cma
OPT_GUI_LIBS += lablgtk.cmxa
FILES_FOR_OCAMLDEP+= src/gui/*.ml src/gui/*.mli

ifeq ("$(OCAMLGRAPH_LOCAL)","")
GUI_INCLUDES += $(OCAMLGRAPH)
endif

ifeq ($(HAS_GNOMECANVAS),yes)
BYTE_GUI_LIBS += lablgnomecanvas.cma
OPT_GUI_LIBS += lablgnomecanvas.cmxa
endif

ifeq ($(HAS_LABLGTK),yes)
EXTRAS	+= gui
endif

ifeq ($(HAS_GTKSOURCEVIEW),yes)
ifeq ($(HAS_LEGACY_GTKSOURCEVIEW),yes)
GUI_INCLUDES  += -I $(LABLGTK_PATH)/lablgtksourceview
endif
BYTE_GUI_LIBS += lablgtksourceview2.cma
OPT_GUI_LIBS  += lablgtksourceview2.cmxa
endif


# NEW dynamic GUI
ifeq (no,yes)

PLUGIN_ENABLE:=$(ENABLE_GUI)
PLUGIN_NAME:=Gui
PLUGIN_DISTRIBUTED:=yes
# PLUGIN_HAS_MLI:=yes
PLUGIN_DIR:=src/gui
#PLUGIN_CMO:=
PLUGIN_CMO:= \
	gtk_helper gtk_form toolbox \
	source_viewer pretty_source source_manager \
	warning_manager \
	filetree \
	launcher \
	menu_manager \
	history \
	design \
	project_manager \
	debug_manager \
	about_dialog \
	property_navigator \
	po_navigator
PLUGIN_BFLAGS:=-I $(LABLGTK_PATH)
PLUGIN_OFLAGS:=-I $(LABLGTK_PATH)
PLUGIN_LINK_BFLAGS:=-I $(LABLGTK_PATH)
PLUGIN_EXTRA_BYTE:=lablgtk.cma lablgtksourceview.cma
PLUGIN_EXTRA_OPT:=lablgtk.cmxa
PLUGIN_DYNAMIC:=yes

lablgtk.cma lablgtksourceview.cma:
lablgtk.cmxa:

include share/Makefile.plugin

gui:: lib/plugins/Gui.cmo

else

SINGLE_GUI_CMI = $(SINGLE_GUI_CMO:.cmo=.cmi)
SINGLE_GUI_CMX = $(SINGLE_GUI_CMO:.cmo=.cmx)

GUICMO += $(SINGLE_GUI_CMO) $(PLUGIN_GUI_CMO_LIST)

MODULES_TODOC+= $(filter-out src/gui/book_manager.mli, \
	$(SINGLE_GUI_CMO:.cmo=.mli))

GUICMI = $(GUICMO:.cmo=.cmi)
GUICMX = $(SINGLE_GUI_CMX) $(PLUGIN_GUI_CMX_LIST)

$(GUICMI) $(GUICMO) bin/viewer.byte$(EXE): BFLAGS+= $(GUI_INCLUDES)
$(GUICMX) bin/viewer.opt$(EXE): OFLAGS+= $(GUI_INCLUDES)

$(PLUGIN_DEP_GUI_CMO_LIST) $(PLUGIN_DYN_DEP_GUI_CMO_LIST): BFLAGS+= $(GUI_INCLUDES)
$(PLUGIN_DEP_GUI_CMX_LIST) $(PLUGIN_DYN_DEP_GUI_CMX_LIST): OFLAGS+= $(GUI_INCLUDES)

.PHONY:gui

gui:: bin/viewer.byte$(EXE) share/Makefile.dynamic_config share/Makefile.kernel
	$(MAKE) install-gui-byte FRAMAC_LIBDIR=lib/fc

ifeq ($(OCAMLBEST),opt)
gui:: bin/viewer.opt$(EXE)
	$(MAKE) install-gui-opt FRAMAC_LIBDIR=lib/fc
endif

ALL_GUI_CMO= $(ALL_CMO) $(GRAPH_GUICMO) $(GUICMO)
ALL_GUI_CMX= $(patsubst %.cma, %.cmxa, $(ALL_GUI_CMO:.cmo=.cmx))

bin/viewer.byte$(EXE): BYTE_LIBS+=$(BYTE_GUI_LIBS) $(GRAPH_GUICMO)
# recompile ocamlgraph on need iff we use its local version
ifneq ("$(OCAMLGRAPH_LOCAL)","")
bin/viewer.byte$(EXE): $(GRAPH_GUICMO)
endif
bin/viewer.byte$(EXE): $(filter-out $(GRAPH_GUICMO), $(ALL_GUI_CMO)) \
			$(GEN_BYTE_LIBS) \
			$(PLUGIN_DYN_CMO_LIST) $(PLUGIN_DYN_GUI_CMO_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLC) $(BLINKFLAGS) -o $@ $(BYTE_LIBS) \
	  $(CMO) \
	  $(filter-out \
	    $(patsubst $(PLUGIN_GUI_LIB_DIR)/%, $(PLUGIN_LIB_DIR)/%, \
	        $(PLUGIN_GUI_CMO_LIST)), \
	    $(PLUGIN_CMO_LIST)) \
	  $(GUICMO) $(STARTUP_CMO)

bin/viewer.opt$(EXE): OPT_LIBS+= $(OPT_GUI_LIBS) $(GRAPH_GUICMX)
# recompile ocamlgraph on need iff we use its local version
ifneq ("$(OCAMLGRAPH_LOCAL)","")
bin/viewer.opt$(EXE): $(GRAPH_GUICMX) $(GRAPH_GUIO)
endif
bin/viewer.opt$(EXE): $(filter-out $(GRAPH_GUICMX), $(ALL_GUI_CMX)) \
			$(GEN_OPT_LIBS) \
			$(PLUGIN_DYN_CMX_LIST) $(PLUGIN_DYN_GUI_CMX_LIST) \
			$(PLUGIN_CMX_LIST) $(PLUGIN_GUI_CMX_LIST)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) $(OLINKFLAGS) -o $@ $(OPT_LIBS) \
	  $(CMX) \
	  $(filter-out \
	    $(patsubst $(PLUGIN_GUI_LIB_DIR)/%, $(PLUGIN_LIB_DIR)/%, \
	      $(PLUGIN_GUI_CMX_LIST)), \
	    $(PLUGIN_CMX_LIST)) \
	  $(GUICMX) $(STARTUP_CMX)
endif
endif

#########################
# Standalone obfuscator #
#########################

obfuscator: bin/obfuscator.$(OCAMLBEST)

bin/obfuscator.byte$(EXE): $(ACMO) $(KERNEL_CMO) $(STARTUP_CMO) $(GEN_BYTE_LIBS)
	$(PRINT_LINKING) $@
	$(OCAMLC) $(BLINKFLAGS) -o $@ $(BYTE_LIBS) $^

bin/obfuscator.opt$(EXE): $(ACMX) $(KERNEL_CMX) $(STARTUP_CMX) $(GEN_OPT_LIBS)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) $(OLINKFLAGS) -o $@ $(OPT_LIBS) $^

#####################
# Config Ocaml File #
#####################

CONFIG_DIR=src/kernel
CONFIG_FILE=$(CONFIG_DIR)/config.ml
CONFIG_CMO=$(CONFIG_DIR)/config.cmo
GENERATED +=$(CONFIG_FILE)

empty:=
space:=$(empty) $(empty)

ifeq ($(ENABLE_GUI),no)
CONFIG_CMO=$(ALL_CMO)
CONFIG_PLUGIN_CMO=$(PLUGIN_CMO_LIST)
else
CONFIG_CMO=$(ALL_GUI_CMO)
CONFIG_PLUGIN_CMO=$(PLUGIN_GUI_CMO_LIST)
endif

$(CONFIG_FILE): VERSION share/Makefile.config Makefile
	$(PRINT_MAKING) $@
	$(RM) $@
	$(ECHO) "(* This file is generated by Makefile. Do not modify. *)" \
		> $@
	$(ECHO) "let version = \""$(VERSION)"\"" >> $@
	$(ECHO) "let date = \""`LC_ALL=C date`"\"" >> $@
	$(ECHO) "let is_gui = ref false" >> $@
	$(ECHO) "let ocamlc = \""$(OCAMLC)"\"" >> $@
	$(ECHO) "let ocamlopt = \""$(OCAMLOPT)"\"" >> $@
	$(ECHO) "let datadir = try Sys.getenv \"FRAMAC_SHARE\" with Not_found -> \"$(FRAMAC_DATADIR)\"" >> $@
	$(ECHO) "let () = Filepath.add_symbolic_dir \"FRAMAC_SHARE\" datadir" \
	 >> $@
	$(ECHO) "let libdir = try Sys.getenv \"FRAMAC_LIB\" with Not_found -> \"$(FRAMAC_LIBDIR)\"" >> $@
	$(ECHO) "let () = Filepath.add_symbolic_dir \"FRAMAC_LIB\" libdir" \
	>> $@
	$(ECHO) "let plugin_dir = try Sys.getenv \"FRAMAC_PLUGIN\" with Not_found -> try (Sys.getenv \"FRAMAC_LIB\") ^ \"/plugins\" with Not_found -> \"$(FRAMAC_PLUGINDIR)\"" >> $@
	$(ECHO) "let () = Filepath.add_symbolic_dir \"FRAMAC_PLUGIN\" plugin_dir" >> $@
	$(ECHO) "let preprocessor = try Sys.getenv \"CPP\" with Not_found -> \"$(FRAMAC_DEFAULT_CPP)\"" >> $@
#       Suppose that if CPP is set, it has a meaningful value.
	$(ECHO) "let preprocessor_keep_comments = try (ignore (Sys.getenv \"CPP\"); true) with Not_found -> $(DEFAULT_CPP_KEEP_COMMENTS)" >> $@
	$(ECHO) "let static_plugins = [" \
	  $(subst $(space),"; ",$(foreach p,$(PLUGIN_LIST),\"$(notdir $p)\")) \
	  "]" >> $@
	$(ECHO) "let static_gui_plugins = [" \
	  $(subst $(space),"; ",$(foreach p,$(CONFIG_PLUGIN_CMO),\"$(notdir $(patsubst %.cmo,%,$p))\")) \
	  "]" >> $@
	$(ECHO) "let compilation_unit_names = [" \
	  $(subst $(space),"; ",$(foreach p,$(CONFIG_CMO),\"$(notdir $(patsubst %.cmo,%,$p))\")) \
	"]" >> $@
ifeq ($(HAS_DOT),yes)
	$(ECHO) "let dot = Some \"$(DOT)\"" >> $@
else
	$(ECHO) "let dot = None" >> $@
endif
	$(CHMOD_RO) $@

#########
# Tests #
#########

ifeq ($(PTESTSBEST),opt)
PTESTS_FILES=ptests_config.cmi ptests_config.cmx ptests_config.o
else
PTESTS_FILES=ptests_config.cmi ptests_config.cmo
endif

.PHONY: tests oracles btests tests_dist libc_tests
tests:: byte opt ptests
	$(PRINT_EXEC) ptests
	time -p ./bin/ptests.$(PTESTSBEST)$(EXE) $(PTESTS_OPTS) \
                   -make "$(MAKE)" $(PLUGIN_TESTS_LIST)
	$(MAKE) external_tests

external_tests: byte opt ptests
	for plugin in $(EXTERNAL_PLUGINS); do \
	  if $(call external_make,$$plugin,run_tests) 2> /dev/null; \
	  then \
	      $(call external_make,$$plugin,tests); \
	  fi \
	done

update_external_tests:
	for plugin in $(EXTERNAL_PLUGINS); do \
	  if $(call external_make,$$plugin,run_tests) 2> /dev/null; \
	  then \
	      PTESTS_OPTS="-update" $(call external_make,$$plugin,tests); \
	  fi \
	done

oracles: byte opt ptests
	$(PRINT_MAKING) oracles
	./bin/ptests.$(PTESTSBEST)$(EXE) -make "$(MAKE)" $(PLUGIN_TESTS_LIST) \
		> /dev/null 2>&1
	./bin/ptests.$(PTESTSBEST)$(EXE) -make "$(MAKE)" -update \
		$(PLUGIN_TESTS_LIST)

btests: byte ./bin/ptests.byte$(EXE)
	$(PRINT_EXEC) ptests -byte
	time -p ./bin/ptests.byte$(EXE) -make "$(MAKE)" -byte \
		$(PLUGIN_TESTS_LIST)

tests_dist: dist ptests
	$(PRINT_EXEC) ptests
	time -p ./bin/ptests.$(PTESTSBEST)$(EXE) -make "$(MAKE)" \
		$(PLUGIN_TESTS_LIST)

# test only one test suite : make suite_tests
%_tests: opt ptests
	$(PRINT_EXEC) ptests
	./bin/ptests.$(PTESTSBEST)$(EXE) -make "$(MAKE)" $($*_TESTS_OPTS) $*

# full test suite
wp_TESTS_OPTS=-j 1
fulltests: tests wp_tests

acsl_tests: byte
	$(PRINT_EXEC) acsl_tests
	find doc/speclang -name \*.c -exec ./bin/toplevel.byte$(EXE) {} \; > /dev/null

# Non-plugin test directories containing some ML files to compile
TEST_DIRS_AS_PLUGIN=dynamic dynamic_plugin journal saveload spec misc syntax
PLUGIN_TESTS_LIST += $(TEST_DIRS_AS_PLUGIN)
$(foreach d,$(TEST_DIRS_AS_PLUGIN),$(eval $(call COMPILE_TESTS_ML_FILES,$d,,)))

# Testing of dynamic plug-ins
#############################

tests/dynamic/.cmi tests/dynamic/empty.cmifoo:tests/dynamic/empty.cmi
	$(CP) $< $@

tests/dynamic/.cmo tests/dynamic/empty.cmofoo:tests/dynamic/empty.cmo \
			tests/dynamic/.cmi tests/dynamic/empty.cmifoo
	$(CP) $< $@

tests/dynamic/Register_mod1.cmo:tests/dynamic_plugin/register_mod1.cmo
	$(OCAMLC) -o $@ -pack $^

tests/dynamic/Register_mod2.cmo:tests/dynamic_plugin/register_mod2.cmo
	$(OCAMLC) -o $@ -pack $^

tests/dynamic/Apply.cmo:tests/dynamic_plugin/apply.cmo
	$(OCAMLC) -o $@ -pack $^

DYNAMIC_TESTS_TARGETS=tests/dynamic/empty.cmo tests/dynamic/empty_gui.cmo \
	tests/dynamic/.cmo tests/dynamic/empty.cmofoo \
	tests/dynamic/Register_mod1.cmo tests/dynamic/Register_mod2.cmo \
	tests/dynamic/Apply.cmo \
	tests/dynamic/abstract.cmo tests/dynamic/abstract2.cmo

.PHONY:tests/dynamic/all
tests/dynamic/all:
	$(QUIET_MAKE) $(DYNAMIC_TESTS_TARGETS)

##############
# Emacs tags #
##############

.PHONY: tags
# otags gives a better tagging of ocaml files than etags
ifdef OTAGS
tags:
	$(OTAGS) -r external src lib cil
vtags:
	$(OTAGS) -vi -r external src lib cil
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

.PHONY: wc doc doc-distrib

wc:
	ocamlwc -p external/*.ml* cil/*/*.ml cil/*/*.ml[ily] cil/src/*/*.ml[ily] cil/src/*/*.ml[ly] src/*/*.ml src/*/*.ml[iyl]

# private targets, useful for recompiling the doc without dependencies
# (too long!)
.PHONY: doc-kernel doc-index plugins-doc doc-update doc-tgz

DOC_DEPEND=$(MODULES_TODOC) bin/toplevel.byte$(EXE) $(DOC_PLUGIN)
ifneq ($(ENABLE_GUI),no)
DOC_DEPEND+=bin/viewer.byte$(EXE)
endif

GENERATED+=$(DOC_DIR)/docgen.ml

ifeq ($(HAS_OCAML4),yes)
$(DOC_DIR)/docgen.ml: $(DOC_DIR)/docgen_ge400.ml share/Makefile.config Makefile
	$(RM) $@
	$(CP) $< $@
	$(CHMOD_RO) $@
else
$(DOC_DIR)/docgen.ml: $(DOC_DIR)/docgen_lt400.ml share/Makefile.config Makefile
	$(RM) $@
	$(CP) $< $@
	$(CHMOD_RO) $@
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
	$(RM) $(DOC_DIR)/docgen.cm* $(DOC_DIR)/docgen.ml

DOC_NOT_FOR_DISTRIB=yes
plugins-doc:
	$(QUIET_MAKE) \
	 $(if $(DOC_NOT_FOR_DISTRIB), $(PLUGIN_DOC_LIST), \
	   $(filter \
	     $(addsuffix _DOC, $(PLUGIN_DISTRIBUTED_NAME_LIST)), \
	     $(PLUGIN_DOC_LIST)))

# to make the documentation for one plugin only,
# the name of the plugin should begin with a capital letter :
# Example for the pdg doc : make Pdg_DOC
# While working on the documentation of a plugin, it can also be useful
# to use : make -o doc/code/kernel-doc.ocamldoc Plugin_DOC
# to avoid redoing the global documentation each time.

STDLIB_FILES:=\
	array \
	big_int \
	buffer \
	char \
	format \
	hashtbl \
	int64 \
	list \
	map \
	marshal \
	obj \
	pervasives \
	printf \
	queue \
	scanf \
	set \
	stack \
	string \
	sys

STDLIB_FILES:=$(patsubst %, $(OCAMLLIB)/%.mli, $(STDLIB_FILES))

.PHONY: doc-kernel
doc-kernel: $(DOC_DIR)/kernel-doc.ocamldoc

$(DOC_DIR)/kernel-doc.ocamldoc: $(DOC_DEPEND)
	$(PRINT_DOC) Kernel Documentation
	$(MKDIR) $(DOC_DIR)/html
	$(RM) $(DOC_DIR)/html/*.html
	$(OCAMLDOC) $(DOC_FLAGS) -I $(OCAMLLIB) \
	  $(addprefix -stdlib , $(STDLIB_FILES)) \
	  -t "Frama-C Kernel" \
	  -sort -css-style ../style.css \
	  -g $(DOC_PLUGIN) \
	  -d $(DOC_DIR)/html -dump $(DOC_DIR)/kernel-doc.ocamldoc \
	  $(MODULES_TODOC)
DYN_MLI_DIR := doc/code/print_api

.PHONY: doc-dynamic
doc-dynamic: doc-kernel
	$(RM) $(DYN_MLI_DIR)/dynamic_plugins.mli
	$(call external_make,$(DYN_MLI_DIR),clean)
	$(call external_make,$(DYN_MLI_DIR),depend)
	$(call external_make,$(DYN_MLI_DIR),byte)
	FRAMAC_PLUGIN=lib/plugins FRAMAC_LIB=lib/fc FRAMAC_SHARE=share \
          ./bin/toplevel.byte -load-module Print_api \
            -print_api $(call winpath, $(FRAMAC_TOP_SRCDIR)/$(DYN_MLI_DIR))
	$(PRINT_DOC) Dynamically registered plugins Documentation
	$(MKDIR) $(DOC_DIR)/dynamic_plugins
	$(RM) $(DOC_DIR)/dynamic_plugins/*.html
	$(OCAMLDOC) $(DOC_FLAGS) -I $(FRAMAC_LIB) -I $(OCAMLLIB) \
	  -docpath $(DOC_DIR)/html \
	  -sort -css-style ../style.css \
	  -load $(DOC_DIR)/kernel-doc.ocamldoc \
	  -t " Dynamically registered plugins" \
	  -g $(DOC_PLUGIN) \
	  -d $(DOC_DIR)/dynamic_plugins \
	  $(DYN_MLI_DIR)/dynamic_plugins.mli
	$(ECHO) '<li><a href="dynamic_plugins/Dynamic_plugins.html">Dynamically registered plugins</a </li>' > $(DOC_DIR)/dynamic_plugins.toc

doc-index: #dependencies in doc target
	$(PRINT_MAKING) doc/code/index.html
	$(CAT)  $(DOC_DIR)/toc_head.htm $(DOC_DIR)/*.toc \
		$(DOC_DIR)/toc_tail.htm > $(DOC_DIR)/index.html

doc-update: doc-kernel doc-dynamic plugins-doc doc-index

doc:: $(DOC_DEPEND)
	$(QUIET_MAKE) doc-kernel doc-dynamic plugins-doc doc-index

doc-tgz:
	$(PRINT_MAKING) frama-c-api.tar.gz
	cd $(DOC_DIR); \
	  $(TAR) zcf tmp.tgz index.html *.txt \
	   $(notdir $(wildcard $(DOC_DIR)/*.css $(DOC_DIR)/*.png \
	                       $(DOC_DIR)/dynamic_plugins*)) \
	   html \
	   $(foreach p, $(PLUGIN_DISTRIBUTED_NAME_LIST), \
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

# Could be optimized
.PHONY: db_doc
db_doc doc/db/db.pdf: doc/db/main.tex doc/db/main.bib doc/db/db.tex
	$(PRINT_MAKING) doc/db/db.pdf
	cd $(dir $@); \
	  pdflatex $(notdir $<); bibtex main; \
	  pdflatex $(notdir $<); pdflatex $(notdir $<); \
	  mv main.pdf $(notdir $@)

#find src -name "*.ml[i]" -o -name "*.ml" -maxdepth 3 | sort -r | xargs
dots: $(ALL_CMO)
	$(PRINT_DOC) callgraph
	$(OCAMLDOC) $(INCLUDES) -o doc/call_graph.dot \
	  -dot -dot-include-all -dot-reduce $(MODULES_TODOC)
	$(QUIET_MAKE) doc/call_graph.svg
	$(QUIET_MAKE) doc/call_graph.ps

datatype_dependencies.dot computation_dependencies.dot: ./bin/toplevel.byte$(EXE)
	$(PRINT_MAKING) $@
	./bin/toplevel.byte$(EXE) -project-debug -dump \
	  > /dev/null 2> /dev/null

.PHONY:display_dependencies
display_dependencies: datatype_dependencies.svg computation_dependencies.svg
	inkscape datatype_dependencies.svg computation_dependencies.svg &

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

ifeq ("$(OCAMLDOC)","ocamldoc.opt")
CHECK_CODE=$(CHECK_API_DIR)/check_code.cmxs
else
CHECK_CODE=$(CHECK_API_DIR)/check_code.cmo
endif

.PHONY: check-devguide
check-devguide: $(CHECK_CODE) $(DOC_DEPEND) $(DOC_DIR)/kernel-doc.ocamldoc
	$(PRINT) 'Checking     developer guide consistency'
	$(MKDIR) $(CHECK_API_DIR)/html
	$(OCAMLDOC) $(DOC_FLAGS) -I $(OCAMLLIB) \
	  -docdevpath `pwd`/$(CHECK_API_DIR) \
	  -load $(DOC_DIR)/kernel-doc.ocamldoc \
	  -g $(CHECK_CODE) \
	  -d $(CHECK_API_DIR)/html
	$(RM) -r  $(CHECK_API_DIR)/html
	$(MAKE) --silent -C $(CHECK_API_DIR) main.idx
	$(MAKE) --silent -C $(CHECK_API_DIR) >$(CHECK_API_DIR)/summary.txt
	$(ECHO) see all the information displayed here \
	        in $(CHECK_API_DIR)/summary.txt
	$(RM) code_file

# Oug (M. Guesdon's tool: could probably be deleted)
#####

oug:
	echo $(ALL_CMX) $(STARTUP_CMX) > cmx.files
	rpl ".cmx" ".ml" cmx.files
	cp cmx.files ml0.files
	rpl ".ml" ".mli" cmx.files
	cat cmx.files >> ml0.files
	(ls -U `cat ml0.files` | grep -v sparecode > ml.files | true)
	cp ml.files files
	oug.x --debug 0 -I `ocamlc -where` $(INCLUDES) -I src/value -I src/pdg -I src/slicing -I src/security_slicing -I $(LABLGTK_PATH) --no-reduce --dump dump.oug `cat files`
	oug.x --load dump.oug --no-reduce --useless-elements useless.txt --aliases-used --print-loc --progress
	oug.x --load dump.oug --useless-elements useless-reduced.txt --aliases-used --print-loc --progress

metrics:
	$(PRINT) Computing metrics
	ocamlmetrics -max-mi 75 -worst-modules 10 -worst-functions 25 \
		$(filter-out $(GENERATED), $(patsubst cil/%,, $(patsubst lib/%,,$(ALL_CMO:.cmo=.ml)))) \
		> doc/metrics.html

################
# Installation #
################

FILTER_INTERFACE_DIRS:=lib/plugins src/gui

ifeq ("$(OCAMLGRAPH_LOCAL)","")
FILTER_INTERFACE_DIRS+= $(OCAMLGRAPH_HOME)
endif

.PHONY: install-kernel-byte install-kernel-opt install-gui-byte install-gui-opt

install-kernel-byte:
	$(PRINT_CP) bytecode kernel API
	$(MKDIR) $(FRAMAC_LIBDIR)
#       line below does not work if INCLUDES contains twice the same directory
#       Do not attempt to copy gui interfaces if gui is disabled
	$(CP) $(wildcard $(foreach d,$(filter-out $(FILTER_INTERFACE_DIRS),$(INCLUDES:-I%=%)), $(d)/*.cmi)) $(FRAMAC_LIBDIR)
	$(CP) $(ALL_BATCH_CMO) $(filter-out %.o, $(GEN_BYTE_LIBS:.cmo=.cmi)) \
		$(GEN_BYTE_LIBS) $(FRAMAC_LIBDIR)

install-kernel-opt:
	$(PRINT_CP) native kernel API
	$(CP) $(ALL_BATCH_CMX) \
	$(filter %.a,$(ALL_BATCH_CMX:.cmxa=.a)) \
	$(filter %.o,$(ALL_BATCH_CMX:.cmx=.o)) \
	 $(FRAMAC_LIBDIR)
	$(CP) $(filter-out %.o, $(GEN_OPT_LIBS)) \
	  $(filter %.o,$(GEN_OPT_LIBS:.cmx=.o)) $(FRAMAC_LIBDIR)

install-gui-byte:
	$(PRINT_CP) bytecode gui API
	$(MKDIR) $(FRAMAC_LIBDIR)
	if [ "$(ENABLE_GUI)" != "no" ]; then \
	  $(CP) $(SINGLE_GUI_CMI) $(SINGLE_GUI_CMO) $(FRAMAC_LIBDIR); \
	fi

install-gui-opt:
	$(PRINT_CP) native gui API
	$(MKDIR) $(FRAMAC_LIBDIR)
	if [ "$(ENABLE_GUI)" != "no" -a "$(OCAMLBEST)" = "opt" ]; then \
	  $(CP) $(SINGLE_GUI_CMX) $(SINGLE_GUI_CMX:.cmx=.o) $(FRAMAC_LIBDIR); \
	fi

install-doc-code:
	$(PRINT_CP) API documentation
	$(MKDIR) $(FRAMAC_DATADIR)/doc/code
	(cd doc ; tar cf - --exclude='.svn' --exclude='*.toc' \
			--exclude='*.htm' --exclude='*.txt' \
			--exclude='*.ml' \
			code \
		| (cd $(FRAMAC_DATADIR)/doc ; tar xf -))

.PHONY: install
install::
	$(PRINT_MAKING) destination directories
	$(MKDIR) $(BINDIR)
	$(MKDIR) $(MANDIR)/man1
	$(MKDIR) $(FRAMAC_PLUGINDIR)/gui
	$(MKDIR) $(FRAMAC_DATADIR)/feedback
	$(MKDIR) $(FRAMAC_DATADIR)/libc/sys
	$(MKDIR) $(FRAMAC_DATADIR)/libc/netinet
	$(MKDIR) $(FRAMAC_DATADIR)/libc/linux
	$(MKDIR) $(FRAMAC_DATADIR)/libc/net
	$(MKDIR) $(FRAMAC_DATADIR)/libc/arpa
	$(PRINT_CP) shared files
	$(CP) \
	  $(wildcard share/*.c share/*.h) share/acsl.el \
	  share/Makefile.dynamic share/Makefile.plugin share/Makefile.kernel \
	  share/Makefile.config share/Makefile.common share/Makefile.generic \
          share/configure.ac \
	  $(FRAMAC_DATADIR)
	$(CP) share/frama-c.rc $(ICONS) $(FRAMAC_DATADIR)
	$(CP) $(FEEDBACK_ICONS) $(FRAMAC_DATADIR)/feedback
	if [ -d $(EMACS_DATADIR) ]; then \
	  $(CP) share/acsl.el $(EMACS_DATADIR); \
	fi
	$(CP) share/Makefile.dynamic_config.external \
	      $(FRAMAC_DATADIR)/Makefile.dynamic_config
	$(PRINT_CP) C standard library
	$(CP) $(wildcard share/libc/*.c share/libc/*.i share/libc/*.h) \
	      $(FRAMAC_DATADIR)/libc
	$(CP) share/libc/sys/*.[ch] $(FRAMAC_DATADIR)/libc/sys
	$(CP) share/libc/arpa/*.[ch] $(FRAMAC_DATADIR)/libc/arpa
	$(CP) share/libc/net/*.[ch] $(FRAMAC_DATADIR)/libc/net
	$(CP) share/libc/netinet/*.[ch] $(FRAMAC_DATADIR)/libc/netinet
	$(CP) share/libc/linux/*.[ch] $(FRAMAC_DATADIR)/libc/linux
	$(PRINT_CP) binaries
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
	$(CP) bin/ptests.$(PTESTSBEST)$(EXE) \
              $(BINDIR)/ptests.$(PTESTSBEST)$(EXE)
	$(PRINT_CP) config files
	$(CP) $(addprefix ptests/,$(PTESTS_FILES)) $(FRAMAC_LIBDIR)
	$(PRINT_CP) manuals
	if [ -d doc/manuals ]; then \
	 $(MKDIR) $(FRAMAC_DATADIR)/manuals ; \
	 if [ -h doc/manuals/acsl.pdf ]; then \
	  $(CP_L) doc/manuals/*.pdf $(FRAMAC_DATADIR)/manuals ; \
	 else \
	  $(CP) doc/manuals/*.pdf $(FRAMAC_DATADIR)/manuals ; \
	 fi \
	fi
	$(PRINT_CP) API documentation
	$(MKDIR) $(FRAMAC_DATADIR)/doc/code
	$(CP) $(wildcard $(DOC_GEN_FILES)) $(FRAMAC_DATADIR)/doc/code
	$(PRINT_CP) dynamic plug-ins
	if [ -d "$(FRAMAC_PLUGIN)" -a "$(PLUGIN_DYN_EXISTS)" = "yes" ]; then \
	  $(CP)  $(patsubst %.cma,%.cmi,$(PLUGIN_DYN_CMO_LIST:%.cmo=%.cmi)) \
		 $(PLUGIN_DYN_CMO_LIST) $(PLUGIN_DYN_CMX_LIST) \
		 $(FRAMAC_PLUGINDIR); \
	fi
	$(PRINT_CP) dynamic gui plug-ins
	if [ -d "$(FRAMAC_PLUGIN_GUI)" -a "$(PLUGIN_DYN_GUI_EXISTS)" = "yes" ]; \
	then \
	  $(CP) $(patsubst %.cma,%.cmi,$(PLUGIN_DYN_GUI_CMO_LIST:.cmo=.cmi)) \
		$(PLUGIN_DYN_GUI_CMO_LIST) $(PLUGIN_DYN_GUI_CMX_LIST) \
		$(FRAMAC_PLUGINDIR)/gui; \
	fi
	$(MAKE) install-kernel-byte
	$(MAKE) install-gui-byte
	if [ "$(OCAMLBEST)" = "opt" ]; then \
	  $(MAKE) install-kernel-opt install-gui-opt; \
	fi
	$(PRINT_CP) man pages
	$(CP) man/frama-c.1 $(MANDIR)/man1/frama-c.1
	$(CP) man/frama-c.1 $(MANDIR)/man1/frama-c-gui.1

.PHONY: uninstall
uninstall::
	$(PRINT_RM) installed binaries
	$(RM) $(BINDIR)/frama-c* $(BINDIR)/ptests.$(PTESTSBEST)$(EXE)
	$(PRINT_RM) installed shared files
	$(RM) -R $(FRAMAC_DATADIR)
	$(PRINT_RM) installed libraries
	$(RM) -R $(FRAMAC_LIBDIR) $(FRAMAC_PLUGINDIR)
	$(PRINT_RM) installed man files
	$(RM) $(MANDIR)/man1/frama-c.1 $(MANDIR)/man1/frama-c-gui.1

################################
# File headers: license policy #
################################

# Modify this variable if you add a new header
HEADERS:= MODIFIED_OCAMLGRAPH MODIFIED_MENHIR CIL CEA_LGPL		\
	 CEA_PROPRIETARY CEA_INRIA_LGPL INRIA_LGPL			\
	 MODIFIED_CAMLLIB INSA_INRIA_LGPL INRIA_BSD ACSL_EL JCF_LGPL	\
	 OCAML_STDLIB AORAI_LGPL CEA_WP MODIFIED_WHY3 UNMODIFIED_WHY3

PROPRIETARY_HEADERS = CEA_PROPRIETARY

# Kernel licences
#################

CIL	= cil/ocamlutil/*.ml* \
	cil/src/*.ml* \
	cil/src/ext/*.ml* \
	cil/src/frontc/*.ml*

CEA_INRIA_LGPL	= configure.in cil/src/logic/*.ml*

MODIFIED_WHY3+=external/sysutil.ml*
MODIFIED_OCAMLGRAPH=src/project/state_topological.ml*
MODIFIED_MENHIR=external/hptmap.ml*
OCAML_STDLIB=src/lib/rangemap.ml src/lib/rangemap.mli \
	src/lib/FCSet.mli src/lib/FCSet.ml src/lib/FCMap.mli src/lib/FCMap.ml
INRIA_LGPL=
INRIA_BSD= external/unmarshal*.ml*
INSA_INRIA_LGPL=

CEA_LGPL= Makefile \
	share/Makefile.config.in share/Makefile.common share/Makefile.generic \
	share/Makefile.plugin share/Makefile.dynamic \
	share/Makefile.dynamic_config.internal \
	share/Makefile.dynamic_config.external \
	share/configure.ac configure.ml \
	config.h.in \
	src/report/*.ml* src/report/configure.ac src/report/Makefile.in \
	share/frama-c.WIN32.rc share/frama-c.Unix.rc \
	external/unz.ml* \
	src/ai/*.ml* \
	src/buckx/*.ml* src/buckx/*.[cS] \
	src/constant_propagation/*.ml* \
	src/from/*.ml* \
	src/gui/*.ml* \
	src/inout/*.ml* \
	src/pdg_types/*.ml* src/pdg/*.ml* doc/code/intro_pdg.txt \
	src/slicing_types/*.ml* src/slicing/*.ml* doc/code/intro_slicing.txt \
	src/scope/*.ml* doc/code/intro_scope.txt \
	src/sparecode/*.ml* doc/code/intro_sparecode.txt \
	src/impact/*.ml* \
	src/kernel/*.ml* \
	src/printer/*.ml* \
	src/lib/*.ml* \
	src/logic/*.ml* \
	src/memory_state/*.ml* \
	src/metrics/*.ml* \
	src/misc/*.ml* \
	src/obfuscator/*.ml* src/obfuscator/configure.ac \
	src/obfuscator/Makefile.in \
	src/occurrence/*.ml* doc/code/intro_occurrence.txt \
	src/postdominators/*.ml* \
	$(patsubst %.cmo, %.ml*, \
	  $(filter-out src/project/state_topological.cmo, $(PROJECT_CMO))) \
	src/project/project_skeleton.ml* \
	src/project/state.ml* \
	src/security_slicing/*.ml* \
	src/security_slicing/configure.ac src/security_slicing/Makefile.in \
	src/semantic_callgraph/*.ml* \
	src/syntactic_callgraph/*.ml* \
	src/toplevel/*.ml* \
	src/type/*.ml* \
	src/users/*.ml* \
	src/value/*.ml* \
	src/dummy/*/*.ml* \
	src/dummy/*/Makefile \
	src/rte/*.ml* \
	src/report/*.ml* \
	cil/src/frontc/cabs_debug.ml \
	ptests/*.ml* \
	doc/code/docgen_*.ml \
	doc/code/style.css \
	doc/code/intro_plugin.txt \
	doc/code/intro_plugin_default.txt \
	doc/code/intro_plugin_D_and_S.txt \
        doc/code/intro_kernel_plugin.txt \
	doc/code/toc_head.htm doc/code/toc_tail.htm \
	doc/code/print_api/*.ml* doc/code/print_api/Makefile \
	man/frama-c.1 \
	bin/lithium2beryllium.sh bin/boron2carbon.sh bin/carbon2nitrogen.sh \
	bin/nitrogen2oxygen.sh bin/oxygen2fluorine.sh bin/fluorine2neon.sh \
	$(FREE_LIBC)

CEA_PROPRIETARY:= \
	src/value/builtins_nonfree*.ml* \
	src/finder/*.ml* src/finder/configure.ac src/finder/Makefile.in \
	$(filter-out $(wildcard $(FREE_LIBC)), $(wildcard $(NONFREE_LIBC)))

ACSL_EL	:= share/acsl.el

# Plug-in specific licences
###########################

AORAI_LGPL:= src/aorai/*.ml* src/aorai/Makefile.in src/aorai/configure.ac

CEA_WP+=doc/code/intro_wp.txt

# Generating headers
####################

.PHONY: headers show_headers $(add_prefix show_,$(HEADERS))

headers:: $(GENERATED)
	@echo "Applying Headers..."
	$(foreach l,$(HEADERS),\
	$(foreach f,$(wildcard $($l)),$(shell if test -f $f; then \
	   LC_ALL=C $(HEADACHE) -h headers/$l $f; fi)))

show_headers: $(patsubst %,show_%,$(HEADERS))

show_%:	
	@echo "files under $(patsubst show_%,%,$@) licence:"
	@echo $($(patsubst show_%,%,$@))

NO_CHECK_HEADERS=tests/*/* doc/manuals/*.pdf \
		 doc/README cil/LICENSE cil/CHANGES Changelog .make* \
		 .force-reconfigure \
		 licenses/* VERSION INSTALL bin/sed* \
		 share/Makefile.kernel $(ICONS) $(FEEDBACK_ICONS) \
		 INSTALL_WITH_WHY

.PHONY: check-headers check-headers-xunit
check-headers: $(GENERATED)
	@echo "Checking Headers..."
	EXIT_VALUE=0; \
	$(foreach f,$(wildcard $(DISTRIB_FILES)),\
	  $(if $(findstring $(f),\
		  $(wildcard $(NO_CHECK_HEADERS)) \
		  $(foreach l,$(filter-out $(PROPRIETARY_HEADERS),$(HEADERS)),\
                              $(wildcard $($l)))),,\
	       EXIT_VALUE=1; \
	       $(if $(findstring $(f),\
                      $(foreach l, $(PROPRIETARY_HEADERS),$(wildcard $($l)))),\
                    echo "file $(f) has a proprietary license", \
                    echo "file $(f) does not have a license");)) \
	exit $$EXIT_VALUE

check-headers-xunit: $(GENERATED)
	@echo '<?xml version="1.0" encoding="UTF-8"?>' > check-headers-xunit.xml
	@echo '<testsuites>' >> check-headers-xunit.xml
	@TIME=`date +%Y-%m-%dT%T`; \
	echo "<testsuite name=\"headers\" package=\"headers\" " \
	     >> check-headers-xunit.xml; \
	echo "id=\"0\" timestamp=\"$$TIME\" hostname=\"`hostname`\" " \
	    >> check-headers-xunit.xml; \
	echo "time=\"0\" errors=\"0\" skipped=\"0\" SUMMARY>" \
	    >> check-headers-xunit.xml; \
	NB_HEADERS=0; NB_NO_LICENSE=0; \
	for f in $(wildcard $(DISTRIB_FILES)); do \
	   NB_HEADERS=$$(($$NB_HEADERS + 1)); \
	   echo "<testcase name=\"$$f\" classname=\"header\" time=\"0\"" \
	     >> check-headers-xunit.xml; \
	   if echo "$(wildcard $(NO_CHECK_HEADERS)) \
		       $(foreach l,\
                           $(filter-out $(PROPRIETARY_HEADERS),$(HEADERS)),\
                           $(wildcard $($l)))" | \
	       grep -q -e $$f; then \
	      echo '/>' >> check-headers-xunit.xml; \
	   else \
	      NB_NO_LICENSE=$$(($$NB_NO_LICENSE + 1)); \
	      echo '>' >> check-headers-xunit.xml; \
              if echo \
                  "$(foreach l, $(PROPRIETARY_HEADERS), $(wildcard $($l)))" | \
                 grep -q -e $$f; then \
                   MSG="file has proprietary header"; \
              else \
                   MSG="file has no header"; \
              fi; \
	      echo "<failure message=\"$$MSG\" type=\"header\"/>" \
	      >> check-headers-xunit.xml; \
              echo "</testcase>" >> check-headers-xunit.xml; \
	   fi; \
	done; \
	$(ISED) -e \
	   "s/SUMMARY/tests=\"$$NB_HEADERS\" failures=\"$$NB_NO_LICENSE\"/" \
	   check-headers-xunit.xml; \
	echo "</testsuite>" >> check-headers-xunit.xml; \
	echo "</testsuites>" >> check-headers-xunit.xml

########################################################################
# Makefile is rebuilt whenever Makefile.in or configure.in is modified #
########################################################################

share/Makefile.config: share/Makefile.config.in config.status
	$(PRINT_MAKING) $@
	./config.status

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
	rm -fr autom4te.conf
	autoconf
	touch configure

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
	$(MAKE) --silent -C $(DYN_MLI_DIR) clean
	if [ -f doc/developer/Makefile ]; then \
	  $(MAKE) --silent -C doc/developer clean; \
	fi
	if [ -f doc/architecture/Makefile ]; then \
	  $(MAKE) --silent -C doc/architecture clean; \
	fi
	if [ -f doc/speclang/Makefile ]; then \
	  $(MAKE)  --silent -C doc/speclang clean; \
	fi
	if [ -f doc/www/src/Makefile ]; then \
	  $(MAKE) --silent -C doc/www/src clean; \
	fi

clean-gui::
	$(PRINT_RM) gui
	$(RM) src/*/*_gui.cm* src/*/*_gui.o src/gui/*.cm* src/gui/*.o

clean:: $(PLUGIN_LIST:=_CLEAN) $(PLUGIN_DYN_LIST:=_CLEAN) \
		clean-tests clean-journal clean-check-libc
	$(PRINT_RM) $(PLUGIN_LIB_DIR)
	$(RM) $(PLUGIN_LIB_DIR)/*.mli $(PLUGIN_LIB_DIR)/*.cm* \
	  $(PLUGIN_LIB_DIR)/*.o
	$(RM) $(PLUGIN_GUI_LIB_DIR)/*.mli $(PLUGIN_GUI_LIB_DIR)/*.cm* \
	  $(PLUGIN_GUI_LIB_DIR)/*.o
	$(PRINT_RM) local installation
	$(RM) lib/*.cm* lib/*.o lib/fc/*.cm* lib/fc/*.o lib/gui/*.cm* lib/*.cm*
	$(PRINT_RM) other sources
	for d in . $(SRC_DIRS) src/gui share; do \
	  $(RM) $$d/*.cm* $$d/*.o $$d/*.a $$d/*.annot $$d/*~ $$d/*.output \
	   	$$d/*.annot $$d/\#*; \
	done
	$(PRINT_RM) generated files
	$(RM) $(GENERATED)
	$(RM) ptests_config.* # temporary clean-up of svn version
	$(PRINT_RM) binaries
	$(RM) bin/*.byte$(EXE) bin/*.opt$(EXE) bin/*.top$(EXE)

smartclean:
	$(MAKE) -f share/Makefile.clean smartclean

distclean-ocamlgraph:
	$(PRINT_RM) ocamlgraph
	if [ -f ocamlgraph/Makefile ]; then \
	  $(MAKE) --silent -C ocamlgraph distclean; \
	  cd ocamlgraph; ./configure; \
	fi

# Do NOT use :: for this rule: it is mandatory to remove share/Makefile.config
# as the very last step performed by make (who'll otherwise try to regenerate
# it in the middle of cleaning)
dist-clean distclean: clean clean-doc distclean-ocamlgraph distclean-mcpp \
	              $(PLUGIN_LIST:=_DIST_CLEAN) \
                      $(PLUGIN_DYN_LIST:=_DIST_CLEAN)
	$(PRINT_RM) config
	$(RM) share/Makefile.config
	$(RM) config.cache config.log config.h
	$(RM) -r autom4te.cache
	$(RM) src/lib/dynlink_common_interface.ml
	$(PRINT_RM) documentation
	$(RM) $(DOC_DIR)/docgen.ml $(DOC_DIR)/kernel-doc.ocamldoc
	$(PRINT_RM) dummy plug-ins
	$(RM) src/dummy/*/*.cm* src/dummy/*/*.o src/dummy/*/*.a \
		src/dummy/*/*.annot src/dummy/*/*~ src/dummy/*/*.output \
	   	src/dummy/*/*.annot src/dummy/*/\#*


ifeq ($(OCAMLWIN32),yes)
# Use Win32 typical ressources
share/frama-c.rc: share/frama-c.WIN32.rc
	$(PRINT_MAKING) $@
	$(CP) $^ $@
else
# Use Unix typical ressources
share/frama-c.rc: share/frama-c.Unix.rc
	$(PRINT_MAKING) $@
	$(CP) $^ $@
endif

GENERATED+=share/frama-c.rc

##########
# Depend #
##########

GENERATED+=ptests/ptests_config.ml
PLUGIN_DEP_LIST:=$(PLUGIN_LIST) $(PLUGIN_DYN_LIST)

.PHONY: depend

depend::  $(PLUGIN_DEP_LIST:%=%_DEP_REDO)

$(ALL_CMO:.cmo=.cmi) $(ALL_CMO) $(ALL_CMX): $(GRAPH_LIB) | .depend

GENERATED_FOR_OCAMLDEP:= $(filter-out $(GRAPH_LIB), $(GENERATED))

.depend depend:: $(GENERATED_FOR_OCAMLDEP) \
		 share/Makefile.dynamic_config share/Makefile.kernel \
	 	 $(PLUGIN_DEP_LIST:%=%_DEP)
	$(PRINT_MAKING) .depend
	$(CHMOD_RW) .depend
	if test "$(PLUGIN_DEP_LIST)" != " "; then \
	  $(CAT) $(foreach d, $(PLUGIN_DEP_LIST), $(dir $d).depend) \
	    > .depend; \
	else \
	  $(TOUCH) .depend; \
	fi
	$(OCAMLDEP) $(DEP_FLAGS) $(FILES_FOR_OCAMLDEP) >> .depend
	$(CHMOD_RO) .depend

include .depend

#####################
# ptest development #
#####################

.PHONY: ptests

PTESTS_SRC=ptests/ptests_config.ml ptests/ptests.ml

ifeq ($(NATIVE_THREADS),yes)
THREAD=-thread
ptests: bin/ptests.$(PTESTSBEST)$(EXE)
else
THREAD=-vmthread
ptests: bin/ptests.byte$(EXE)
endif

bin/ptests.byte$(EXE): $(PTESTS_SRC)
	$(PRINT_LINKING) $@
	$(OCAMLC) -I ptests -dtypes $(THREAD) -g -o $@ \
	    unix.cma threads.cma str.cma dynlink.cma $^

bin/ptests.opt$(EXE): $(PTESTS_SRC)
	$(PRINT_LINKING) $@
	$(OCAMLOPT) -I ptests -dtypes $(THREAD) -o $@ \
            unix.cmxa threads.cmxa str.cmxa dynlink.cmxa $^

#         "let default_suites = ref [" $(PLUGIN_TESTS_LIST:%='"%";') "];;" >> $@

ptests/ptests_config.ml: Makefile share/Makefile.config
	$(PRINT_MAKING) $@
	$(RM) $@
	  $(TOUCH) $@;
	$(ECHO) \
	 "let default_suites : string list ref = ref [" $(PLUGIN_TESTS_LIST:%='"%";') "];;" >> $@
	$(ECHO) \
	 "let no_native_dynlink = " \
	 $(subst yes,false,$(subst no,true,$(USABLE_NATIVE_DYNLINK))) ";;" \
	 >> $@
	$(ECHO) \
	"let toplevel_path = ref \"bin/toplevel.$(OCAMLBEST)$(EXE)\";;" >> $@
	$(ECHO) \
	"let framac_session = ref Filename.current_dir_name;;" >> $@
	$(ECHO) \
	"let framac_share = ref (Filename.concat Filename.current_dir_name \
\"share\");;" >> $@
	$(ECHO) \
	"let framac_plugin = ref \
(Filename.concat (Filename.concat Filename.current_dir_name \"lib\")\
 \"plugins\");;" >> $@
	$(ECHO) \
	"let framac_plugin_gui = ref \
(Filename.concat !framac_plugin \"gui\");;" >> $@
	$(ECHO) \
	"let framac_lib = ref \
(Filename.concat (Filename.concat Filename.current_dir_name \"lib\")\
\"fc\");;" >> $@
	$(CHMOD_RO) $@

GENERATED+=ptests/ptests_config.ml

#######################
# Source distribution #
#######################

STANDALONE_PLUGINS_FILES = \
	$(addprefix src/dummy/hello_world/, hello_world.ml Makefile) \
	$(addprefix src/dummy/untyped_metrics/, count_for.ml Makefile)

DISTRIB_FILES += $(PLUGIN_DISTRIBUTED_LIST) $(PLUGIN_DIST_EXTERNAL_LIST) \
		 $(PLUGIN_DIST_DOC_LIST) $(STANDALONE_PLUGINS_FILES)

EXPORT=frama-c-$(VERSION)

NONFREE=no
ifeq ($(NONFREE),no)
DISTRIB_FILES := $(filter-out src/value/builtins_nonfree%, \
			$(wildcard $(DISTRIB_FILES)))
else
DISTRIB_FILES:=$(DISTRIB_FILES) $(NONFREE_LIBC)
endif

src-distrib: src-distrib-ocamlgraph
	$(MAKE) clean
	$(PRINT_TAR) tmp-distrib
	$(TAR) chf tmp.tar --exclude="*/non-free/*" $(wildcard $(DISTRIB_FILES))
	$(PRINT_MAKING) export directories
	$(MKDIR) $(EXPORT)/bin
	$(MKDIR) $(EXPORT)/lib/plugins
	$(MKDIR) $(EXPORT)/lib/gui
	$(MKDIR) $(EXPORT)/external
	$(PRINT_UNTAR) tmp-distrib
	cd $(EXPORT); $(TAR) xf ../tmp.tar; autoconf; \
	  rm -rf autom4te.cache src/*/autom4te.cache
	$(PRINT_RM) tmp-distrib
	$(RM) tmp.tar
	$(PRINT_MAKING) test directories
	for dir in $(EXPORT)/tests/*; do \
	  $(MKDIR) $$dir/result; \
	  $(MKDIR) $$dir/oracle; \
	done
	$(PRINT_MAKING) archive
	$(TAR) czf frama-c-src.tar.gz $(EXPORT)
	$(PRINT) Cleaning
	$(RM) -fr $(EXPORT)

bin-distrib: depend configure Makefile
	$(PRINT_MAKING) bin-distrib
	$(RM) -r $(VERSION)
	./configure $(CONFIG_DISTRIB_BIN)
	$(QUIET_MAKE) DESTDIR=$(FRAMAC_SRC)/$(VERSION) install
	$(CP) README $(VERSION)

clean-distrib: dist-clean
	$(PRINT_RM) distrib
	$(RM) -r $(EXPORT) $(EXPORT).tar.gz

ifeq ($(OCAMLGRAPH_LOCAL),"")
src-distrib-ocamlgraph:
	$(PRINT_MAKING) distrib-ocamlgraph
	@ $(ECHO) "Cannot make distrib tar ball without local ocamlgraph installation"
	@ exit 2
else
src-distrib-ocamlgraph:
	$(PRINT_MAKING) distrib-ocamlgraph
	$(MKDIR) $(EXPORT)
	$(CP) ocamlgraph.tar.gz $(EXPORT)
endif

# Compiling Frama-C's mcpp

# force "make mcpp" to be executed for all SVN users
force-mcpp:
	expr `$(CAT) .make-mcpp-stamp` + 1 > .make-mcpp-stamp

bin/frama-c-mcpp$(EXE): .make-mcpp-stamp
	$(PRINT_MAKING) Frama-C\'s mcpp
	$(QUIET_MAKE) -C mcpp
	$(CP) mcpp/src/mcpp$(EXE) $@

distclean-mcpp:
	if test -f mcpp/Makefile ; then $(QUIET_MAKE) -C mcpp clean ; fi

ifneq ($(FC_MCPP),no)
all:: bin/frama-c-mcpp$(EXE)
install::
	$(MKDIR) $(BINDIR)
	$(PRINT_CP) frama-c-mcpp$(EXE)
	$(CP) bin/frama-c-mcpp$(EXE) $(BINDIR)
uninstall::
	$(PRINT_RM) frama-c-mcpp$(EXE)
	$(RM)  $(BINDIR)/frama-c-mcpp$(EXE)  
endif

include share/Makefile.generic

###############################################################################
# Local Variables:
# compile-command: "LC_ALL=C make"
# End:
