##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2017                                               #
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

# This file is intended to be included by a classic Makefile when doing
# non-trivial analyses with Frama-C and its EVA plugin. For instance, you
# can start your Makefile with the following line:
#
# include path/to/frama-c.mk
#
# This Makefile uses the following variables.
#
# FRAMAC        the frama-c binary
# FRAMAC_GUI    the frama-c gui binary
# CPPFLAGS      preprocessing flags
# FCFLAGS       general flags to use with frama-c
# FCGUIFLAGS    flags to use with frama-c-gui
# EVAFLAGS      flags to use with the EVA plugin
# SLEVEL        the part of the frama-c command line concerning slevel
#               (you can use EVAFLAGS for this, if you don't intend
#               to use slevel-tweaker.sh)
# EVABUILTINS   EVA builtins to be set (via -val-builtin)
# EVAUSESPECS   EVA functions to be overridden by specs (-val-use-spec)
#
# FLAMEGRAPH    path to flamegraph.pl (github.com/brendangregg/FlameGraph)
#
# There are several ways to define or change these variables.
#
# With an environment variable:
#   export FRAMAC=~/bin/frama-c
#   make
#
# With command line arguments:
#   make FRAMAC=~/bin/frama-c
#
# In your Makefile, when you want to change a parameter for all analyses :
#   FCFLAGS += -verbose 2
#
# In your Makefile, for a single target :
#   target.eva: FCFLAGS += -main my_main
#
# In order to define an analysis target named target, you must in addition
# give the list of source files containing the code to be analyzed by adding
# them as dependencies of target.parse, a in
#
# target.parse: file1.c file2.c file3.c...
#

# Test if Makefile is > 4.0
ifneq (4.0,$(firstword $(sort $(MAKE_VERSION) 4.0)))
  $(error This Makefile requires Make >= 4.0 - available at http://ftp.gnu.org/gnu/make/)
endif

# Test if on a Mac (and therefore sed has fewer options)
UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
  SED_UNBUFFERED:=sed
define time_with_output
  /usr/bin/time -p
endef
else
  SED_UNBUFFERED:=sed --unbuffered
define time_with_output
  /usr/bin/time --format='user_time=%U\nmemory=%M' --output="$(1)"
endef
endif

# If FLAMEGRAPH is not defined, try using one in the PATH
FLAMEGRAPH ?= $(shell command -v flamegraph.pl 2> /dev/null)

# --- Utilities ---

define display_command =
  $(info )
  $(info $(shell tput setaf 4)Command: $(1)$(shell tput sgr0))
  $(info )
endef

space :=
space +=
comma := ,

fc_list = $(subst $(space),$(comma),$(strip $1))


# --- Default configuration ---

FRAMAC     ?= frama-c
FRAMAC_GUI ?= frama-c-gui
SLEVEL     ?=
EVAFLAGS   ?= \
  -no-val-print -no-val-show-progress -value-msg-key=-initial-state \
  -val-print-callstacks -no-val-warn-on-alarms \
  -no-deps-print -no-calldeps-print \
  -memexec-all -calldeps -permissive -from-verbose 0 \
  $(SLEVEL) \
  $(if $(EVABUILTINS), -val-builtin=$(call fc_list,$(EVABUILTINS)),) \
  $(if $(EVAUSESPECS), -val-use-spec $(call fc_list,$(EVAUSESPECS)),)
FCFLAGS    ?=
FCGUIFLAGS ?=

export LIBOVERLAY_SCROLLBAR=0


# --- Cleaning ---

.PHONY: clean
clean::
	$(RM) -r *.parse *.eva

clean-backups:
	find . -regextype posix-extended \
	  -regex '^.*_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}\.eva(\.(log|stats|alarms|warnings|metrics))?' \
	  -delete


# --- Generic rules ---

TIMESTAMP    := $(shell date +"%Y-%m-%d_%H-%M-%S")
HR_TIMESTAMP := $(shell date +"%H:%M:%S %d/%m/%Y")# Human readable
DIR          := $(dir $(lastword $(MAKEFILE_LIST)))
SHELL        := /bin/bash
.SHELLFLAGS  := -eu -o pipefail -c

.ONESHELL:
.SECONDEXPANSION:
.FORCE:
.SUFFIXES: # Disable make builtins

%.parse/command %.eva/command:
	@#

%.parse: SOURCES = $(filter-out %/command,$^)
%.parse: PARSE = $(FRAMAC) $(FCFLAGS) -cpp-extra-args="$(CPPFLAGS)" $(SOURCES)
%.parse: $$(if $$^,,.IMPOSSIBLE) $$(shell $(DIR)cmd-dep.sh $$@/command $$(PARSE))
	@$(call display_command,$(PARSE))
	mkdir -p $@
	mv -f $@/{command,running}
	{
	  $(call time_with_output,$@/stats.txt) \
	    $(PARSE) \
	      -kernel-log w:$@/warnings.log \
	      -variadic-log w:$@/warnings.log \
	      -save $@/framac.sav \
	      -print -ocode $@/framac.ast -then -no-print \
	    || ($(RM) $@/stats.txt && false) # Prevents having error code reporting in stats.txt
	} 2>&1 |
	  tee $@/parse.log
	{
	  printf 'timestamp=%q\n' "$(HR_TIMESTAMP)";
	  printf 'warnings=%s\n' "`cat $@/warnings.log | grep ':\[kernel\]' | wc -l`";
	  printf 'cmd_args=%q\n' "$(subst ",\",$(wordlist 2,999,$(PARSE)))"
	} >> $@/stats.txt
	mv $@/{running,command}
	touch $@ # Update timestamp and prevents remake if nothing changes

%.slevel.eva: SLEVEL = -slevel $(word 2,$(subst ., ,$*))
%.eva: EVA = $(FRAMAC) $(FCFLAGS) -val $(EVAFLAGS)
%.eva: PARSE_RESULT = $(word 1,$(subst ., ,$*)).parse
%.eva: $$(PARSE_RESULT) $$(shell $(DIR)cmd-dep.sh $$@/command $$(EVA)) $(if $(BENCHMARK),.FORCE,)
	@$(call display_command,$(EVA))
	mkdir -p $@
	mv -f $@/{command,running}
	{
	  $(call time_with_output,$@/stats.txt) \
	    $(EVA) \
	      -load $(PARSE_RESULT)/framac.sav -save $@/framac.sav \
	      -val-flamegraph $@/flamegraph.txt \
	      -report-csv $@/alarms.csv -report-no-proven \
	      -kernel-log w:$@/warnings.log \
	      -from-log w:$@/warnings.log \
	      -inout-log w:$@/warnings.log \
	      -report-log w:$@/warnings.log \
	      -scope-log w:$@/warnings.log \
	      -value-log w:$@/warnings.log \
	      -metrics-log a:$@/metrics.log \
	      -metrics-value-cover \
	    || ($(RM) $@/stats.txt && false) # Prevents having error code reporting in stats.txt
	} 2>&1 |
	  $(SED_UNBUFFERED) '/\[value\] Values at end of function/,999999d' |
	  tee $@/eva.log
	$(DIR)parse-coverage.sh $@/eva.log $@/stats.txt
	{
	  printf 'timestamp=%q\n' "$(HR_TIMESTAMP)";
	  printf 'warnings=%s\n' "`cat $@/warnings.log | grep ':\[\(value\|kernel\|from\)\]' | wc -l`";
	  printf 'alarms=%s\n' "`expr $$(cat $@/alarms.csv | wc -l) - 1`";
	  printf 'cmd_args=%q\n' "$(subst ",\",$(wordlist 2,999,$(EVA)))"
	} >> $@/stats.txt
	mv $@/{running,command}
	touch $@ # Update timestamp and prevents remake if nothing changes
	cp -r $@ $*_$(TIMESTAMP).eva

%.gui: %
	$(FRAMAC_GUI) $(FCGUIFLAGS) -load $^/framac.sav &

# Run loop bound analysis plug-in and store result in *.loop
%.loop: %
	@
	{
	  $(FRAMAC) $(FCFLAGS) -load $^/framac.sav -loop -loop-no-branches |
	    sed -e '1,/Add this to your command line:/d'
	} > $@

# Produce an SVG from raw flamegraph data produced by EVA
%/flamegraph.svg: %/flamegraph.txt
ifneq ($(FLAMEGRAPH),)
	$(FLAMEGRAPH) $^ > $@
else
	$(error "FLAMEGRAPH not defined and flamegraph.pl not in the PATH")
endif


# clean is generally not the default goal, but if there is no default
# rule when including this file, it would be.

ifeq ($(.DEFAULT_GOAL),clean)
  .DEFAULT_GOAL :=
endif
