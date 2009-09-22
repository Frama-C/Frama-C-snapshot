##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2009                                               #
#    INSA  (Institut National des Sciences Appliquees)                   #
#    INRIA (Institut National de Recherche en Informatique et en         #
#           Automatique)                                                 #
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

# Makefile for compiling Aorai independently of Frama-C.
#
# To be used independently of Frama-C, a version of Frama-C compatible with
# Aorai has to be properly installed as long as the Aorai-specific stuff.

# Do not use ?= to initialize both below variables 
# (fixed efficiency issue, see GNU Make manual, Section 8.11)
ifndef FRAMAC_SHARE
FRAMAC_SHARE  :=$(shell frama-c -journal-disable -print-path)
endif
ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR :=$(shell frama-c -journal-disable -print-libpath)
endif
PLUGIN_DIR ?=.

PLUGIN_NAME:=Ltl_to_acsl
PLUGIN_GENERATED:= $(addprefix ${PLUGIN_DIR}/, \
		promelalexer_withexps.ml promelaparser_withexps.ml \
		promelaparser_withexps.mli \
		promelalexer.ml promelaparser.ml promelaparser.mli \
		ltllexer.ml ltlparser.ml ltlparser.mli \
		yalexer.ml yaparser.ml yaparser.mli)
PLUGIN_CMO:= bool3 \
	spec_tools \
	ltl_to_acsl_option \
	data_for_ltl \
	promelaoutput \
	ltl_logic \
	ltl_utils \
	ltl_output \
	utils_parser \
	ltlparser \
	ltllexer \
	yaparser \
	yalexer \
	promelaparser \
	promelalexer \
	promelaparser_withexps \
	promelalexer_withexps \
	abstract_ai \
	bycase_ai \
	ltl_to_acsl_visitors \
	ltl_to_acsl_register
PLUGIN_CMI:= ltlast \
	promelaast
PLUGIN_HAS_MLI:=yes
PLUGIN_DISTRIBUTED:=yes
PLUGIN_DISTRIB_BIN:=no
# if ltltoba is not present, do not attempt to run the tests by default.
ifneq ("$(HAS_LTLTOBA)","yes")
PLUGIN_NO_DEFAULT_TEST:=yes
endif

include $(FRAMAC_SHARE)/Makefile.dynamic


