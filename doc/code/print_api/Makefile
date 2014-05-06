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

# Print_api plugin
##################

# Frama-C should be properly installed with "make install"
# befor any use of this makefile

ifndef FRAMAC_SHARE
FRAMAC_SHARE	:=$(shell frama-c.byte -print-path)
endif
ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR	:=$(shell frama-c.byte -print-libpath)
endif

PLUGIN_DIR	?= .
PLUGIN_NAME	:= Print_api
PLUGIN_CMO	:= grammar lexer print_interface
PLUGIN_GENERATED:= $(PLUGIN_DIR)/grammar.ml $(PLUGIN_DIR)/grammar.mli \
		$(PLUGIN_DIR)/lexer.ml

include $(FRAMAC_SHARE)/Makefile.dynamic

clean:: 
	$(RM) $(Print_api_DIR)/dynamic_plugins.mli
	$(RM) $(Print_api_DIR)/grammar.output
	$(RM) $(Print_api_DIR)/grammar.ml
	$(RM) $(Print_api_DIR)/grammar.mli
	$(RM) $(Print_api_DIR)/lexer.ml
