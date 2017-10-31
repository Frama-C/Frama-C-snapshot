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

#######################
# Frama-C Environment #
#######################

ifndef FRAMAC_SHARE
FRAMAC_SHARE	:= $(shell frama-c-config -print-share-path)
endif

ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR	:= $(shell frama-c-config -print-libpath)
endif

#########################
# Plug-in configuration #
#########################

PLUGIN_DIR ?=.
PLUGIN_NAME:=Studia
PLUGIN_CMO:=options writes reads
PLUGIN_GUI_CMO:=studia_gui
PLUGIN_DEPENDENCIES:=Value
PLUGIN_DISTRIB_EXTERNAL:= Makefile

################
# Generic part #
################

include $(FRAMAC_SHARE)/Makefile.dynamic
