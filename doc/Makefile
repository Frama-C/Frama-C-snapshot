##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2015                                               #
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

# This file just download the manuals in the released version

all:download


FRAMAC_SRC=..
MAKECONFIG_DIR=$(FRAMAC_SRC)/share

include $(FRAMAC_SRC)/share/Makefile.common

DOCDIR ?= "$(DESTDIR)${prefix}/share/doc"
FRAMAC_DOCDIR ?= $(DOCDIR)/frama-c

###################
# Frama-C Version #
###################

VERSION=$(shell $(SED) -e 's/\\(.*\\)/\\1/' $(FRAMAC_SRC)/VERSION)

ifeq ($(findstring +dev,$(VERSION)),+dev)
DEVELOPMENT=yes
else
DEVELOPMENT=no
endif

###

.PHONY: force


MANUALS=acsl

#MANUALS that depend on the frama-c version
VERSIONED_MANUALS=acsl-implementation aorai-manual rte-manual	\
		  wp-manual metrics-manual user-manual		\
		  plugin-development-guide value-analysis

FILES= $(addprefix manuals/, \
	 $(addsuffix -$(VERSION).pdf, $(VERSIONED_MANUALS)) \
         $(addsuffix .pdf, $(MANUALS)) \
	)


ifeq ($(DEVELOPMENT),yes)

download: force
	@echo "You can't download the manuals in this way for the development version"

else

download: force
	$(MKDIR) manuals
	wget -nv -N -P manuals \
	  $(addprefix http://frama-c.com/download/,$(addsuffix -$(VERSION).pdf, $(VERSIONED_MANUALS))) \
	  $(addprefix http://frama-c.com/download/,$(addsuffix .pdf, $(MANUALS)))

endif

install:
	$(MKDIR) $(FRAMAC_DOCDIR)
	$(CP) $(FILES) $(FRAMAC_DOCDIR)
