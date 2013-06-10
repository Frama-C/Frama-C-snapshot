##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2013                                               #
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
# ---  Shared Resources for WP 0.7                                       ---
# --------------------------------------------------------------------------

.PHONY: all coq depend

all: coq

# --------------------------------------------------------------------------
# ---  Coq Libraries for WP                                              ---
# --------------------------------------------------------------------------

COQL= Qedlib Cint Bits Cbits Cfloat Cmath Vset Memory 
COQSRC= $(addsuffix .v,$(COQL))
COQBIN= $(addsuffix .vo,$(COQL))
COQREF= $(addsuffix .glob,$(COQL))

coq: $(COQBIN)

# --------------------------------------------------------------------------

clean:
	rm -f *.vo *~ *.glob *.cm?
	rm -fr html/*

depend:
	coqdep -I . *.v > .depend

.SUFFIXES: .v .vo

.v.vo:
	coqc -I . $<

sinclude .depend
sinclude Makefile.wp # Wp developers only

# --------------------------------------------------------------------------
