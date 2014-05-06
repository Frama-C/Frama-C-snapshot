##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2014                                               #
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
# ---  QED : a prover / simplifier in classical logic modulo             ---
# --------------------------------------------------------------------------

PACK?=no

include Makefile.src

LIB_ML:=$(QED_LIB_ML)
SRC_ML:=$(QED_SRC_ML)
SRC_MLI:=$(QED_SRC_MLI)

LBO= $(addsuffix .cmo,$(LIB_ML))
LBX= $(addsuffix .cmx,$(LIB_ML))
CML= $(addsuffix .cmi,$(SRC_MLI))
CMO= $(addsuffix .cmo,$(SRC_ML))
CMX= $(addsuffix .cmx,$(SRC_ML))
CMI= $(addsuffix .cmi,$(LIB_ML) $(SRC_MLI) $(SRC_ML))
DOC= $(addsuffix .mli,$(LIB_ML) $(SRC_MLI) $(SRC_ML))

.PHONY: all byte opt doc dot

all:  byte opt
byte: ../bin/Qed.cmo Makefile Makefile.src
opt:  ../bin/Qed.cmx Makefile Makefile.src
doc:  ../html/index.html
dot:  depend.pdf

Qed.cmo: $(LBO) $(CMO) $(CML)
	$(OCAMLC) -pack -o $@ $(LBO) $(CML) $(CMO)

ifeq ($(PACK),no)
Qed.cmx: $(LBX) $(CMX) $(CML)
	$(OCAMLOPT) -pack -o $@ $(LBX) $(CML) $(CMX)
else
Qed.cmx: $(LBX) $(CMX) $(CML)
	$(OCAMLOPT) -pack -for-pack $(PACK) -o $@ $(LBX) $(CML) $(CMX)
endif

../bin/Qed.cmo: Qed.cmo
	@mkdir -p ../bin
	cp Qed.cmo Qed.cmi ../bin/

../bin/Qed.cmx: Qed.cmx
	@mkdir -p ../bin
	cp Qed.cmx Qed.cmi Qed.o ../bin/

../html/index.html: $(DOC) Makefile feedback.ml
	@rm -fr ../html
	@mkdir -p ../html
	$(OCAMLDOC) -v -html -d ../html -t "Qed Library" -stars $(DOC) | ocaml unix.cma feedback.ml
	@cp -f ocamldoc.css ../html/style.css

depend: numbers.ml

depend.pdf: depend.dot
	dot -T pdf -Grotate=180 -o $@ $<

depend.dot: .depend $(CMI)
	$(OCAMLDOC) -dot-reduce -o $@ -dot $(addsuffix .mli,$(SRC_MLI)) $(addsuffix .ml,$(LIB_ML) $(SRC_ML))

clean::
	rm -fr ../html
	rm -f depend.dot depend.pdf

distclean::
	rm -f ../bin/Qed.*

INCLUDES=
OPTCMI=
OPTCMO= -annot -w PSUZ+7 -warn-error PSUZ+7
ifeq ($(PACK),no)
OPTCMX= -annot -w PSUZ+7 -warn-error PSUZ+7 -for-pack Qed
else
OPTCMX= -annot -w PSUZ+7 -warn-error PSUZ+7 -for-pack $(PACK).Qed
endif

include MakeOcaml
