##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2009                                               #
#    CEA   (Commissariat à l'Énergie Atomique)                           #
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
#  See the GNU Lesser General Public License version v2.1                #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

FD=../..
WD=$(FD)/tests/more_wp
WHYDIR=$(FD)/why

EXEC=$(FD)/bin/toplevel.opt
CMD=FRAMAC_SHARE=$(FD)/share WHYLIB=$(WHYDIR)/lib WHYBIN=$(WHYDIR)/bin/why.opt WHYDP=$(WHYDIR)/bin/why-dp.opt $(EXEC) -journal-disable -wp-verbose 1 -wp-no-bot -wp-proof
#CMD=FRAMAC_SHARE=$(FD)/share $(EXEC) -journal-disable -wp-verbose 1 

all : res_0 res_2 

.PHONY : all 

JD=$(FD)/tests/jessie
JFILES=$(wildcard $(JD)/*.c)

WP_FILES=$(JFILES:$(JD)/%.c=$(WD)/%.X)
STATUS=valid invalid unknown timeout failure
RES_FILES=$(STATUS:%=%.X) failed.X errors.X

LOG_0=$(WP_FILES:%.X=%.0.log)
ERR_0=$(WP_FILES:%.X=%.0.err)
RES_0 = $(RES_FILES:%.X=%.0)
.PRECIOUS :  $(LOG_0) $(ERR_0)
.PHONY : RES_0 

LOG_0 : $(LOG_0)
ERR_0 : $(ERR_0)
RES_0 : $(RES_0)

LOG_2=$(WP_FILES:%.X=%.2.log)
ERR_2=$(WP_FILES:%.X=%.2.err)
RES_2 = $(RES_FILES:%.X=%.2)
.PRECIOUS :  $(LOG_2) $(ERR_2)
.PHONY : RES_2

LOG_2 : $(LOG_2)
ERR_2 : $(ERR_2)
RES_2 : $(RES_2)

LOG_0 ERR_0 LOG_2 ERR_2 :
	# we need to have a way to store the timestamp
	touch $@

$(WD)/%.0.log $(WD)/%.0.err : $(JD)/%.c $(EXEC)
	-$(CMD) -wp-mm 0 $< > $(WD)/$*.0.log 2> $(WD)/$*.0.err

$(WD)/%.2.log $(WD)/%.2.err : $(JD)/%.c $(EXEC) $(WD)/%.2.no
	@echo "Don't run test M2 for $<"
	touch $@

$(WD)/%.2.log $(WD)/%.2.err : $(JD)/%.c $(EXEC)
	-$(CMD) -wp-mm 2 $< > $(WD)/$*.2.log 2> $(WD)/$*.2.err

errors.% : ERR_%
	-grep Fatal $($<) > $@
	-grep Unbound $($<) >> $@
	-grep "Could not run why" $($<) >> $@
	-grep "Could not run why" $(LOG_$*) >> $@
	echo "TOTAL errors = `wc -l $@`" >> $@

failed.% : LOG_%
	-grep Failed $($<) > $@
	echo "TOTAL failed = `wc -l $@`" >> $@

%.0 : $(LOG_0)
	-grep $* $+ | grep "%" | grep -v " 0%" > $@
	echo "TOTAL $* = \
	  `gawk 'BEGIN { cpt = 0; } { cpt += $$3; } END { print cpt; }' \
	  $@`" >> $@

%.2 : $(LOG_2)
	-grep $* $+ | grep "%" | grep -v " 0%" > $@
	echo "TOTAL $* = \
	  `gawk 'BEGIN { cpt = 0; } { cpt += $$3; } END { print cpt; }' \
	  $@`" >> $@

res_% : RES_%
	@echo "=== Results for model $* :"
	grep "TOTAL" $($<) > $@
	cat $@

failed.%.m : failed.% Makefile
	cat $< \
	  | grep -v " [cfg] switch handling" \
	  | grep -v " char constant expr" \
	  | grep -v " string constant expr" \
	  | grep -v " cast" \
	  | grep -v " logic function with labels" \
	  | grep -v " predicate with label" \
	  | grep -v " fol term range" \
	  | grep -v " unsupported C or logic type" \
	  > $@


# identification of "normal" (or known) failed cases
f0 : failed.0.m
	-cat $< \
	  | grep -v ": indirect assign" \
	  | grep -v ": indirect access" \
	  | grep -v ": no assigns clause in called function" \

f2 : failed.2.m
	-cat $< \
	  | grep -v ": pointer to logic_type not implemented" \

clean :
	rm -f $(LOG_0) $(ERR_0) $(RES_0) LOG_0 ERR_0 RES_0
	rm -f $(LOG_2) $(ERR_2) $(RES_2) LOG_2 ERR_2 RES_2
	rm -f frama_c_journal.ml
	rm -f failed.*.m res_0 res_2
	rm -f gwhy.cache

# .SILENT :

