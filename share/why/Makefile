# This Makefile build why model files because it seems that [why]
# looks for included files only in its installation directory.
# So, a workaround is to simulate inclusion.

# define those variables is they are not to be able to use make locally.
WP_LIB_DIR?=.
PRINT_MAKING?= echo 'Generating  '#
WHY_BUILD?=
WHYBIN?=why
WHYDPBIN?=why-dp

WP_LIB_BASE= types acsl model0 model1 model2 low-level
WP_LIB_FILES= $(WP_LIB_BASE:%=$(WP_LIB_DIR)/%.why)

WP_LIB_TESTS = $(WP_LIB_BASE:%=$(WP_LIB_DIR)/test_%.why)
WP_LIB_OK = $(WP_LIB_TESTS:%.why=%.ok)

wp_lib : $(WP_LIB_OK)

types_FILES=$(WP_LIB_DIR)/types.why
acsl_FILES=$(types_FILES) $(WP_LIB_DIR)/acsl.why
model0_FILES=$(acsl_FILES) $(WP_LIB_DIR)/model0.why
model1_FILES=$(model0_FILES) $(WP_LIB_DIR)/model1.why
model2_FILES=$(model1_FILES) $(WP_LIB_DIR)/model2.why
low-level_FILES=$(acsl_FILES) $(WP_LIB_DIR)/low-level.why



#==== TESTS :

$(WP_LIB_TESTS) : $(WP_LIB_FILES)

$(WP_LIB_DIR)/test_%.why : $(WP_LIB_DIR)/test_%.why.src
	$(PRINT_MAKING) $@
	@echo "(* Generated file : see $< *)" > $@
	cat $($*_FILES) >> $@
	cat $@.src >> $@

%_why.why : %.why $(WHY_BUILD)
	$(PRINT_MAKING) $@
	$(WHYBIN) --why $<

%.log : %_why.why
	$(PRINT_MAKING) $@
	$(WHYDPBIN) -no-timings $< > $@ 2>&1

.PRECIOUS : $(WP_LIB_TESTS) $(WP_LIB_OK:%.ok=%.oracle) $(WP_LIB_OK:%.ok=%.log)

%.ok : %.log %.oracle
	$(PRINT_MAKING) $@
	@if diff $*.oracle $*.log > /dev/null ; then \
          echo "Bravo... Test [32mOk[0m" ; \
          touch $@ ; \
        else \
          echo "[31mDIFFERENCES[0m : diff $*.oracle $*.log" ; \
          echo "Pour forcer une nouvelle execution du test :" ; \
          echo "  rm $*.log ; make $*.ok"; \
          echo "[31mPour accepter faire[0m : " ; \
          echo "  cp $*.log $*.oracle" ; \
          rm -f $@ ; \
        fi

%.oracle :
	$(PRINT_MAKING) $@
	echo "	[31m[ATTENTION : génération automatique d'oracle][0m"
	$(MAKE) $*.log
	cp $*.log $*.oracle


#==== CLEAN :

clean::
	rm -f $(WP_LIB_OK)
	rm -f $(WP_LIB_BASE:%=$(WP_LIB_DIR)/%_why.why)
	rm -f $(WP_LIB_OK) $(WP_LIB_OK:%.ok=%.why)
	rm -f $(WP_LIB_OK) $(WP_LIB_OK:%.ok=%.log)

#==== END.
