FRAMAC=frama-c
STUBS=../../lib/stubs.c
BIND_DIR=bind
BIND_CA_DIR=$(BIND_DIR)/CA-1999-14
BIND_RREXTRACT_NXT_DIR=$(BIND_CA_DIR)/rrextract-nxt

APACHE_DIR=apache
APACHE_2004_DIR=$(APACHE_DIR)/CVE-2004-0940

EDBROWSE_DIR=edbrowse/CVE-2006-6909/ftpls

GXINE_DIR=gxine/CVE-2007-0406/main

OPENSER_DIR=OpenSER
OPENSER_6876_DIR=$(OPENSER_DIR)/CVE-2006-6876/fetchsms

all:bind gxine apache edbrowse openser

bind:bind_CA bind_CVE

bind_CA:bind_rrextract_nxt

bind_rrextract_nxt:$(BIND_RREXTRACT_NXT_DIR)/simp_bad.c
	$(FRAMAC) -val $(STUBS) $(BIND_RREXTRACT_NXT_DIR)/simp_bad.c >$(BIND_RREXTRACT_NXT_DIR)/bin_rrextract_nxt.log 2>$(BIND_RREXTRACT_NXT_DIR)/bind_rrxtract_nxt.err

gxine:$(GXINE_DIR)/simp_bad.c
	$(FRAMAC) -val $(STUBS) $(GXINE_DIR)/simp_bad.c >$(GXINE_DIR)/frama-c.log  2>$(GXINE_DIR)/frama-c.err

bind_CVE:$(APACHE_2004_DIR)/apache.c

apache:apache_CVE_2004 apache_CVE_2006

apache_CVE_2004_iter1_prefixLong_arr: $(APACHE_2004_DIR)/apache.c
	$(FRAMAC) -val $(STUBS) $(APACHE_2004_DIR)/apache.c $(APACHE_2004_DIR)/get_tag/iter1_prefixLong_arr_bad.c  >$(APACHE_2004_DIR)/ap_iter1_prefixLong_arr.log 2>$(APACHE_2004_DIR)/ap_iter1_prefixLong_arr.err

apache_CVE_2004:apache_CVE_2004_iter1_prefixLong_arr

apache_CVE_2006:

edbrowse: $(EDBROWSE_DIR)/strchr_bad.c
	$(FRAMAC) -val $(STUBS) $(EDBROWSE_DIR)/strchr_bad.c >$(EDBROWSE_DIR)/strchr_bad.log 2>$(EDBROWSE_DIR)/strchr_bad.err

openser:openser_6876

openser_6876:$(OPENSER_6876_DIR)/full_bad.c
	$(FRAMAC) -val $(STUBS) $(OPENSER_6876_DIR)/full_bad.c -slevel 500 >$(OPENSER_6876_DIR)/full_bad.log 2>$(OPENSER_6876_DIR)/full_bad.err