##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2016                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  All rights reserved.                                                  #
#  Contact CEA LIST for licensing.                                       #
#                                                                        #
##########################################################################

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS],[m4_include(FRAMAC_SHARE/configure.ac)])

check_plugin(security_slicing,PLUGIN_RELATIVE_PATH(plugin_file),
	     [support for Security_slicing plug-in],yes,yes)

plugin_require(security_slicing,slicing)
plugin_require(security_slicing,value_analysis)
plugin_require(security_slicing,pdg)
plugin_require(security_slicing,gui)
check_plugin_dependencies

write_plugin_config(Makefile)
