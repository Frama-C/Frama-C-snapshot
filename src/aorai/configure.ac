##########################################################################
#                                                                        #
#  This file is part of Aorai plug-in of Frama-C.                        #
#                                                                        #
#  Copyright (C) 2007-2014                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#    INRIA (Institut National de Recherche en Informatique et en         #
#           Automatique)                                                 #
#    INSA  (Institut National des Sciences Appliquees)                   #
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

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS],
         [m4_include(FRAMAC_SHARE/configure.ac)]
        )

check_plugin(aorai,PLUGIN_RELATIVE_PATH(plugin_file),[support for Aorai plug-in],yes,yes)

if test "$ENABLE_AORAI" != "no"; then
  plugin_use_external(aorai,ltltoba)
  # ltl2ba library
  configure_tool([LTLTOBA],[ltl2ba],[ltl2ba not found.],no)
  check_plugin_dependencies
fi

write_plugin_config(Makefile)
