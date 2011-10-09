##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2011                                               #
#    CEA (Commissariat a l'énergie atomique et aux énergies              #
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

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS],
         [m4_include(FRAMAC_SHARE/configure.ac)]
        )

check_plugin(wp,PLUGIN_RELATIVE_PATH(plugin_file),[support for wp plug-in],yes,yes)

if test "$ENABLE_WP" != "no"; then
  plugin_use(wp,gui)
  plugin_use(wp,rte_annotation)
  plugin_use_external(wp,altergo)
  plugin_use_external(wp,coq)
  plugin_use_external(wp,why)
  plugin_use_external(wp,dot)
  plugin_use_external(wp,whydp)

  # Why
  configure_tool([WHY],[why],[why not found],no)
  
  # Why-dp 
  configure_tool([WHYDP],[why-dp],[why-dp not found],no)

  # alt-ergo
  configure_tool([ALTERGO],[alt-ergo],[alt-ergo not found],no)

  ALTERGO_VERSION=`alt-ergo -version | sed -e 's/Alt-Ergo \(.*\)/\1/' `
  AC_MSG_NOTICE([alt-ergo version is $ALTERGO_VERSION.])
  case $ALTERGO_VERSION in
    0.92.2) AC_MSG_NOTICE([good.]);;
    *) AC_MSG_NOTICE([alt-ergo's array theory unsupported.]);;
  esac


 # coq
  configure_tool([COQ],[coqc],[coq not found],no)

  check_plugin_dependencies
fi

write_plugin_config(Makefile)

AC_SUBST(ALTERGO_VERSION)
