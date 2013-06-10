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

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS],
         [m4_include(FRAMAC_SHARE/configure.ac)]
        )

check_plugin(wp,PLUGIN_RELATIVE_PATH(plugin_file),[WP plug-in],yes,yes)

AC_ARG_ENABLE(
  wpcoq,
  [  --enable-wpcoq          Wp precompiled coq libraries (default: yes)],
  WPCOQ=$enableval,
  WPCOQ=yes
)

if test "$ENABLE_WP" != "no"; then
  plugin_use(wp,gui)
  plugin_use(wp,rte_annotation)

  if test "$WPCOQ" = "yes" ; then
    ## coq
    AC_CHECK_PROG(COQC,coqc,yes,no)
    if test "$COQC" = "yes" ; then
      COQVERSION=`coqc -v | sed -n -e 's|.*version* *\([[^ ]]*\) .*$|\1|p' `
      case $COQVERSION in
        8.4*|trunk) 
          AC_MSG_RESULT(coqc version $COQVERSION found)
          ;;
        *) 
          AC_MSG_RESULT(wp needs coq 8.4, found $COQVERSION) 
          COQC="no"
          ;;
      esac
    else
      AC_MSG_NOTICE(rerun configure to make wp using coq 8.4)
    fi
  else
    COQC="no"
  fi
  AC_SUBST(COQC)

  check_plugin_dependencies
fi

write_plugin_config(Makefile)

