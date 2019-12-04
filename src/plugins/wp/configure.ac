##########################################################################
#                                                                        #
#  This file is part of WP plug-in of Frama-C.                           #
#                                                                        #
#  Copyright (C) 2007-2019                                               #
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
  wp-coq,
  [  --enable-wp-coq         Wp precompiled Coq libraries (default: yes)],
  WPCOQ=$enableval,
  WPCOQ=yes
)

plugin_require(wp,qed)
plugin_require(wp,rtegen)
plugin_use(wp,gui)

# Why3 API dependency
######################
# REQUIRE_LIBRARY: library *must* be present in order to build plugins
# USE_LIBRARY: better for plugins if library is present, but not required
# HAS_LIBRARY: is the library available?

REQUIRE_WHY3=
USE_WHY3=
HAS_WHY3=

WHY3_PATH=`ocamlfind query why3 | tr -d '\\r\\n'`
echo "Ocamlfind -> using $WHY3_PATH"
WHY3PATH_FOR_CONFIGURE=$WHY3_PATH

configure_library([WHY3],
                  [$WHY3PATH_FOR_CONFIGURE/why3.$LIB_SUFFIX],
                  [$WHY3PATH_FOR_CONFIGURE/why3.$LIB_SUFFIX not found.],
                  no)


plugin_require_external(wp,why3)

AC_SUBST(HAS_WHY3)


check_plugin_dependencies

# NB: this would deserve to use plugin_requires mechanism
if test "$ENABLE_WP" != "no"; then

  ## Configuring for WP-COQ
  if test "$WPCOQ" = "yes" ; then
    AC_CHECK_PROG(COQC,coqc,yes,no)
    if test "$COQC" = "yes" ; then
      COQVERSION=`coqc -v | sed -n -e 's|.*version* *\([[^ ]]*\) .*$|\1|p' `
      case $COQVERSION in
        8.7*|8.8*|8.9*|8.10*|trunk)
          AC_MSG_RESULT(coqc version $COQVERSION found)
          ;;
        *)
          AC_MSG_RESULT(unsupported coqc version $COQVERSION)
          COQC="no"
          ;;
      esac
    else
      AC_MSG_NOTICE(rerun configure to make wp using coq 8.7.2 or higher)
    fi
  else
    COQC="no"
  fi
  AC_SUBST(COQC)

fi

write_plugin_config(Makefile)
