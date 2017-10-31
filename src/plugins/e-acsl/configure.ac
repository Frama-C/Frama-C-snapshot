##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2017                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
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

########################################
# E-ACSL as a standard Frama-C plug-in #
########################################

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS], [m4_include(FRAMAC_SHARE/configure.ac)])

check_plugin(e_acsl,PLUGIN_RELATIVE_PATH(plugin_file),
             [support for E-ACSL plug-in],yes)

plugin_require(e_acsl,rtegen)

# E-ACSL only works on Linux as of now. Disable it on Mac OS and Windows
AC_CHECK_PROG(UNAME,uname,uname,no)

if test "$UNAME" = "no"; then
  plugin_disable(e_acsl,[Not on a POSIX platform.])
fi

HOST_OS=$($UNAME -s)
case $HOST_OS in
Darwin | Linux | DragonFly | FreeBSD | NetBSD | OpenBSD)
# BSDs haven't really be tested, but it should at least compile fine
;;
*)
  plugin_disable(e_acsl,[unsupported system $HOST_OS])
;;
esac

# perform additional checks when not compiled
# directly with main Frama-C distribution
m4_ifndef([FRAMAC_MAIN_AUTOCONF],
[

check_plugin_dependencies

# Check Frama-C version
#######################

AC_MSG_CHECKING(for Frama-C version)
AC_MSG_RESULT($FRAMAC_VERSION)

VERSION_NUMBER=`echo $FRAMAC_VERSION | sed -n -e 's/[[a-zA-Z]]\+-\([[0-9]]\+\).*/\1/p'`
EXTRA_VERSION_NUMBER=`echo $FRAMAC_VERSION | sed -n -e 's/[[a-zA-Z]]\+-[[0-9]]\+-\(.*\)/\1/p'`
REQUIRED_NUMBER=20161101
REQUIRED_NAME=Silicon

case $FRAMAC_VERSION in
  $REQUIRED_NAME-$REQUIRED_NUMBER+dev*)
    # at the time being, must use the Frama-C development version
    ;;
#   $REQUIRED_NAME-$REQUIRED_NUMBER*)
#     if test -n "$EXTRA_VERSION_NUMBER"; then
#       AC_MSG_WARN(no guarantee of compatibility with your Frama-C custom \
# version of $REQUIRED_NAME)
#     fi;;
  *)
    if test $VERSION_NUMBER -lt $REQUIRED_NUMBER; then
      AC_MSG_ERROR(Frama-C version must be at least \
$REQUIRED_NAME-$REQUIRED_NUMBER.)
    else
      if test $VERSION_NUMBER -gt $REQUIRED_NUMBER; then
        AC_MSG_WARN(Frama-C version is more recent than $REQUIRED_NAME: \
use it at your own risk)
      fi
    fi;;
esac
],
[]) # end of configuration specific to external compilation mode

# no need to continue checking other features if E-ACSL is disabled
if test "$ENABLE_E_ACSL" != "no"; then

###############################
# C specific stuff for E-ACSL #
###############################

MAY_RUN_TESTS=yes

# C compiler and stdio.h
#########################

AC_CHECK_HEADERS([stdio.h],[HAVE_STDIO_H=yes],)

# not sure it can actually happen:
# looks like configure stops on error if no C compiler detected
if test -z $HAVE_STDIO_H; then
  MAY_RUN_TESTS=no
  AC_MSG_WARN([stdio.h missing: non-regression tests unavailable.])
fi

# Doxygen
#########

AC_CHECK_PROG(DOXYGEN,doxygen,doxygen,no)
AC_CHECK_PROG(HAVE_DOT,dot,yes,no)
AC_OUTPUT(PLUGIN_RELATIVE_PATH(doc/doxygen/doxygen.cfg), [  ])

#######################
# Generating Makefile #
#######################

AC_SUBST(OPTIMIZED_RTL)
AC_SUBST(MAY_RUN_TESTS)
AC_SUBST(OCAMLVERSION)

write_plugin_config(Makefile)

###############################
# Configure contrib libraries #
###############################

m4_ifndef([plugin_dir], [
  m4_define([plugin_dir],[./])
])

fi
