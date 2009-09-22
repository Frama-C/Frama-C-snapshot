##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2009                                               #
#    CEA (Commissariat à l'Énergie Atomique)                             #
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

m4_ifdef([FRAMAC_MAIN_AUTOCONF],,
         [m4_ifdef([plugin_file],[AC_INIT(plugin_file)],[AC_INIT(aclocal.m4)])])

upper() {
  echo "$1" | tr "a-z-" "A-Z_"
}

lower() {
  echo "$1" | tr "A-Z_" "a-z-"
}

m4_define([tovarname],[m4_esyscmd(echo $1 | tr "a-z-" "A-Z_")])

new_section() {
  banner=`echo "* $1 *" | sed -e 's/./*/g'`
  title=`echo "* $1 *" | tr "a-z" "A-Z"`
  AC_MSG_NOTICE($banner)
  AC_MSG_NOTICE($title)
  AC_MSG_NOTICE($banner)
}

define([FRAMAC_M4_MACROS])

# sadly, there's no way to define a new diversion beside the internal ones.
# hoping for the best here...
m4_define([frama_c_configure_tool],m4_incr(m4_divnum))

m4_define([PLUGINS_LIST],[])

# to distinguish internal plugins, known by the main configure, from
# purely external plugins living in src/ and compiled together with the main
# frama-c

define([KNOWN_SRC_DIRS],[])

define([check_plugin],
[
define([PLUGIN_NAME],$1)
define([PLUGIN_FILE],$2)
define([PLUGIN_MSG],$3)
define([PLUGIN_DEFAULT],$4)
define([PLUGIN_DYNAMIC],$5)
define([PLUGIN_ADDITIONAL_DIR],$6)
default=PLUGIN_DEFAULT
AC_CHECK_FILE(PLUGIN_FILE,plugin_present=yes,plugin_present=no;default=no)
FORCE=no
define([PLUGIN_HELP],
       AC_HELP_STRING([--enable-PLUGIN_NAME],
                      [PLUGIN_MSG (default: PLUGIN_DEFAULT)]))
AC_ARG_ENABLE(
  [PLUGIN_NAME],
  PLUGIN_HELP,
  ENABLE=$enableval;FORCE=$enableval,
  ENABLE=$default
)

define([KNOWN_SRC_DIRS],KNOWN_SRC_DIRS PLUGIN_FILE PLUGIN_ADDITIONAL_DIR)

# Test to change for static plugin, dynamic option
#default_dyn=no
#define([PLUGIN_HELP_DYN],
#	AC_HELP_STRING([--enable-PLUGIN_NAME-dynamic],
#                     [PLUGIN_MSG (default: static)])
#define([PLUGIN_NAME_DYN],[PLUGIN_NAME]-dynamic)
#AC_ARG_ENABLE(
# [PLUGIN_NAME_DYN],
#  PLUGIN_HELP_DYN,
#  ENABLE=$enableval;
#  FORCE=$enableval
#  ENABLE=$default_dyn
#)
#eval ENABLE_DYNAMIC_$up=\$ENABLE

if test "$plugin_present" == "no" -a "$FORCE" == "yes"; then
  AC_MSG_ERROR([PLUGIN_NAME is not available])
fi
up=`upper PLUGIN_NAME`
eval FORCE_$up=\$FORCE
PLUGINS_FORCE_LIST=${PLUGINS_FORCE_LIST}" "FORCE_$up
define([upup],[tovarname(PLUGIN_NAME)])
define([PLUGINS_LIST],PLUGINS_LIST upup)
eval ENABLE_$up=\$ENABLE
eval NAME_$up=PLUGIN_NAME

if test "$default" == "no" -a "$FORCE" == "no"; then
  # [JS 2009/03/27] don't know why that doesn't work in one single line
  tmp=" (not available by default)"
  eval INFO_$up=\$tmp
fi

# Dynamic plug-ins configuration
m4_if("PLUGIN_DYNAMIC","yes",
  [define([STATIC_HELP],
          AC_HELP_STRING([--with-PLUGIN_NAME-static],
                         [link PLUGIN_NAME statically (default: no)]))
   AC_ARG_WITH(PLUGIN_NAME[-static],STATIC_HELP,
               [is_static=$withval],
	       [is_static=$IS_ALL_STATIC])
   undefine([STATIC_HELP])],
   [is_static=yes])

if test "$is_static" == "no"; then
  USE_NATIVE_DYNLINK="${USE_NATIVE_DYNLINK} PLUGIN_NAME"
  eval DYNAMIC_$up=yes
else
  eval DYNAMIC_$up=no
fi

echo "PLUGIN_NAME... $ENABLE"
]) # end of check_plugin

# 1st param: uppercase name of the library
# 2nd param: file which must exist. This parameter can be a list of files.
# In this case, they will be tried in turn until one of them exists. The
# name of the file found will be put in the variable SELECTED_$1
# 3d  param: warning to display if problem
# 4th param: yes iff checking the library must always to be done
# (even if there is no plug-in using it)
m4_define([configure_library],
[
# No need to check the same thing multiple times.
  m4_ifdef(SELECTED_$1,,
  [
  m4_divert_push(frama_c_configure_tool)
  m4_define([VAR],[$1])
  m4_define([SELECTED_VAR],[SELECTED_$1])
  m4_define([PROG],[$2])
  m4_define([require],[$REQUIRE_$1])
  m4_define([use],[$USE_$1])
  m4_define([msg],[$3])
  m4_define([has],[HAS_$1])
  m4_define([file],[FILE_$1])
# [JS 2009/06/02] sh tests and m4 variables do not mix well together.
# It works by chance but it is not robust enough.
# Should be rewritten
  if test -n "require" -o -n "use" -o "$4" == "yes"; then
    has=no
    m4_foreach(file,[PROG],
       [if test "$has" != "yes"; then
          AC_CHECK_FILE(file,has=yes,has=no)
          if test "$has" == "yes"; then SELECTED_VAR=file
          fi
       fi]
       )
    if test "$has" == "no"; then
      AC_MSG_WARN(msg)
      if test "require" != ""; then
        echo "plug-ins disabled:
 require"
        for p in require; do
	  fp=FORCE_`upper "$p"`
	  if eval test "\$$fp" == "yes"; then
	    AC_MSG_ERROR($p requested but PROG missing.)
	  fi
          ep=ENABLE_`upper "$p"`
	  eval $ep="no\ \(see\ warning\ about\ PROG\)"
        done
      fi
      if test "use" != ""; then
        echo "plug-ins not fully functional:
 use"
        for p in use; do
          ep=ENABLE_`upper "$p"`
	  eval eep="\$$ep"
	  if test "`echo $eep | sed -e 's/ .*//' `" != "no"; then
	    eval $ep="partial\ \(see\ warning\ about\ PROG\)"
          fi
        done
      fi
      echo
    fi
  fi
  AC_SUBST(VAR)
  AC_SUBST(has)
  undefine([SELECTED_VAR])
  undefine([VAR])
  undefine([PROG])
  undefine([require])
  undefine([use])
  undefine([msg])
  undefine([has])
  m4_divert_pop(frama_c_configure_tool)
  ])
])

# 1st param: uppercase name of the program
# 2nd param: program which must exist. See comment on configure_library()
# on how to deal with multiple choices for a given program.
# 3d  param: warning to display if problem
# 4th param: yes iff checking the tool must always to be done
# (even if there is no plug-in using it)
m4_define([configure_tool],
[
m4_ifdef(HAS_$1,,
  [
  m4_divert_push(frama_c_configure_tool)
  define([VAR],[$1])
  define([PROG],[$2])
  define([require],[$REQUIRE_$1])
  define([use],[$USE_$1])
  define([msg],[$3])
  define([has],[HAS_$1])
  define([force_check],[$4])

  if test -n "require" -o -n "use" -o "$force_check" == "yes"; then
    for file in PROG; do
     has=
    AC_CHECK_PROG(has,$file,yes,no)
      if test "$has" == "yes"; then SELECTED_VAR=$file break; fi
      done
    if test "$has" == "no"; then
      AC_MSG_WARN([msg])
      if test "require" != ""; then
        echo "plug-ins disabled:
 require"
        for p in require; do
	  fp=FORCE_`upper "$p"`
	  if eval test "\$$fp" == "yes"; then
	    AC_MSG_ERROR([$p requested but PROG missing.])
	  fi
          ep=ENABLE_`upper "$p"`
	  eval $ep="no\ \(see\ warning\ about\ PROG\)"
        done
      fi
      if test "use" != ""; then
        echo "plug-ins not fully functional:
 use"
        for p in use; do
          ep=ENABLE_`upper "$p"`
	  eval eep="\$$ep"
	  if test "`echo $eep | sed -e 's/ .*//' `" != "no"; then
	    eval $ep="partial\ \(see\ warning\ about\ PROG\)"
          fi
        done
      fi
      echo
    else
        VAR=PROG
    fi
  fi
  AC_SUBST(VAR)
  AC_SUBST(has)
  undefine([VAR])
  undefine([PROG])
  undefine([require])
  undefine([use])
  undefine([msg])
  undefine([has])
  m4_divert_pop(frama_c_configure_tool)
])
])

EXTERNAL_PLUGINS=

define([check_frama_c_dependencies],[m4_undivert(frama_c_configure_tool)])
define([check_plugin_dependencies],
       [m4_ifdef([FRAMAC_INTERNAL_AUTOCONF],,
       [m4_undivert(frama_c_configure_tool)])])
define([write_plugin_config],
       [m4_ifndef([plugin_prefix],[define([plugin_prefix],[.])])
        m4_define([plugin_files],
                  AC_FOREACH([plugin_file],$1,[plugin_prefix/plugin_file ]))
        m4_define([files_chmod],
                  AC_FOREACH([plugin_file],plugin_files,[chmod -w plugin_file]))
       AC_CONFIG_FILES(plugin_files,files_chmod)
       m4_ifdef([FRAMAC_MAIN_AUTOCONF],
                EXTERNAL_PLUGINS="${EXTERNAL_PLUGINS} plugin_prefix",
                [AC_OUTPUT()])
       ])
