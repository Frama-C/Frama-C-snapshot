##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2014                                               #
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

# AC_ARG_WITH(frama-c,
#   AC_HELP_STRING([Frama-C executable name (default is 'frama-c')]),
#   [FRAMA_C=$withval],
#   [FRAMA_C=frama-c])

# AC_ARG_WITH(frama-c-gui,
#     AC_HELP_STRING([Frama-C executable name (default is 'frama-c')]),
#     [FRAMA_C_GUI=$withval],
#     [FRAMA_C_GUI=frama-c-gui])

m4_ifdef([FRAMAC_MAIN_AUTOCONF],,
         [m4_ifdef([plugin_file],
             [AC_INIT(plugin_file)],
             [AC_INIT(aclocal.m4)])
          [KNOWN_PLUGINS=$(frama-c -help | \
            sed -e '0,/^\*\*\*\*\*/ d' \
                -e 's/\([^ ][^ ]*\( [^ ][^ ]*\)*\)  .*/\1/' \
                -e '/^ /d' -e '/^$/d' | \
            tr "a-z- " "A-Z__")
          for plugin in ${KNOWN_PLUGINS}; do
            export $(echo ENABLE_$plugin)=yes
          done
          ]
          AC_SUBST([FRAMAC_VERSION],
                   [`frama-c -version | sed -n -e "s|^Version: *\(.*\)$|\1|p"`])
          AC_CHECK_PROG(ENABLE_GUI,[frama-c-gui],[yes],[no])
         ])

m4_define([PLUGIN_RELATIVE_PATH],
	  [m4_ifdef([plugin_prefix],plugin_prefix/$1,$1)])

upper() {
  echo "$1" | tr "a-z-" "A-Z_"
}

lower() {
  echo "$1" | tr "A-Z" "a-z"
}

m4_define([tovarname],[m4_esyscmd(printf "%s" $1 | tr "a-z-" "A-Z_")])

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
AC_CHECK_FILE(PLUGIN_FILE,
              default=PLUGIN_DEFAULT;plugin_present=yes,
	      plugin_present=no;default=no)

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

if test "$ONLY_KERNEL" = "yes" -a "$FORCE" = "no"; then
  ENABLE=no
fi

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

if test "$plugin_present" = "no" -a "$FORCE" = "yes"; then
  AC_MSG_ERROR([PLUGIN_NAME is not available])
fi
define([UP],[tovarname(PLUGIN_NAME)])
[FORCE_]UP=$FORCE
PLUGINS_FORCE_LIST=${PLUGINS_FORCE_LIST}" "[FORCE_]UP
define([PLUGINS_LIST],PLUGINS_LIST UP)
[ENABLE_]UP=$ENABLE
[NAME_]UP=PLUGIN_NAME
if test "$default" = "no" -a "$FORCE" = "no"; then
  [INFO_]UP=" (not available by default)"
fi

# Dynamic plug-ins configuration
m4_if("PLUGIN_DYNAMIC","yes",
  [define([STATIC_HELP],
          AC_HELP_STRING([--with-PLUGIN_NAME-static],
                         [link PLUGIN_NAME statically (default: no)]))
   AC_ARG_WITH(PLUGIN_NAME[-static],STATIC_HELP,
               [is_static=$withval],
	       [is_static=$IS_ALL_STATIC])
   undefine([STATIC_HELP])
   # is_static = "yes" iff the user forces the plug-in to be static
   # is_static = "no"  iff the user forces the plug-in to be dynamic
   # is_static = "" in others cases (no special instruction)
   [STATIC_]UP=$is_static
   if test "$is_static" != "yes"; then
     USE_NATIVE_DYNLINK="${USE_NATIVE_DYNLINK} PLUGIN_NAME"
     [DYNAMIC_]UP=yes
   else
     [DYNAMIC_]UP=no
   fi],
   # static plug-in
   [[DYNAMIC_]UP=no])

AC_SUBST([ENABLE_]UP)
AC_SUBST([DYNAMIC_]UP)
echo "PLUGIN_NAME... $ENABLE"
# kept defined for write_plugin_config. A bit ugly, but not more than
# usual autoconf stuff.
# m4_undefine([PLUGIN_NAME])
m4_undefine([PLUGIN_FILE])
m4_undefine([PLUGIN_MSG])
m4_undefine([PLUGIN_DEFAULT])
m4_undefine([PLUGIN_DYNAMIC])
m4_undefine([PLUGIN_ADDITIONAL_DIR])
m4_undefine([UP])
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
    has=no
    m4_foreach(file,[PROG],
       [if test "$has" != "yes"; then
          AC_CHECK_FILE(file,has=yes,has=no)
          if test "$has" = "yes"; then SELECTED_VAR=file
          fi
       fi]
       )
    VAR=$SELECTED_VAR
    m4_divert_push(frama_c_configure_tool)
    if test -n "require" -o -n "use" -o "$force_check" = "yes"; then
      if test "$has" = "no"; then
        AC_MSG_WARN([msg])
        reason="PROG missing"
        $5
        for p in require; do
	  up=`upper "$p"`
          ep=ENABLE_$up
          eval enable_p=\$$ep
          if test "$enable_p" != "no"; then
            fp=FORCE_`upper "$p"`
	    if eval test "\$$fp" = "yes"; then
	      AC_MSG_ERROR([$p requested but $reason.])
	    fi
	    eval $ep="no\ \(see\ warning\ about\ PROG\)"
            AC_MSG_WARN([$p disabled because $reason.])
	    eval INFO_$up=\", $reason\"
          fi
        done
        for p in use; do
          up=`upper "$p"`
          ep=ENABLE_$up
          eval eep="\$$ep"
	  if test "`echo $eep | sed -e 's/ .*//' `" != "no"; then
	    eval $ep="partial\ \(see\ warning\ about\ PROG\)"
            AC_MSG_WARN([$p partially enabled because $reason.])
	    eval INFO_$up=\", $reason\"
          fi
        done
      fi
    fi
    m4_divert_pop(frama_c_configure_tool)
    AC_SUBST(VAR)
    AC_SUBST(has)
    undefine([SELECTED_VAR])
    undefine([VAR])
    undefine([PROG])
    undefine([require])
    undefine([use])
    undefine([msg])
    undefine([has])
    undefine([file])
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
  define([VAR],[$1])
  define([PROG],[$2])
  define([require],[$REQUIRE_$1])
  define([use],[$USE_$1])
  define([msg],[$3])
  define([has],[HAS_$1])
  define([force_check],[$4])

  for file in PROG; do
   has=
AC_CHECK_PROG(has,$file,yes,no)
    if test "$has" = "yes"; then SELECTED_VAR=$file break; fi
  done
  m4_divert_push(frama_c_configure_tool)
  if test -n "require" -o -n "use" -o "$force_check" = "yes"; then
    if test "$has" = "no"; then
      AC_MSG_WARN([msg])
      reason="PROG missing"
      for p in require; do
        up=`upper "$p"`
        ep=ENABLE_$up
        eval enable_p=\$$ep
        if test "$enable_p" != "no"; then
          fp=FORCE_`upper "$p"`
	  if eval test "\$$fp" = "yes"; then
	    AC_MSG_ERROR([$p requested but $reason.])
	  fi
	  eval $ep="no\ \(see\ warning\ about\ PROG\)"
	  AC_MSG_WARN([$p disabled because $reason.])
	  eval INFO_$up=\", $reason\"
        fi
      done
      for p in use; do
        up=`upper "$p"`
        ep=ENABLE_$up
        eval eep="\$$ep"
	if test "`echo $eep | sed -e 's/ .*//' `" != "no"; then
	  eval $ep="partial\ \(see\ warning\ about\ PROG\)"
	  AC_MSG_WARN([$p partially enabled because $reason.])
	  eval INFO_$up=\", $reason\"
        fi
      done
    else
        VAR=PROG
    fi
  fi
  m4_divert_pop(frama_c_configure_tool)
  AC_SUBST(VAR)
  AC_SUBST(has)
  undefine([VAR])
  undefine([PROG])
  undefine([require])
  undefine([use])
  undefine([msg])
  undefine([has])
])
])

EXTERNAL_PLUGINS=

define([plugin_require_external],
       [m4_define([UPORIG],tovarname($2))
        m4_define([REQUIRE],[REQUIRE_]UPORIG)
        REQUIRE=$REQUIRE" "$1
        m4_undefine([REQUIRE])
        m4_undefine([UPORIG])])

define([plugin_use_external],
       [m4_define([UPORIG],tovarname($2))
        m4_define([USE],[USE_]UPORIG)
        USE=$USE" "$1
        m4_undefine([USE])
        m4_undefine([UPORIG])])

define([plugin_require],
       [m4_define([UPTARGET],tovarname($1))
        m4_define([UPORIG],tovarname($2))
        m4_define([REQUIRE],[REQUIRE_]UPORIG)
        m4_define([REQUIRED],[REQUIRED_]UPTARGET)
        REQUIRE=$REQUIRE" "$1
        REQUIRED=$REQUIRED" "$2
        m4_undefine([UPTARGET])
        m4_undefine([UPORIG])
        m4_undefine([REQUIRE])
        m4_undefine([REQUIRED])
       ])

define([plugin_use],
       [m4_define([UPTARGET],tovarname($1))
        m4_define([UPORIG],tovarname($2))
        m4_define([USE],[USE_]UPORIG)
        m4_define([USED],[USED_]UPTARGET)
        USE=$USE" "$1
        USED=$USED" "$2
        m4_undefine([UPTARGET])
        m4_undefine([UPORIG])
        m4_undefine([USE])
        m4_undefine([USED])
       ])
# Implementation of an ordering $1 < $2: "" < yes < partial < no
lt_mark () {
  first=`echo "$1" | sed -e 's/ .*//' `
  second=`echo "$2" | sed -e 's/ .*//' `
  case $first in
    "") echo "true";;
    "yes"*)
      case $second in
        "yes") echo "";;
        "partial" | "no") echo "true";;
      esac;;
    "partial"*)
      case $second in
        "yes" | "partial") echo "";;
        "no") echo "true";;
      esac;;
    "no"*) echo "";;
  esac
}

# Check and propagate marks to requires and users.
# $1: parent plugin
# $2: mark to propagate to requires
# $3: mark to propagate to users
check_and_propagate () {
  # for each requiers
  r=REQUIRE_$1
  eval require="\$$r"
  for p in $require; do
    up=`upper "$p"`
    m=MARK_"$up"
    eval mark="\$$m"
    if test -z "$mark"; then
      m=ENABLE_"$up"
      eval mark="\$$m"
    fi
    if test `lt_mark "$mark" "$2" `; then
      # update marks
      eval MARK_$up=\"$2\";
      TODOLIST=$TODOLIST" "$p
      # display a warning or an error if required
      short_mark=`echo $2 | sed -e 's/ .*//'`
      lp=`lower $p`
      reason=`echo $2 | sed -e 's/no (\(.*\))/\1/' `
      if test "$short_mark" = "no"; then
        fp=FORCE_"$up"
        if eval test "\$$fp" = "yes"; then
	  AC_MSG_ERROR([$lp requested but $reason.])
        else
	  AC_MSG_WARN([$lp disabled because $reason.])
        fi
      else
        if test "$short_mark" = "partial"; then
          reason=`echo $2 | sed -e 's/partial (\(.*\))/\1/' `
	  AC_MSG_WARN([$lp only partially enable because $reason.])
        fi
      fi
      eval INFO_$up=\", $reason\"
    fi
  done
  # for each users
  u=USE_$1
  eval use="\$$u"
  for p in $use; do
    up=`upper "$p"`
    m=MARK_$up
    eval mark="\$$m"
    if test -z "$mark"; then
      m=ENABLE_"$up"
      eval mark="\$$m"
    fi
    if test `lt_mark "$mark" "$3" `; then
      # update marks
      eval MARK_$up=\"$3\";
      TODOLIST=$TODOLIST" "$p
      # display a warning if required
      lp=`lower $p`
      reason=`echo $3 | sed -e 's/partial (\(.*\))/\1/' `
      if test "$reason" != "$3"; then
        AC_MSG_WARN([$lp only partially enabled because $reason.])
      fi
      eval INFO_$up=\", $reason\"
    fi
  done
}

# checks direct dependencies of a plugin. Useful for dynamic plugins which
# have a dependency toward already installed (or not) plug-ins, since the old
# plugins are not in the TODO list from the beginning (and need not their
# mutual dependencies be rechecked anyway
check_required_used () {
  ep=ENABLE_$1
  eval enabled=\$$ep

  if test "$enabled" != "no"; then

    r=REQUIRED_$1
    u=USED_$1
    m=MARK_$1
    eval required=\$$r
    eval used=\$$u
    eval $m=yes

    reason=

    for p in $required; do
      up=`upper $p`
      ec=ENABLE_$up
      eval enabled=\$$ec
      case `echo "$enabled" | sed -e 's/ .*//'` in
        "") reason="$p unknown";;
        "yes" | "partial");;
        "no") reason="$p not enabled";;
      esac
    done
    if test -n "$reason"; then
      eval $m=\"no\ \($reason\)\"
      p_name=`lower $1`
      AC_MSG_WARN([$p_name disabled because $reason.])
      eval INFO_$1=\", $reason\"
    else
      for p in $used; do
        up=`upper $p`
        ec=ENABLE_$up
        eval enabled=\$$ec
        case `echo "$enabled" | sed -e 's/ .*//'` in
          "") reason="$p unknown";;
          "yes" | "partial");;
          "no") reason="$p not enabled";;
        esac
      done
      if test -n "$reason"; then
        eval $m=\"partial\ \($reason\)\"
        p_name=`lower $1`
        AC_MSG_WARN([$p_name partially enabled because $reason.])
        eval INFO_$1=\", $reason\"
      fi
    fi

  else # $enabled = "no"
    eval $m=\"no\"
  fi
}

# Recursively check the plug-in dependencies using the plug-in dependency graph
compute_dependency () {
  plugin=`echo $TODOLIST | sed -e 's/ .*//' `
  TODOLIST=`echo $TODOLIST | sed -e 's/[[^ ]]* *\(.*\)/\1/' `

  lplugin=`lower "$plugin"`
  uplugin=`upper "$plugin"`
  # new mark to consider
  m=MARK_$uplugin
  eval mark="\$$m"
  # old mark to consider
  r=REMEMBER_$uplugin
  eval remember="\$$r"
  # the exact mark (final result),
  # also the old mark if plugin already visited
  e=ENABLE_$uplugin
  eval enable="\$$e"
  #first visit. Performs additional checks over requirements.
  if test -z "$mark"; then
     check_required_used "$uplugin";
     eval mark=\$$m
  fi

#  echo "plug-in $lplugin (mark=$mark, remember=$remember, enable=$enable)"
  if test `lt_mark "$remember" "$mark"`; then
    # visit the current plugin:
    # mark <- max(mark, enable)
    case `echo "$mark" | sed -e 's/ .*//' ` in
      "") echo "problem?"; exit 3;;
      "yes")
       if test -n "$enable"; then mark="$enable"; else mark="yes"; fi;;
      "partial") if test "$enable" = "no"; then mark="no"; fi;;
      "no")      ;;
    esac
    # update plug-in attributes with the new mark
#    echo "update attributes with $mark"
    eval $m=\"$mark\"
    eval $e=\"`echo "$mark" | sed -e 's/ .*//' `\"
    enable="$mark"
    eval $r=\"$mark\"
    # compute and propagate a new mark to requires and users
    case `echo "$enable" | sed -e 's/ .*//' ` in
      "")        echo "problem?"; exit 3;;
      "yes")     check_and_propagate $uplugin "yes" "yes";;
      "partial")
# if a plug-in is partial, does not consider its dependencies as partial
# so the second argument is "yes" and not "partial"
        check_and_propagate \
	  "$uplugin" \
	  "yes" \
	  "yes";;
      "no")
        check_and_propagate \
	  "$uplugin" \
	  "no ($lplugin not enabled)" \
	  "partial ($lplugin not enabled)";;
    esac
  fi
  # recursively consider the next plugins
  if test -n "$TODOLIST"; then
    compute_dependency;
  fi
}

define([compute_plugin_dependencies],
[
# First, initialize some variables
for fp in ${PLUGINS_FORCE_LIST}; do
  if test "$fp" != "FORCE_GTKSOURCEVIEW"; then
    plugin=`echo $fp | sed -e "s/FORCE_\(.*\)/\1/" `
    TODOLIST=$TODOLIST" "$plugin
    eval MARK_$plugin=
    eval REMEMBER_$plugin=
  fi
done
# main call
compute_dependency
])

define([check_frama_c_dependencies],
       [m4_undivert(frama_c_configure_tool)
        compute_plugin_dependencies])

define([check_plugin_dependencies],
       [m4_ifdef([FRAMAC_MAIN_AUTOCONF],,
       [m4_undivert(frama_c_configure_tool)
        compute_plugin_dependencies])])

define([write_plugin_summary],
[
 m4_ifdef([FRAMAC_MAIN_AUTOCONF],,
          [
# Compute INFO_* and exported ENABLE_* from previously computed ENABLE_*
           for fp in ${PLUGINS_FORCE_LIST}; do
             if test "$fp" != "FORCE_GTKSOURCEVIEW"; then
              plugin=`echo $fp | sed -e "s/FORCE_\(.*\)/\1/" `
              ep=ENABLE_$plugin
              eval v=\$$ep
              eval ep_v=`echo $v | sed -e 's/ .*//' `
              eval ENABLE_$plugin=$ep_v
              reason=`echo $v | sed -e 's/[[a-z]]*\( .*\)/\1/' `
              n=NAME_$plugin
              eval name=\$$n
              info=
              if test "$reason" != "$ep_v"; then
               info=$reason
              fi
              AC_MSG_NOTICE([$name: $ep_v$info])
             fi
           done])])

define([write_plugin_config],
       [m4_ifndef([plugin_prefix],[define([plugin_prefix],[.])])
        m4_define([plugin_files],
                  AC_FOREACH([plugin_file],$1,[plugin_prefix/plugin_file ]))
        m4_define([files_chmod],
                  AC_FOREACH([plugin_file],plugin_files,[chmod -w plugin_file]))
        AC_CONFIG_FILES(plugin_files,files_chmod)
        m4_ifdef(
         [FRAMAC_MAIN_AUTOCONF],
         if test "$[ENABLE_]tovarname(PLUGIN_NAME)" != "no"; then
            [EXTERNAL_PLUGINS="${EXTERNAL_PLUGINS} plugin_prefix"];
         fi,
         [
           write_plugin_summary
           AC_OUTPUT()
         ])
       ])
