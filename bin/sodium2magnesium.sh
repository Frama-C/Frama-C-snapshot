##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2015                                               #
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

#! /bin/sh
#
# convert a Frama-C plugin from Frama-C Sodium to Frama-C Magnesium
# as most as possible (no guarantee that the result is fully compatible)
#
# known miss features: don't work if a directory name contains some spaces

ARGS=$@

DIR=

# verbosing on by default
VERBOSE="v"

sedi ()
{
  if [ -n "`sed --help 2> /dev/null | grep \"\\-i\" 2> /dev/null`" ]; then
    eval sed -i "$@"
  else
      # option '-i' is not recognized by sed: use a tmp file
    new_temp=`mktemp /tmp/frama-c.XXXXXXX` || exit 1
    eval sed "$@" > $new_temp
    eval last=\${$#}
    mv $new_temp $last
  fi
}

dirs ()
{
  if [ -z "$DIR" ]; then
    DIR=.
  fi
}

safe_goto ()
{
  dir=$1
  cd $dir
  $3
  cd $2
}

goto ()
{
  if [ -d $1 ]; then
    safe_goto $1 $2 $3
  else
    echo "Directory '$1' does not exist. Omitted."
  fi
}

process_file ()
{
  file=$1
  if [ "$VERBOSE" ]; then
    echo "Processing file $file"
  fi
  sedi \
   -e "\"s/Extlib.pretty_position/Cil_datatype.Location.pretty/g\"" \
   -e "\"s/Dynlink_common_interface/FCDynlink/g\"" \
   -e "\"s/Varinfo.pretty_vname/Varinfo.pretty/g\"" \
   -e "\"s/Ast.is_last_decl/Ast.is_def_or_last_decl/g\"" \
   -e "\"s/Hptmap.NoCache/Hptmap_sig.NoCache/g\"" \
   -e "\"s/Hptmap.PersistentCache/Hptmap_sig.PersistentCache/g\"" \
   -e "\"s/Hptmap.TemporaryCache/Hptmap_sig.TemporaryCache/g\"" \
   -e "\"s/CEA_DUMP/Frama_C_dump_each/g\"" \
   -e "\"s/Properties.Interp.lval/Properties.Interp.term_lval/g\"" \
   -e "\"s/Properties.Interp.expr/Properties.Interp.term/g\"" \
   -e "\"s/Dynamic.is_plugin_present/Plugin.is_present/g\"" \
   -e "\"s/Errorloc.getPosition/Errorloc.currentLoc/g\"" \
   -e "\"s/Cabshelper.getPosition/Errorloc.currentLoc/g\"" \
   -e "\"s/Ival.singleton_zero/Ival.zero/g\"" \
   -e "\"s/Ival.singleton_one/Ival.one/g\"" \
   -e "\"s/Ival.Float_abstract/Fval/g\"" \
   -e "\"s/Ival.F\b/Fval.F/g\"" \
   -e "\"s/Ival.min_and_max_float\b/Fval.min_and_max/g\"" \
   -e "\"s/!Db.Semantic_Callgraph.topologically_iter_on_functions/Callgraph.Uses.iter_in_rev_order/g\"" \
   -e "\"s/!Db.Semantic_Callgraph.iter_on_callers/Callgraph.Uses.iter_on_callers/g\"" \
   -e "\"s/!Db.Semantic_Callgraph.accept_base/Callgraph.Uses.accept_base/g\"" \
   -e "\"s/!Db.Semantic_Callgraph.dump/Callgraph.Cg.Services.dump/g\"" \
   -e "\"s/!Db.Syntactic_Callgraph.dump/Callgraph.Cg.Services.dump/g\"" \
   -e "\"s/!Semantic_Callgraph.topologically_iter_on_functions/Callgraph.Uses.iter_in_rev_order/g\"" \
   -e "\"s/!Semantic_Callgraph.iter_on_callers/Callgraph.Uses.iter_on_callers/g\"" \
   -e "\"s/!Semantic_Callgraph.accept_base/Callgraph.Uses.accept_base/g\"" \
   -e "\"s/!Semantic_Callgraph.dump/Callgraph.Cg.Services.dump/g\"" \
   -e "\"s/!Syntactic_Callgraph.dump/Callgraph.Cg.Services.dump/g\"" \
    $file
}

apply_one_dir ()
{
  if [ "$VERBOSE" ]; then
    echo "Processing directory `pwd`"
  fi
  for f in `ls -p1 *.ml* 2> /dev/null`; do
    process_file $f
  done
}

apply_recursively ()
{
  apply_one_dir
  for d in `ls -p1 | grep \/`; do
    safe_goto $d .. apply_recursively
  done
}

applying_to_list ()
{
  dirs
  tmpdir=`pwd`
  for d in $DIR; do
    goto $d $tmpdir $1
  done
}

help ()
{
  echo "Usage: $0 [options | directories]

Options are:
  -r | --recursive       Check subdirectories recursively
  -h | --help            Display help message
  -q | --quiet           Quiet mode (i.e. non-verbose mode)
  -v | --verbose         Verbose mode (default)"
  exit 0
}

error ()
{
  echo "$1.
Do \"$0 -h\" for help."
  exit 1
}

FN="apply_one_dir"

parse_arg ()
{
  case $1 in
    -r | --recursive)     FN="apply_recursively";;
    -h | -help      )     help; exit 0;;
    -q | --quiet    )     VERBOSE=;;
    -v | --verbose  )     VERBOSE="v";;
    -* )                  error "Invalid option $1";;
    * )                   DIR="$DIR $1";;
  esac
}

cmd_line ()
{
  for s in $ARGS; do
    parse_arg $s
  done
  applying_to_list $FN
}

cmd_line
exit 0
