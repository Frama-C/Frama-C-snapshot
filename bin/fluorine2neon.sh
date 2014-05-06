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

#! /bin/sh
#
# fluorine2neon:
# convert a Frama-C plugin from Frama-C Fluorine to Frama-C Neon 
# as most as possible (no guarantee that the result is fully compatible)
#
# known miss features: don't work if a directory name contains some spaces

NAME=fluorine2neon
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
   -e "\"s/Upper_Semi_Lattice/Join_Semi_Lattice/g\""\
   -e "\"s/!Db.Dominators.is_dominator/Dominators.dominates/g\"" \
   -e "\"s/sizeOf_int/bytesSizeOf/g\"" \
   -e "\"s/\\\([^a-zA-Z0-9]\\\)Base\\\.create_varinfo/\\\1Base.of_varinfo/g\"" \
   -e "\"s/\\\([^a-zA-Z0-9]\\\)Base\\\.create_logic/\\\1Base.of_varinfo/g\"" \
   -e "\"s/\\\([^a-zA-Z0-9]\\\)Base\\\.find/\\\1Base.of_varinfo/g\"" \
   -e "\"s/generic_symetric_existential_predicate/generic_symetric_predicate/g\"" \
   -e "\"s/generic_generic_is_included/generic_predicate/g\"" \
   -e "\"s/symetric/symmetric/g\"" \
   -e "\"s/Symetric/Symmetric/g\"" \
   -e "\"s/Cil.isPtrType/Cil.isPointerType/g\"" \
   -e "\"s/Base.cstring_of_string_id//g\"" \
   -e "\"s/Ival.fold\\\([^_]\\\)/Ival.fold_int\\\1/g\"" \
   -e "\"s/Abstract_interp.Lattice_/Lattice_type.Lattice_/g\"" \
   -e "\"s/Abstract_interp.Lattice/Lattice_type.AI_Lattice_with_cardinal_one/g\"" \
   -e "\"s/Loop.get_loop_stmts/Stmts_graph.get_stmt_stmts/g\"" \
   -e "\"s/Lmap_bitwise.From_Model/Function_Froms.Memory/g\"" \
\
   -e "\"s/Structural_descr.Abstract/Structural_descr.t_abstract/g\"" \
\
   -e "\"s/Structural_descr.Unknown/Structural_descr.t_unknown/g\"" \
\
   -e "\"s/Plugin.group/Cmdline.Group.t/g\"" \
   -e "\"s/\\\([^.a-zA-Z_0-9]\\\)Parameter\\\([^_]\\\)/\\1Typed_parameter\\2/g\"" \
   -e "\"s/Plugin.Parameter_input/Parameter_sig.Input/g\"" \
   -e "\"s/Plugin.Parameter_input_with_arg/Parameter_sig.Input_with_arg/g\"" \
   -e "\"s/Plugin.Parameter/Parameter_sig.S/g\"" \
   -e "\"s/Plugin.Bool/Parameter_sig.Bool/g\"" \
   -e "\"s/Plugin.WithOutput/Parameter_sig.With_output/g\"" \
   -e "\"s/Plugin.Int/Parameter_sig.Int/g\"" \
   -e "\"s/Plugin.String/Parameter_sig.String/g\"" \
   -e "\"s/Plugin.String_collection/Parameter_sig.String_collection/g\"" \
   -e "\"s/Plugin.String_set/Parameter_sig.String_set/g\"" \
   -e "\"s/Plugin.String_list/Parameter_sig.String_list/g\"" \
   -e "\"s/Plugin.String_hashtbl/Parameter_sig.String_hashtbl/g\"" \
   -e "\"s/Plugin.Indexed_val/Parameter_sig.Indexed_val/g\"" \
   -e "\"s/Plugin.Indexed_val_input/Parameter_sig.Indexed_val_input/g\"" \
   -e "\"s/Plugin.Specific_dir/Parameter_sig.Specific_dir/g\"" \
\
   -e "\"s/Plugin.set_cmdline_stage/Parameter_customize.set_cmdline_stage/g\"" \
   -e "\"s/Plugin.do_not_journalize/Parameter_customize.do_not_journalize/g\"" \
   -e "\"s/Plugin.do_not_projectify/Parameter_customize.do_not_projectify/g\"" \
   -e "\"s/Plugin.do_not_reset_on_copy/Parameter_customize.do_not_reset_on_copy/g\"" \
   -e "\"s/Plugin.do_not_save/Parameter_customize.do_not_save/g\"" \
   -e "\"s/Plugin.set_negative_option_name/Parameter_customize.set_negative_option_name/g\"" \
   -e "\"s/Plugin.set_unset_option_name/Parameter_customize.set_unset_option_name/g\"" \
   -e "\"s/Plugin.set_unset_option_help/Parameter_customize.set_unset_option_help/g\"" \
   -e "\"s/Plugin.set_group/Parameter_customize.set_group/g\"" \
   -e "\"s/Plugin.is_invisible/Parameter_customize.is_invisible/g\"" \
   -e "\"s/Plugin.argument_is_function_name/Parameter_customize.argument_is_function_name/g\"" \
   -e "\"s/Plugin.do_iterate/Parameter_customize.do_iterate/g\"" \
   -e "\"s/Plugin.do_not_iterate/Parameter_customize.do_not_iterate/g\"" \
   -e "\"s/Plugin.get_selection/Parameter_state.get_selection/g\"" \
   -e "\"s/Plugin.get_reset_selection/Parameter_state.get_reset_selection/g\"" \
   -e "\"s/Plugin.get_selection_context/Parameter_state.get_selection_context/g\"" \
   -e "\"s/Base.get_varinfo/Base.to_varinfo/g\"" \
   -e "\"s/Base.Not_a_variable/Base.Not_a_C_variable/g\"" \
\
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
  echo "Usage: $NAME [options | directories]

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
Do \"$NAME -h\" for help."
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
