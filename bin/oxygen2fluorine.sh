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
# oxygen2fluorine:
# convert a Frama-C plugin from Frama-C Oxygen to Frama-C Fluorine 
# as most as possible (no guarantee that the result is fully compatible)
#
# known miss features: don't work if a directory name contains some spaces

NAME=oxygen2fluorine
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
   -e "\"s/Cilutil.out_some/Extlib.the/g\"" \
   -e "\"s/Cil_types.rooted_code_annotation/Cil_types.code_annotation/g\"" \
   -e "\"s/Ast_info.is_trivial_rooted_assertion/Ast_info.is_trivial_annotation/g\"" \
   -e "\"s/Ast_info.lift_annot_func//g\"" \
   -e "\"s/Ast_info.lift_annot_list_func//g\"" \
   -e "\"s/Ast_printer.d_rooted_code_annotation/Ast_printer.d_code_annotation/g\"" \
   -e "\"s/Annotation.code_annotation_of_rooted//g\"" \
   -e "\"s/V_Offsetmap_ext/V_Offsetmap/g\"" \
   -e "\"s/My_bigint/Integer/g\"" \
   -e "\"s/State_dependency_graph.Static/State_dependency_graph/g\"" \
\
   -e "\"s/#pVarName/#varname/g\"" \
   -e "\"s/#pVar/#varinfo/g\"" \
   -e "\"s/#pVDecl/#vdecl/g\"" \
   -e "\"s/#pAttr/#attribute/g\"" \
   -e "\"s/#pType/#typ/g\"" \
   -e "\"s/#pOffset/#offset/g\"" \
   -e "\"s/#pExp/#exp/g\"" \
   -e "\"s/#pLval/#lval/g\"" \
   -e "\"s/#pInstr/#instr/g\"" \
   -e "\"s/#pStmt/#stmt/g\"" \
   -e "\"s/#pStmtNext/#next_stmt/g\"" \
   -e "\"s/#pCode_annot/#code_annotation/g\"" \
   -e "\"s/#pGlobal/#global/g\"" \
   -e "\"s/#pLabel/#label/g\"" \
   -e "\"s/#pBlock/#block/g\"" \
   -e "\"s/#pFieldDecl/#fieldinfo/g\"" \
   -e "\"s/#pSpec/#funspec/g\"" \
   -e "\"s/#pLogic_type/#logic_type/g\"" \
   -e "\"s/#pTerm/#term/g\"" \
   -e "\"s/#pLogic_const/#logic_constant/g\"" \
   -e "\"s/#pPredicate/#predicate/g\"" \
   -e "\"s/#pIdentified/#identified/g\"" \
   -e "\"s/#pAssigns/#assigns/g\"" \
   -e "\"s/#pFrom/#from/g\"" \
   -e "\"s/#pDecreases/#decreases/g\"" \
   -e "\"s/#pAssumes/#assumes/g\"" \
   -e "\"s/#pRequires/#requires/g\"" \
   -e "\"s/#pPost_cond/#post_cond/g\"" \
   -e "\"s/#pAllocation/#allocation/g\"" \
   -e "\"s/#pBehavior/#behavior/g\"" \
   -e "\"s/#pCompleteBehavior/#complete_behavior/g\"" \
   -e "\"s/#pDisjointBehavior/#disjoint_behavior/g\"" \
   -e "\"s/#pSpec/#funspec/g\"" \
   -e "\"s/#pAnnotation/#global_annotation/g\"" \
\
   -e "\"s/!Ast_printer.d_funspec/Printer.pp_funspec/g\"" \
   -e "\"s/!Ast_printer.d_global/Printer.pp_global/g\"" \
   -e "\"s/!Ast_printer.d_stmt/Printer.pp_stmt/g\"" \
   -e "\"s/!Ast_printer.d_exp/Printer.pp_exp/g\"" \
   -e "\"s/!Ast_printer.d_type/Printer.pp_typ/g\"" \
   -e "\"s/!Ast_printer.d_ident/Format.pp_print_string/g\"" \
   -e "\"s/!Ast_printer.d_var/Printer.pp_varinfo/g\"" \
   -e "\"s/!Ast_printer.d_block/Printer.pp_block/g\"" \
   -e "\"s/!Ast_printer.d_label/Printer.pp_label/g\"" \
   -e "\"s/!Ast_printer.d_term/Printer.pp_term/g\"" \
   -e "\"s/!Ast_printer.d_logic_type/Printer.pp_logic_type/g\"" \
   -e "\"s/!Ast_printer.d_lval/Printer.pp_lval/g\"" \
   -e "\"s/!Ast_printer.d_instr/Printer.pp_instr/g\"" \
   -e "\"s/!Ast_printer.d_attrlist/Printer.pp_attributes/g\"" \
   -e "\"s/!Ast_printer.d_file/Printer.pp_file/g\"" \
   -e "\"s/!Ast_printer.d_code_annotation/Printer.pp_code_annotation/g\"" \
   -e "\"s/!Ast_printer.d_predicate_named/Printer.pp_predicate_named/g\"" \
   -e "\"s/!Ast_printer.d_relation/Printer.pp_relation/g\"" \
   -e "\"s/!Ast_printer.d_term_lval/Printer.pp_term_lval/g\"" \
   -e "\"s/!Ast_printer.d_logic_var/Printer.pp_logic_var/g\"" \
   -e "\"s/!Ast_printer.d_unop/Printer.pp_unop/g\"" \
   -e "\"s/!Ast_printer.d_binop/Printer.pp_binop/g\"" \
\
   -e "\"s/Cil.compareLoc/Cil_datatype.Location.compare/g\"" \
   -e "\"s/Cil.d_thisLoc/Cil.pp_thisLoc/g\"" \
   -e "\"s/Cil.d_loc/Printer.pp_location/g\"" \
   -e "\"s/Cil.d_constant/Printer.pp_constant/g\"" \
   -e "\"s/Cil.d_ikind/Printer.pp_ikind/g\"" \
   -e "\"s/Cil.d_type/Printer.pp_typ/g\"" \
   -e "\"s/Cil.d_exp/Printer.pp_exp/g\"" \
   -e "\"s/Cil.d_var/Printer.pp_varinfo/g\"" \
   -e "\"s/Cil.d_lval/Printer.pp_lval/g\"" \
   -e "\"s/Cil.d_offset/Printer.pp_offset/g\"" \
   -e "\"s/Cil.d_init/Printer.pp_init/g\"" \
   -e "\"s/Cil.d_binop/Printer.pp_binop/g\"" \
   -e "\"s/Cil.d_unop/Printer.pp_unop/g\"" \
   -e "\"s/Cil.d_attr/Printer.pp_attribute/g\"" \
   -e "\"s/Cil.d_attrparam/Printer.pp_attrparam/g\"" \
   -e "\"s/Cil.d_attrlist/Printer.pp_attributes/g\"" \
   -e "\"s/Cil.d_label/Printer.pp_label/g\"" \
   -e "\"s/Cil.d_stmt/Printer.pp_stmt/g\"" \
   -e "\"s/Cil.d_block/Printer.pp_block/g\"" \
   -e "\"s/Cil.d_global/Printer.pp_global/g\"" \
   -e "\"s/Cil.d_file/Printer.pp_file/g\"" \
   -e "\"s/Cil.d_relation/Printer.pp_relation/g\"" \
   -e "\"s/Cil.d_model_info/Printer.pp_model_info/g\"" \
   -e "\"s/Cil.d_term_lval/Printer.pp_term_lval/g\"" \
   -e "\"s/Cil.d_logic_var/Printer.pp_logic_var/g\"" \
   -e "\"s/Cil.d_logic_type/Printer.pp_logic_type/g\"" \
   -e "\"s/Cil.d_identified_term/Printer.pp_identified_term/g\"" \
   -e "\"s/Cil.d_term/Printer.pp_term/g\"" \
   -e "\"s/Cil.d_term_offset/Printer.pp_term_offset/g\"" \
   -e "\"s/Cil.d_predicate_named/Printer.pp_predicate_named/g\"" \
   -e "\"s/Cil.d_identified_predicate/Printer.pp_identified_predicate/g\"" \
   -e "\"s/Cil.d_code_annotation/Printer.pp_code_annotation/g\"" \
   -e "\"s/Cil.d_funspec/Printer.pp_funspec/g\"" \
   -e "\"s/Cil.d_behavior/Printer.pp_behavior/g\"" \
   -e "\"s/Cil.d_annotation/Printer.pp_code_annotation/g\"" \
   -e "\"s/Cil.d_decreases/Printer.pp_decreases/g\"" \
   -e "\"s/Cil.d_loop_variant/Printer.pp_variant/g\"" \
   -e "\"s/Cil.d_from/Printer.pp_from/g\"" \
   -e "\"s/Cil.d_assigns/Printer.pp_assigns/g\"" \
   -e "\"s/Cil.d_allocation/Printer.pp_allocation/g\"" \
   -e "\"s/Cil.d_loop_from/Printer.pp_loop_from/g\"" \
   -e "\"s/Cil.defaultCilPrinterClass/Printer.extensible_printer/g\"" \
   -e "\"s/!Cilutil.list_last/Extlib.last/g\"" \
   -e "\"s/!Cilutil.list_iteri/Extlib.iteri/g\"" \
   -e "\"s/!Cilutil.swap/Extlib.swap/g\"" \
   -e "\"s/!Cilutil./Extlib./g\"" \
   -e "\"s/location_shift/shift/g\"" \
   -e "\"s/alignOf_int/bytesAlignOf/g\"" \
   -e "\"s/valid_enumerate_bits/enumerate_valid_bits/g\"" \
   -e "\"s/Value_aux.accept_base/!Db.Semantic_callgraph.accept_base/g\"" \
   -e "\"s/Value_aux/Value_types/g\"" \
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
