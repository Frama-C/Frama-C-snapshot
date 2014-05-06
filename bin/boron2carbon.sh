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
# boron2carbon:
# convert caml source files from Frama-C Boron to Frama-C Carbon
#
# known miss features: don't work if a directory name contains some spaces

NAME=boron2carbon
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

# Do not automatically process these ones: too much false positive
#    -e "\"s/Instr\\\([\\\.a-z]*\\\)/Cil_datatype.Instr\\\1/g\"" \
#    -e "\"s/StringSet\\\([\\\.a-z]*\\\)/Datatype.String.Set\\\1/g\"" \
#    -e "\"s/Cilutil.Instr\\\([\\\.a-z]*\\\)/Cil_datatype.Instr\\\1/g\"" \

process_file ()
{
  file=$1
  if [ "$VERBOSE" ]; then
    echo "Processing file $file"
  fi
# apply change to Cilutil first in order to not conflict with changes
# supposing that Cilutil is opened: order in which sed's "-e" applies does not
# seem to be specified
  sedi \
    -e "\"s/Properties_status.identified_property/Property.t/g\"" \
\
\
    -e "\"s/Cil.prepareCfg/Cfg.prepareCFG/g\"" \
    -e "\"s/Cil.computeCFGInfo/Cfg.computeCFGInfo/g\"" \
\
\
    -e "\"s/Cilutil.locUnknown/Cil_datatype.Location.unknown/g\"" \
    -e "\"s/Cilutil.get_instrLoc/Cil_datatype.Instr.loc/g\"" \
    -e "\"s/Cilutil.get_globalLoc/Cil_datatype.Global.loc/g\"" \
    -e "\"s/Cilutil.get_stmtLoc/Cil_datatype.Stmt.loc/g\"" \
    -e "\"s/Cilutil.get_code_annotationLoc/Cil_datatype.Code_annotation.loc/g\"" \
    -e "\"s/Cilutil.get_global_annotationLoc/Cil_datatype.Global_annotation.loc/g\"" \
\
\
    -e "\"s/Cilutil.StringMap\\\([\\\.a-z]*\\\)/Datatype.String.Map\\\1/g\"" \
    -e "\"s/Cilutil.StringSet\\\([\\\.a-z]*\\\)/Datatype.String.Set\\\1/g\"" \
    -e "\"s/Cilutil.StringSet\\\([\\\.a-z]*\\\)/Datatype.String.Set\\\1/g\"" \
    -e "\"s/Cilutil.InstrHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Instr.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.StmtMap\\\([\\\.a-z]*\\\)/Cil_datatype.Stmt.Map\\\1/g\"" \
    -e "\"s/Cilutil.StmtHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Stmt.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.StmtComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Stmt\\\1/g\"" \
    -e "\"s/Cilutil.KinstrComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Kinstr\\\1/g\"" \
    -e "\"s/Cilutil.VarinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo\\\1/g\"" \
    -e "\"s/Cilutil.VarinfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.VarinfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Map\\\1/g\"" \
    -e "\"s/Cilutil.VarinfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Set\\\1/g\"" \
    -e "\"s/Cilutil.EnuminfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo\\\1/g\"" \
    -e "\"s/Cilutil.EnuminfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.EnuminfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Map\\\1/g\"" \
    -e "\"s/Cilutil.EnuminfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Set\\\1/g\"" \
    -e "\"s/Cilutil.EnumitemComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem\\\1/g\"" \
    -e "\"s/Cilutil.EnumitemHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.EnumitemMap\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Map\\\1/g\"" \
    -e "\"s/Cilutil.EnumitemSet\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Set\\\1/g\"" \
    -e "\"s/Cilutil.Compinfo\\\([\\\.a-z]*\\\)/Cil_datatype.Compinfo\\\1/g\"" \
    -e "\"s/Cilutil.LogicVarComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var\\\1/g\"" \
    -e "\"s/Cilutil.LogicVarHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.LogicVarMap\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Map\\\1/g\"" \
    -e "\"s/Cilutil.LogicVarSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Set\\\1/g\"" \
    -e "\"s/Cilutil.LogicInfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_info.Set\\\1/g\"" \
    -e "\"s/Cilutil.LogicInfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_info.Set\\\1/g\"" \
    -e "\"s/Cilutil.FieldinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo\\\1/g\"" \
    -e "\"s/Cilutil.FieldinfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.FieldinfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Map\\\1/g\"" \
    -e "\"s/Cilutil.FieldinfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Set\\\1/g\"" \
    -e "\"s/Cilutil.TypeComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Typ\\\1/g\"" \
    -e "\"s/Cilutil.TypeHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Typ.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.TypeSet\\\([\\\.a-z]*\\\)/Cil_datatype.Typ.Set\\\1/g\"" \
    -e "\"s/Cilutil.LogictypeComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type\\\1/g\"" \
    -e "\"s/Cilutil.LogictypeHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Hashtbl\\\1/g\"" \
    -e "\"s/Cilutil.LogictypeMap\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Map\\\1/g\"" \
    -e "\"s/Cilutil.LogictypeSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Set\\\1/g\"" \
    -e "\"s/Cilutil.LvalComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Lval\\\1/g\"" \
    -e "\"s/Cilutil.LvalSet\\\([\\\.a-z]*\\\)/Cil_datatype.Lval.Set\\\1/g\"" \
    -e "\"s/Cilutil.TermComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Term\\\1/g\"" \
    -e "\"s/Cilutil.TermSet\\\([\\\.a-z]*\\\)/Cil_datatype.Term.Set\\\1/g\"" \
\
\
    $file
  sedi \
    -e "\"s/prepareCfg/Cfg.prepareCFG/g\"" \
    -e "\"s/computeCFGInfo/Cfg.computeCFGInfo/g\"" \
    -e "\"s/Rooted_Code_Annotation_Before_After/Rooted_code_annotation_before_after/g\"" \
    -e "\"s/locUnknown/Cil_datatype.Location.unknown/g\"" \
    -e "\"s/pTypeSig/Cil_datatype.pTypeSig/g\"" \
    -e "\"s/get_instrLoc/Cil_datatype.Instr.loc/g\"" \
    -e "\"s/get_globalLoc/Cil_datatype.Global.loc/g\"" \
    -e "\"s/get_stmtLoc/Cil_datatype.Stmt.loc/g\"" \
    -e "\"s/Ast_info.loc_stmt/Cil_datatype.Stmt.loc/g\"" \
    -e "\"s/get_code_annotationLoc/Cil_datatype.Code_annotation.loc/g\"" \
    -e "\"s/get_global_annotationLoc/Cil_datatype.Global_annotation.loc/g\"" \
    -e "\"s/BuiltinFunctions\\\([\\\.a-z]*\\\)/Cil.Builtin_functions\\\1/g\"" \
    -e "\"s/StringMap\\\([\\\.a-z]*\\\)/Datatype.String.Map\\\1/g\"" \
    -e "\"s/InstrHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Instr.Hashtbl\\\1/g\"" \
    -e "\"s/StmtMap\\\([\\\.a-z]\\\*\\\)/Cil_datatype.Stmt.Map\\\1/g\"" \
    -e "\"s/StmtHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Stmt.Hashtbl\\\1/g\"" \
    -e "\"s/StmtComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Stmt\\\1/g\"" \
    -e "\"s/KinstrComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Kinstr\\\1/g\"" \
    -e "\"s/VarinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo\\\1/g\"" \
    -e "\"s/VarinfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Hashtbl\\\1/g\"" \
    -e "\"s/VarinfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Map\\\1/g\"" \
    -e "\"s/VarinfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Varinfo.Set\\\1/g\"" \
    -e "\"s/EnumitemComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem\\\1/g\"" \
    -e "\"s/EnumitemHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Hashtbl\\\1/g\"" \
    -e "\"s/EnumitemMap\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Map\\\1/g\"" \
    -e "\"s/EnumitemSet\\\([\\\.a-z]*\\\)/Cil_datatype.Enumitem.Set\\\1/g\"" \
    -e "\"s/EnuminfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo\\\1/g\"" \
    -e "\"s/EnuminfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Hashtbl\\\1/g\"" \
    -e "\"s/EnuminfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Map\\\1/g\"" \
    -e "\"s/EnuminfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Enuminfo.Set\\\1/g\"" \
    -e "\"s/CompinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Compinfo\\\1/g\"" \
    -e "\"s/LogicVarComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var\\\1/g\"" \
    -e "\"s/LogicVarHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Hashtbl\\\1/g\"" \
    -e "\"s/LogicVarMap\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Map\\\1/g\"" \
    -e "\"s/LogicVarSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_var.Set\\\1/g\"" \
    -e "\"s/LogicInfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_info.Set\\\1/g\"" \
    -e "\"s/LogicInfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_info.Set\\\1/g\"" \
    -e "\"s/FieldinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo\\\1/g\"" \
    -e "\"s/FieldinfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Hashtbl\\\1/g\"" \
    -e "\"s/FieldinfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Map\\\1/g\"" \
    -e "\"s/FieldinfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Fieldinfo.Set\\\1/g\"" \
    -e "\"s/TypComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Typ\\\1/g\"" \
    -e "\"s/TypHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Typ.Hashtbl\\\1/g\"" \
    -e "\"s/TypSet\\\([\\\.a-z]*\\\)/Cil_datatype.Typ.Set\\\1/g\"" \
    -e "\"s/TypeinfoComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Typeinfo\\\1/g\"" \
    -e "\"s/TypeinfoHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Typeinfo.Hashtbl\\\1/g\"" \
    -e "\"s/TypeinfoSet\\\([\\\.a-z]*\\\)/Cil_datatype.Typeinfo.Set\\\1/g\"" \
    -e "\"s/TypeinfoMap\\\([\\\.a-z]*\\\)/Cil_datatype.Typeinfo.Map\\\1/g\"" \
    -e "\"s/ExpComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Exp\\\1/g\"" \
    -e "\"s/ExpHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Exp.Hashtbl\\\1/g\"" \
    -e "\"s/ExpSet\\\([\\\.a-z]*\\\)/Cil_datatype.Exp.Set\\\1/g\"" \
    -e "\"s/ExpMap\\\([\\\.a-z]*\\\)/Cil_datatype.Exp.Map\\\1/g\"" \
    -e "\"s/LogictypeComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type\\\1/g\"" \
    -e "\"s/LogictypeHashtbl\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Hashtbl\\\1/g\"" \
    -e "\"s/LogictypeMap\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Map\\\1/g\"" \
    -e "\"s/LogictypeSet\\\([\\\.a-z]*\\\)/Cil_datatype.Logic_type.Set\\\1/g\"" \
    -e "\"s/LvalComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Lval\\\1/g\"" \
    -e "\"s/LvalSet\\\([\\\.a-z]*\\\)/Cil_datatype.Lval.Set\\\1/g\"" \
    -e "\"s/TermComparable\\\([\\\.a-z]*\\\)/Cil_datatype.Term\\\1/g\"" \
    -e "\"s/TermSet\\\([\\\.a-z]*\\\)/Cil_datatype.Term.Set\\\1/g\"" \
\
\
\
\
    -e "\"s/Logic_env.LogicBuiltin\\\([\\\.a-z]*\\\)/Logic_env.Logic_builtin\\\1/g\""  \
    -e "\"s/Logic_env.LogicBuiltinUsed\\\([\\\.a-z]*\\\)/Logic_env.Logic_builtin_used\\\1/g\"" \
    -e "\"s/Logic_env.LogicInfo\\\([\\\.a-z]*\\\)/Logic_env.Logic_info\\\1/g\"" \
    -e "\"s/Logic_env.LogicTypeInfo\\\([\\\.a-z]*\\\)/Logic_env.Logic_type_info\\\1/g\"" \
    -e "\"s/Logic_env.LogicCtorInfo\\\([\\\.a-z]*\\\)/Logic_env.Logic_ctor_info\\\1/g\"" \
\
\
    -e "\"s/Type.unit/Datatype.unit/g\"" \
    -e "\"s/Type.bool/Datatype.bool/g\"" \
    -e "\"s/Type.int/Datatype.int/g\"" \
    -e "\"s/Type.int32/Datatype.int32/g\"" \
    -e "\"s/Type.int64/Datatype.int64/g\"" \
    -e "\"s/Type.nativeint/Datatype.nativeint/g\"" \
    -e "\"s/Type.float/Datatype.float/g\"" \
    -e "\"s/Type.char/Datatype.char/g\"" \
    -e "\"s/Type.string/Datatype.string/g\"" \
    -e "\"s/Type.formatter/Datatype.formatter/g\"" \
    -e "\"s/Type.big_int/Datatype.big_int/g\"" \
    -e "\"s/Type.t_ref/Datatype.t_ref/g\"" \
    -e "\"s/Type.option/Datatype.option/g\"" \
    -e "\"s/Type.list/Datatype.list/g\"" \
    -e "\"s/Type.queue/Datatype.queue/g\"" \
    -e "\"s/Type.tuple/Datatype.pair/g\"" \
    -e "\"s/Type.func/Datatype.func/g\"" \
    -e "\"s/Type.func2/Datatype.func2/g\"" \
    -e "\"s/Type.func3/Datatype.func3/g\"" \
    -e "\"s/Type.func4/Datatype.func4/g\"" \
\
\
    -e "\"s/Kernel_function.Set/Kernel_function.Hptset/g\"" \
    -e "\"s/File.pretty/File.pretty_ast/g\"" \
    -e "\"s/File.name/File.get_name/g\"" \
    -e "\"s/State.name/State.get_name/g\"" \
    -e "\"s/State.unique_name/State.get_unique_name/g\"" \
    -e "\"s/Project.name/Project.get_name/g\"" \
    -e "\"s/Project.unique_name/Project.get_unique_name/g\"" \
\
\
    -e "\"s/Baseutil.BaseMap\\\([\\\.a-z]*\\\)/Base.Map\\\1/g\"" \
    -e "\"s/Baseutil.BaseSet\\\([\\\.a-z]*\\\)/Base.Set\\\1/g\"" \
    -e "\"s/Baseutil.BaseHashtbl\\\([\\\.a-z]*\\\)/Base.Hashtbl\\\1/g\"" \
\
\
    -e "\"s/Properties_status.compare/Property.compare/g\"" \
    -e "\"s/Properties_status.equal/Property.equal/g\"" \
    -e "\"s/Properties_status.hash/Property.hash/g\"" \
    -e "\"s/Properties_status.pretty /Property.pretty /g\"" \
    -e "\"s/Properties_status.identified_\\\([\\\.a-z]*\\\)/Property.identified_\\\1/g\"" \
    -e "\"s/Properties_status.IP\\\([\\\A-Za-z]*\\\)/Property.IP\\\1/g\"" \
    -e "\"s/Properties_status.PK\\\([\\\A-Za-z]*\\\)/Property.PK\\\1/g\"" \
    -e "\"s/Properties_status.predicate_kind/Property.predicate_kind/g\"" \
    -e "\"s/Properties_status.get_ip_kinstr/Property.get_kinstr/g\"" \
    -e "\"s/Properties_status.get_ip_kf/Property.get_kf/g\"" \
    -e "\"s/Properties_status.get_ip_behavior/Property.get_behavior/g\"" \
\
\
    -e "\"s/let descr/let help/g\"" \
\
\
    -e "\"s/Kernel_type.string_set/Datatype.String.Set.ty/g\"" \
    -e "\"s/Kernel_type.cil_file/Cil_datatype.File.ty/g\"" \
    -e "\"s/Kernel_type.cabs_file/Cil_datatype.Cabs_file.ty/g\"" \
\
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
  echo "Usage: boron2carbon [options | directories]

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
