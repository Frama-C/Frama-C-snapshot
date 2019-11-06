#! /bin/sh
##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2019                                               #
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

#
# Converts a Frama-C plugin from Frama-C 19 Potassium to Frama-C 20 Calcium,
# on a best-efforts basis (no guarantee that the result is fully compatible).
#
# known missing features:
# - doesn't work if a directory name contains spaces
# - doesn't follow symbolic links to directories

ARGS=$@

DIR=

# verbosing on by default
VERBOSE="v"

sedi ()
{
  if [ -n "`sed --help 2> /dev/null | grep \"\\-i\" 2> /dev/null`" ]; then
    sed -i "$@"
  else
      # option '-i' is not recognized by sed: use a tmp file
    new_temp=`mktemp /tmp/frama-c.XXXXXXX` || exit 1
    sed "$@" > $new_temp
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
   -e "s/Transitioning\.Char\./Char./g" \
   -e "s/Transitioning\.List\./List./g" \
   -e "s/Transitioning\.Stack\./Stack./g" \
   -e "s/Transitioning\.String\./String./g" \
   -e "s/Cil\.Eid/Cil_const\.Eid/g" \
   -e "s/Cil\.Sid/Cil_const\.Sid/g" \
   -e "s/Cil\.mkCompInfo/Cil_const\.mkCompInfo/g" \
   -e "s/Cil\.copyCompInfo/Cil_const\.copyCompInfo/g" \
   -e "s/Cil\.visitor_behavior/Visitor_behavior\.t/g" \
   -e "s/Cil\.inplace_visit/Visitor_behavior\.inplace/g" \
   -e "s/Cil\.copy_visit/Visitor_behavior\.copy/g" \
   -e "s/Cil\.refresh_visit/Visitor_behavior\.refresh/g" \
   -e "s/Cil\.is_fresh_behavior/Visitor_behavior\.is_fresh/g" \
   -e "s/Cil\.is_copy_behavior/Visitor_behavior\.is_copy/g" \
   -e "s/Cil\.reset_behavior_/Visitor_behavior\.Reset\./g" \
   -e "s/Cil\.get_original_/Visitor_behavior\.Get_orig\./g" \
   -e "s/Cil\.get_/Visitor_behavior\.Get\./g" \
   -e "s/Cil\.set_orig_/Visitor_behavior\.Set_orig\./g" \
   -e "s/Cil\.set_/Visitor_behavior\.Set\./g" \
   -e "s/Cil\.unset_orig_/Visitor_behavior\.Unset_orig\./g" \
   -e "s/Cil\.unset_/Visitor_behavior\.Unset\./g" \
   -e "s/Cil\.memo_/Visitor_behavior\.Memo\./g" \
   -e "s/Cil\.iter_visitor_/Visitor_behavior\.Iter\./g" \
   -e "s/Cil\.fold_visitor_/Visitor_behavior\.Fold\./g" \
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
