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
# lithium2beryllium: 
# convert caml source files from Frama-C Lithium to Frama-C Beryllium
#
# known miss features: don't work if a directory name contains some spaces

NAME=lithium2beryllium
ARGS=$@

DIR=

# verbosing on by default
VERBOSE="v"
CONVERT_CMDLINE="yes"

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
  tmp=""
  if [ "$CONVERT_CMDLINE" ]; then
    sedi -e "\"s/Cmdline\\\($\\\|[\\\.]\\\+\\\)/Parameters\\\1/g\"" $file
  fi
  sedi \
    -e "\"s/Dynamic\\\.apply\\\($\\\|[ ]\\\+\\\)/Dynamic.get\\\1/g\"" \
    -e "\"s/Dynamic\\\.Main\\\.extend\\\($\\\|[ ]\\\+\\\)/Db.Main.extend\\\1/g\"" \
    -e "\"s/Version\\\.dataroot\\\($\\\|[ ]\\\+\\\)/Version.datadir\\\1/g\"" \
    -e "\"s/Cil_state\\\.file/Ast.get/g\"" \
    -e "\"s/Cil_state\\\./Ast./g\"" \
    -e "\"s/Options\\\.register_plugin_init\\\($\\\|[ ]\\\+\\\)/Cmdline.run_after_exiting_stage\\\1/g\"" \
    -e "\"s/[^_]Options\\\($\\\|[\\\.]\\\+\\\)/Cmdline\\\1/g\"" \
    -e "\"s/Parameters\\\.get_selection\\\($\\\|[ ]\\\+\\\)/Parameters_factory.get_selection\\\1/g\"" \
    -e "\"s/Parameters\\\.iter_on_options\\\($\\\|[ ]\\\+\\\)/Parameters_factory.iter_on_options\\\1/g\"" \
    -e "\"s/Parameters\\\.MinValidAbsoluteAddress\\\.get\\\($\\\|[ ]\\\+\\\)/Base.min_valid_absolute_address\\\1/g\"" \
    -e "\"s/Parameters\\\.MaxValidAbsoluteAddress\\\.get\\\($\\\|[ ]\\\+\\\)/Base.max_valid_absolute_address\\\1/g\"" \
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
  -a | --all             Check subdirectories recursively
  -h | --help            Display help message
  -q | --quiet           Quiet mode (i.e. non-verbose mode)
  -s | --second          Put this option if you have previously applied this script on this code
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
    -s | --second   )     CONVERT_CMDLINE="";;
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
