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

#!/bin/sh -e

# Convenience wrapper for small runs of E-ACSL Frama-C plugin

# Print a message to STDERR and exit. If the second argument (exit code)
# is provided and it is '0' then do nothing.
error () {
  if [ -z "$2" ] || ! [ "$2" = 0 ]; then
    echo "e-acsl-gcc: fatal error: $1" 1>&2
    exit 1;
  fi
}

# Check if a given executable name can be found by in the PATH
has_tool() {
  which "$@" >/dev/null 2>&1 && return 0 || return 1
}

# Check if a given executable name is indeed an executable or can be found
# in the $PATH. Abort the execution if not.
check_tool() {
   { has_tool "$1" || test -e "$1"; } || error "No executable $1 found";
}

# Check whether getopt utility supports long options
check_getopt() {
  local out="$(getopt -l "ab:,cd" -o "x:,y" -- --ab 1 -x 1 --cd  -y \
    | sed "s/[ \']//g")"
  error "system getopt has no support for long option processing" $?
  if ! [ "$out" = "--ab1-x1--cd-y--" ]; then
    error "unexpected output of system getopt" 1
  fi
}

# Check if $1 is positive integer and whether $1 is greater than $2
# Returns $1 is the above holds, otherwise return
#  '-' if $1 is not a positive integer
#  '<' if $1 is a positive integer but it is less than $2
# NB: No checking is done for $2
is_number() {
  local n="$1"
  local lim="$2"

  if [ "$n" -eq "$n" ] 2>/dev/null; then
    if [ "$n" -lt "$lim" ]; then
      echo '<'
    else
      echo $n
    fi
  else
    echo '-'
  fi
}

# Portable realpath using pwd
realpath() {
  if [ -e "$1" ]; then
    if [ -d "$1" ]; then
      (cd "$1" && pwd)
    else
      local name=$(basename "$1")
      local dir=$(cd $(dirname "$1") && pwd)
      echo $dir/$name
    fi
    return 0
  else
    echo "realpath: no such file or directory: '$1'" 1>&2
    return 1
  fi
}

# Split a comma-separated string into a space-separated string, remove
# all duplicates and trailing, leading or multiple spaces
tokenize() {
  echo -n "$@" \
    | sed -e 's/\s//g' -e 's/,/ /g' -e 's/\s\+/\n/g' \
    | sort -u \
    | tr '\n' ' ' \
    | sed 's/\s*$//'
}

# Given a token (first argument) and a list (remaining arguments)
# evaluate to true if the token is in the list, and to false otherwise
has_token() {
  local token="$1"
  local opt
  shift
  for opt in $@; do
    [ "$opt" = "$token" ] && return 0
  done
  return 1
}

# Filter out a token given by the first argument from the list of tokens
# given by the remaining arguments
shift_token() {
  local token="$1"
  shift
  for opt in $@; do
    [ "$opt" = "$token" ] && true || echo $opt
  done
}

# Generate option string for RTE plugin based on the value given via --rte
# and --rte-select flags
rte_options() {
  # Frama-C assertions
  local fc_options="signed-overflow unsigned-overflow \
    signed-downcast unsigned-downcast"
  # RTE assertions
  local rte_options="div float-to-int mem pointer-call shift \
    trivial-annotations"
  # RTE assertions which are negated in all cases except when
  # explicitly specified
  # Option supported by RTE but unsupported in E-ACSL, should
  # always be negated
  local rte_options_unsupported="precond"
  local rte_options_explicit="trivial-annotations"
  local generated="-rte" # Generated Frama-C options

  # Clean-up option strings
  local full_options="$fc_options $rte_options"
  local input_asserts="$(tokenize "$1")"
  local fselect="$2"

  # If there is 'all' keyword found enable all assertions
  if has_token all $input_asserts; then
    asserts="$full_options"
    for opt in $rte_options_explicit; do
      if ! has_token $opt $input_asserts; then
        asserts="$(shift_token $opt $asserts)"
      fi
    done
  else
    asserts="$input_asserts"
  fi

  if [ -n "$asserts" ]; then
    # Check input options
    local opt
    for opt in $asserts; do
      # Check whether a given input option exists, i.e., found in $full_options
      if ! has_token $opt $full_options; then
        echo "$opt"
        return 1
      fi
    done

    local prefix
    # Generate assertion options for Frama-C (i.e., -warn-* or -no-warn-*)
    for opt in $fc_options; do
      has_token $opt $asserts && prefix="-warn" || prefix="-no-warn"
      generated="$generated $prefix-$opt"
    done

    # Generate assertion options for RTE (i.e., -rte-* or -rte-no-*)
    for opt in $rte_options $rte_options_unsupported; do
      has_token $opt $asserts && prefix="-rte" || prefix="-rte-no"
      generated="$generated $prefix-$opt"
    done

    # Pass -rte-select option of RTE
    if [ -n "$fselect" ]; then
      fselect="$(echo $fselect | sed 's/\s//g')"
      generated="$generated -rte-select=$fselect"
    fi

    echo $generated -then
  fi
  return 0
}

# Output -D flags enabling a given E_ACSL memory model
mmodel_features() {
  local model="$1"

  # Memory model
  case $model in
    bittree) flags="-DE_ACSL_BITTREE_MMODEL" ;;
    segment) flags="-DE_ACSL_SEGMENT_MMODEL" ;;
    *) error "Memory model '$model' is not available in this distribution" ;;
  esac

  # Temporal analysis
  if [ -n "$OPTION_TEMPORAL" ]; then
    flags="$flags -DE_ACSL_TEMPORAL"
  fi

  # Trigger failures in assertions
  if [ -n "$OPTION_KEEP_GOING" ]; then
    flags="$flags -DE_ACSL_NO_ASSERT_FAIL"
  fi

  # Enable debug mode
  if [ -n "$OPTION_RT_DEBUG" ]; then
    flags="$flags -DE_ACSL_DEBUG"
  fi

  # Set stack shadow size
  if [ -n "$OPTION_STACK_SIZE" ]; then
    flags="$flags -DE_ACSL_STACK_SIZE=$OPTION_STACK_SIZE"
  fi

  # Set heap shadow size
  if [ -n "$OPTION_HEAP_SIZE" ]; then
    flags="$flags -DE_ACSL_HEAP_SIZE=$OPTION_HEAP_SIZE"
  fi

  # Set runtime verosity flags
  if [ -n "$OPTION_RT_VERBOSE" ]; then
    flags="$flags -DE_ACSL_VERBOSE -DE_ACSL_DEBUG_VERBOSE"
  fi

  if [ -n "$OPTION_FAIL_WITH_CODE" ]; then
    flags="$flags -DE_ACSL_FAIL_EXITCODE=$OPTION_FAIL_WITH_CODE "
  fi

  if [ -n "$OPTION_WEAK_VALIDITY" ]; then
    flags="$flags -DE_ACSL_WEAK_VALIDITY"
  fi

  if [ -n "$OPTION_FREE_VALID_ADDRESS" ]; then
    flags="$flags -DE_ACSL_FREE_VALID_ADDRESS"
  fi

  if [ -n "$OPTION_EXTERNAL_ASSERT" ]; then
    flags="$flags -DE_ACSL_EXTERNAL_ASSERT"
  fi
  echo $flags
}

# Check if system getopt supports long option processing
check_getopt;

# Getopt options
LONGOPTIONS="help,compile,compile-only,debug:,ocode:,oexec:,verbose:,
  frama-c-only,extra-cpp-args:,frama-c-stdlib,full-mmodel,gmp,quiet,logfile:,
  ld-flags:,cpp-flags:,frama-c-extra:,memory-model:,keep-going,
  frama-c:,gcc:,e-acsl-share:,instrumented-only,rte:,oexec-e-acsl:,
  print-mmodels,rt-debug,rte-select:,then,e-acsl-extra:,check,fail-with-code:,
  temporal,weak-validity,stack-size:,heap-size:,rt-verbose,free-valid-address,
  external-assert:"
SHORTOPTIONS="h,c,C,d:,D,o:,O:,v:,f,E:,L,M,l:,e:,g,q,s:,F:,m:,I:,G:,X,a:,T,k,V"
# Prefix for an error message due to wrong arguments
ERROR="ERROR parsing arguments:"

# Variables holding getopt options
OPTION_CFLAGS=                           # Compiler flags
OPTION_CPPFLAGS=                         # Preprocessor flags
OPTION_LDFLAGS=                          # Linker flags
OPTION_FRAMAC="frama-c"                  # Frama-C executable name
OPTION_CC="gcc"                          # GCC executable name
OPTION_ECHO="set -x"                     # Echo executed commands to STDOUT
OPTION_INSTRUMENT=1                      # Perform E-ACSL instrumentation
OPTION_DEBUG=                            # Set Frama-C debug flag
OPTION_VERBOSE=                          # Set Frama-C verbose flag
OPTION_COMPILE=                          # Compile instrumented program
OPTION_RT_DEBUG=                         # Enable runtime debug features
OPTION_RT_VERBOSE=                       # Set runtime verbosity level
OPTION_OUTPUT_CODE="a.out.frama.c"       # Name of the translated file
OPTION_OUTPUT_EXEC="a.out"               # Generated executable name
OPTION_EACSL_OUTPUT_EXEC=""              # Name of E-ACSL executable
OPTION_EACSL="-e-acsl"                   # Specifies E-ACSL run
OPTION_FRAMA_STDLIB="-no-frama-c-stdlib" # Use Frama-C stdlib
OPTION_FULL_MMODEL=                      # Instrument as much as possible
OPTION_GMP=                              # Use GMP integers everywhere
OPTION_EACSL_MMODELS="segment"           # Memory model used
OPTION_EACSL_SHARE=                      # Custom E-ACSL share directory
OPTION_INSTRUMENTED_ONLY=                # Do not compile original code
OPTION_TEMPORAL=                         # Enable temporal analysis
OPTION_WEAK_VALIDITY=                    # Use notion of weak validity
OPTION_RTE=                              # Enable assertion generation
OPTION_FAIL_WITH_CODE=                   # Exit status code for failures
OPTION_CHECK=                            # Check AST integrity
OPTION_FRAMAC_CPP_EXTRA=""               # Extra CPP flags for Frama-C
OPTION_FREE_VALID_ADDRESS="" # Fail if NULL is used as input to free function
OPTION_RTE_SELECT=       # Generate assertions for these functions only
OPTION_THEN=             # Adds -then in front of -e-acsl in FC command.
OPTION_STACK_SIZE=32     # Size of a heap shadow space (in MB)
OPTION_HEAP_SIZE=128     # Size of a stack shadow space (in MB)
OPTION_KEEP_GOING=       # Report failing assertions but do not abort execution
OPTION_EXTERNAL_ASSERT="" # Use custom definition of assert function

SUPPORTED_MMODELS="bittree,segment" # Supported memory model names
MIN_STACK=16 # Minimal size of a tracked program stack
MIN_HEAP=64 # Minimal size of a tracked program heap

manpage() {
  printf "e-acsl-gcc.sh - instrument and compile C files with E-ACSL
Usage: e-acsl-gcc.sh [options] files
Options:
  -h         show this help page
  -c         compile instrumented code
  -l         pass additional options to the linker
  -e         pass additional options to the prepreprocessor
  -E         pass additional arguments to the Frama-C preprocessor
  -p         output the generated code to STDOUT
  -o <file>  output the generated code to <file> [a.out.frama.c]
  -O <file>  output the generated executables to <file> [a.out, a.out.e-acsl]
  -M         maximize memory-related instrumentation
  -g         always use GMP integers instead of C integral types
  -q         suppress any output except for errors and warnings
  -s <file>  redirect all output to <file>
  -I <file>  specify Frama-C executable [frama-c]
  -G <file>  specify C compiler executable [gcc]

Notes:
  This help page shows only basic options.
  See man (1) e-acsl-gcc.sh for full up-to-date documentation.\n"
  exit 1
}

# Base dir of this script
BASEDIR="$(realpath `dirname $0`)"
# Directory with contrib libraries of E-ACSL
LIBDIR="$BASEDIR/../lib"

# Run getopt
ARGS=`getopt -n "$ERROR" -l "$LONGOPTIONS" -o "$SHORTOPTIONS" -- "$@"`

# Print and exit if getopt fails
if [ $? != 0 ]; then
  exit 1;
fi

# Set all options in $@ before -- and other after
eval set -- "$ARGS"

# Switch statements for other options
for i in $@
do
  case "$i" in
    # Do compile instrumented code
    --help|-h)
      shift;
      manpage;
    ;;
    # Do not echo commands to STDOUT
    # Set log and debug flags to minimal verbosity levels
    --quiet|-q)
      shift;
      OPTION_ECHO=
      OPTION_DEBUG="-e-acsl-debug 0"
      OPTION_VERBOSE="-e-acsl-verbose 0"
    ;;
    # Redirect all output to a given file
    --logfile|-s)
      shift;
      exec > $1
      exec 2> $1
      shift;
    ;;
    # Enable runtime debug features, i.e., compile unoptimized executable
    # with assertions, extra checks and other debug features
    --rt-debug|-D)
      shift
      OPTION_RT_DEBUG=1
      OPTION_CHECK=1
    ;;
    --rt-verbose|-V)
      shift;
      OPTION_RT_VERBOSE=1
    ;;
    # Pass an option to a Frama-C invocation
    --frama-c-extra|-F)
      shift;
      FRAMAC_FLAGS="$FRAMAC_FLAGS $1"
      shift;
    ;;
    # Do compile instrumented code
    --compile|-c)
      shift;
      OPTION_COMPILE=1
    ;;
    # Set Frama-C debug flag
    --debug|-d)
      shift;
      if [ "$1" -eq "$1" ] 2>/dev/null; then
        OPTION_DEBUG="-e-acsl-debug $1"
      else
        error "-d|--debug option requires integer argument"
      fi
      shift;
    ;;
    # Set Frama-C verbose flag
    --verbose|-v)
      shift;
      if [ "$1" -eq "$1" ] 2>/dev/null; then
        OPTION_VERBOSE="-e-acsl-verbose $1"
      else
        error "-v|--verbose option requires integer argument"
      fi
      shift;
    ;;
    # Specify the name of the default source file where instrumented
    # code is to be written
    --ocode|-o)
      shift;
      OPTION_OUTPUT_CODE="$1"
      shift
    ;;
    # Specify the base name of the executable generated from the
    # instrumented and non-instrumented sources.
    --oexec|-O)
      shift;
      OPTION_OUTPUT_EXEC="$1"
      shift
    ;;
    # Specify the output name of the E-ACSL generated executable
    --oexec-e-acsl)
      shift;
      OPTION_EACSL_OUTPUT_EXEC="$1"
      shift;
    ;;
    # Additional CPP arguments
    --extra-cpp-args|-E)
      shift;
      OPTION_FRAMAC_CPP_EXTRA="$OPTION_FRAMAC_CPP_EXTRA $1"
      shift;
    ;;
    # Additional flags passed to the linker
    --ld-flags|-l)
      shift;
      OPTION_LDFLAGS="$OPTION_LDFLAGS $1"
      shift;
    ;;
    # Additional flags passed to the pre-processor (compile-time)
    --cpp-flags|-e)
      shift;
      OPTION_CPPFLAGS="$OPTION_CPPFLAGS $1"
      shift;
    ;;
    # Do not perform the instrumentation, only compile the provided sources
    # This option assumes that the source files provided at input have
    # already been instrumented
    --compile-only|-C)
      shift;
      OPTION_INSTRUMENT=
      OPTION_COMPILE="1"
    ;;
    # Run only Frama-C related instrumentation
    --frama-c-only|-f)
      shift;
      OPTION_EACSL=
    ;;
    # Do not compile original source file
    --instrumented-only|-X)
      shift;
      OPTION_INSTRUMENTED_ONLY=1
    ;;
    # Do use Frama-C stdlib, which is the default behaviour of Frama-C
    --frama-c-stdlib|-L)
      shift;
      OPTION_FRAMA_STDLIB="-frama-c-stdlib"
    ;;
    # Use as much memory-related instrumentation as possible
    -M|--full-mmodel)
      shift;
      OPTION_FULL_MMODEL="-e-acsl-full-mmodel"
    ;;
    # Use GMP everywhere
    -g|--gmp)
      shift;
      OPTION_GMP="-e-acsl-gmp-only"
    ;;
    # Supply Frama-C executable name
    -I|--frama-c)
      shift;
      OPTION_FRAMAC="$(which $1)"
      shift;
    ;;
    # Supply GCC executable name
    -G|--gcc)
      shift;
      OPTION_CC="$(which $1)"
      shift;
    ;;
    # Specify EACSL_SHARE directory (where C runtime library lives) by hand
    # rather than compute it
    --e-acsl-share)
      shift;
      OPTION_EACSL_SHARE="$1"
      shift;
    ;;
    # Runtime assertion generation
    --rte|-a)
      shift;
      OPTION_RTE="$1"
      shift;
    ;;
    # Runtime assertion generation for given functions only
    --rte-select|-A)
      shift;
      OPTION_RTE_SELECT="$1"
      shift;
    ;;
    # Check AST integrity (mostly for developers of E-ACSL)
    --check)
      OPTION_CHECK=1
      FRAMAC_FLAGS="-check $FRAMAC_FLAGS"
      shift;
    ;;
    # Enable instrumentations of temporal validity analysis
    -T|--temporal)
      shift;
      OPTION_TEMPORAL=-e-acsl-temporal-validity
    ;;
    # A memory model  (or models) to link against
    -m|--memory-model)
      shift;
      # Convert comma-separated string into white-space separated string
      OPTION_EACSL_MMODELS="`echo $1 | sed -s 's/,/ /g'`"
      shift;
    ;;
    # Print names of the supported memody models.
    --print-mmodels)
      shift;
      echo $SUPPORTED_MMODELS
      exit 0
    ;;
    # Separate extra Frama-C flags from e-acsl launch with -then.
    --then)
      shift;
      OPTION_THEN=-then
      FRAMAC_FLAGS="-e-acsl-prepare $FRAMAC_FLAGS"
    ;;
    # Extra E-ACSL options
    --e-acsl-extra)
      shift;
      OPTION_EACSL="$1 $OPTION_EACSL"
      shift;
    ;;
    # Report failing assertions but do not abort execution
    -k|--keep-going)
      shift;
      OPTION_KEEP_GOING=1
    ;;
    # Exit with a given code on assertion failure instead of raising abort
    --fail-with-code)
      shift;
      if [ "$1" -eq "$1" ] 2>/dev/null; then
        OPTION_FAIL_WITH_CODE="$1"
      else
        error "--fail-with-code option requires integer argument"
      fi
      shift;
    ;;
    # Use notion of weak validity
    --free-valid-address)
      shift;
      OPTION_FREE_VALID_ADDRESS=1
    ;;
    # Use notion of weak validity
    --weak-validity)
      shift;
      OPTION_WEAK_VALIDITY=1
    ;;
    # Set heap shadow size
    --heap-size)
      shift;
      zone_size="$(is_number "$1" $MIN_HEAP)"
      case $zone_size in
        '-') error "invalid number: '$1'" ;;
        '<') error "heap limit less than minimal size [$MIN_HEAP"]
          ;;
        *) OPTION_HEAP_SIZE=$zone_size ;;
      esac;
      shift;
    ;;
    # Set stack shadow size
    --stack-size)
      shift;
      zone_size="$(is_number "$1" $MIN_STACK)"
      case $zone_size in
        '-') error "invalid number: '$1'" ;;
        '<') error "stack limit less than minimal size [$MIN_STACK"] ;;
        *) OPTION_STACK_SIZE=$zone_size ;;
      esac;
      shift;
    ;;
    # Custom runtime assert function
    --external-assert)
      shift;
      OPTION_EXTERNAL_ASSERT="$1"
      shift;
    ;;
  esac
done
shift;

# Bail if no files to translate are given
if [ -z "$1" ]; then
  error "no input files";
fi

# Check Frama-C and GCC executable names
check_tool "$OPTION_FRAMAC"
check_tool "$OPTION_CC"

# Frama-C directories
FRAMAC="$OPTION_FRAMAC"
: ${FRAMAC_SHARE:="`$FRAMAC -print-share-path`"}
: ${FRAMAC_PLUGIN:="`$FRAMAC -print-plugin-path`"}

# Check if this is a development or an installed version
if [ -f "$BASEDIR/../E_ACSL.mli" ]; then
  # Development version
  DEVELOPMENT="$(realpath "$BASEDIR/..")"
  # Check if the project has been built, as if this is a non-installed
  # version that has not been built Frama-C will fallback to an installed one
  # for instrumentation but still use local RTL
  error "Plugin in $DEVELOPMENT not compiled" \
    `test -f "$DEVELOPMENT/META.frama-c-e_acsl" -o \
          -f "$FRAMAC_PLUGIN/META.frama-c-e_acsl"; echo $?`
  EACSL_SHARE="$DEVELOPMENT/share/e-acsl"
  # Add the project directory to FRAMAC_PLUGINS,
  # otherwise Frama-C uses an installed version
  if test -f "$DEVELOPMENT/META.frama-c-e_acsl"; then
    FRAMAC_FLAGS="-add-path=$DEVELOPMENT/top -add-path=$DEVELOPMENT $FRAMAC_FLAGS";
  fi
else
  # Installed version. FRAMAC_SHARE should not be used here as Frama-C
  # and E-ACSL may not be installed to the same location
  EACSL_SHARE="$BASEDIR/../share/frama-c/e-acsl/"
fi

# Architecture-dependent flags. Since by default Frama-C uses 32-bit
# architecture we need to make sure that same architecture is used for
# instrumentation and for compilation.
MACHDEPFLAGS="`getconf LONG_BIT`"
# Check if getconf gives out the value accepted by Frama-C/GCC
echo "$MACHDEPFLAGS" | grep '16\|32\|64' 2>&1 >/dev/null \
  || error "$MACHDEPFLAGS-bit architecture not supported"
# -machdep option sent to Frama-C
MACHDEP="-machdep gcc_x86_$MACHDEPFLAGS"
# Macro for correct preprocessing of Frama-C generated code
CPPMACHDEP="-D__FC_MACHDEP_X86_$MACHDEPFLAGS"
# GCC machine option
GCCMACHDEP="-m$MACHDEPFLAGS"

# RTE flags
RTE_FLAGS="$(rte_options "$OPTION_RTE" "$OPTION_RTE_SELECT")"
error "Invalid argument $1 to --rte|-a option" $?

# Frama-C and related flags
# Additional flags passed to Frama-C preprocessor via `-cpp-extra-args`
#  -std=c99 -D_DEFAULT_SOURCE: use C99 + default features. This is important
#    in OSX which by default enables `blocks` unsupported by Frama-C
#  -D__NO_CTYPE: prevent `isupper` (and similar functions) from being used as
#    macros, otherwise E-ACSL cannot track them at runtime
FRAMAC_CPP_EXTRA="\
 -std=c99 -D_DEFAULT_SOURCE -D__NO_CTYPE $CPPMACHDEP\
 $OPTION_FRAMAC_CPP_EXTRA"
EACSL_MMODEL="$OPTION_EACSL_MMODEL"

# Re-set EACSL_SHARE  directory is it has been given by the user
if [ -n "$OPTION_EACSL_SHARE" ]; then
  EACSL_SHARE="$OPTION_EACSL_SHARE"
fi

if [ -n "$OPTION_THEN" ]; then
  FRAMAC_FLAGS="-e-acsl-share=$EACSL_SHARE $FRAMAC_FLAGS";
fi

# Select optimization flags for both instrumented and noon-instrumented code
# compilation
if [ -n "$OPTION_RT_DEBUG" ]; then
  OPT_CFLAGS="-g3 -O0 -fno-omit-frame-pointer"
else
  OPT_CFLAGS="-g -O2"
fi

# Gcc and related flags
CC="$OPTION_CC"
CFLAGS="$OPTION_CFLAGS
  -std=c99 $GCCMACHDEP $OPT_CFLAGS
  -fno-builtin -fno-merge-constants
  -Wall \
  -Wno-long-long \
  -Wno-attributes \
  -Wno-nonnull \
  -Wno-undef \
  -Wno-unused \
  -Wno-unused-function \
  -Wno-unused-result \
  -Wno-unused-value \
  -Wno-unused-function \
  -Wno-unused-variable \
  -Wno-unused-but-set-variable \
  -Wno-implicit-function-declaration \
  -Wno-empty-body"

# Disable extra warning for clang
if [ "`basename $CC`" = 'clang' ]; then
  CFLAGS="-Wno-unknown-warning-option \
    -Wno-extra-semi \
    -Wno-tautological-compare \
    -Wno-gnu-empty-struct \
    -Wno-incompatible-pointer-types-discards-qualifiers"
fi

CPPFLAGS="$OPTION_CPPFLAGS"
LDFLAGS="$OPTION_LDFLAGS"

# Extra Frama-C Flags E-ACSL needs
FRAMAC_FLAGS="$FRAMAC_FLAGS -variadic-no-translation"

# C, CPP and LD flags for compilation of E-ACSL-generated sources
EACSL_CFLAGS="$OPTION_EXTERNAL_ASSERT"
EACSL_CPPFLAGS="-I$EACSL_SHARE"
EACSL_LDFLAGS="$LIBDIR/libeacsl-dlmalloc.a $LIBDIR/libeacsl-gmp.a -lm"

# Output file names
OUTPUT_CODE="$OPTION_OUTPUT_CODE" # E-ACSL instrumented source
OUTPUT_EXEC="$OPTION_OUTPUT_EXEC" # Output name of the original executable

# Output name of E-ACSL-modified executable
if [ -z "$OPTION_EACSL_OUTPUT_EXEC" ]; then
  EACSL_OUTPUT_EXEC="$OPTION_OUTPUT_EXEC.e-acsl"
else
  EACSL_OUTPUT_EXEC="$OPTION_EACSL_OUTPUT_EXEC"
fi

# Build E-ACSL plugin argument string
if [ -n "$OPTION_EACSL" ]; then
  EACSL_FLAGS="
    $OPTION_THEN
    $OPTION_EACSL
    $OPTION_GMP
    $OPTION_FULL_MMODEL
    $OPTION_TEMPORAL
    $OPTION_VERBOSE
    $OPTION_DEBUG
    -e-acsl-share="$EACSL_SHARE"
    -then-last"
fi

# Instrument
if [ -n "$OPTION_INSTRUMENT" ]; then
  ($OPTION_ECHO; \
    $FRAMAC \
    $FRAMAC_FLAGS \
    $MACHDEP \
    -cpp-extra-args="$FRAMAC_CPP_EXTRA" \
    $OPTION_FRAMA_STDLIB \
    "$@" \
    $RTE_FLAGS \
    $EACSL_FLAGS \
    -print -ocode "$OPTION_OUTPUT_CODE");
    error "aborted by Frama-C" $?;
fi

# Compile
if [ -n "$OPTION_COMPILE" ]; then
  # Compile original source code
  # $OPTION_INSTRUMENT is set -- both, instrumented and original, sources are
  # available. Do compile the original program unless instructed to not do so
  # by a user
  if [ -n "$OPTION_INSTRUMENT" ]; then
    if [ -z "$OPTION_INSTRUMENTED_ONLY" ]; then
      ($OPTION_ECHO; $CC $CPPFLAGS $CFLAGS "$@" -o "$OUTPUT_EXEC" $LDFLAGS);
      error "fail to compile/link un-instrumented code" $?;
    fi
  # If $OPTION_INSTRUMENT is unset then the sources are assumed to be already
  # instrumented, so skip compilation of the original files
  else
    OUTPUT_CODE="$@"
  fi

  # Compile and link E-ACSL-instrumented file with all models specified
  for model in $OPTION_EACSL_MMODELS; do
    # If multiple models are specified then the generated executable
    # is appended a '-MODEL' suffix, where MODEL is the name of the memory
    # model used
    if ! [ "`echo $OPTION_EACSL_MMODELS | wc -w`" = 1 ]; then
      OUTPUT_EXEC="$EACSL_OUTPUT_EXEC-$model"
    else
      OUTPUT_EXEC="$EACSL_OUTPUT_EXEC"
    fi
    # RTL sources
    EACSL_RTL="$EACSL_SHARE/e_acsl_rtl.c"
    EACSL_MMODEL_FEATURES="$(mmodel_features $model)"
    ($OPTION_ECHO;
     $CC \
       $EACSL_MMODEL_FEATURES \
       $CFLAGS $CPPFLAGS \
       $EACSL_CFLAGS $EACSL_CPPFLAGS \
       -o "$OUTPUT_EXEC" \
       "$OUTPUT_CODE" \
       $EACSL_RTL \
       $LDFLAGS \
       $EACSL_LDFLAGS)
    error "fail to compile/link instrumented code" $?
  done
fi
exit 0;
