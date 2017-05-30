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

# Convenience script for running tests with E-ACSL. Given a source file the
# sequence is as follows:
#   1. Instrument and compile a given source file with `e-acsl-gcc.sh`
#   2. Run executables generated from the instrumented source
#      and check that it does not fail

# Test failure is detected if:
#   - `e-acsl-gcc.sh` fails (i.e., instrumentation- or compile-time failure)
#   - A generated executable exists with a non-zero status
#   - The run of this executable stops with a non-zero exit status

# Arguments:
#   $1 - base name of a test source file excluding its extension (e.g., addrOf)
#   $2 - base name of a test suite directory the test file is located in
#     (e.g., runtime). Provided that ROOT is the directory
#     holding an E-ACSL repository there should be either:
#       * $ROOT/test/runtime/addrOf.i or
#       * $ROOT/test/runtime/addrOf.c
#   $3 - if specified, re-run test sequence with -e-acsl-gmp-only flag
#   $4 - extra flags for a `e-acsl-gcc.sh` run
#   $5 - names of memory models to use
#   $6 - if specified print extra messages and retain log files (DEBUG option)

set -e

TEST="$1"   # Base name of the test file
PREFIX="$2" # Test suite directory (e.g., runtime)
GMP="$3"    # Whether to issue an additional run with -e-acsl-gmp-only
EXTRA="$4"  # Extra e-acsl-gcc.sh flags
MODELS="$5" # Specify models
DEBUG="$6"  # Debug option

EACSLGCC="$(dirname $0)/e-acsl-gcc.sh $EXTRA" # E-ACSL wrapper script
MODELS=${5-"segment bittree"} # Memory models to use (unless specified)

ROOTDIR="`readlink -f $(dirname $0)/../`" # Root directory of the repository
TESTDIR="$ROOTDIR/tests/$PREFIX" # Test suite directory
RESDIR=$TESTDIR/result # Result directory within the test suite
TESTFILE=`ls $TESTDIR/$TEST.[ic]` # Source test file

LOG="$RESDIR/$TEST.testrun" # Base name for log files
OUT="$RESDIR/gen_$TEST"     # Base name for instrumented files
RUNS=1                      # Nth run of `run_test` function

# Print a message if the DEBUG flag is set
debug() {
  if [ -n "$DEBUG" ]; then
    echo " ** DEBUG: $1" 1>&2
  fi
}

# Clean up log/output files unless DEBUG is set
clean() {
  if [ -z "$DEBUG" ]; then
    rm -f $LOG.* $OUT.*
  fi
}

# Error reporting
#  $1 - error message
#  $2 - log file. If supplied, the contents of the log file are dumped to
#     STDERR with each line prefixed by ' > '.
error() {
  echo "Error: $1" 1>&2
  if [ -n "$2" ]; then
    cat $2 2>&1 | sed 's/^/ > /' 1>&2
    debug "See $2 for details"
  fi
  exit 1
}

debug "Test: $PREFIX/$TEST with $MODELS"

# Do a clean-up on exit
trap "clean" EXIT HUP INT QUIT TERM

# Run executable and report results
#  $1 - path to an executable
#  $2 - path to a log file
#  $3 - memory model the executable has been linked against
run_executable() {
  local executable="$1"
  local log="$2"
  local model="$3"

  debug "Run: $executable"
  debug "Log: $log"

  if ! `$executable > $log 2>&1`; then
    error "[$3 model] Runtime failure in test case '$TEST'" $log
  fi
}

# Instrument the given test using e-acsl-gcc.sh and check that the generated
# executable stops with a zero exit status
run_test() {
  local ocode=$OUT.$RUNS.c # Generated source file
  local logfile=$LOG.$RUNS.elog # Log file for e-acsl-gcc.sh output
  local oexec=$OUT.$RUNS.out # Generated executable name
  local oexeclog=$LOG.$RUNS.rlog # Log for executable output
  local extra="$1" # Additional arguments to e-acsl-gcc.sh

  # Command for instrumenting the source file
  COMMAND="$EACSLGCC $TESTFILE --ocode=$ocode --logfile=$logfile $extra"

  debug "Run $COMMAND"
  $COMMAND || error "Command $COMMAND failed" "$logfile"

  # Compile instrumented source and run executable
  for model in $MODELS; do
    # Command for compiling the instrumented file with a given memory model.
    # Make sure executables are compiled in the debug mode.
    COMMAND="$EACSLGCC --compile-only --rt-debug --memory-model=$model \
      --logfile=$logfile --oexec-e-acsl=$oexec-$model $ocode"
    debug "Run $COMMAND"
    $COMMAND || error "Command $COMMAND failed" "$logfile"
    run_executable $oexec-$model $oexeclog-$model $model
  done

  RUNS=$((RUNS+1))
}

run_test ""
# Run GMP tests if specified
if [ -n "$GMP" ]; then
  run_test "--gmp"
fi

exit 0
