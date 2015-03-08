#!/bin/sh

BASE=$1

EXE_FILE=tests/slicing/$BASE.byte
RES_FILE=tests/slicing/result/$BASE.res.log
ERR_FILE=tests/slicing/result/$BASE.err.log

make -s $EXE_FILE

CMD="$EXE_FILE -deps tests/slicing/$BASE.c"

echo "$CMD"
#echo "RES = $RES_FILE"
#echo "ERR = $ERR_FILE"

$CMD > $RES_FILE 2> $ERR_FILE
