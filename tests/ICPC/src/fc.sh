#!/bin/bash

export CPP="gcc -Dfprintf=Frama_C_show_each -DTESTCASE=$1 -C -E -nostdinc -I. -I../../../share/libc"
FILES="../../../share/libc/math.c impls.c main.c roco.c sim.c"
TESTCASE="main_testcase_$1.c"

exec ../../../bin/toplevel.opt ${FILES} ${TESTCASE} -val -val-signed-overflow-alarms -calldeps -save state$1
