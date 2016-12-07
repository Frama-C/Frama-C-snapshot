#!/bin/sh

cd share/libc

for A in `ls *.h */*.h`;
do
    if ! grep -q $A ../../tests/libc/fc_libc.c ;
    then echo "#include \"$A"\";
    fi ;
done;

for A in `ls *.c`;
do
    if ! grep -q $A fc_runtime.c ../../tests/libc/fc_libc.c ;
    then echo Not included implementation \'$A\';
    fi ;
done;
