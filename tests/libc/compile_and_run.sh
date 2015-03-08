#!/bin/sh

if command -v valgrind
then VALGRIND=`command -v valgrind`" -q"
else VALGRIND=""
fi

gcc -Wall $1 -o $1.exe && $VALGRIND ./$1.exe && rm ./$1.exe
