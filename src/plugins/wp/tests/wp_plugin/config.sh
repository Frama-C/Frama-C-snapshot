#!/bin/sh

ERGO=`alt-ergo -version`
WHY3=`why3 --version`

# sed command to prevent from diffs about the date: "(Month Year)" 
COQC=`coqc --version | head -1 | sed -e 's: ([A-Z][a-z]* [0-9]*)$::'`

echo "----------------------------------------------------------"
echo "WP Requirements for Qualif Tests (3)"
echo "----------------------------------------------------------"
echo "1. The Alt-Ergo theorem prover, version ${ERGO}"
echo "2. The ${WHY3}"
echo "3. ${COQC}"
echo "----------------------------------------------------------"
