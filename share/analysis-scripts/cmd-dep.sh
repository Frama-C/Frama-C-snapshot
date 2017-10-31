#!/bin/bash
set -eu

if [ $# -lt 1 ]
then
  (
    echo "usage: $0 FILE STRING"
    echo "Test whether the contents of FILE are different from STRING." \
         "If it does, FILE is updated to match STRING. The file" \
         "name is always printed."
  ) >&2
  exit 1
fi

FILE=$1
shift
STRING=$*

if
  [ ! -e $FILE ] ||
  ! (diff --brief --ignore-space-change $FILE - <<< "$STRING")
then
  mkdir -p $(dirname "$FILE")
  echo $STRING > "$FILE"
fi

echo $FILE
