#!/bin/sh

if [ $# -lt 5 ] ; then
  echo "tests/tests.sh: a subshell invoked by default to perform testing run."
  exit 1 
fi

DIFF="diff -b -B"

Compare() {
  if [ -e $1$2 ]; then
    File=`basename $1$2 .log`.oracle
    Dir=`dirname $1`
    Dir=`dirname $Dir`
    File="$Dir/oracle/$File"
    if [ -e ${File} ]; then
      if ! ${DIFF} --brief $1$2 ${File} >/dev/null
      then
         echo ". KO:  ${DIFF} $1$2 ${File}"
      fi
    else
         echo ". NO oracle ${File}"
    fi
  fi
}

# input file
Src=$1
shift

# prefix for the out files
PreFix=$1
shift

# extension for out files issued from stdout
PostFix1=$1
shift

# extension for out files issued from stderr
PostFix2=$1
shift

# command running the test
Cmd=$1
shift

# check the compilation of the source code.
gcc -c ${Src} -o ${PreFix}.o 2> /dev/null
Res=$?
rm -f ${PreFix}.o
if [ "${Res}" != 0 ] ; then
  echo "# compilation problem with: gcc -c ${Src} -o ${PreFix}.o"
fi

# run the test on the input file
echo "${Cmd} $* ${Src}"
${Cmd} $* ${Src} > ${PreFix}${PostFix1} 2> ${PreFix}${PostFix2} 
Res=$?
if [ "${Res}" != 0 ] ; then
  exit ${Res}
fi
Compare ${PreFix} ${PostFix1} stdout
Compare ${PreFix} ${PostFix2} stderr