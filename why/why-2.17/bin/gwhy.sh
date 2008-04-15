#!/bin/sh

case $1 in
  *.java)
	b=`basename $1 .java`
	krakatoa $1 || exit 1
	echo "krakatoa on $b.java done"
	jessie -locs $b.jloc -why-opt -split-user-conj $b.jc || exit 2
	echo "jessie done"
	make -f $b.makefile gui
	;;
  *.c)
	b=`basename $1 .c`
	caduceus -why-opt -split-user-conj $1 || exit 1
	make -f $b.makefile gui
	;;
  *.jc)
	b=`basename $1 .jc`
	jessie $b.jc || exit 1
	make -f $b.makefile gui
	;;
  *.mlw|*.why)
	gwhy-bin -split-user-conj $1
	;;
  *)
	echo "don't know what to do with $1"
esac


