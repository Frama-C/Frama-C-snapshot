#!/bin/sh

DIR=`pwd`
LIBDIR=`grep "libdir" $DIR/src/version.ml | sed -e 's|[^"]*"\([^"]*\)"[^"]*|\1|g' | head -n 1`

echofilename () {
  echo "========== file $1 =========="
}

mycat() {
  echofilename $1
  cat $1
}

mycatfilterdir () {
  echofilename $1
  sed -e "s|$DIR|HOME|g" -e "s|$LIBDIR|WHYLIB|g" $1 
}

case $1 in
  *.java)
        d=`dirname $1`
	b=`basename $1 .java`
        f=$d/$b
	mycat $f.java
	echo "========== krakatoa execution =========="
	rm -f $f.jc
	rm -f $f.jloc
	KRAKATOALIB=$DIR/lib bin/krakatoa.opt $1 || exit 1
	mycat $f.jc 
	mycatfilterdir $f.jloc
	echo "========== jessie execution =========="
	rm -f $f.makefile 
	rm -f $d/why/$b.why
	rm -f $f.loc
	WHYLIB=$DIR/lib bin/jessie.opt -locs $f.jloc -why-opt -split-user-conj $f.jc || exit 2
	mycatfilterdir $f.makefile
	mycatfilterdir $f.loc
	mycat $d/why/$b.why
	echo "========== make project execution =========="
	rm -f $d/why/$b.wpr	
	rm -f $d/why/$b'_ctx'.why	
	rm -f $d/why/$b'_po'*.why
	WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile project
	mycatfilterdir $d/why/$b.wpr	
	mycat $d/why/$b'_ctx'.why	
	for i in $d/why/$b'_po'*.why; do mycat $i; done
	echo "========== generation of Simplify VC output =========="
	WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile simplify/$b'_why'.sx	
	mycatfilterdir $d/simplify/$b'_why'.sx
	echo "========== running Simplify =========="
	DP="$DIR/bin/why-dp.opt -no-timings -timeout 10" WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile simplify	
	echo "========== generation of alt-ergo VC output =========="
	WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile why/$b'_why'.why	
	mycat $d/why/$b'_why'.why
	echo "========== running alt-ergo =========="
	DP="$DIR/bin/why-dp.opt -no-timings -timeout 10" WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile ergo	
	if grep RUNCOQ $f.java ; then
	    echo "========== generation of Coq VC output =========="
	    WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile coq	
	    mycatfilterdir $d/coq/$b'_why'.v
	    echo "========== running Coq =========="
	    DP="$DIR/bin/why-dp.opt -no-timings -timeout 10" WHYLIB=$DIR/lib WHYEXEC=$DIR/bin/why.opt make --quiet -C $d -f $b.makefile coq		   
        fi
	;;
  *.c)
	b=`basename $1 .c`
	exit 1
	caduceus -why-opt -split-user-conj $1 || exit 1
	make --quiet -f $b.makefile gui
	;;
  *.jc)
	b=`basename $1 .jc`
	exit 1
	jessie $b.jc || exit 1
	make --quiet -f $b.makefile gui
	;;
  *.why)
	exit 1
	gwhy-bin -split-user-conj $1
	;;
  *)
	echo "don't know what to do with $1"
	exit 1
esac


