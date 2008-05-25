#!/bin/sh

# Note: the LIBDIR variable is a free variable
# Note: the mkdirs are needed for the Ocamlbuild Makefile.

. ./Version

# Why
WHYVF=src/version.ml
mkdir -p src
echo "let coqversion = \""v8"\"" > $WHYVF
echo "let version = \""$VERSION"\"" >> $WHYVF
echo "let date = \""`date`"\"" >> $WHYVF
echo "let libdir = \""$LIBDIR/why"\"" >> $WHYVF

# Jessie
JESSIEVF=jc/jc_version.ml
mkdir -p jc
echo "let version = \""$JCVERSION"\"" > $JESSIEVF
echo "let date = \""`date`"\"" >> $JESSIEVF
echo "let libdir = \""$LIBDIR/jessie"\"" >> $JESSIEVF

# Krakatoa
KRAKATOAVF=java/java_version.ml
mkdir -p java
echo "let version = \""$KVERSION"\"" > $KRAKATOAVF
echo "let date = \""`date`"\"" >> $KRAKATOAVF
echo "let libdir = \""$LIBDIR/krakatoa"\"" >> $KRAKATOAVF

# Caduceus
CADUCEUSVF=c/cversion.ml
mkdir -p c
echo "let version = \""$CVERSION"\"" > $CADUCEUSVF
echo "let date = \""`date`"\"" >> $CADUCEUSVF
echo "let libdir = \""$LIBDIR/caduceus"\"" >> $CADUCEUSVF


# Doc
DOCF=doc/version.tex
mkdir -p doc
printf '\\newcommand{\\whyversion}{'$VERSION'}\n' > $DOCF
printf '\\newcommand{\\caduceusversion}{'$CVERSION'}\n' >> $DOCF
printf '\\newcommand{\\jessieversion}{'$JCVERSION'}\n' >> $DOCF
printf '\\newcommand{\\krakatoaversion}{'$KVERSION'}\n' >> $DOCF
