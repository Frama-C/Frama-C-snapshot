/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* integer arithmetic overflows (requires --int-overflow) */

/* operations */

/*@ ensures \result == 2 */
int add1() {
  unsigned char uc = 1;
  signed short s = 1;
  return uc + s;
}

/*@ requires x <= 1000
    ensures \result == x+1 */
int add2(int x) {
  return x+1;
}

int lsl() { return 1<<2; }

/* comparisons */

int cmp1() { return 1<2; }

/*@ ensures \result == 0 */
int not1() { signed char c = 1; return !c; }

/* constants */
void constant1() { int x = 1; }

/* incrementation */

void incr1() {
  unsigned char c = 254;
  c++;
  /*@ assert c == 255 */
}

unsigned short us;

/*@ requires us == 13 ensures us == 15 */
void incr2() { us++; ++us; }

/* casts */

void cast1() { 
  char c = 1;
  unsigned char uc = c;
}

/* arrays */

int t[10];

void array1() {
  signed char c = 1;
  t[c] = 0;
  t[c+1] = 0;
}

/*@ requires t[0] <= 1000 assigns t[0] */
void incr3() {
  t[0]++;
}

/* loops */

void loop1() {
  unsigned char uc;
  short ss;
  /*@ invariant 0 <= uc <= 255 variant 256-uc */
  for (uc = 0; uc < 255; uc++) ss = uc;
}

/* structures */

struct S {
  unsigned char uc;
  signed char c;
  int i;
} s;

/*@ requires s.c == 0
    ensures  s.c == 3 && \result == 2 */
int struct1() { ++s.c; s.c++; return s.c++; }

/* bit fields */

struct BF {
  int f1 :1 ;
  unsigned int uf1 :1 ; 
  int f2 :2 ;
  unsigned int uf2 :2 ;
  int f7 :7;
} bf;

void bit_fields() {
  bf.f1 = 0;
}

/*
Local Variables: 
compile-command: "make overflows.overflows"
End: 
*/
