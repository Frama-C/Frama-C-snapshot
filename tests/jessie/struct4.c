/**************************************************************************/
/*                                                                        */
/*  The Why/Caduceus/Krakatoa tool suite for program certification        */
/*  Copyright (C) 2002-2006                                               */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU General Public                   */
/*  License version 2, as published by the Free Software Foundation.      */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/*  See the GNU General Public License version 2 for more details         */
/*  (enclosed in the file GPL).                                           */
/*                                                                        */
/**************************************************************************/

typedef struct A { unsigned char v; } A ;

typedef struct S { A a; A *b; A c[3]; struct S *s; unsigned char i; } S;


/* @ predicate is_unsigned_char(int x) { 0 <= x <= 255 } */

/* @ predicate is_struct_A(A x) reads x.v */

/* @ axiom is_struct_A_def : 
  \forall A x ; is_struct_A(x) <=> is_unsigned_char(x.v) 
 */

/* @ predicate is_struct_S(S x) reads x.a,x.b,x.c,x.i */

/* @ axiom is_struct_S_def : 
  \forall S x ; is_struct_S(x) <=> 
       ( is_struct_A(x.a) 
       && (\forall int i; \valid(x.b+i) => is_struct_A( *(x.b+i)))
       && \valid_range(x.c,0,3) 
       && (\forall int i; 0<=i<=3 => is_struct_A(x.c[i]))
       && (\forall int i; \valid(x.s+i) => is_struct_S( *(x.s+i)))
       && is_unsigned_char(x.i))
       
 */

struct S aaa;

/*@ requires \valid(x.s); */
int f(struct S x) {
  x.s->a.v = 0;
  aaa.i = 'a';
  return x.c[1].v;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make struct4"
End:
*/
