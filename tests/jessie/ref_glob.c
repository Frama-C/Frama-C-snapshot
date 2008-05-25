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

/*@ requires \valid(p);
  @ assigns *p;
  @ ensures *p == 1;
  @*/
void g(int *p) {
  *p = 1;
}

int x = 45;

/*@ assigns x;
  @ ensures x == 1;
  @*/
void f1() {
  x = 1;
}

/*@ assigns x;
  @ ensures x == 1;
  @*/
void f2() {
  g(&x);
}

typedef struct{ int c1; int c2; } las;
las * plas; 

/*@ 
  @ requires \valid(plas) ;
  @ assigns plas->c1,plas->c2 ;
  @ ensures plas->c1 == 1 && plas->c2 == 1;
  @*/ 
void f4() 
{ 
  plas->c2 = 2; 
  g(&plas->c1); 
  g(&plas->c2); 
}



int t[3] = {1,2,3};

/*@ requires \valid(p) && \valid( *p);
  @ assigns **p;
  @ ensures **p == 2;
  @*/
void h(int **p) {
  **p = 2;
}

// mal typ‚ !
// /*@ requires t[1] == 2
//   @ ensures t[0] == 2 && t[1] == 2 */
// void f3() {
//   h(&t);
// }


/* 
Local Variables:
compile-command: "LC_ALL=C make ref_glob"
End:
*/
