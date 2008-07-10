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

/*@ axiom A : \forall double t[]; \forall int i; \valid(t+i) => t[i]>0.0 => t[i]>=0.0 */

typedef struct U { int t2[5]; int t2bis[5]; int *p2; } las2;
typedef struct V { int t1[5]; int t1bis[5]; int *p1; las2 * pp; } las;
las u,v,w;
las2 z;
las2 y1[5];
las2 y2[10];

/* predicate separation_intern_struct_U(las2* p) reads p->t2, p->t2bis */

/* axiom sep_U : 
  \forall las2 *p; \valid(p) => separation_intern_struct_U(p) */


/*@ requires \valid(x) */
void f(struct U *x) { x->t2[0] = 1; *u.p1 = 1; *z.p2 = 2; }

int ff(double a,double b)
{
int x;
x=(a<b);
return x;
}

//@ ensures \result < a
double f2(double a) { return (a - 1.2e-3); }
 
/*@ requires 0.0 < x <= 1.0
*/
void f3(double x)
{
/* ... */
x = x * 2.0;
/* ... */
}

typedef double arr[5];

/*@ requires \valid(d)
  @
  @*/
void f4(arr * d)
{
int i=0;
for(i=0;i<5;i++) (*d)[i]=i;
}

arr t;

void g4()
{ f4(&t); }
