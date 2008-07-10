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

typedef struct { int p1[5]; int p2[5]; int v1; int v2; int *pp1; } las;
typedef struct { int *pp2; } las2;
las u, v, w, m;
las2 u2, v2;

/*
  invariant inv1:
  \forall las x,las y;
  &x != &y =>
  \base_addr(x.p1) != \base_addr(y.pp1)
  &&  \base_addr(x.p2) != \base_addr(y.pp1)
  &&  \base_addr(x.pp1) != \base_addr(y.pp1)
*/
/*
  invariant inv2:
  \forall las x,las2 y;
  \base_addr(x.p1) != \base_addr(y.pp2)
  &&  \base_addr(x.p2) != \base_addr(y.pp2)
  &&  \base_addr(x.pp1) != \base_addr(y.pp2)
*/
/*
  invariant inv3:
  \forall las2 x,las2 y;
  &x != &y => \base_addr(x.pp2) != \base_addr(y.pp2)
*/

/*@
  requires \valid(p) && \valid(p->p1) && \valid(p->p2) && \valid(p->pp1)
  && \valid_range(p->p1,0,4) && \valid_range(p->p2,0,4)
  assigns p->p1[0 .. 5] ,p->p2[0 .. 5], p->v1, p->v2, *p->pp1 
  ensures p->p1[1] <= p->v1
*/
void g(las * p);

/*@
  requires \valid(p) && \valid(p->pp2)
  assigns *p->pp2
  ensures *p->pp2>=5
*/
void g2(las2 * p);

/*@
  requires 
  // \valid(u.pp1) && \valid(v.pp1) 
  // && \valid(w.pp1) && 
  \valid(m.pp1) 
  // && \valid(u2.pp2) && \valid(v2.pp2)
  
//
//  assigns u.v1,u.v2,u.p1[0 .. 5],u.p2[0 .. 5],*u.pp1
//  ,v.v1,v.v2,v.p1[0 .. 5],v.p2[0 .. 5],*v.pp1
//  ,w.v1,w.v2,w.p1[0 .. 5],w.p2[0 .. 5],*w.pp1
//  ,m.v1,m.v2,m.p1[0 .. 5],m.p2[0 .. 5],*m.pp1
//  ,*u2.pp2,*v2.pp2

  ensures
   (\exists int i; u.p1[i] <= u.v1) &&
   (\exists int i; v.p1[i] <= v.v1) &&
   (\exists int i; w.p1[i] <= w.v1) &&
   ( // \exists int i;
    m.p1[1] <= m.v1) 
*/
void f()
{ 
  g(&u);
  g(&v);
  g(&w);
  g(&m);
  g2(&u2); 
  g2(&v2); 
}
