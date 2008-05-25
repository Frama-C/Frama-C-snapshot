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

struct s1 {
  int t[2];
  int u[3];
} s;

struct s1 ss;

int v[4];

//@ ensures \result == 1
int f() {
  ss.t[0] = 0;
  s.t[0] = 1;
  s.u[0] = 2;
  v[0] = 3;
  return s.t[0];
}

int g(){
  return v[0];
}

/*@ ensures \result == v[0] */
int g0();

struct {
  int x[1];
} tab[5];


int h(){
  return tab[0].x[0];
}

/* Dillon's example */

typedef struct { 
  int p1[5]; 
  int p2[5];
  int v1; 
  int v2;
} las; 

/*@ requires 
  @   \valid(p) 
  @ assigns p->p1[0 .. 4],p->p2[0 .. 4], p->v1, p->v2 
  @ // ensures ...
  @*/ 
void g1(las * p); 

las u1, u2; 

/*@ assigns 
  @   u1.v1,u1.v2,u1.p1[0 .. 4],u1.p2[0 .. 4], 
  @   u2.v1,u2.v2,u2.p1[0 .. 4],u2.p2[0 .. 4] 
  @ // ensures ... 
  @*/ 
void f1() { 
  g1(&u1); g1(&u2); 
}

/* Dillon's example of separation hypothesis of large size */
 
typedef struct { 
  int p1[5]; 
  int p2[5]; 
  int v1; 
  int v2; 
  int * p3; 
  int * p4; 
  int * p5; 
  int * p6; 
  int * p7; 
  int * p8; 
  int * p9; 
  int * p10;
} las2; 

/*@ requires 
  @   \valid(p)
  @ assigns p->p1[0 .. 4],p->p2[0 .. 4], p->v1, p->v2 
  @ // ensures ...
  @*/ 
void g3(las2 * p); 

las2 u3, u4; 


/* ajout : */ 
las2 w1,w2,w3,w4,w5,w6,w7,w8,w9,w10; 
las2 var1,var2,var3,var4,var5,var6,var7,var8,var9,var10; 

/*@ assigns u3.v1,u3.v2,u3.p1[0 .. 4],u3.p2[0 .. 4], 
  @   u4.v1,u4.v2,u4.p1[0 .. 4],u4.p2[0 .. 4], 
  @   w1.v1,w1.v2,w1.p1[0 .. 4],w1.p2[0 .. 4], 
  @   w2.v1,w2.v2,w2.p1[0 .. 4],w2.p2[0 .. 4], 
  @   w3.v1,w3.v2,w3.p1[0 .. 4],w3.p2[0 .. 4], 
  @   w4.v1,w4.v2,w4.p1[0 .. 4],w4.p2[0 .. 4], 
  @   w5.v1,w5.v2,w5.p1[0 .. 4],w5.p2[0 .. 4], 
  @   w6.v1,w6.v2,w6.p1[0 .. 4],w6.p2[0 .. 4], 
  @   w7.v1,w7.v2,w7.p1[0 .. 4],w7.p2[0 .. 4], 
  @   w8.v1,w8.v2,w8.p1[0 .. 4],w8.p2[0 .. 4], 
  @   w9.v1,w9.v2,w9.p1[0 .. 4],w9.p2[0 .. 4], 
  @   w10.v1,w10.v2,w10.p1[0 .. 4],w10.p2[0 .. 4] 
  @ // ensures ... 
  @*/ 
void f3() {
  g3(&u3); 
  g3(&u4); 
  g3(&w1);g3(&w2);g3(&w3);g3(&w4);g3(&w5);
  g3(&w6);g3(&w7);g3(&w8);g3(&w9);g3(&w10); 
}
