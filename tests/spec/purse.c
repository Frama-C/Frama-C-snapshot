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
#define FRAMA_C_MALLOC_INDIVIDUAL
#include "share/libc/stdlib.c"
typedef struct purse {
  int balance;
} purse;

//@ predicate purse_inv{L}(purse *p) = \valid(p) && p->balance >= 0 ;

/*@ requires purse_inv(p) && s >= 0;
  @ assigns p->balance;
  @ ensures purse_inv(p) && p->balance == \old(p->balance) + s ;
  @*/
void credit(purse *p,int s) {
  p->balance = p->balance + s;
}

/*@ requires purse_inv(p) && s >= 0 ;
  @ assigns p->balance;
  @ ensures purse_inv(p) && p->balance == \old(p->balance) + s ;
  @*/
void f(purse *p,int s) {
  p->balance = p->balance + s;
}

/*@ requires purse_inv(p) && 0 <= s <= p->balance;
  @ assigns p->balance;
  @ ensures purse_inv(p) && p->balance == \old(p->balance) - s;
  @*/
void withdraw(purse *p,int s) {
  p->balance = p->balance - s;
}


/*@ requires purse_inv(p1) && purse_inv(p2)  ;
  @ assigns p1->balance, p2->balance;
  @ ensures \result == 0;
  @*/
int test1(purse *p1, purse *p2) {
    p1->balance = 0;
    credit(p2,100);
    return p1->balance;
}

/*@ assigns \empty;
  @ ensures \fresh(\result,sizeof(purse)) && purse_inv(\result) && \result->balance == 0;
  @*/
purse *new_purse() {
  purse* p = (purse*) malloc(1 * sizeof(purse));
  p->balance = 0;
  return p;
}

/*@ ensures \result == 150;
  @*/
int test2() {
  purse *p1 = new_purse();
  purse *p2 = new_purse();
  credit(p1,100);
  credit(p2,200);
  withdraw(p1,50);
  withdraw(p2,100);
  return p1->balance + p2->balance;
}


/*
void main() {
  purse *p = new_purse();
  test1(p,p);
}
*/
