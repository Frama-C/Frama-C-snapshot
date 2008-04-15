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


int y;

/*@ ensures \result == 0 && y == \old(y) */
int g() {
  int y; y = 0 ;
  return y;
}


/*@ ensures (x==0 => \result == 1) && (x !=0 => \result == 2) */
int f(int x) {
  if (x == 0) {
    int y; 
    y = 1;
    return y;
  }
  else {
    int y;
    y = 2;
    return y;
  }
}


/*@ ensures (x == 0 => \result == 1) && (x != 0 => \result == 2) */
int h(int x) {
  int y;
  y = 2;
  if (x == 0) {
    int y; 
    y = 1;
    return y;
  }
  return y;
}


typedef struct {
    int toto;
    int titi;    
} S1;

/*@ assigns ma_structure.toto */
void f1(S1 ma_structure)
{
    int toto;
    toto = 0;
    ma_structure.toto = toto;
 }

typedef struct {
    int fst;
    int sec;
} S2;

typedef struct {
    S2 substruct;
    int titi;
} S3;

/*@ requires \valid(ma_structure) && \valid(ma_structure.substruct)
  @ assigns ma_structure.substruct.fst */
void f2(S3 ma_structure)
{
    S2 substruct;
    substruct.fst = 0;
    ma_structure.substruct.fst = substruct.fst;
 }


