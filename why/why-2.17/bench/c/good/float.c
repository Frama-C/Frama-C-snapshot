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

/* The following functions are not intended to be correct;
   this is only a test file for the syntax and the typing of floating-point
   annotations */
   
const float ff = -(1 / (85 / (float)99));
float f;
double d;
long double l = 123.L;

/*@ requires -f == -0 + (int)1.0 + 2.0
  @ ensures  d >= 1 - 2.34
  @*/
void f1() { 
  f = (float)1 + (int)1.2;
  d = -f + 1.0 + 12.L;
  l = f + d + (long double)3;
}

/*@ requires x == \exact(x) && | x | <= 1
  @ ensures  \round_error(\result) <= 2 ^^ (-48)
  @*/
double my_exp(double x) {
  return 1 + x + x*x/2;
}

/*@ requires \model(x) == 0.0
  @ ensures \total_error(\result) <= 0.1
  @*/
double f2(double x) {
  return x + 1.0f + 2 * 3.14 / 3.6l;
}

/*@ requires x == y
  @ ensures \result == 1
  @*/
double f3(double x, float y) {
  long double z;
  if (x < y ) z = y; else z = x;
  return z;
}

/*@ ensures \result == 2 ^^ 40
  @*/
double f4(double x) {
  return x;
}

//@ logic real f_double_to_real(double x)

/*@ ensures \result == f_double_to_real(x) */
double f5(double x) {
  return x - 1;
}

/*@ ensures \result == -(1.0) */
double f6(double x) { return -1.0; }
