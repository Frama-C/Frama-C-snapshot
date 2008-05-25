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

/* LL parser */

/* grammar for arithmetic expressions:

     E ::= T | T "+" E 
     E ::= F | F "*" T
     F ::= "id" | "(" E ")"

*/

enum T { PLUS, MULT, LPAR, RPAR, ID, EOF };
typedef enum T token;

token *text; // should be ghost?

//@ ensures \result == *text
token next_token();

//@ ensures text == text+1
void advance();

/*@ requires 1 // TODO
  @ ensures  0 // never returns
  @*/
void parse_error();

/*@ requires 1 // TODO
  @ ensures  0 // never returns
  @*/
void success();

void parse_S();
void parse_E();
void parse_T();
void parse_F();

void parse_S() {
  parse_E();
  if (next_token() == EOF) 
    success(); 
  else 
    parse_error();
}

void parse_E() {
  parse_T();
  switch (next_token()) {
  case PLUS: 
    advance(); 
    parse_E(); 
    return;
  default: 
    return;
  }
}

void parse_T() {
  parse_F();
  switch (next_token()) {
  case MULT: 
    advance(); 
    parse_T(); 
    return;
  default: 
    return;
  }
}

void parse_F() {
  switch (next_token ()) { 
  case ID: 
    advance();
    return;
  case LPAR:
    advance();
    parse_E();
    switch (next_token()) { 
    case RPAR: 
      advance();
      return;
    default:
      parse_error();
    }
  default:
    parse_error();
  }
}

