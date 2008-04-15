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

/*@ predicate is_string(char *s) { 
      \exists int n; \valid_range(s,0,n) && s[n] == 0 
    } */

/*@ logic int length(char *s) reads s[..] */

/*@ axiom length_non_negative :
      \forall char *s; is_string(s) => 0 <= length(s) */

/*@ axiom length_not_zero :
      \forall char *s; is_string(s) => 
      \forall int i; 0 <= i < length(s) => s[i] != 0 */

/*@ axiom length_zero :
      \forall char *s; is_string(s) => s[length(s)] == 0 */

/*@ axiom is_string_valid :
      \forall char *s; is_string(s) =>
      \forall int i; 0 <= i <= length(s) => \valid(s+i) */

/*@ requires is_string(s)
    ensures  \valid(s+\result) && s[\result] == 0 
             // && \forall int i; 0 <= i < \result => s[i] != 0 
 */
int strlen(char * s) {
  int len = 0;
  /*@ invariant \valid(s + len) && len <= length(s)
      variant length(s) - len */
  while (s[len] != 0) len++;
  return len;
}

