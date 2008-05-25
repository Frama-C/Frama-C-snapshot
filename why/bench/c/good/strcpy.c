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


/*@ predicate is_c_string(char *src, int n) {
       n >= 0 && \valid_range(src,0,n) && src[n] == 0 && 
       (\forall int i; 0 <= i < n => src[i] != 0) }
*/


/*@ requires 
  @   \base_addr(src) != \base_addr(dest) &&
  @   (\exists int n; is_c_string(src,n) && \valid_range(dest,0,n)) 
  @      
  @ ensures
  @   \forall int n ;  is_c_string(src,n) =>
  @      \forall int k; 0 <= k <= n => dest[k] == src[k]
  @*/
void strcpy(char *dest, const char *src) {
  /*@ invariant 
    @   \old(dest) <= dest && \old(src) <= src &&
    @   dest - \old(dest) == src - \old(src) &&
    @   (\forall char *p;  \old(dest) <= p < dest => 
    @          *p == *(\old(src) + (p-\old(dest)))) &&
    @   (\forall int n ;  is_c_string(\old(src),n) =>
    @      dest <= \old(dest)+n && src <= \old(src) + n)
    @ loop_assigns dest[..]
    @ */   
  while (*dest++ = *src++);
}







/*

> man strcpy

STRCPY(3)                  Linux Programmer's Manual                 STRCPY(3)

NAME
       strcpy, strncpy - copy a string

SYNOPSIS
       #include <string.h>

       char *strcpy(char *dest, const char *src);

       char *strncpy(char *dest, const char *src, size_t n);

DESCRIPTION
       The  strcpy()  function  copies the string pointed to by src (including
       the terminating `\0' character) to the array pointed to by  dest.   The
       strings  may not overlap, and the destination string dest must be large
       enough to receive the copy.

       The strncpy() function is similar, except that not more than n bytes of
       src  are copied. Thus, if there is no null byte among the first n bytes
       of src, the result will not be null-terminated.

       In the case where the length of src is less than that of n, the remain-
       der of dest will be padded with nulls.

RETURN VALUE
       The  strcpy()  and strncpy() functions return a pointer to the destina-
       tion string dest.

BUGS
       If the destination string of a strcpy() is not large enough  (that  is,
       if  the programmer was stupid/lazy, and failed to check the size before
       copying) then anything might happen.  Overflowing fixed length  strings
       is a favourite cracker technique.

*/



/* axiom is_c_string_uniq :
  \forall char * s; \forall int n1 ; \forall int n2 ;
    is_c_string(s,n1) => is_c_string(s,n2) => n1 == n2
*/
