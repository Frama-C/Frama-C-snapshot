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

/* 112 characters long and calculates pi to 2880 decimal places: 

g,o,p,i=1e4,a[10001];main(x){for(;p?g=g/x*p+a[p]*i+2*!o:
53^(printf("%.4d",o+g/i),p=i,o=g%i);a[p--]=g%x)x=p*2-1;}

*/

void printint(int);

int g,o,p,i=1e4,a[10001];

void main(x) {
  for(; p?g=g/x*p+a[p]*i+2*!o: 53^(printint(o+g/i),p=i,o=g%i); a[p--]=g%x)
    x=p*2-1;
}
