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

/******
int a=10000,b,c=2800,d,e,f[2801],g;main(){for(;b-c;)f[b++]=a/5;
for(;d=0,g=c*2;c-=14,printf("%.4d",e+d/a),e=d%a)for(b=c;d+=f[b]*a,
f[b]=d%--g,d/=g--,--b;d*=b);}
******/

/*@ axiom simplify_dumb_1 : 2800 % 14 == 0 */
/*@ axiom simplify_dumb_2 : \forall int c; c*2>0 => c*2>1 */
/*@ axiom simplify_dumb_3 : \forall int c; c%14==0 => (c-14)%14==0 */
/*@ axiom simplify_dumb_4 : \forall int c; c%14==0 => c>0 => c>=14 */
/*@ axiom simplify_dumb_5 : 10000 / 5 == 2000 */

void print4(int);

int a=10000,b,c=2800,d,e,f[2801],g,i;

/*@ requires b == 0 && c == 2800 && a == 10000 */
void main(){
  /*@ invariant 0 <= b <= 2800 && \forall int i; 0 <= i < b => f[i] == 2000
      variant c-b */
  for(; b-c; b++) 
    f[b] = a/5;

  /*@ invariant 0 <= c <= 2800 && c%14==0
      variant c */
  for(; d=0, g=c*2; ) {
    /*@ invariant 1 <= b <= c && g == b*2
        variant b */
    for(b=c; 1; ) {
      d += f[b]*a;
      f[b] = d % --g;
      d /= g--;
      --b;
      if (!b) break; 
      d *= b;
    }
    c -= 14; 
    print4(e+d/a); 
    e = d%a;
    }
}
