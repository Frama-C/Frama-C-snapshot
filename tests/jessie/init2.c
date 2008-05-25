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

typedef struct {int a;int b;} lastruct1; 
typedef struct {lastruct1 c;int d;} lastruct2; 
lastruct2 ls = {{1,2},3}; 

int varglo=123; 


typedef enum 
{ 
 a = 12, 
 b = 34, 
 c = 56 
} lenum;

 
lenum le=b; 


const int laconstante=7; 


/*@ 
assigns \empty ;
ensures  ls.c.a==1 && varglo==123 &&  \result==4  && le==34  
   && laconstante==7 ;
*/ 
int f() 
{ 
  int varloc=4; 
  return varloc;  

}

/* 
Local Variables:
compile-command: "LC_ALL=C make init2"
End:
*/
