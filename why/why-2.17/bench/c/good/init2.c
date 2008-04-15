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
assigns \nothing 
ensures  ls.c.a==1 && varglo==123 &&  \result==4  && le==34  
   && laconstante==7 
*/ 
int f() 
{ 
  int varloc=4; 
  return varloc;  

}
