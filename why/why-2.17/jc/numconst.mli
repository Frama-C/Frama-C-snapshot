(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: numconst.mli,v 1.6 2008/11/05 14:03:16 filliatr Exp $ i*)

(* Evaluation of constant literals: superset of C and Java

  general rule: case insensitive

  decimal constants:
    . regexp 0 | [1-9][0-9]*

  octal constants:
    . regexp 0[0-7]+

  hexadecimal constants:
    . regexp 0x[0-9a-f]+

  for each of the three above: suffix allowed =  (u|l)*

  characters: between single quotes, with either 
    . ASCII chars
    . octal chars: \[0-7]^3 
    . unicode chars a la Java: \'u'+[0-9a-f]^4 (TODO) 
    . special chars \n, \r, \t, etc. (TODO: give exact list)   

  extended chars a la C: 'L''"'[^'"']*'"' (TODO)

*)

val zero : Num.num

val integer : string -> Num.num


