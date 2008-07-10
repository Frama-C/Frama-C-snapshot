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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  let string_of_option = function None -> "" | Some s -> s
}

rule split = parse
  | (['0'-'9']+ as int) '.' (['0'-'9']* as frac) 
    (['e''E'](['-''+']?['0'-'9']+ as exp))? ['f''F''d''D'] ?
      { (int, frac, string_of_option exp) }

  | '.' (['0'-'9']+ as frac) (['e''E'](['-''+']?['0'-'9']+ as exp))? 
    ['f''F''d''D'] ?
      { ("", frac, string_of_option exp) }

  | (['0'-'9']+ as int) ['e''E'] (['-''+']?['0'-'9']+ as exp) ['f''F''d''D'] ?
      { (int, "", exp) }

{
  let split s = split (from_string s)
}
