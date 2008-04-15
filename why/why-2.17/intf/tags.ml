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

open Colors

type loc = { file:string; line:string; sp:string; ep:string }

let last_colored = ref [(GText.tag ())]
let tag = ref 0

let gtktags = Hashtbl.create 57 (* tag id -> gtk tag *)
let loctags = Hashtbl.create 57 (* tag id -> loc *)

let tag_ref = !tag

let new_tag (l:loc) =
  incr tag;
  let mytag = string_of_int !tag in
  Hashtbl.add loctags mytag l;
  mytag

let get_tag t = 
  try 
    Hashtbl.find loctags t
  with Not_found -> 
    assert false

let add_gtktag (index:string) (tag:GText.tag) = 
  Hashtbl.add gtktags index tag

let get_gtktag index = 
  try 
    Hashtbl.find gtktags index
  with Not_found -> 
    assert false

let reset_last_colored () = 
  List.iter 
    (fun tag ->
       tag#set_properties 
	 [`BACKGROUND (get_bc_predicate ()); 
	  `FOREGROUND (get_fc_predicate ())])
    !last_colored;
  last_colored := [GText.tag ()]

let refresh_last_colored tag = 
  reset_last_colored ();
  last_colored := tag
