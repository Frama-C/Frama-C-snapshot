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

open Tags

type window = 
  | Color
  | About
  | Help

let grab_infos = 
  let r_loc = Str.regexp "File \"\\(.+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)" in
  fun s -> 
    if Str.string_match r_loc s 0 then 
      let source = Filename.concat (Sys.getcwd ()) (Str.matched_group 1 s) in
      Some({file=source;
            line=(Str.matched_group 2 s);
            sp=(Str.matched_group 3 s);
            ep=(Str.matched_group 4 s)})
    else None

let decomp_name =
  let r = Str.regexp "\\(.*\\)_po_\\([0-9]+\\)" in
  fun s ->
    if Str.string_match r s 0 then
      Str.matched_group 1 s, Str.matched_group 2 s
    else
      "", s

let get_home () = 
  try Sys.getenv "HOME"
  with Not_found -> ""

(*
 * Live update
 *)
let live = ref true
let swap_live () = live := not !live
let set_live v = live := v
let live_update () = !live

(* 
 * Timeout 
 *)
let timeout = ref 10
let set_timeout v = timeout := v
let get_timeout () = !timeout

