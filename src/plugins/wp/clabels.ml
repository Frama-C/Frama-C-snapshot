(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Normalized C-labels                                                --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

type c_label = string

let compare = String.compare
let equal (a:string) (b:string) = (a = b)

module T = struct type t = c_label let compare = compare end
module LabelMap = Datatype.String.Map
module LabelSet = Datatype.String.Set

let init = "wp:init"
let here = "wp:here"
let pre = "wp:pre"
let post = "wp:post"
let formal a = a

let pretty = Format.pp_print_string

let is_here h = (h = here)

let stmt s = "wp:sid"  ^ string_of_int s.sid
let loop_entry s = stmt s (* same point *)
let loop_current s = "wp:head" ^ string_of_int s.sid

let to_logic a = FormalLabel a
let of_logic = function
  | BuiltinLabel Here -> here
  | BuiltinLabel Init -> init
  | BuiltinLabel Pre -> pre
  | BuiltinLabel Post -> post
  | FormalLabel name -> name
  | ( BuiltinLabel (Old|LoopCurrent|LoopEntry) | StmtLabel _) as l -> 
      let desc = Format.asprintf "Non-normalized label %a"
          Printer.pp_logic_label l
      in raise (Wp_error.Error("c-labels",desc))

let name = function FormalLabel a -> a | _ -> ""

let lookup labels a =
  try
    List.find (fun (l,_) -> name l = a) labels |> snd
  with Not_found ->
    Wp_parameters.fatal
      "Unbound label parameter '%s' in predicate or function call" a

(* -------------------------------------------------------------------------- *)
