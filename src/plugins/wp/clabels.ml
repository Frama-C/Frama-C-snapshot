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

type c_label =
  | Here
  | Init
  | Pre
  | Post
  | Exit
  | At of string list * stmt
  | LabelParam of string

let pretty fmt = function
  | Init -> Format.pp_print_string fmt "\\init"
  | Here -> Format.pp_print_string fmt "\\here"
  | Pre  -> Format.pp_print_string fmt "\\pre"
  | Post -> Format.pp_print_string fmt "\\post"
  | Exit -> Format.pp_print_string fmt "\\exit"
  | LabelParam label -> Format.fprintf fmt "Label '%s'" label
  | At(label::_,_) -> Format.fprintf fmt "Stmt '%s'" label
  | At([],s) -> Format.fprintf fmt "Stmt sid:%d" s.sid

let compare a b =
  if a==b then 0 else
    match a,b with
    | At(_,s) , At(_,s') -> Pervasives.compare s.sid s'.sid
    | At _ , _ -> 1
    | _ , At _ -> (-1)
    | _ -> Pervasives.compare a b

let equal a b =
  a == b || compare a b = 0

module T = struct type t = c_label let compare = compare end
module LabelMap = FCMap.Make(T)
module LabelSet = FCSet.Make(T)

let has_prefix p s =
  let rec scan k p s =
    ( k >= String.length p ) ||
    ( k < String.length s && p.[k] = s.[k] && scan (succ k) p s )
  in scan 0 p s

let rec names_at = function
  | [] -> []
  | Default _ :: labels -> "default" :: names_at labels
  | Label(l,_,_) :: labels ->
      (*TODO [LC] see mk_logic_label and loop_head_label *)
      if has_prefix "wp!" l || has_prefix "return_label" l
      then names_at labels
      else l :: names_at labels
  | Case(e,_) :: labels ->
      match Ctypes.get_int e with
      | None -> "case" :: names_at labels
      | Some n ->
          if n < 0L
          then ("caseneg" ^ Int64.to_string (Int64.neg n)) :: names_at labels
          else ("case" ^ Int64.to_string n) :: names_at labels

let c_label = function
  | LogicLabel (None, "Init") -> Init
  | LogicLabel (None, "Here") -> Here
  | LogicLabel (None, "Pre")  -> Pre
  | LogicLabel (None, "Post") -> Post
  | LogicLabel (None, "Exit") -> Exit
  | LogicLabel (None, l) -> LabelParam l
  | LogicLabel (Some stmt, _)
  | StmtLabel { contents=stmt } -> At(names_at stmt.labels,stmt)

(*TODO [LC] : Use extension of Clabels instead *)
let loop_head_label s =
  LogicLabel (None, "wp!loop_"^(string_of_int s.sid)^"_head")

(*TODO [LC] : Use extension of Clabels instead *)
let mk_logic_label s =
  LogicLabel (Some s, "wp!stmt_"^(string_of_int s.sid))

let mk_stmt_label s = (* TODO: clean that !*) c_label (mk_logic_label s)
let mk_loop_label s = (* TODO: clean that !*) c_label (loop_head_label s)

let lookup_name = function
  | Init -> "Init"
  | Pre  -> "Pre"
  | Here -> "Here"
  | Post -> "Post"
  | Exit -> "Exit"
  | LabelParam p -> p
  | At(_,s) -> Printf.sprintf "<stmt:%d>" s.sid

let lookup labels param =
  try
    let is_param p = function (LogicLabel (None, a),_) -> a = p | _ -> false in
    c_label (snd (List.find (is_param param) labels))
  with Not_found -> Wp_parameters.fatal
                      "Unbound label parameter '%s' in predicate or function call" param
