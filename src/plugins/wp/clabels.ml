(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
  | At of string list * int
  | CallAt of int
  | LabelParam of string

let equal = (=)

module T = struct type t = c_label let compare = Pervasives.compare end
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
  | StmtLabel { contents=stmt } -> At(names_at stmt.labels,stmt.sid)

(*TODO [LC] : Use extension of Clabels instead *)
let loop_head_label s =
  LogicLabel (None, "wp!loop_"^(string_of_int s.sid)^"_head")

(*TODO [LC] : Use extension of Clabels instead *)
let mk_logic_label s =
  LogicLabel (Some s, "wp!stmt_"^(string_of_int s.sid))

let mk_stmt_label s = (* TODO: clean that !*) c_label (mk_logic_label s)
let mk_loop_label s = (* TODO: clean that !*) c_label (loop_head_label s)

let pretty fmt = function
  | Init -> Format.pp_print_string fmt "\\init"
  | Here -> Format.pp_print_string fmt "\\here"
  | Pre  -> Format.pp_print_string fmt "\\pre"
  | Post -> Format.pp_print_string fmt "\\post"
  | Exit -> Format.pp_print_string fmt "\\exit"
  | LabelParam label -> Format.fprintf fmt "Label '%s'" label
  | CallAt sid -> Format.fprintf fmt "Call sid:%d" sid
  | At(label::_,_) -> Format.fprintf fmt "Stmt '%s'" label
  | At([],sid) -> Format.fprintf fmt "Stmt sid:%d" sid

let lookup_name = function
  | Init -> "Init"
  | Pre  -> "Pre"
  | Here -> "Here"
  | Post -> "Post"
  | Exit -> "Exit"
  | LabelParam p -> p
  | CallAt sid -> Printf.sprintf "<call:%d>" sid
  | At(_,sid) -> Printf.sprintf "<stmt:%d>" sid

let lookup labels param =
  try
    let is_param p = function (LogicLabel (None, a),_) -> a = p | _ -> false in
    c_label (snd (List.find (is_param param) labels))
  with Not_found -> Wp_parameters.fatal
                      "Unbound label parameter '%s' in predicate or function call" param
