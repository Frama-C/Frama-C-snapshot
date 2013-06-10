(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(* This type indicates to eval_stmt.mls computer what to do at a stmt
 * returned by determine_mode
 *)
type mode = Normal
          | Split of local_slevel_info
          | Merge
          | MergeSplit of local_slevel_info
(* This type contains all local_slevel infos needed in computer *)
and local_slevel_info =
  { mutable prevmode : mode
  ; merges : Cil_datatype.Stmt.Hptset.t
  ; slevel : int option}

let empty_info () =
  { prevmode = Normal
  ; merges = Cil_datatype.Stmt.Hptset.empty
  ; slevel = None }

let d_mode ff mode = Format.fprintf ff "%s" (match mode with
  | Normal       -> "Local_slevel.Normal"
  | Split _      -> "Local_slevel.Split"
  | Merge        -> "Local_slevel.Merge"
  | MergeSplit _ -> "Local_slevel.MergeSplit")

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
