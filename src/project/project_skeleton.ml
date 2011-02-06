(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* ************************************************************************** *)
(** {2 Logging machinery} *)
(* ************************************************************************** *)

module Output =
  Log.Register
    (struct
       let channel = Log.kernel_channel_name
       let label = Log.kernel_label_name
       let verbose_atleast n = !Cmdline.kernel_verbose_atleast_ref n
       let debug_atleast n = !Cmdline.kernel_debug_atleast_ref n
     end)

(* ************************************************************************** *)
(** {2 Type declaration} *)
(* ************************************************************************** *)

type t = { pid: int; mutable name: string; mutable unique_name: string }
type project = t

(* ************************************************************************** *)
(** {2 Constructor} *)
(* ************************************************************************** *)

let dummy = { pid = 0; name = ""; unique_name = ""}

module Make_setter(X: sig val mem: string -> bool end) = struct

  let make_unique_name from =
    let rec build base id =
      let fullname = base ^ " " ^ string_of_int id in
      if X.mem fullname then build base (succ id) else fullname
    in
    if X.mem from then build from 2 else from

  let make =
    let pid = ref 0 in
    fun name ->
      incr pid;
      { pid = !pid; name = name; unique_name = make_unique_name name }

  let set_name p s =
    p.unique_name <- make_unique_name s;
    p.name <- s

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
