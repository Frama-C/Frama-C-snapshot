(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

type t =
  | Approximation of string
  | Imprecision of string
  | Costly of string
  | Unsoundness of string

type emitter = int

module Emitter_name =
State_builder.Hashtbl(Datatype.Int.Hashtbl)(Datatype.String)(struct
  let name = "Lattice_messages.Emitter_name";;
  let dependencies = [];; (* This table has no dependencies, and is thus copied
     between projects. This is the desired semantics, as message emitters
     are global. We projectify it is so that they are saved and loaded. To
     guarantee that Ids are always the same, we simply use the hash of the
     name of the emitter as its id. *)
  let size = 16
end
)

let find_hash_conflict hash =
  Emitter_name.fold
    (fun hash' name acc -> if hash = hash' then Some name else acc) None

let emitter_name id =
  try Emitter_name.find id
  with Not_found -> assert false

let message_destination:(emitter -> t -> unit) ref =
  ref (fun _msg -> Kernel.fatal "Undefined Lattice_messages message_destination function");;

let register name =
  let h = Hashtbl.hash name in
  begin match find_hash_conflict h with
  | None -> Emitter_name.replace h name;
  | Some name' ->
     if name <> name' then
       Kernel.abort "Name conflict between emitters '%s' and '%s'" name name';
  end;
  h
;;

let emit emitter = !message_destination emitter

let emit_approximation emitter = Format.kfprintf (fun _fmt ->
  let str = Format.flush_str_formatter() in
  !message_destination emitter (Approximation str)) Format.str_formatter
;;

let emit_costly emitter = Format.kfprintf (fun _fmt ->
  let str = Format.flush_str_formatter() in
  !message_destination emitter (Costly str)) Format.str_formatter
;;

let emit_imprecision emitter str =
  !message_destination emitter (Imprecision str)
;;


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
