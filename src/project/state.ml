(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Project_skeleton

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

type state_on_disk =
    { on_disk_value: Obj.t;
      on_disk_computed: bool;
      on_disk_saved: bool;
      on_disk_digest: Digest.t }

type private_ops =
    { mutable descr: Structural_descr.pack;
      create: t -> unit;
      remove: t -> unit;
      mutable clear: t -> unit;
      mutable clear_some_projects: (t -> bool) -> t -> bool;
      copy: t -> t -> unit;
      commit: t -> unit;
      update: t -> unit;
      on_update: (unit -> unit) -> unit;
      clean: unit -> unit;
      serialize: t -> state_on_disk;
      unserialize: t -> state_on_disk -> unit }

type state =
    { unique_name: string;
      mutable name: string;
      private_ops: private_ops }

module type Local = sig
  type t
  val create: unit -> t
  val clear: t -> unit
  val get: unit -> t
  val set: t -> unit
  val clear_some_projects: (Project_skeleton.t -> bool) -> t -> bool
end

(* ************************************************************************** *)
(** {2 Datatype} *)
(* ************************************************************************** *)

let never_called _ = assert false
let dummy_private_ops () =
  { descr = Descr.pack Descr.unmarshable;
    create = never_called;
    remove = never_called;
    clear = never_called;
    clear_some_projects = never_called;
    copy = never_called;
    commit = never_called;
    update = never_called;
    on_update = never_called;
    serialize = never_called;
    unserialize = never_called;
    clean = never_called }

let dummy_state_on_disk =
  { on_disk_value = Obj.repr ();
    on_disk_computed = false;
    on_disk_saved = false;
    on_disk_digest = "" }

let dummy_unique_name = ""

let dummy =
  { name = "";
    unique_name = dummy_unique_name;
    private_ops = dummy_private_ops () }

module Caml_hashtbl = Hashtbl

include Datatype.Make_with_collections
    (struct
      type t = state
      let name = "State"
      let structural_descr = Structural_descr.t_unknown
      let reprs = [ dummy ]
      let compare x y =
        if x == y then 0 else String.compare x.unique_name y.unique_name
      let equal = (==)
      let hash x = Hashtbl.hash x.unique_name
      let copy = Datatype.undefined
      let rehash = Datatype.undefined
      let internal_pretty_code p_caller fmt s =
        let pp fmt =
          Format.fprintf fmt "@[<hv 2>State.get@;%S@]" s.unique_name
        in
        Type.par p_caller Type.Call fmt pp
      let pretty fmt s = Format.fprintf fmt "state %S" s.unique_name
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)

let is_dummy = equal dummy

(* ************************************************************************** *)
(** {2 Getters} *)
(* ************************************************************************** *)

exception Incompatible_datatype of string

let get_name s = s.name
let get_unique_name s = s.unique_name
let private_ops s = s.private_ops
let get_descr s = s.private_ops.descr

let set_name s n = s.name <- n
let add_hook_on_update s f = s.private_ops.on_update f

(* ************************************************************************** *)
(** {2 States are comparable values} *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {2 Internals}

    All this stuff should not be used outside of the Project library.*)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {3 Managing the set of known states} *)
(* ************************************************************************** *)

let states : t Datatype.String.Hashtbl.t = Datatype.String.Hashtbl.create 997

exception Unknown
let get s = 
  try Datatype.String.Hashtbl.find states s 
  with Not_found -> raise Unknown

let delete s =
  let uname = s.unique_name in
  assert (Datatype.String.Hashtbl.mem states uname);
  Datatype.String.Hashtbl.remove states uname

let add s =
  let uname = s.unique_name in
  assert
    (Project_skeleton.Output.verify
       (not (Datatype.String.Hashtbl.mem states uname))
       "state %S already exists."
       uname);
  Datatype.String.Hashtbl.add states uname s

let unique_name_from_name =
  let module M =
        Project_skeleton.Make_setter
          (struct let mem s = Datatype.String.Hashtbl.mem states s end)
  in
  M.make_unique_name

(* ************************************************************************** *)
(** {3 State generators} *)
(* ************************************************************************** *)

let create
    ~descr ~create ~remove ~clear ~clear_some_projects ~copy
    ~commit ~update ~on_update ~clean ~serialize ~unserialize
    ~unique_name ~name =
  let ops =
    { descr = descr;
      create = create;
      remove = remove;
      clear = clear;
      clear_some_projects = clear_some_projects;
      copy = copy;
      commit = commit;
      update = update;
      on_update = on_update;
      clean = clean;
      serialize = serialize;
      unserialize = unserialize }
  in
  let self =
    { name = name;
      unique_name = unique_name;
      private_ops = ops }
  in
  add self;
  self

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
