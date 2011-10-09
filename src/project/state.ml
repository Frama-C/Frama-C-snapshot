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

open Project_skeleton

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

type standard_kind =
  [
  | `Correctness
  | `Internal
  ]

type user_kind =
  [
  | standard_kind
  | `Tuning
  | `Irrelevant
  ]

type kind =
  [
  | user_kind
  | `Proxy of [ `Correctness | `Internal ]
  ]

type state_on_disk =
    { on_disk_value: Obj.t;
      on_disk_computed: bool;
      on_disk_saved: bool;
      on_disk_digest: Digest.t }

type private_ops =
    { descr: Structural_descr.pack;
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
      mutable kind: kind;
      mutable cluster: cluster option;
      private_ops: private_ops }

and cluster = { c_name: string; mutable states: state list }

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
    kind = `Irrelevant;
    cluster = None;
    private_ops = dummy_private_ops () }

module Caml_hashtbl = Hashtbl

include Datatype.Make_with_collections
    (struct
      type t = state
      let name = "State"
      let structural_descr = Structural_descr.Unknown
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

let get_name s = s.name
let get_unique_name s = s.unique_name
let kind s = s.kind
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

module States = struct
  type state = t
  type t = (string, state) Caml_hashtbl.t
  let states = ref (Caml_hashtbl.create 997)
  let statics = Caml_hashtbl.create 997
  let create () = Caml_hashtbl.copy statics
  let clear _h = () (* will be done by dynamic graphs *)
  let get () = !states
  let set s = states := s
  let clear_some_projects _ _  = false
  let iter f = Caml_hashtbl.iter f !states
  let exists f =
    try
      Caml_hashtbl.iter (fun _ s -> if f s then raise Exit) !states;
      false
    with Exit ->
      true
end
open States

module States_datatype =
  Datatype.Make
    (struct
      include Datatype.Undefined
      type t = States.t
      let name = "State.States_datatype"
      let reprs = [ States.statics ]
     end)

exception Unknown
let get s = try Caml_hashtbl.find !states s with Not_found -> raise Unknown

let delete s =
  let uname = s.unique_name in
  assert (Caml_hashtbl.mem !states uname);
  Caml_hashtbl.remove !states uname

let add s is_static =
  let uname = s.unique_name in
  assert
    (Project_skeleton.Output.verify
       (not (Caml_hashtbl.mem !states uname))
       "state %S already exists."
       uname);
  Caml_hashtbl.add !states uname s;
  if is_static then Caml_hashtbl.add statics uname s

let unique_name_from_name =
  let module M =
        Project_skeleton.Make_setter
          (struct let mem s = Caml_hashtbl.mem !states s end)
  in
  M.make_unique_name

(* ************************************************************************** *)
(** {3 Cluster} *)
(* ************************************************************************** *)

module Cluster = struct

  let edit_cluster c states =
    let set_cluster s =
      if s.cluster <> None then
        Output.fatal "state %S already in a cluster." s.unique_name;
      s.cluster <- Some c
    in
    List.iter set_cluster states

  let create_and_return name states =
    if
      States.exists
        (fun s -> match s.cluster with
        | None -> false
        | Some c -> c.c_name = name)
    then
      Output.fatal "cluster %S already exists." name;
    let c = { c_name = name; states = states } in
    edit_cluster c states;
    c

  let create name states = ignore (create_and_return name states)

  exception Found of cluster

  let unsafe_extend c states =
    edit_cluster c states;
    (* assume there are less given states than the existing ones. *)
    c.states <- states @ c.states

  let extend name states =
    try
      States.iter
        (fun _ s -> match s.cluster with
        | None -> ()
        | Some c -> raise (Found c));
      Output.fatal "no existing cluster %S." name;
    with Found c ->
      unsafe_extend c states

  let states s = match s.cluster with
    | None -> []
    | Some c -> c.states

  let name s = match s.cluster with
    | None -> None
    | Some c -> Some c.c_name

  (* Cluster unmarshalling does not work if there is a cluster of name [n] of
     disk and another one with the same name in RAM.
     If such a case appear in practice, must implement a full rehashconsing
     heavier technic. *)
  let unmarshal, after_load =
    let h = Datatype.String.Hashtbl.create 7 in
    (fun name state -> match name with
    | None -> ()
    | Some n ->
      let l = [ state ] in
      try
        let c = Datatype.String.Hashtbl.find h n in
        unsafe_extend c l
      with Not_found ->
        let c = create_and_return n l in
        Datatype.String.Hashtbl.add h n c),
    (fun () -> Datatype.String.Hashtbl.clear h)

end

(* ************************************************************************** *)
(** {3 State generators} *)
(* ************************************************************************** *)

let unusable ~name unique_name =
  let s =
    { unique_name = unique_name;
      kind = `Internal;
      name = name;
      cluster = None;
      private_ops = dummy_private_ops () }
  in
  add s false;
  s

let update_unusable s k clear =
  s.kind <- k;
  s.private_ops.clear <- clear;
  s.private_ops.clear_some_projects <- fun _ _ -> false

let is_usable s = s.private_ops.clear != never_called

let create
    ~descr ~create ~remove ~clear ~clear_some_projects ~copy
    ~commit ~update ~on_update ~clean ~serialize ~unserialize
    ~unique_name ~name kind =
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
      kind = kind;
      cluster = None;
      private_ops = ops }
  in
  add self true;
  self

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
