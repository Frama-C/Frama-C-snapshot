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

type 'a accessor =
    < fold:'acc. ('a -> 'acc -> 'acc) -> 'acc -> 'acc (* folder on elements *);
 mem:('a -> bool) (* mem *) >

type 'a category = 
    { name: string; 
      ty: 'a Type.t;
      fold: 'b. ('a -> 'b -> 'b) -> 'b -> 'b;
      mem: 'a -> bool;
      mutable states: State.t list }

type 'a t = 'a category

module Categories = struct 

  module By_name = Type.String_tbl(struct type 'a t = 'a category end)

  (* categories are indexed by [ty] and [name].
     To be typable, the [ty] is encoded by its digest, which is a string *)
  let tbl
      : By_name.t Datatype.String.Hashtbl.t 
      = Datatype.String.Hashtbl.create 7

  let check c =
    try
      let internal = Datatype.String.Hashtbl.find tbl (Type.digest c.ty) in
      try 
	ignore (By_name.find internal c.name c.ty);
	(* just a warning for compatibility purpose: E.g if the kernel creates a
	   new standard category at release N, then plug-ins which already
	   create this category at release N-1 would be warned, but still work
	   as before. *)
	Cmdline.Kernel_log.warning "overriding category `%s' for type `%s'" 
	  c.name
	  (Type.name c.ty)
      with
      | By_name.Unbound_value _ -> ()
      | By_name.Incompatible_type _ -> assert false
    with Not_found ->
      ()

  let add c  = 
    check c;
    let internal =
      try Datatype.String.Hashtbl.find tbl (Type.digest c.ty)
      with Not_found -> By_name.create 7
    in
    By_name.add internal c.name c.ty c

end

let create name ty ~register states (accessor: 'a accessor) =
  let c = 
    { name; 
      ty; 
      fold = (fun x acc -> accessor#fold x acc); 
      mem = accessor#mem; 
      states } 
  in
  if register then Categories.add c else Categories.check c;
  c

let copy_and_rename name ~register c =
  let c = { c with name } in
  if register then Categories.add c else Categories.check c;
  c

let use state c = State_dependency_graph.add_codependencies ~onto:state c.states

let get_name c = c.name
let get_fold c = c.fold
let get_mem c = c.mem

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
