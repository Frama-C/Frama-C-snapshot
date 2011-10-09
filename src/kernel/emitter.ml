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

(* Module [Kernel] is not usable here. Thus use this module to emit messages. *)
module Output = Project_skeleton.Output

(**************************************************************************)
(** {2 Datatype} *)
(**************************************************************************)

type emitter =
    { e_name: string;
      tuning_parameters: Parameter.t list;
      correctness_parameters: Parameter.t list }

include Datatype.Make_with_collections
      (struct
	open Datatype
	type t = emitter
	let name = "Emitter.t"
	let rehash = identity
	let structural_descr = Structural_descr.Abstract
	let reprs = 
	  [ { e_name = ""; 
	      tuning_parameters = []; 
	      correctness_parameters = [] } ]
	let equal = ( == )
	let compare x y = if x == y then 0 else String.compare x.e_name y.e_name
	let hash x = String.hash x.e_name
	let copy x = x (* strings are immutable here *)
	let pretty fmt x = Format.pp_print_string fmt x.e_name
	let internal_pretty_code = undefined
	let varname _ = assert false (* unused while [internal_pretty_code]
					unimplemented *)
	let mem_project = never_any_project
       end)

type usable_emitter =
    { id: int;
      u_name: string;
      mutable used: bool;
      mutable version: int;
      (* maps below associate the parameter to its value (as a string) at the
	 time of using. *)
      tuning_values: string Datatype.String.Map.t;
      correctness_values: string Datatype.String.Map.t }

let has_several_versions_ref = Extlib.mk_fun "has_several_versions"

module Usable_emitter = struct

  include Datatype.Make_with_collections
	(struct
	  open Datatype
	  type t = usable_emitter
	  let name = "Emitter.Usable_emitter.t"
	  let rehash = identity
	  let structural_descr = Structural_descr.Abstract
	  let reprs = 
	    [ let p = Datatype.String.Map.empty in
	      { id = -1;
		u_name = "";
		used = false;
		version = -1; 
		tuning_values = p; 
		correctness_values = p } ]
	  let equal = ( == )
	  let compare x y = if x == y then 0 else Datatype.Int.compare x.id y.id
	  let hash x = Datatype.Int.hash x.id
	  let copy x = x (* strings are immutable here *)
	  let pretty fmt x = 
	    if !has_several_versions_ref x.u_name then
	      Format.fprintf fmt "%s (v%d)" x.u_name x.version
	    else
	      Format.pp_print_string fmt x.u_name
	  let internal_pretty_code = undefined
	  let varname _ = assert false (* unused while [internal_pretty_code]
					  unimplemented *)
	  let mem_project = never_any_project
	 end)

  let get_name e = e.u_name
  let get_unique_name e = Pretty_utils.sfprintf "%a" pretty e

  let compare_with_emitter ue e = Datatype.String.compare ue.u_name e.e_name

  let correctness_parameters e = 
    Datatype.String.Map.fold (fun p _ acc -> p :: acc) e.correctness_values []

  let tuning_parameters e = 
    Datatype.String.Map.fold (fun p _ acc -> p :: acc) e.tuning_values []

  let pretty_parameter fmt ~tuning e s =
    let map = if tuning then e.tuning_values else e.correctness_values in
    let v = Datatype.String.Map.find s map in
    Format.fprintf fmt "%s %s" s v

end

(**************************************************************************)
(** {2 Implementation for Plug-in Developers} *)
(**************************************************************************)

let create name ~correctness ~tuning = 
  { e_name = name; 
    correctness_parameters = correctness;
    tuning_parameters = tuning }

let get_name e = e.e_name

let correctness_parameters e = 
  List.map (fun p -> p.Parameter.name) e.correctness_parameters

let tuning_parameters e = 
  List.map (fun p -> p.Parameter.name) e.tuning_parameters

(**************************************************************************)
(** {2 State of all known emitters} *)
(**************************************************************************)

module Id = State_builder.Counter(struct let name = "Emitter.Ids" end)

(* For each emitter, the info required to be able to get the right usable
   emitter.

   Use names of emitters as keys instead of emitters (compared by (==)) in order
   to be safe when unmarshaling: if we use the emitter datatype, we need to
   rehash the keys *)
module Emitters = 	  
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Datatype.Pair
       (Datatype.Ref(Usable_emitter)) (* current usable emitter with the 
					 current parameter values *)
       (Datatype.Ref(Usable_emitter.Set))) (* existing usables emitters with
					      the old parameter values *)
    (struct 
      let name = "Emitter.Emitters" 
      let size = 7 
      let kind = `Correctness
      let dependencies = [ Id.self ]
     end)

type available_emitters = Emitters.Datatype.t

let self = Emitters.self

let has_several_versions s =
  try 
    let _, set = Emitters.find s in
    Usable_emitter.Set.cardinal !set > 1
  with Not_found -> 
    Kernel.fatal "Unknown emitter %s" s

let () = has_several_versions_ref := has_several_versions

(**************************************************************************)
(** {2 Kernel Internal Implementation} *)
(**************************************************************************)

(* set the value of a parameter of an emitter *)
let update_usable_emitter tuning ~used usable_e param_name value = 
  let id = Id.next () in
  let name = usable_e.u_name in
  let add = Datatype.String.Map.add param_name value in
  if tuning then
    { id = id;
      u_name = name;
      used = used;
      version = -1; (* delayed *)
      tuning_values = add usable_e.tuning_values;
      correctness_values = usable_e.correctness_values }
  else
    { id = id;
      u_name = name;
      used = used;
      version = -1; (* delayed *)
      tuning_values = usable_e.tuning_values;
      correctness_values = add usable_e.correctness_values }

exception Found of Usable_emitter.t

let update_parameter tuning usable_e p =
  let param_name = p.Parameter.name in
  let value = Parameter.get_value p in
  try 
    let _, set = Emitters.find usable_e.u_name in
    try
      Usable_emitter.Set.iter
	(fun e ->
	  let map = if tuning then e.tuning_values else e.correctness_values in
	  let exists =
	    try
	      Datatype.String.equal
		value
		(Datatype.String.Map.find param_name map)
	    with Not_found -> 
	      false
	  in
	  if exists then raise (Found e))
	!set;
      (* we are setting the value of a parameter, but we are not sure yet that
	 the corresponding usable emitter will be used *)
      let e = 
	update_usable_emitter tuning ~used:false usable_e param_name value 
      in
      set := Usable_emitter.Set.add e !set;
      e
    with Found e -> 
      (* we already create an usable emitter with this value for this 
	 parameter *) 
      e
  with Not_found -> 
    (* we are creating the first usable emitter of the given name:
       it is going to be used *)
    update_usable_emitter tuning ~used:true usable_e param_name value

let property_status_state = ref State.dummy

let register_parameter tuning usable_e p =
  (* TODO: the added dependency and hook are never removed,
     even if the emitter is deleted. 
     In particular, [register_parameter] may be applied once by emitter **and**
     by project, thus the same [update] may be hooked for the same emitter
     several times (one for each project) *)
  if not tuning then
    State_dependency_graph.Static.add_dependencies
      ~from:(State.get p.Parameter.name)
      [ !property_status_state ];
  let usable_e = update_parameter tuning usable_e p in
  let name = usable_e.u_name in
  let update () =
    if tuning then
      try
	let current, set = Emitters.find name in
	let c = !current in
	let v = c.version in
	let new_e = update_parameter tuning c p in
	if c.used then new_e.version <- v + 1 
	else begin
	  set := Usable_emitter.Set.remove c !set;
	  new_e.version <- v
	end;
	current := new_e
      with Not_found ->
	(* in multi-sessions mode (e.g. save/load), the emitters could exist in
	   the previous session but not in the current one. In this case, there
	   is nothing to do.

	   Additionnally, even if it still exists, it could be not yet restored
	   since the project library does not ensure that it restores the table
	   of emitters before the states of parameters. In such a case, it is
	   also possible to do nothing since the right table in the right state
	   is going to be restored. *)
	()
  in
  (match p.Parameter.accessor with
  (* factorisation requires GADT (will be in OCaml 3.13?) *)
  | Parameter.Bool(a, _) -> 
    a.Parameter.add_set_hook (fun _ _ -> update ())
  | Parameter.Int(a, _) ->  
    a.Parameter.add_set_hook (fun _ _ -> update ())
  | Parameter.String(a, _) -> 
    a.Parameter.add_set_hook (fun _ _ -> update ())
  | Parameter.String_set a -> 
    a.Parameter.add_set_hook (fun _ _ -> update ())
  | Parameter.String_list a -> 
    a.Parameter.add_set_hook (fun _ _ -> update ()));
  usable_e

let create_usable_emitter e =
  let id = Id.next () in
  let usable_e = 
    { id = id; 
      u_name = e.e_name; 
      used = true;
      version = -1; (* delayed *)
      tuning_values = Datatype.String.Map.empty;
      correctness_values = Datatype.String.Map.empty }
  in
  let usable_e = 
    List.fold_left (register_parameter true) usable_e e.tuning_parameters
  in
  let usable_e =
    List.fold_left (register_parameter false) usable_e e.correctness_parameters
  in      
  usable_e.version <- 1;
  usable_e

let get e = 
  let name = e.e_name in
  try 
    let current, _ = Emitters.find name in
    let c = !current in
    c.used <- true;
    c
  with Not_found ->
    let usable_e = create_usable_emitter e in
    Emitters.add 
      name
      (ref usable_e, ref (Usable_emitter.Set.singleton usable_e));
    usable_e

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
