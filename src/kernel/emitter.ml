(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

type kind = Property_status | Code_annot | Funspec | Global_annot

type emitter =
    { e_name: string;
      kinds: kind list;
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
	      kinds = [];
              tuning_parameters = []; 
              correctness_parameters = [] } ]
	(* does not use (==) in order to prevent unmarshalling issue + in order
	   to be able to compare emitters coming from Usable_emitter.get *)
        let equal x y = Datatype.String.equal x.e_name y.e_name
        let compare x y = String.compare x.e_name y.e_name
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
      u_kinds: kind list;
      mutable used: bool;
      mutable version: int;
      (* maps below associate the parameter to its value (as a string) at the
         time of using. *)
      tuning_values: string Datatype.String.Map.t;
      correctness_values: string Datatype.String.Map.t }

let has_several_versions_ref = Extlib.mk_fun "Emitter.has_several_versions"

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
		u_kinds = [];
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

  let get e = 
    let get_params map =
      Datatype.String.Map.fold (fun s _ acc -> Parameter.get s :: acc) map []
    in
    { e_name = e.u_name;
      kinds = e.u_kinds;
      correctness_parameters = get_params e.correctness_values;
      tuning_parameters = get_params e.tuning_values }

  let get_name e = e.u_name
  let get_unique_name e = Pretty_utils.sfprintf "%a" pretty e

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

module Names = 
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Datatype.Unit)
    (struct
      let dependencies = []
      let size = 17
      let name = "Emitter.Names"
     end)

let create name kinds ~correctness ~tuning = 
  if Names.mem name then
    Kernel.fatal "An emitter of name `%s' already exists" name;
  Names.add name ();
  { e_name = name; 
    kinds = kinds;
    correctness_parameters = correctness;
    tuning_parameters = tuning }

let get_name e = e.e_name

let correctness_parameters e = 
  List.map (fun p -> p.Parameter.name) e.correctness_parameters

let tuning_parameters e = 
  List.map (fun p -> p.Parameter.name) e.tuning_parameters

let end_user = 
  create
    "End-User" 
    [ Property_status; Code_annot; Funspec; Global_annot ]
    ~correctness:[] 
    ~tuning:[]

let kernel = 
  create
    "Frama-C kernel" 
    [ Property_status; Funspec ] 
    ~correctness:[] 
    ~tuning:[]

(**************************************************************************)
(** {2 State of all known emitters} *)
(**************************************************************************)

module Id = State_builder.SharedCounter(struct let name = "Emitter.Ids" end)

(* For each emitter, the info required to be able to get the right usable
   emitter. *)
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
      let dependencies = [ Id.self ]
     end)

let self = Emitters.self

let has_several_versions s =
  try 
    let _, set = Emitters.find s in
    Usable_emitter.Set.cardinal !set > 1
  with Not_found -> 
    Kernel.fatal "Unknown emitter %s" s

let () = has_several_versions_ref := has_several_versions

let distinct_parameters get_them tuning e =
  let s = e.u_name in
  let values = get_them e in
  let get e s =
    Pretty_utils.sfprintf
      "%t"
      (fun fmt -> Usable_emitter.pretty_parameter fmt ~tuning e s)
  in
  try
    let _, set = Emitters.find s in
    Usable_emitter.Set.fold
      (fun e' acc -> 
	List.fold_left2
	  (fun acc s1 s2 -> 
	    if get e s1 = get e' s2 then acc
	    else Datatype.String.Set.add s1 acc)
	  acc
	  values
	  (get_them e))
      !set
      Datatype.String.Set.empty
  with Not_found ->
    Kernel.fatal "Unknown emitter %s" s

let distinct_tuning_parameters = 
  distinct_parameters Usable_emitter.tuning_parameters true

let distinct_correctness_parameters =
  distinct_parameters Usable_emitter.correctness_parameters false

(**************************************************************************)
(** {2 Kernel Internal Implementation} *)
(**************************************************************************)

(* set the value of a parameter of an emitter *)
let update_usable_emitter tuning ~used usable_e param_name value = 
  let id = Id.next () in
  let name = usable_e.u_name in
  let kinds = usable_e.u_kinds in
  let add = Datatype.String.Map.add param_name value in
  if tuning then
    { id = id;
      u_name = name;
      u_kinds = kinds;
      used = used;
      version = -1; (* delayed *)
      tuning_values = add usable_e.tuning_values;
      correctness_values = usable_e.correctness_values }
  else
    { id = id;
      u_name = name;
      u_kinds = kinds;
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
let code_annot_state = ref State.dummy
let funspec_state = ref State.dummy
let global_annot_state = ref State.dummy

let correctness_states: unit State.Hashtbl.t = State.Hashtbl.create 7

let register_correctness_parameter name kinds =
  let convert_kind = function
    | Property_status -> !property_status_state
    | Code_annot -> !code_annot_state
    | Funspec -> !funspec_state
    | Global_annot -> !global_annot_state
  in
  let state = State.get name in
  State.Hashtbl.replace correctness_states state ();
  State_dependency_graph.Static.add_dependencies
    ~from:state
    (List.map convert_kind kinds)

let parameter_hooks 
    : (unit -> unit) Datatype.String.Hashtbl.t Parameter.Hashtbl.t
    = Parameter.Hashtbl.create 97

let register_tuning_parameter emitter_name p =
  let update () =
    try
      let current, set = Emitters.find emitter_name in
      let c = !current in
      let v = c.version in
      let new_e = update_parameter true c p in
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
  try
    let tbl = Parameter.Hashtbl.find parameter_hooks p in
    Datatype.String.Hashtbl.replace tbl emitter_name update
  with Not_found ->
    Kernel.fatal "[Emitter] no hook table for parameter %s" p.Parameter.name

let () = 
  Cmdline.run_after_extended_stage
    (fun () ->
      State_selection.Static.iter
	(fun s -> 
	  let tbl = Datatype.String.Hashtbl.create 7 in
	  let p = Parameter.get (State.get_name s) in
	  Parameter.Hashtbl.add parameter_hooks p tbl;
	  let update () = Datatype.String.Hashtbl.iter (fun _ f -> f ()) tbl in
	  match p.Parameter.accessor with
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
	    a.Parameter.add_set_hook (fun _ _ -> update ()))
	(* [JS 2012/02/07] should be limited to [Plugin.get_selection_context],
	   but it is not possible while each plug-in (including Wp) is not
	   projectified *)
(*	(Plugin.get_selection_context ~is_set:false ()))*)
	(Plugin.get_selection ~is_set:false ()))

let update_table tbl =
  (* remove old stuff *)
  Emitters.iter
    (fun _ (_, all_usable_e) ->
      Usable_emitter.Set.iter
	(fun e ->
	  (* remove dependencies corresponding to old correctness parameters *)
	  Datatype.String.Map.iter 
	    (fun p _ ->
	      State_dependency_graph.Static.remove_codependencies
		~onto:!property_status_state
		[ State.get p ])
	    e.correctness_values;
	  (* remove hooks corresponding to old tuning parameters *)
	  Parameter.Hashtbl.iter 
	    (fun _ tbl -> Datatype.String.Hashtbl.clear tbl)
	    parameter_hooks)
	!all_usable_e);
  (* register new stuff *)
  Datatype.String.Hashtbl.iter
    (fun e_name (_, all_usable_e) -> 
      Usable_emitter.Set.iter
	(fun e -> 
	  Datatype.String.Map.iter
	    (fun p _ -> register_correctness_parameter p e.u_kinds)
	    e.correctness_values;
	  Datatype.String.Map.iter
	    (fun p _ -> register_tuning_parameter e_name (Parameter.get p))
	    e.tuning_values)
	!all_usable_e)
    tbl

let () = Emitters.add_hook_on_update update_table

let register_parameter tuning usable_e p =
  let usable_e = update_parameter tuning usable_e p in
  if tuning then register_tuning_parameter usable_e.u_name p
  else register_correctness_parameter p.Parameter.name usable_e.u_kinds;
  usable_e

let create_usable_emitter e =
  let id = Id.next () in
  let usable_e = 
    { id = id; 
      u_name = e.e_name; 
      u_kinds = e.kinds;
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

module Make_table
  (H: Datatype.Hashtbl)
  (E: sig 
    include Datatype.S_with_collections
    val local_clear: H.key -> 'a Hashtbl.t -> unit
    val get: t -> Usable_emitter.t 
  end)
  (D: Datatype.S) 
  (Info: sig 
    val name: string 
    val dependencies: State.t list 
    val remove_binding: H.key -> D.t -> unit
  end) =
struct

  (* this list is computed after defining [self] *)
  let static_dependencies = ref []

  let must_clear_all sel =
    List.exists (State_selection.mem sel) !static_dependencies

  (* [KNOWN LIMITATION] only works iff the selection contains the parameter'
     state. In particular, that does not work if one writes something like

     let selection = 
     State_selection.only_dependencies Kernel.MainFunction.self 
     in
     Project.clear ~selection () *)
  let must_local_clear sel =
    try 
      State.Hashtbl.iter
	(fun s () -> if State_selection.mem sel s then raise Exit)
	correctness_states;
      true
    with Exit ->
      false

  let create () = H.create 97

  let state = ref (create ())

  module Tbl = E.Hashtbl.Make(D)
  type internal_tbl = Tbl.t

  module H_datatype = H.Make(Tbl)

  (* standard projectified hashtbl, but an ad-hoc function 'clear' *)
  include State_builder.Register
  (H_datatype)
  (struct
     type t = Tbl.t H.t
     let create = create
     let clear tbl =
       let sel = Project.get_current_selection () in
       (*       Kernel.feedback "SELECT: %a" State_selection.pretty sel;*)
       if must_clear_all sel then begin
	 (* someone explicitly requires to fully reset the table *)
	 (*	 Kernel.feedback "FULL CLEAR of %s in project %a" 
		 Info.name Project.pretty (Project.current ());*)
	 H.clear tbl
       end else 
	 (* AST is unchanged *)
	 if must_local_clear sel then begin
	   (* one have to clear the table, but we have to keep the keys *)
(*	   Kernel.feedback "LOCAL CLEAR of %s in project %a"
	     Info.name Project.pretty (Project.current ());*)
	   H.iter 
	     (fun k h -> 
	       E.Hashtbl.iter (fun _ x -> Info.remove_binding k x) h;
	       E.local_clear k h)
	     tbl;
	 end else begin
	   (* we have to clear only the bindings corresponding to the selected
	      correctness parameters *)
	   let to_be_removed = ref [] in
	   H.iter
	     (fun k h -> 
	       E.Hashtbl.iter
		 (fun e x ->
		   let is_param_selected =
		     List.exists
		       (fun p -> State_selection.mem sel (State.get p))
		       (Usable_emitter.correctness_parameters (E.get e))
		   in
		   if is_param_selected then 
		     to_be_removed := (k, e, x) :: !to_be_removed)
		 h)
	     tbl;
(*	   List.iter 
	     (fun (k, _, _) -> 
	       Kernel.feedback "%a -->" H.Key.pretty k;
	       Kernel.feedback "hash %d" (H.Key.hash k);
	       Kernel.feedback "equal %b" (H.Key.equal k k))
	     !to_be_removed; *)
	   List.iter
	     (fun (k, e, x) -> 
	       try 
		 let h = H.find tbl k in
(*		 Kernel.feedback "CLEARING binding %a of %s in project %a" 
		   Usable_emitter.pretty (E.get e)
		   Info.name
		   Project.pretty (Project.current ());*)
		 E.Hashtbl.remove h e;
		 Info.remove_binding k x
	       with Not_found -> 
		 assert false) 
	     !to_be_removed
	 end
     let get () = !state
     let set x = state := x
     let clear_some_projects _f _h = false
   end)
  (struct include Info let unique_name = name end)

  (* compute which states always impact this one (i.e. [self]) *)
  let () =
    let get_dependencies () =
      State_dependency_graph.G.fold_pred
	(fun s acc -> s :: acc)
	State_dependency_graph.graph
	self
	[]    
    in
    Cmdline.run_after_early_stage
      (fun () -> static_dependencies := get_dependencies ())

  let add key v = H.add !state key v
  let find key = H.find !state key
  let mem key = H.mem !state key
  let remove key = H.remove !state key
  let iter f = H.iter f !state
  let fold f acc = H.fold f !state acc

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
