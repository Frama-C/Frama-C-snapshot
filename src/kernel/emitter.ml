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

(* Modules [Hashtbl] and [Kernel] are not usable here. Thus use above modules
   instead. *)
module Output = Project_skeleton.Output

(**************************************************************************)
(** {2 Datatype} *)
(**************************************************************************)

type kind = Property_status | Alarm | Code_annot | Funspec | Global_annot

type emitter =
    { name: string;
      kinds: kind list;
      tuning_parameters: Typed_parameter.t list;
      correctness_parameters: Typed_parameter.t list }

module D =
  Datatype.Make_with_collections
    (struct
      type t = emitter
      let name = "Emitter.t"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_unknown
      let reprs = 
        [ { name = ""; 
	    kinds = [];
            tuning_parameters = []; 
            correctness_parameters = [] } ]
      (* does not use (==) in order to prevent unmarshalling issue + in order
	 to be able to compare emitters coming from Usable_emitter.get *)
      let equal x y = Datatype.String.equal x.name y.name
      let compare x y = Datatype.String.compare x.name y.name
      let hash x = Datatype.String.hash x.name
      let copy x = x (* strings are immutable here *)
      let pretty fmt x = Format.pp_print_string fmt x.name
      let internal_pretty_code = Datatype.undefined
      let varname _ = assert false (* unused while [internal_pretty_code]
        			      unimplemented *)
      let mem_project = Datatype.never_any_project
     end)

type usable_emitter =
    { u_id: int;
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
      type t = usable_emitter
      let name = "Emitter.Usable_emitter.t"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_abstract
      let reprs = 
	let p = Datatype.String.Map.empty in
	[ { u_id = -1;
	    u_name = "";
	    u_kinds = [ Property_status ];
            used = false;
            version = -1; 
            tuning_values = p; 
            correctness_values = p } ]
      let equal = ( == )
      let compare x y = if x == y then 0 else Datatype.Int.compare x.u_id y.u_id
      let hash x = Datatype.Int.hash x.u_id
      let copy x = x (* strings are immutable here *)
      let pretty fmt x = 
	let name = x.u_name in
        if !has_several_versions_ref name then
          Format.fprintf fmt "%s (v%d)" name x.version
        else
          Format.pp_print_string fmt name
      let internal_pretty_code = Datatype.undefined
      let varname _ = assert false (* unused while [internal_pretty_code]
        			      unimplemented *)
      let mem_project = Datatype.never_any_project
     end)

  let get e =
    let get_params map =
      Datatype.String.Map.fold
	(fun s _ acc -> Typed_parameter.get s :: acc) 
	map 
	[]
    in
    { name = e.u_name;
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

let names: unit Datatype.String.Hashtbl.t = Datatype.String.Hashtbl.create 7

let create name kinds ~correctness ~tuning = 
  if Datatype.String.Hashtbl.mem names name then
    Kernel.fatal "emitter %s already exists with the same parameters" name;
  let e =
    { name = name; 
      kinds = kinds;
      correctness_parameters = correctness;
      tuning_parameters = tuning }
  in
  Datatype.String.Hashtbl.add names name ();
  e

let get_name e = e.name

let correctness_parameters e = 
  List.map (fun p -> p.Typed_parameter.name) e.correctness_parameters

let tuning_parameters e = 
  List.map (fun p -> p.Typed_parameter.name) e.tuning_parameters

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

module Usable_id = 
  State_builder.SharedCounter(struct let name = "Emitter.Usable_id" end)

(* For each emitter, the info required to be able to get the right usable
   emitter. *)
module Usable_emitters_of_emitter =           
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Datatype.Pair
       (Datatype.Ref(Usable_emitter)) (* current usable emitter with the 
        				 current parameter values *)
       (Datatype.Ref(Usable_emitter.Set))) (* existing usables emitters with
        				      the old parameter values *)
    (struct 
      let name = "Emitter.Usable_emitters_of_emitter" 
      let size = 7 
      let dependencies = [ Usable_id.self ]
     end)

let self = Usable_emitters_of_emitter.self

let has_several_versions name =
  try 
    let _, set = Usable_emitters_of_emitter.find name in
    Usable_emitter.Set.cardinal !set > 1
  with Not_found -> 
    Kernel.fatal "Unknown emitter %s" name

let () = has_several_versions_ref := has_several_versions

let distinct_parameters get_them tuning e =
  let name = e.u_name in
  let values = get_them e in
  let get e s =
    Pretty_utils.sfprintf
      "%t"
      (fun fmt -> Usable_emitter.pretty_parameter fmt ~tuning e s)
  in
  try
    let _, set = Usable_emitters_of_emitter.find name in
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
    Kernel.fatal "Unknown emitter %s" name

let distinct_tuning_parameters = 
  distinct_parameters Usable_emitter.tuning_parameters true

let distinct_correctness_parameters =
  distinct_parameters Usable_emitter.correctness_parameters false

(**************************************************************************)
(** {2 Kernel Internal Implementation} *)
(**************************************************************************)

(* set the value of a parameter of an emitter *)
let update_usable_emitter tuning ~used usable_e param_name value = 
  let id = Usable_id.next () in
  let name = usable_e.u_name in
  let kinds = usable_e.u_kinds in
  let add = Datatype.String.Map.add param_name value in
  if tuning then
    { u_id = id;
      u_name = name;
      u_kinds = kinds;
      used = used;
      version = -1; (* delayed *)
      tuning_values = add usable_e.tuning_values;
      correctness_values = usable_e.correctness_values }
  else
    { u_id = id;
      u_name = name;
      u_kinds = kinds;
      used = used;
      version = -1; (* delayed *)
      tuning_values = usable_e.tuning_values;
      correctness_values = add usable_e.correctness_values }

exception Found of Usable_emitter.t

let update_parameter tuning usable_e p =
  let param_name = p.Typed_parameter.name in
  let value = Typed_parameter.get_value p in
  try 
    let _, set = Usable_emitters_of_emitter.find usable_e.u_name in
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

let kinds: (kind, State.t list) Hashtbl.t = Hashtbl.create 7

let iter_on_kinds f l = 
  List.iter
    (fun k ->
      try 
	let states = Hashtbl.find kinds k in
	f states
      with Not_found -> 
	())
    l

let correctness_states: unit State.Hashtbl.t = State.Hashtbl.create 7

let register_correctness_parameter name kinds =
  let state = State.get name in
  State.Hashtbl.replace correctness_states state ();
  iter_on_kinds (State_dependency_graph.add_dependencies ~from:state) kinds

let parameter_hooks 
    : (unit -> unit) Datatype.String.Hashtbl.t Typed_parameter.Hashtbl.t
    = Typed_parameter.Hashtbl.create 97

let register_tuning_parameter name p =
  let update () =
    try
      let current, set = Usable_emitters_of_emitter.find name in
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
    let tbl = Typed_parameter.Hashtbl.find parameter_hooks p in
    Datatype.String.Hashtbl.replace tbl name update
  with Not_found ->
    Kernel.fatal
      "[Emitter] no hook table for parameter %s" 
      p.Typed_parameter.name

let () = 
  Cmdline.run_after_extended_stage
    (fun () ->
      State_selection.Static.iter
	(fun s -> 
	  let tbl = Datatype.String.Hashtbl.create 7 in
	  let p = Typed_parameter.get (State.get_name s) in
	  Typed_parameter.Hashtbl.add parameter_hooks p tbl;
	  let update () = Datatype.String.Hashtbl.iter (fun _ f -> f ()) tbl in
	  match p.Typed_parameter.accessor with
	  (* factorisation requires GADT (OCaml 4.01) *)
	  | Typed_parameter.Bool(a, _) -> 
	    a.Typed_parameter.add_set_hook (fun _ _ -> update ())
	  | Typed_parameter.Int(a, _) ->  
	    a.Typed_parameter.add_set_hook (fun _ _ -> update ())
	  | Typed_parameter.String(a, _) -> 
	    a.Typed_parameter.add_set_hook (fun _ _ -> update ())
	  | Typed_parameter.String_set a -> 
	    a.Typed_parameter.add_set_hook (fun _ _ -> update ())
	  | Typed_parameter.String_list a -> 
	    a.Typed_parameter.add_set_hook (fun _ _ -> update ()))
	(* [JS 2012/02/07] should be limited to
	   [Option_functor.get_selection_context], but it is not possible while
	   each plug-in (including Wp) is not projectified *)
	(*	(Option_functor.get_selection_context ~is_set:false ()))*)
	(Parameter_state.get_selection ~is_set:false ()))

let update_table tbl =
  (* remove old stuff *)
  Usable_emitters_of_emitter.iter
    (fun _ (_, all_usable_e) ->
      Usable_emitter.Set.iter
	(fun e ->
	  (* remove dependencies corresponding to old correctness parameters *)
	  Datatype.String.Map.iter 
	    (fun p _ ->
	      iter_on_kinds
		(State_dependency_graph.remove_dependencies ~from:(State.get p))
		e.u_kinds)
	    e.correctness_values;
	  (* remove hooks corresponding to old tuning parameters *)
	  Typed_parameter.Hashtbl.iter 
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
	    (fun p _ -> 
	      register_tuning_parameter e_name (Typed_parameter.get p))
	    e.tuning_values)
	!all_usable_e)
    tbl

let () = Usable_emitters_of_emitter.add_hook_on_update update_table

let register_parameter tuning usable_e p =
  let usable_e = update_parameter tuning usable_e p in
  if tuning then register_tuning_parameter usable_e.u_name p
  else register_correctness_parameter p.Typed_parameter.name usable_e.u_kinds;
  usable_e

let create_usable_emitter e =
  let id = Usable_id.next () in
  let usable_e = 
    { u_id = id; 
      u_name = e.name;
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
  let name = e.name in
  try 
    let current, _ = Usable_emitters_of_emitter.find name in
    let c = !current in
    c.used <- true;
    c
  with Not_found ->
    let usable_e = create_usable_emitter e in
    Usable_emitters_of_emitter.add 
      name
      (ref usable_e, ref (Usable_emitter.Set.singleton usable_e));
    usable_e

module ED = D (* for debugging *)

module Make_table
  (H: Datatype.Hashtbl)
  (E: sig 
    include Datatype.S_with_collections
    val local_clear: H.key -> 'a Hashtbl.t -> unit
    val usable_get: t -> Usable_emitter.t 
    val get: t -> emitter
  end)
  (D: Datatype.S) 
  (Info: sig include State_builder.Info_with_size val kinds: kind list end) =
struct

  module Remove_hooks = Hook.Build(struct type t = E.t * H.key * D.t end)
  let add_hook_on_remove f = Remove_hooks.extend (fun (e, k, d) -> f e k d)
  let apply_hooks_on_remove e k d = Remove_hooks.apply (e, k, d)

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

  let create () = H.create Info.size

  let state = ref (create ())

  module Tbl = E.Hashtbl.Make(D)
  type internal_tbl = Tbl.t

  module H_datatype = H.Make(Tbl)

  let dkey = Kernel.register_category "emitter"

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
	 Kernel.debug ~dkey ~level:3 "FULL CLEAR of %s in %a" 
	   Info.name Project.pretty (Project.current ());
	 H.clear tbl
       end else 
	 (* AST is unchanged *)
	 if must_local_clear sel then begin
	   (* one have to clear the table, but we have to keep the keys *)
	   Kernel.debug ~dkey ~level:3 "LOCAL CLEAR of %s in %a"
	     Info.name Project.pretty (Project.current ());
	   H.iter 
	     (fun k h ->
	       if not (Remove_hooks.is_empty ()) then
		 E.Hashtbl.iter (fun e x -> apply_hooks_on_remove e k x) h;
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
		       (Usable_emitter.correctness_parameters (E.usable_get e))
		   in
		   if is_param_selected then 
		     to_be_removed := (k, e, x) :: !to_be_removed)
		 h)
	     tbl;
	   List.iter
	     (fun (k, e, x) -> 
	       try 
		 let h = H.find tbl k in
		 Kernel.debug ~dkey ~level:3 "CLEARING binding %a of %s in %a" 
		   ED.pretty (E.get e)
		   Info.name
		   Project.pretty (Project.current ());
		 E.Hashtbl.remove h e;
		 apply_hooks_on_remove e k x
	       with Not_found -> 
		 assert false) 
	     !to_be_removed
	 end
     let get () = !state
     let set x = state := x
     let clear_some_projects _f _h = false
   end)
  (struct 
    include Info
    let unique_name = name
    let dependencies = self :: dependencies
   end)

  let add_kind k =
    try
      let l = Hashtbl.find kinds k in
      Hashtbl.replace kinds k (self :: l)
    with Not_found ->
      Hashtbl.add kinds k [ self ]

  (* compute which states always impact this one (i.e. [self]) *)
  let () =
    List.iter add_kind Info.kinds;
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
  let iter f = H.iter f !state
  let fold f acc = H.fold f !state acc
  let remove key = 
    if not (Remove_hooks.is_empty ()) then begin
      try
	let tbl = find key in
	E.Hashtbl.iter (fun e v -> apply_hooks_on_remove e key v) tbl;
      with Not_found ->
	()
    end;    
    H.remove !state key;

end

include D

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
