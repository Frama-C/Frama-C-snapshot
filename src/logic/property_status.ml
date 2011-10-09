(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(** {3 Datatypes} *)
(**************************************************************************)

module Caml_hashtbl = Hashtbl
open Emitter

module Emitted = struct 
  type t = True | False_if_reachable | False_and_reachable | Dont_know 
end
type emitted_status = Emitted.t = 
    True | False_if_reachable | False_and_reachable | Dont_know

module Emitted_status =
  Datatype.Make_with_collections
    (struct
      type t = emitted_status
      include Datatype.Serializable_undefined
      let name = "Property_status.emitted_status"
      let reprs = [ True; False_if_reachable; False_and_reachable; Dont_know ]
      let mem_project = Datatype.never_any_project
      let pretty fmt s = 
	Format.fprintf fmt "%s"
	  (match s with
	  | True -> "VALID"
	  | False_if_reachable | False_and_reachable -> "**NOT** VALID"
	  | Dont_know -> "unknown")
      let compare (s1:t) s2 = Pervasives.compare s1 s2
      let equal (s1:t) s2 = s1 = s2
      let hash (s:t) = Caml_hashtbl.hash s
     end)

type emitter_with_properties = 
    { emitter: Usable_emitter.t; 
      properties: Property.t list;
      logical_consequence: bool }

module Emitter_with_properties =
  Datatype.Make_with_collections
    (struct
      type t = emitter_with_properties
      let name = "Property_status.emitter"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.Abstract
      let reprs = 
	List.fold_left
	  (fun acc e -> 
	    { emitter = e; 
	      properties = Property.reprs; 
	      logical_consequence = false } 
	    :: acc) 
	  [] 
	  Usable_emitter.reprs
	  
      let equal x y = Usable_emitter.equal x.emitter y.emitter
      let compare x y = Usable_emitter.compare x.emitter y.emitter
      let hash x = Caml_hashtbl.hash x.emitter
	
      let copy = Datatype.undefined
      let pretty fmt e = Usable_emitter.pretty fmt e.emitter
      let internal_pretty_code = Datatype.undefined
      let varname _ = assert false (* unused while [internal_pretty_code]
				      unimplemented *)
      let mem_project = Datatype.never_any_project
     end)

type inconsistent =
    { valid: emitter_with_properties list; 
      invalid: emitter_with_properties list }

module Local = struct
  type t =
    | Never_tried
    | Best of emitted_status * emitter_with_properties list
    | Inconsistent of inconsistent
end

type status = Local.t =
	      | Never_tried
	      | Best of emitted_status * emitter_with_properties list
	      | Inconsistent of inconsistent

module L = Datatype.Make
  (struct
    type t = status
    include Datatype.Serializable_undefined
    let name = "Property_status.t"
    let reprs = 
      let l = Emitter_with_properties.reprs in
      [ Never_tried; Best(True, []); Inconsistent { valid = l; invalid = l } ]
    let mem_project = Datatype.never_any_project
    let pretty fmt s = 
      let pp_emitters fmt l =
	Pretty_utils.pp_list ~sep:", " ~last:" and " 
	  Emitter_with_properties.pretty fmt l
      in
      match s with
      | Never_tried -> Format.fprintf fmt "no verification attempted"
      | Best(Dont_know as s, l) ->
	Format.fprintf fmt "@[%a@ @[(%a tried%s to verify@ \
but could not decide)@]@]"
	  Emitted_status.pretty s
	  pp_emitters l
	  (match l with [] | [ _ ] -> "" | _ :: _ -> " each")
      | Best(True | False_if_reachable | False_and_reachable as s, l) ->
	Format.fprintf fmt "%a according to %a%s" 
	  Emitted_status.pretty s 
	  pp_emitters l
	  (match l with
	  | [] -> assert false
	  | { properties = [] } :: _ -> ""
	  | { properties = _ :: _ } :: _ -> " (under hypotheses)")
      | Inconsistent i ->
	Format.fprintf fmt "@[<hv 2>inconsistent status:@ \
@[%a according to %a@]@ \
@[but %a according to %a@]"
	  Emitted_status.pretty True pp_emitters i.valid
	  Emitted_status.pretty False_if_reachable pp_emitters i.invalid
   end)
include L
    
(**************************************************************************)
(** {3 Projectified tables} *)
(**************************************************************************)

let register_as_kernel_logical_consequence_ref = 
  Extlib.mk_fun "register_as_kernel_logical_consequence_ref"

(* property -> emitter -> emitted_t 
   
   Quick access to emitted_t required in order to emit quickly the new status
   according to the old one. *)
module Status : sig
  type tbl = Emitted_status.t Emitter_with_properties.Hashtbl.t
  val self: State.t
  val add: Property.t -> tbl -> unit
  val find: Property.t -> tbl
  val mem: Property.t -> bool
  val remove: Property.t -> unit
  val iter: (Property.t -> tbl -> unit) -> unit
  val fold: (Property.t -> tbl -> 'a -> 'a) -> 'a -> 'a
end = struct

  let state_dependencies = 
    [ Ast.self; 
      Emitter.self 
   (* the other dependencies are postponed: 
      grep Property_status.self to find them *) ]
  
  (* this list is computed after defining [self] *)
  let static_dependencies = ref []

  let must_clear_all () =
    let sel = Project.get_current_selection () in
    try 
      List.iter
	(fun s -> if State_selection.mem sel s then raise Exit)
	!static_dependencies;
      false
    with Exit ->
      true

  let create () = Property.Hashtbl.create 97

  let state = ref (create ())

  module Tbl = Emitter_with_properties.Hashtbl.Make(Emitted_status)
  type tbl = Tbl.t

  module D = Property.Hashtbl.Make(Tbl)

  (* standard projectified hashtbl, but an ad-hoc function 'clear' *)
  include State_builder.Register
  (D)
  (struct
     type t = tbl Property.Hashtbl.t
     let create = create
     let clear tbl =
       if must_clear_all () then  
	 (* full clear *)
	 Property.Hashtbl.clear tbl
       else 
	 (* AST and annotations are unchanged: 
	    keep properties as keys, but clear associated tables *)
	 Property.Hashtbl.iter
	   (fun p h -> 
	     Emitter_with_properties.Hashtbl.clear h;
	     !register_as_kernel_logical_consequence_ref p) 
	   tbl
     let get () = !state
     let set x = state := x
     let clear_some_projects _f _h = false
   end)
  (struct 
    let dependencies = state_dependencies
    let unique_name = "Property_status"
    let name = "Property_status"
    let kind = `Correctness
   end)

  (* compute which states always impact this one (i.e. [self]) *)
  let () =
    let get_dependencies () =
      State_dependency_graph.Static.G.fold_pred
	(fun s acc -> s :: acc)
	State_dependency_graph.Static.graph
	self
	[]    
    in
    Cmdline.run_after_early_stage
      (fun () -> static_dependencies := get_dependencies ())

  let add key v = Property.Hashtbl.add !state key v
  let find key = Property.Hashtbl.find !state key
  let mem key = Property.Hashtbl.mem !state key
  let remove key = Property.Hashtbl.remove !state key
  let iter f = Property.Hashtbl.iter f !state
  let fold f acc = Property.Hashtbl.fold f !state acc

end

let self = Status.self
let () = Emitter.property_status_state := self

let iter f = Status.iter (fun p _ -> f p)
let fold f = Status.fold (fun p _ -> f p)

let auto_emitter = Emitter.create "Frama-C kernel" ~correctness:[] ~tuning:[]

(* ok to be computed once right now since there is no parameter dependency *)
let usable_auto_emitter = Emitter.get auto_emitter

let auto_emitter_with_ppts = 
  { emitter = usable_auto_emitter; 
    (* other fields not used as index *)
    properties = []; 
    logical_consequence = false }

let clear_consolidation = Extlib.mk_fun "clear_consolidation"

(**************************************************************************)
(** {3 Unconsolidated property status} *)
(**************************************************************************)

exception Inconsistent_emitted_status of emitted_status * emitted_status

(* @return [true] if the strongest is the first parameter. [false] otherwise.
   In case of equality, return [false].
   @raise Inconsistent_emitted_status if the check fails *)
let check_strongest_emitted x y = match x, y with
  | True, (False_if_reachable | False_and_reachable)
  | (False_if_reachable | False_and_reachable), True -> 
     raise (Inconsistent_emitted_status (x, y))
  | Dont_know, (True | False_if_reachable | False_and_reachable | Dont_know)
  | True, True 
  | False_if_reachable, (False_and_reachable | False_if_reachable)
  | False_and_reachable, False_and_reachable
    -> false
  | (True | False_if_reachable | False_and_reachable), Dont_know 
  | False_and_reachable, False_if_reachable 
    -> true

(* [strenghten emitter emitted_status status] gets [status] and updates it
   according to [emitted_status] (which was emitted by [emitter]): that returns
   the strongest status between them, or an inconsistency if any. *)
let strenghten emitter emitted_status status = 
  match status, emitted_status with
  | Never_tried, (True | False_if_reachable | False_and_reachable | Dont_know) 
    -> 
    (* was not tried, but now we have tried :) *)
    Best(emitted_status, [ emitter ])
  | Best(s, l), s2 when s = s2 -> 
    (* status are equal: update the emitters *)
    Best(s, emitter :: l)
  | Best(s, l), s2 (* when s <> emitted_status *) ->
    (try
       let first = check_strongest_emitted s s2 in
       if first then
	 (* the old one is the strongest, keep it *)
	 status
       else 
	 (* the new one is the strongest, replace the old one *)
	 Best(emitted_status, [ emitter ]) 
     with Inconsistent_emitted_status _ ->
       (* inconsistency detected *)
       (match s with
       | True -> 
	 assert (emitted_status = False_if_reachable
		|| emitted_status = False_and_reachable);
	 (* the old one is valid, but the new one is invalid *)
	 Inconsistent { valid = l; invalid = [ emitter ] }
       | False_if_reachable | False_and_reachable ->
	 assert (emitted_status = True);
	 (* the old one is invalid, but the new one is valid *)
	 Inconsistent { valid = [ emitter ]; invalid = l }
       | Dont_know -> assert false))
  | Inconsistent i, True -> 
    (* was already inconsistent and the new one is valid: update the valid 
       field *)
    Inconsistent { i with valid = emitter :: i.valid }
  | Inconsistent i, (False_if_reachable | False_and_reachable) -> 
    (* was already inconsistent and the new one is invalid: update the invalid 
       field *)
    Inconsistent { i with invalid = emitter :: i.invalid }
  | Inconsistent _, Dont_know -> 
    (* was already inconsistent, but the new one gets no new info: ignore it *)
    status
    
(* @return [true] if one must keep the status of the first parameter. 
   [false] otherwise. In case of equality, return [false]. *)
let merge_distinct_emitted x y = match x, y with
  | False_and_reachable, False_if_reachable
  | (False_if_reachable | False_and_reachable), (True | Dont_know)
  | Dont_know, True -> true
  | (True | False_if_reachable | Dont_know), 
    (False_if_reachable | False_and_reachable)
  | False_and_reachable, False_and_reachable
  | (True | Dont_know), Dont_know
  | True, True -> false

let rec register ppt =
  (*Kernel.feedback "REGISTERING %a in %a" Property.pretty ppt
    Project.pretty (Project.current ());*)
  if Status.mem ppt then
    Kernel.fatal "trying to register twice property `%a'.\n\
That is forbidden (kernel invariant broken)."
      Property.pretty ppt;
  let h = Emitter_with_properties.Hashtbl.create 7 in
  Status.add ppt h;
  register_as_kernel_logical_consequence ppt

(* the functions below and this one MUST be synchronized *)
and register_as_kernel_logical_consequence ppt = match ppt with
  | Property.IPAxiom _ 
  | Property.IPPredicate(Property.PKAssumes _, _, _, _) ->
    (* always valid: logical consequence of the empty conjunction *)
    logical_consequence auto_emitter ppt []
  | Property.IPAxiomatic(_, l) -> logical_consequence auto_emitter ppt l
  | Property.IPBehavior(kf, ki, b) ->
    (* logical consequence of its postconditions *)
    logical_consequence
      auto_emitter ppt (Property.ip_post_cond_of_behavior kf ki b)
  | Property.IPUnreachable(Property.UProperty p) ->
    (match Property.get_kf p with
    | None -> 
      (* invalid: global properties are always reachable *)
      emit_invalid ppt
    | Some kf ->
      (* cannot use module [Kernel_function] nor [Globals] here *)
      let f = kf.Cil_types.fundec in
      if Ast_info.Function.is_definition f then
	if Ast_info.Function.get_name f = Kernel.MainFunction.get () then
	  (* preconditions and assumes of the main are always reachable *)
	  (match p with
	  | Property.IPPredicate
	      ((Property.PKRequires _ | Property.PKAssumes _), _, _, _) ->
	    emit_invalid ppt
	  | _ -> ()))
  | Property.IPOther _  | Property.IPUnreachable(Property.UStmt _)
  | Property.IPPredicate _ | Property.IPCodeAnnot _ | Property.IPComplete _ 
  | Property.IPDisjoint _ | Property.IPAssigns _ | Property.IPFrom _ 
  | Property.IPDecrease _ | Property.IPLemma _ ->
    ()

(* the functions above and below MUST be synchronized *)
and is_kernel_logical_consequence ppt = match ppt with
  | Property.IPAxiom _
  | Property.IPAxiomatic _
  | Property.IPPredicate(Property.PKAssumes _, _, _, _)
  | Property.IPBehavior(_, _, _) ->
    true
  | Property.IPUnreachable(Property.UProperty p) ->
    (match Property.get_kf p with
    | None -> true
    | Some kf ->
      (* cannot use module [Kernel_function] nor [Globals] here *)
      let f = kf.Cil_types.fundec in
      if Ast_info.Function.is_definition f then
	if Ast_info.Function.get_name f = Kernel.MainFunction.get () then
	  (* preconditions and assumes of the main are always reachable *)
	  match p with
	  | Property.IPPredicate
	      ((Property.PKRequires _ | Property.PKAssumes _), _, _, _) ->
	    true
	  | _ -> false
	else
	  false
      else
	false)
  | Property.IPOther _  | Property.IPUnreachable(Property.UStmt _)
  | Property.IPPredicate _ | Property.IPCodeAnnot _ | Property.IPComplete _
  | Property.IPDisjoint _ | Property.IPAssigns _ | Property.IPFrom _
  | Property.IPDecrease _ | Property.IPLemma _ ->
    false

and unsafe_emit_and_get e ~hyps ~auto ppt ?(distinct=false) s =
  try
    let by_emitter = Status.find ppt in
    let emitter = 
      { emitter = Emitter.get e; 
	properties = hyps; 
	logical_consequence = auto } 
    in
    let emit s = 
      (* do not use Hashtbl.replace, see OCaml BTS #5349 *)
      Emitter_with_properties.Hashtbl.remove by_emitter emitter;
      !clear_consolidation ();
      (match s with
      | True | Dont_know | False_and_reachable ->
	Emitter_with_properties.Hashtbl.add by_emitter emitter s;
      | False_if_reachable ->
	let unreach_ppt = Property.ip_unreachable_ppt ppt in
	if is_kernel_logical_consequence unreach_ppt then 
	  emit_invalid unreach_ppt;
	Emitter_with_properties.Hashtbl.add
	  by_emitter 
	  { emitter with properties = unreach_ppt :: hyps }
	  s);
      s
    in
    (try
       if auto then 
	 (* registering again a logical consequence because dependencies change,
	    thus erase the previous (now erroneous) calculus *)
	 emit s
       else
	 let old_s = Emitter_with_properties.Hashtbl.find by_emitter emitter in
	 let first = 
	   (if distinct then merge_distinct_emitted 
	    else check_strongest_emitted)
	     s 
	     old_s 
	 in
	 if first then emit s else old_s
     with Not_found ->
       emit s)
  with Not_found ->
    (* assume that all ACSL properties are registered, except non-ACSL and
       conjunctions ones (but conjunctions are automatically computed and so
       already registered) *)
    match ppt with
    | Property.IPOther _ | Property.IPUnreachable _ -> 
      register ppt; 
      unsafe_emit_and_get e ~hyps ~auto ppt ~distinct s
    | Property.IPPredicate _ | Property.IPCodeAnnot _ | Property.IPComplete _ 
    | Property.IPDisjoint _ | Property.IPAssigns _ | Property.IPFrom _ 
    | Property.IPDecrease _ | Property.IPBehavior _
    | Property.IPAxiom _ | Property.IPAxiomatic _ | Property.IPLemma _ ->
      Kernel.fatal "unregistered property %a" Property.pretty ppt

and logical_consequence e ppt hyps = 
  ignore (unsafe_emit_and_get e ~hyps ~auto:true ppt Dont_know)

and emit_invalid ppt =
  ignore
    (unsafe_emit_and_get auto_emitter ~hyps:[] ~auto:true ppt 
       False_and_reachable)

let () = 
  register_as_kernel_logical_consequence_ref :=
    register_as_kernel_logical_consequence

let emit_and_get e ~hyps ppt ?distinct s =
  (match ppt with
  | Property.IPBehavior _ | Property.IPAxiom _ | Property.IPAxiomatic _
  | Property.IPPredicate (Property.PKAssumes _, _, _, _) ->
    Kernel.fatal
      "only the kernel should set the status of property %a"
      Property.pretty
      ppt
  | Property.IPPredicate _ | Property.IPCodeAnnot _ | Property.IPComplete _ 
  | Property.IPDisjoint _ | Property.IPAssigns _ | Property.IPFrom _ 
  | Property.IPDecrease _ | Property.IPLemma _ | Property.IPUnreachable _
  | Property.IPOther _ -> ());
  unsafe_emit_and_get e ~hyps ~auto:false ppt ?distinct s

let emit e ~hyps ppt ?distinct s = ignore (emit_and_get e ~hyps ppt ?distinct s)

let remove ppt =
(*  Kernel.feedback "REMOVING %a in %a" Property.pretty ppt
    Project.pretty (Project.current ());*)
  Status.remove ppt

let merge ~old l =
  let property_id fmt p = 
    Format.fprintf fmt "%a(%d)" Property.pretty p (Property.hash p)
  in
  (*Kernel.feedback "MERGING ###%a###@\nWITH ###%a###"
    (Pretty_utils.pp_list ~sep:"\n###" property_id) old
    (Pretty_utils.pp_list ~sep:"\n###" property_id) l; *)
  let old_h = Property.Hashtbl.create 17 in
  List.iter
    (fun p -> 
      assert (Kernel.verify (Status.mem p) "Unknown property %a" property_id p);
      Property.Hashtbl.add old_h p ()) 
    old;
  List.iter 
    (fun p -> 
      if Property.Hashtbl.mem old_h p then begin
        (* [p] belongs to both lists *)
	(*Kernel.feedback "UNCHANGED %a" Property.pretty p;*)
        Property.Hashtbl.remove old_h p;
	(* if [p] was a logical consequence, its dependencies may change *)
	register_as_kernel_logical_consequence p
      end else begin
        (* [p] belongs only to the new list *)
	(*Kernel.feedback "ADD %a" Property.pretty p;*)
        register p
      end)
    l;
  (* remove the properties which are not in the new list *)
  Property.Hashtbl.iter
    (fun p () ->
      (* Kernel.feedback "REMOVE %a" Property.pretty p; *)
      remove p) 
    old_h

let conjunction s1 s2 = match s1, s2 with
  (* order does matter *)
  | False_and_reachable, _ | _, False_and_reachable -> False_and_reachable
  | False_if_reachable, _ | _, False_if_reachable -> False_if_reachable
  | Dont_know, _ | _, Dont_know -> Dont_know
  | True, True -> True

let is_not_verifiable_but_valid ppt status = match status with
  | Never_tried | Best(Dont_know, _) ->
    (match ppt with
    | Property.IPOther _ -> 
      (* Non-ACSL properties are not verifiable *) 
      false
    | Property.IPUnreachable _ -> false
    | _ ->
      match Property.get_kf ppt with
      | None -> false
      | Some kf ->
	(* cannot use module [Kernel_function] nor [Globals] here *)
	let f = kf.Cil_types.fundec in
	if Ast_info.Function.is_definition f then
	  false
	else
	  (* postconditions of functions without code are not verifiable *)
	  match ppt with
	  | Property.IPPredicate
	      ((Property.PKEnsures _ | Property.PKTerminates), _, _, _)
	  | Property.IPAssigns _ 
	  | Property.IPFrom _ -> true
	  | _ -> false)
  | Best((True | False_if_reachable | False_and_reachable), _) 
  | Inconsistent _ ->
    false

let rec compute_automatic_status e properties = 
  let local_get p = 
    (* get the status of this emitter [e], not the best status,
       except if the emitter is the kernel itself *)
    if Emitter.Usable_emitter.compare_with_emitter e.emitter auto_emitter = 0
    then match get p with
    | Never_tried | Inconsistent _ -> Dont_know
    | Best(s, _) -> s
    else
      try
	let by_emitter = Status.find p in
	try Emitter_with_properties.Hashtbl.find by_emitter e 
	with Not_found -> 
	  (* try to know if the kernel automatically computes a status *)
	  try 
	    Emitter_with_properties.Hashtbl.find 
	      by_emitter 
	      auto_emitter_with_ppts
	  with Not_found -> 
	    Dont_know
      with Not_found ->
	(* assume that all ACSL properties are registered, 
	   except non-ACSL ones *)
	match p with
	| Property.IPOther _ | Property.IPUnreachable _ -> 
	  register p; 
	  Dont_know
	| Property.IPBehavior _ 
	| Property.IPPredicate _ | Property.IPCodeAnnot _ 
	| Property.IPComplete _ | Property.IPDisjoint _ 
	| Property.IPAssigns _ | Property.IPFrom _ | Property.IPDecrease _ 
	| Property.IPAxiom _ | Property.IPAxiomatic _ | Property.IPLemma _ ->
	  Kernel.fatal "unregistered property %a" Property.pretty p
  in
  let local_get p = 
    let s = local_get p in
    if is_not_verifiable_but_valid p (Best(s, [])) then 
      True 
    else 
      s
  in
  List.fold_left (fun s p -> conjunction s (local_get p)) True properties

and get ppt = 
  try
    let by_emitter = Status.find ppt in
    Emitter_with_properties.Hashtbl.fold 
      (fun e s acc -> 
	let s, tried = 
	  if e.logical_consequence && Emitted_status.equal s Dont_know then
	    let ppts = e.properties in
	    let new_s = compute_automatic_status e ppts in
	    match new_s with
	    | True | False_if_reachable | False_and_reachable -> 
	      (* the status is now known: register it *)
	      Emitter_with_properties.Hashtbl.replace by_emitter e new_s;
	      new_s, true
	    | Dont_know -> 
	      (* no change *)
	      new_s, 
	      (* there is a status for this logical consequence iff 
		 there is a status for one of its hypotheses *)
	      List.exists (fun p -> get p <> Never_tried) ppts
	  else
	    s, true
	in
	if tried then strenghten e s acc else acc)
      by_emitter 
      Never_tried
  with Not_found ->
    (* assume that all ACSL properties are registered, except non-ACSL ones *)
    match ppt with
    | Property.IPOther _ | Property.IPUnreachable _ -> 
      register ppt; 
      if is_kernel_logical_consequence ppt then get ppt else Never_tried
    | Property.IPBehavior _ 
    | Property.IPPredicate _ | Property.IPCodeAnnot _ | Property.IPComplete _ 
    | Property.IPDisjoint _ | Property.IPAssigns _ | Property.IPFrom _ 
    | Property.IPDecrease _ 
    | Property.IPAxiom _ | Property.IPAxiomatic _ | Property.IPLemma _ ->
      Kernel.fatal "trying to get status of unregistered property `%a'.\n\
That is forbidden (kernel invariant broken)." 
	Property.pretty ppt

(* local alias: too much local definitions of get implies name clashes *)
let get_status = get 

let automatically_proven ppt =
  is_kernel_logical_consequence ppt 
  &&
    (* nobody else tried to prove it *)
    try
      let by_emitter = Status.find ppt in
      try 
	Emitter_with_properties.Hashtbl.iter
	  (fun e _ -> 
	    if Emitter.Usable_emitter.compare_with_emitter 
	      e.emitter auto_emitter <> 0
	    then raise Exit)
	  by_emitter;
	true
      with Exit ->
	false
    with Not_found ->
      true

(**************************************************************************)
(** {3 Consolidated property status} *)
(**************************************************************************)

module Consolidation = struct

  type pending =
      Property.Set.t Usable_emitter.Map.t Usable_emitter.Map.t

  type consolidated_status =
    | Never_tried
    | Considered_valid
    | Valid of Usable_emitter.Set.t
    | Valid_under_hyp of pending
    | Unknown of pending
    | Invalid of Emitter.Usable_emitter.Set.t
    | Invalid_under_hyp of pending
    | Invalid_but_dead of pending
    | Valid_but_dead of pending
    | Unknown_but_dead of pending
    | Inconsistent of string

  module D = Datatype.Make
    (struct
      type t = consolidated_status
      include Datatype.Serializable_undefined
      let name = "Property_status.consolidated_status"
      let reprs = 
	[ Never_tried;
	  Considered_valid;
	  Valid Usable_emitter.Set.empty; 
	  Valid_under_hyp Usable_emitter.Map.empty;
	  Unknown Usable_emitter.Map.empty;
	  Invalid Usable_emitter.Set.empty; 
	  Invalid_under_hyp Usable_emitter.Map.empty; 
	  Invalid_but_dead Usable_emitter.Map.empty; 
	  Valid_but_dead Usable_emitter.Map.empty; 
	  Unknown_but_dead Usable_emitter.Map.empty; 
	  Inconsistent "" ]

      let mem_project = Datatype.never_any_project
      let pretty fmt s = 
	let pp_emitters f fmt l =
	  Pretty_utils.pp_list ~sep:", " ~last:" and " f fmt l
	in
	match s with
	| Never_tried -> Format.fprintf fmt "no verification attempted"
	| Considered_valid -> 
	  Format.fprintf fmt
	    "no verification attempted, but considered %a by external review"
	    Emitted_status.pretty Emitted.True
	| Valid set | Invalid set ->
	  Format.fprintf fmt "%a according to %a" 
	    Emitted_status.pretty
	    (match s with
	    | Valid _ -> Emitted.True
	    | Invalid _ -> Emitted.False_and_reachable
	    | _ -> assert false)
	    (pp_emitters Usable_emitter.pretty)
	    (Usable_emitter.Set.elements set)
	| Valid_under_hyp map | Invalid_under_hyp map ->
	  let l = Usable_emitter.Map.fold (fun e _ acc -> e :: acc) map [] in
	  Format.fprintf fmt "@[%a@ @[(%a according to %a, but properties \
remain to be verified)@]@]"
	    Emitted_status.pretty Emitted.Dont_know
	    Emitted_status.pretty
	    (match s with 
	    | Valid_under_hyp _ -> Emitted.True
	    | Invalid_under_hyp	_ -> Emitted.False_and_reachable
	    | _ -> assert false)
	    (pp_emitters Usable_emitter.pretty) l
	| Unknown map ->
	  let l = Usable_emitter.Map.fold (fun e _ acc -> e :: acc) map [] in
	  Format.fprintf fmt "@[%a@ @[(%a tried%s to verify@ \
but could not decide)@]@]"
	    Emitted_status.pretty Emitted.Dont_know
	    (pp_emitters Usable_emitter.pretty) l
	    (match l with [] | [ _ ] -> "" | _ :: _ -> " each")
	| Valid_but_dead map
	| Invalid_but_dead map
	| Unknown_but_dead map ->
	  let l = Usable_emitter.Map.fold (fun e _ acc -> e :: acc) map [] in
	  Format.fprintf fmt "%a according to %a, but it is dead anyway" 
	    Emitted_status.pretty 
	    (match s with
	    | Valid_but_dead _ -> Emitted.True
	    | Invalid_but_dead _ -> Emitted.False_and_reachable
	    | Unknown_but_dead _ -> Emitted.Dont_know
	    | _ -> assert false)
	    (pp_emitters Usable_emitter.pretty) l
	| Inconsistent msg -> 
	  Format.fprintf fmt "inconsistency detected:\n%s.\n\
Check your axiomatics and implicit hypotheses."
	    msg
     end)
  include D

  module Consolidated_status =
    State_builder.Hashtbl
      (Property.Hashtbl)
      (D)
      (struct
	let name = "Consolidated_status"
	let dependencies = [ Status.self ]
	let size = 97
	let kind = `Correctness
       end)

  let auto_status_emitter l = 
    Usable_emitter.Map.add usable_auto_emitter l Usable_emitter.Map.empty

  let merge_property e ppt map =
    try
      let set = Usable_emitter.Map.find e map in
      Usable_emitter.Map.add e (Property.Set.add ppt set) map
    with Not_found ->
      Usable_emitter.Map.add e (Property.Set.singleton ppt) map

  let merge_properties e set map =
    try
      let set2 = Usable_emitter.Map.find e map in
      Usable_emitter.Map.add e (Property.Set.union set set2) map
    with Not_found ->
      assert (not (Property.Set.is_empty set));
      Usable_emitter.Map.add e set map

  let flatten_map init map =
    Usable_emitter.Map.fold
      (fun _ -> Usable_emitter.Map.fold merge_properties) 
      map 
      init

  let flatten_set init h set = 
    Usable_emitter.Set.fold (fun e map -> merge_property e h map) set init

  let reduce_hypothesis_status ppt = function
    | Never_tried | Inconsistent _ ->
      let singleton_map v = 
	Usable_emitter.Map.add 
	  usable_auto_emitter 
	  v
	  Usable_emitter.Map.empty
      in
      Unknown (singleton_map (singleton_map (Property.Set.singleton ppt)))
    | Invalid_under_hyp m -> Unknown m
    | Considered_valid
    | Valid _ -> Valid Emitter.Usable_emitter.Set.empty
    | Invalid_but_dead m
    | Valid_but_dead m
    | Unknown_but_dead m -> 
      (* Must keep where are invalidities, thus keep the map.
	 But anyway, each of these three "dead" status are consolidated in the
	 same way *)
      Valid_but_dead m 
    | Valid_under_hyp m
    | Unknown m -> Unknown m
    | Invalid _ as s -> s

  (* s1 = consolidated status of previous hypotheses;
     s2 = consolidated status of hypothesis h;
     e is the emitter of s2 for property h
     issues are the issues already computed
     compute:
     - consolidated status of (h1 /\ h2) 
     - where are the issues and who finds them *)
  let hypotheses_conjunction issues h s1 s2 = match s1, s2 with
    (* order of patterns does matter *)
    | _, Never_tried
    | Considered_valid, _ | _, Considered_valid
    | Valid_under_hyp _, _ | _, Valid_under_hyp _
    | Inconsistent _, _ | _, Inconsistent _  
    | Invalid_under_hyp _, _ | _, Invalid_under_hyp _ 
    | Invalid_but_dead _, _ | _, Invalid_but_dead _
    | Unknown_but_dead _, _ | _, Unknown_but_dead _ ->
      (* handle at callsite *)
      assert false
    | Never_tried, Unknown m ->
      (* first status encountered: keep the issues of the first hypothesis *)
      assert (Usable_emitter.Map.is_empty issues);
      Unknown Usable_emitter.Map.empty, flatten_map issues m
    | Never_tried, (Valid _ | Valid_but_dead _) ->
      (* first status encountered: no issue with the first hypothesis *)
      assert (Usable_emitter.Map.is_empty issues);
      Valid Usable_emitter.Set.empty, issues
    | Invalid set1, Invalid set2 -> 
      assert (Usable_emitter.Set.is_empty set1);
      Invalid Usable_emitter.Set.empty, flatten_set issues h set2
    | _, Invalid set -> 
      Invalid Usable_emitter.Set.empty, 
      flatten_set Usable_emitter.Map.empty h set
    | Invalid set, _ -> 
      assert (Usable_emitter.Set.is_empty set);
      Invalid Usable_emitter.Set.empty, issues
    | Unknown m1, Unknown m2 ->
      assert (Usable_emitter.Map.is_empty m1);
      Unknown Usable_emitter.Map.empty, flatten_map issues m2
    | Unknown m, (Valid _ | Valid_but_dead _) 
    | (Valid _ | Valid_but_dead _), Unknown m -> 
      Unknown Usable_emitter.Map.empty, 
      flatten_map issues m
    | (Valid _ | Valid_but_dead _), (Valid _ | Valid_but_dead _) -> 
      assert (Usable_emitter.Map.is_empty issues);
      Valid Usable_emitter.Set.empty, issues

  let singleton_map e m = Usable_emitter.Map.add e m Usable_emitter.Map.empty

  (* compute the best status [s] and add the emitter [e] if it computes [s] *)
  let choose_best_emitter old_status e (status, issues) = 
    match old_status, status with
    | _, Never_tried
    | Considered_valid, _ | _, Considered_valid
    | Valid_under_hyp _, _ | _, Valid_under_hyp _
    | Invalid_under_hyp _, _ | _, Invalid_under_hyp _
    | Valid_but_dead _, _ | _, Valid_but_dead _
    | Unknown_but_dead _, _ | _, Unknown_but_dead _
    | Inconsistent _, _ | _, Inconsistent _ 
    | Invalid _, _ (* the current best status cannot be invalid, but
		      invalid_but_dead instead *)
    | _, Invalid_but_dead _ (* the last computed status cannot be
			       invalid_but_dead, but invalid instead *)
      -> 
      Kernel.fatal "@[[Property_status] invariant of consolidation broken:@ \
either status %a or %a not allowed when choosing the best emitter@]"
	pretty old_status
	pretty status

    (* first status encountered: keep it *)
    | Never_tried, Valid _ -> Valid (Usable_emitter.Set.singleton e)
    | Never_tried, Invalid _ -> Invalid_but_dead (singleton_map e issues)
    | Never_tried, Unknown _ -> Unknown (singleton_map e issues)

    (* the old computed status remains the best one *)
    | (Valid _ | Invalid_but_dead _), Unknown _ -> 
      old_status

    (* [e] is the best *)
    | Unknown _, Valid _ -> Valid (Usable_emitter.Set.singleton e)
    | Unknown _, Invalid _ -> Invalid_but_dead (singleton_map e issues)

    (* [e] is as good as the previous best emitter *)
    | Valid set, Valid _ -> Valid (Usable_emitter.Set.add e set)
    | Invalid_but_dead m, Invalid _ -> 
      Invalid_but_dead (Usable_emitter.Map.add e issues m)
    | Unknown m, Unknown _ -> Unknown (Usable_emitter.Map.add e issues m)

    (* Inconsistency! *)
    | Invalid_but_dead m, Valid _ ->
      assert (Usable_emitter.Map.is_empty issues);
      Inconsistent
	(let l = Usable_emitter.Map.fold (fun e _ acc -> e :: acc) m [] in
	 Pretty_utils.sfprintf
	   "@[Valid for: %a (at least).@\n\
Invalid for: %a.@]"
	   Usable_emitter.pretty e
	   (Pretty_utils.pp_list ~sep:", " ~last:" and " Usable_emitter.pretty) 
	   l)
    | Valid set, Invalid _ ->
      Inconsistent
	(let l = Usable_emitter.Set.elements set in
	 Pretty_utils.sfprintf
	   "@[Valid for: %a.@\n\
Invalid for: %a (at least).@]"
	   (Pretty_utils.pp_list ~sep:", " ~last:" and " Usable_emitter.pretty) 
	   l
	   Usable_emitter.pretty 
	   e)

  let mk_issue e ppt = 
    Usable_emitter.Map.add
      e
      (Property.Set.singleton ppt) 
      Usable_emitter.Map.empty

  let issues_without_emitter issues = 
    Usable_emitter.Map.fold
      (fun _ -> Usable_emitter.Map.fold Usable_emitter.Map.add) 
      issues
      Usable_emitter.Map.empty

  let hyp_issues emitters issues = 
    let m = issues_without_emitter issues in
    List.fold_left
      (fun acc ep -> Usable_emitter.Map.add ep.emitter m acc) 
      Usable_emitter.Map.empty
      emitters 

  let local_hyp_issues emitters ppt issues =
    let m = issues_without_emitter issues in
    List.fold_left
      (fun acc ep -> 
	let e = ep.emitter in
	Usable_emitter.Map.add e (merge_property e ppt m) acc)
      Usable_emitter.Map.empty
      emitters

  let merge_hypotheses_and_local_status ppt hyps_status local_status =
    match hyps_status, local_status with

    (* impossible cases: handle at callsite *)
    | Never_tried, _ 
    | Considered_valid, _
    | Valid_under_hyp _, _
    | Invalid_under_hyp _, _
    | Valid_but_dead _, _
    | Unknown_but_dead _, _
    | Invalid _, _
    | _, Local.Never_tried ->
      Kernel.fatal "@[[Property_status] invariant of consolidation broken:@ \
either status %a or %a not allowed when merging status@]"
	pretty hyps_status
	L.pretty local_status

    (* status of hypotheses = valid;
       filter emitters by the one for which hypotheses are valid *)
    | Valid set, Best(Emitted.Dont_know, _) -> 
      let mk e = mk_issue e ppt in
      let map = 
	Usable_emitter.Set.fold
	  (fun e -> Usable_emitter.Map.add e (mk e)) 
	  set
	  Usable_emitter.Map.empty
      in
      Unknown map
    | Valid _, Best(Emitted.True, _) ->
      hyps_status
    | Valid set, 
      Best((Emitted.False_and_reachable | Emitted.False_if_reachable), _) ->
      Invalid set
    | Valid set, (Local.Inconsistent i as s) ->
      let mk = 
	let internal_map = 
	  Usable_emitter.Map.add 
	    usable_auto_emitter
	    (Property.Set.singleton ppt)
	    Usable_emitter.Map.empty
	in
	List.fold_left
	  (fun acc ep -> 
	    let e = ep.emitter in
	    if Usable_emitter.Set.mem e set then 
	      Usable_emitter.Map.add e internal_map acc
	    else 
	      acc)
	  Usable_emitter.Map.empty
      in
      let valid_map = mk i.valid in
      let invalid_map = mk i.invalid in
      (* something strange locally appears: the only way that there is no
	 global inconsistency if that this program point is actually dead *)
      if Usable_emitter.Map.is_empty valid_map then begin
	assert (not (Usable_emitter.Map.is_empty invalid_map));
	Invalid_but_dead invalid_map
      end else
	if Usable_emitter.Map.is_empty invalid_map then Valid_but_dead valid_map
	else Inconsistent (Pretty_utils.sfprintf "%a" L.pretty s)

    (* status of hypotheses = invalid (encoded by invalid_but_dead) *)
    | Invalid_but_dead m, 
	  Best((Emitted.False_and_reachable | Emitted.False_if_reachable), _) ->
      Invalid_but_dead m
    | Invalid_but_dead m, Best(Emitted.True, _) -> 
      Valid_but_dead m
    | Invalid_but_dead m, (Best(Emitted.Dont_know, _) | Local.Inconsistent _) ->
      Unknown_but_dead m

    (* status of hypotheses = dont_know *)
    | Unknown m, Best(Emitted.True, _) ->
      Valid_under_hyp m
    | Unknown m, Best((Emitted.False_if_reachable 
			    | Emitted.False_and_reachable), _) ->
      Invalid_under_hyp m
    | Unknown m, Best(Emitted.Dont_know, emitters) ->
      Unknown (local_hyp_issues emitters ppt m)
    | Unknown m, Local.Inconsistent _ ->
      Unknown m

    (* status of hypotheses = inconsistent *)
    | Inconsistent _, _ ->  hyps_status

  let visited_ppt = Property.Hashtbl.create 97
    
  let rec memo_consolidated ppt =
    Consolidated_status.memo 
      (fun ppt ->
	if Property.Hashtbl.mem visited_ppt ppt then begin
	  (*	  Inconsistent "mutual dependency with one of its hypothesis"*)
	  (* [JS 2011/09/14] the above line [Inconsistent ...] is the correct
	     one, but... temporary allow crazy plug-ins to declare cycles
	     without any verification *) 
	  Considered_valid
	end else begin
	  Property.Hashtbl.add visited_ppt ppt ();
	  let status = get ppt in
	  let consolidated_status =
	    if is_not_verifiable_but_valid ppt status then 
	      Considered_valid
	    else
	      match status with
	      | Local.Never_tried -> Never_tried
	      | Best(_, l) as local -> 
		let status = consolidated_emitters l in
		(*	      Kernel.feedback "status of hypotheses of %a: %a" 
			      Property.pretty ppt
			      pretty hyps_status;*)
		let s = merge_hypotheses_and_local_status ppt status local in
		(*	      Kernel.feedback "consolidated status of %a: %a" 
			      Property.pretty ppt
			      pretty s;*)
		s
	      | Local.Inconsistent { valid = valid; invalid = invalid } 
		  as local -> 
		let hyps_status = consolidated_emitters (valid @ invalid) in
		merge_hypotheses_and_local_status ppt hyps_status local
	  in
	    (* [JS 2011/09/14] uncomment the following lines when
	       re-implementing the fully correct solution with inconsistency *)
(*	  try 
	    (* was previously added during its own calculus 
	       in case of mutual dependency *)
	    Consolidated_status.find ppt
	  with Not_found ->*)
	    consolidated_status
	end)
      ppt

  and consolidated_emitters l = 
    (* [l] is the list of the best emitters of the local status of [ppt]. 
       As they emit the same local status, we only choose the best one according
       to the status of their hypotheses. *)
    let status = 
      List.fold_left
	(fun current_status e -> 
	  let (s, issues) =
	    (* compute the status of conjunction of hypotheses of [e],
	       with related issues *)
	    List.fold_left
	      (fun (status, issues) h -> 
		let s = memo_consolidated h in
		let s = reduce_hypothesis_status h s in
		(*	      		Kernel.feedback "status of hypothesis %a (for %a): %a" 
					Property.pretty h
					Property.pretty ppt
					pretty s;*)
		hypotheses_conjunction issues h status s)
	      (Never_tried, Usable_emitter.Map.empty)
	      e.properties
	  in
	  let hyps_status = match s with
	    | Never_tried -> 
	      (* if no hypothesis, status of hypotheses must be valid *)
	      Valid (Usable_emitter.Set.singleton usable_auto_emitter)
	    | Valid _ | Invalid _  | Unknown _ -> s
	    | Considered_valid | Inconsistent _
	    | Valid_under_hyp _ | Invalid_under_hyp _
	    | Valid_but_dead _ | Invalid_but_dead _ | Unknown_but_dead _ -> 
	      Kernel.fatal "@[[Property_status] invariant of consolidation \
broken:@ status %a not allowed when simplifying hypothesis status@]"
		pretty s

	  in
	  let cur =
	    choose_best_emitter current_status e.emitter (hyps_status, issues)
	  in
	  (*	  Kernel.feedback
		  "status of hypotheses for emitter `%a': %a" 
		  Usable_emitter.pretty e.emitter pretty s;
		  Kernel.feedback "current best status: %a" pretty cur;*)
	  cur)
	Never_tried
	l
    in
    match status with
    | Never_tried ->
      (* if no hypothesis, status of hypotheses must be valid *)
      Valid (Usable_emitter.Set.singleton usable_auto_emitter)
    | _ -> status

    let get ppt =
      let s = memo_consolidated ppt in
      Property.Hashtbl.clear visited_ppt;
      s

  let get_conjunction ppts =
    let tmp = Property.ip_other "$Feedback.tmp$" None Cil_types.Kglobal in
    logical_consequence auto_emitter tmp ppts ;
    let s = get tmp in
    Status.remove tmp ;
    Consolidated_status.remove tmp ;
    s

end

module Feedback = struct

  type t =
    | Never_tried
    | Considered_valid 
    | Valid
    | Valid_under_hyp
    | Unknown
    | Invalid
    | Invalid_under_hyp
    | Invalid_but_dead
    | Valid_but_dead
    | Unknown_but_dead
    | Inconsistent

  let from_consolidation = function
    | Consolidation.Never_tried -> Never_tried
    | Consolidation.Considered_valid -> Considered_valid
    | Consolidation.Valid _ -> Valid
    | Consolidation.Valid_under_hyp _ -> Valid_under_hyp
    | Consolidation.Unknown _ -> Unknown
    | Consolidation.Invalid _ -> Invalid
    | Consolidation.Invalid_under_hyp _ -> Invalid_under_hyp
    | Consolidation.Invalid_but_dead _ -> Invalid_but_dead
    | Consolidation.Valid_but_dead _ -> Valid_but_dead
    | Consolidation.Unknown_but_dead _ -> Unknown_but_dead
    | Consolidation.Inconsistent _ -> Inconsistent

  let get p = from_consolidation (Consolidation.get p)
  let get_conjunction l = from_consolidation (Consolidation.get_conjunction l)

end

(**************************************************************************)
(** {3 Consolidation graph} *)
(**************************************************************************)

module Consolidation_graph = struct

  type v = 
    | Property of Property.t
    | Emitter of string
    | Tuning_parameter of string
    | Correctness_parameter of string

  module Vertex = struct

    type t = v

    let compare v1 v2 = match v1, v2 with
      | Property p1, Property p2 -> Property.compare p1 p2
      | Emitter s1, Emitter s2 -> String.compare s1 s2
      | Tuning_parameter s1, Tuning_parameter s2
      | Correctness_parameter s1, Correctness_parameter s2 ->
	String.compare s1 s2
      | Property _, _ 
      | Emitter _, (Tuning_parameter _ | Correctness_parameter _)
      | Tuning_parameter _, Correctness_parameter _ -> 1
      | _, _ -> -1

    let equal v1 v2 = compare v1 v2 = 0

    let hash = function
      | Property p -> Caml_hashtbl.hash (0, Property.hash p)
      | Emitter s -> Caml_hashtbl.hash (1, s)
      | Tuning_parameter s -> Caml_hashtbl.hash (2, s)
      | Correctness_parameter s -> Caml_hashtbl.hash (3, s)

  end

  module Edge = struct

    include Datatype.Option_with_collections
      (Emitted_status)
      (struct let module_name = "Property_status.Consolidation_graph.Edge" end)

    let default = None

  end

  module G = Graph.Persistent.Digraph.ConcreteLabeled(Vertex)(Edge)
  module G_oper = Graph.Oper.P(G)

  module Graph_by_property =
    State_builder.Hashtbl
      (Property.Hashtbl)
      (Datatype.Make
	 (struct
	   type t = G.t
	   let name = "consolidation graph"
	   let reprs = [ G.empty ]
	   include Datatype.Serializable_undefined
	  end))
      (struct
	let name = "Consolidation graph"
	let size = 97
	let kind = `Correctness
	let dependencies = [ Consolidation.Consolidated_status.self ]
       end)

  type t = G.t

  let get_parameter_string ~tuning e s =
    Pretty_utils.sfprintf
      "%t"
      (fun fmt -> Usable_emitter.pretty_parameter fmt ~tuning e s)

  let already_done = Property.Hashtbl.create 17

  let rec get ppt =
    Graph_by_property.memo
      (fun ppt ->
	(* [JS 2011/07/21] Only the better proof is added on the graph.  For
	   instance, if the consolidated status is valid thanks to WP, it does
	   not show the dont_know proof tried by Value. *)
	if Property.Hashtbl.mem already_done ppt then G.empty
	else begin
	  Property.Hashtbl.add already_done ppt ();
	  let v_ppt = Property ppt in
	  (* adding the property *)
	  let g = G.add_vertex G.empty v_ppt in
	  match get_status ppt with
	  | Never_tried -> g
	  | Best(s, emitters) -> 
	    get_emitters g v_ppt s emitters
	  | Inconsistent i ->
	    let g = get_emitters g v_ppt True i.valid in
	    get_emitters g v_ppt False_and_reachable i.invalid
	end)
      ppt

  and get_emitters g v_ppt s l =
    List.fold_left
      (fun g e -> 
	let emitter = e.emitter in
	let v_e = Emitter (Usable_emitter.get_unique_name emitter) in
	(* adding the emitter with its computed status *)
	let g = G.add_edge_e g (v_ppt, Some s, v_e) in
	let g = 
	  (* adding the tuning parameters *)
	  List.fold_left
	    (fun g p -> 
	      let s = get_parameter_string ~tuning:true emitter p in
	      G.add_edge g v_e (Tuning_parameter s))
	    g
	    (Usable_emitter.tuning_parameters emitter)
	in
	let g = 
	  (* adding the correctness parameters *)
	  List.fold_left
	    (fun g p -> 
	      let s = get_parameter_string ~tuning:false emitter p in
	      G.add_edge g v_e (Correctness_parameter s))
	    g
	    (Usable_emitter.correctness_parameters emitter)
	in
	(* adding the hypotheses *)
	List.fold_left
	  (fun g h -> 
	    let g' = get h in
	    let union = G.fold_edges_e (fun e g -> G.add_edge_e g e) g g' in
	    G.add_edge union v_ppt (Property h))
	  g
	  e.properties)
      g
      l

  let get ppt =
    let g = get ppt in
    Property.Hashtbl.clear already_done;
    g

  let dump graph dot_file =
    let module Dot = Graph.Graphviz.Dot
	  (struct

            include G

            let emitted_status_color = function
              | True -> 0x00ff00 (* green *)
	      | False_if_reachable | False_and_reachable -> 0xff0000 (* red *)
              | Dont_know -> 0xffa500 (* orange *)

            let status_color = function
	      | Never_tried -> 0x00ffff (* cyan *)
              | Best(s, _) -> emitted_status_color s
	      | Inconsistent _ -> 0x808080 (* gray *)

            let graph_attributes _ = []
	      
	    let vname v = 
	      let s = match v with
		| Property p -> Pretty_utils.sfprintf "%a" Property.pretty p
		| Emitter s | Tuning_parameter s | Correctness_parameter s -> s
	      in String.escaped s

	    (* TODO: potential issue here if 2 distincts properties share
	       the same vertex name ==> dot merges them into the same vertex. *)
            let vertex_name v = Pretty_utils.sfprintf "\"%s\"" (vname v)

	    let label v = `Label (vname v)

            let vertex_attributes = function
              | Property p as v ->
		let color = status_color (get_status p) in
		[ label v; `Color color; `Shape `Box; `Style `Filled ]
              | Emitter _ as v -> 
		[ label v; `Shape `Diamond; `Color 0xb0c4de; `Style `Filled ]
              | Tuning_parameter _ as v ->
		[ label v; `Style `Dotted; `Color 0xb0c4de;  ]
	      | Correctness_parameter _ as v -> [ label v; `Color 0xb0c4de ]

	    let edge_attributes e = match E.label e with
	      | None -> []
	      | Some s ->
		let c = emitted_status_color s in
		[ `Color c; `Fontcolor c; `Style `Bold ]

        let default_vertex_attributes _ = []
        let default_edge_attributes _ = []
        let get_subgraph _ = None
       end)
    in
  (* TODO: removing dot_file *)
  let cout = open_out dot_file in
  Kernel.Unicode.without_unicode (Dot.output_graph cout) graph;
  close_out cout

end

let () = 
  (* would also clear the memoized consolidation graph *)
  clear_consolidation :=
    fun () ->
      Consolidation.Consolidated_status.clear ();
      Consolidation_graph.Graph_by_property.clear ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
