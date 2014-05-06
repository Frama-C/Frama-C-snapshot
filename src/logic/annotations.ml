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

open Property
open Cil_types
open Cil_datatype

(**************************************************************************)
(** {2 Utilities} *)
(**************************************************************************)

let exists_in_funspec f tbl =
  try
    Emitter.Usable_emitter.Hashtbl.iter (fun _ s -> if f s then raise Exit) tbl;
    false
  with Exit ->
    true

(**************************************************************************)
(** {2 Internal State} *)
(**************************************************************************)

module Usable_emitter = struct
  include Emitter.Usable_emitter
  let local_clear _ h = Hashtbl.clear h
  let usable_get e = e
end

module Real_globals = Globals

module Globals =
  Emitter.Make_table
    (Global_annotation.Hashtbl)
    (Usable_emitter)
    (Datatype.Unit)
    (struct
      let dependencies = [ Ast.self ]
      let name = "Annotations.Globals"
      let kinds = [ Emitter.Global_annot ]
      let size = 17
     end)
let global_state = Globals.self
let () =
  Logic_env.init_dependencies global_state;
  Ast.add_linked_state global_state;
  Globals.add_hook_on_remove
    (fun _ a () -> 
      List.iter Property_status.remove (Property.ip_of_global_annotation a))

module Model_fields =
  Emitter.Make_table
    (Cil_datatype.TypNoUnroll.Hashtbl)
    (Usable_emitter)
    (Datatype.List(Cil_datatype.Model_info))
    (struct
      let dependencies = [ Globals.self ]
      let name = "Annotations.Model_fields"
      let kinds = [ Emitter.Global_annot ]
      let size = 7
     end)
let () = Ast.add_linked_state Model_fields.self

module Funspecs =
  Emitter.Make_table
    (Kf.Hashtbl)
    (Usable_emitter)
    (Funspec)
    (struct
      let dependencies = [ Ast.self; Real_globals.Functions.self ]
      let name = "Annotations.Funspec"
      let kinds = [ Emitter.Funspec ]
      let size = 97
     end)
let funspec_state = Funspecs.self
let () = 
  Ast.add_linked_state funspec_state;
  Funspecs.add_hook_on_remove
    (fun _ kf spec ->
      let ppts = Property.ip_of_spec kf Kglobal spec in
      List.iter Property_status.remove ppts)

module Code_annots =
  Emitter.Make_table
    (Stmt.Hashtbl)
    (Usable_emitter)
    (Datatype.Ref(Datatype.List(Code_annotation)))
    (struct
      let dependencies = [ Ast.self ]
      let name = "Annotations.Code_annots"
      let kinds = [ Emitter.Code_annot; Emitter.Alarm ]
      let size = 97
     end)
let code_annot_state = Code_annots.self

let remove_alarm_ref = Extlib.mk_fun "Annotations.remove_alarm_ref"
let kf_ref = ref None
let () = 
  Ast.add_linked_state code_annot_state;
  Code_annots.add_hook_on_remove
    (fun e stmt l ->
      let kf = match !kf_ref with
	| None ->
	  (try Kernel_function.find_englobing_kf stmt 
	   with Not_found ->
	     Kernel.fatal "[Annotations] no function for stmt %a (%d)" 
	       Cil_printer.pp_stmt stmt stmt.sid)
	| Some kf -> 
	  kf
      in
      List.iter
	(fun a ->
	  !remove_alarm_ref e stmt a;
	  let ppts = Property.ip_of_code_annot kf stmt a in
	  List.iter Property_status.remove ppts)
	!l)

(**************************************************************************)
(** {2 Getting annotations} *)
(**************************************************************************)

let code_annot ?emitter ?filter stmt =
  try
    let tbl = Code_annots.find stmt in
    match emitter with
    | None ->
      let filter l acc = match filter with
	| None -> l @ acc
	| Some f ->
	  let rec aux acc = function
	    | [] -> acc
	    | x :: l -> aux (if f x then x :: acc else acc) l
	  in
	  aux acc l
      in
      Emitter.Usable_emitter.Hashtbl.fold (fun _ l acc -> filter !l acc) tbl []
    | Some e ->
      let l = !(Emitter.Usable_emitter.Hashtbl.find tbl (Emitter.get e)) in
      match filter with
      | None -> l
      | Some f -> List.filter f l
  with Not_found ->
    []

let code_annot_emitter ?filter stmt =
  try
    let tbl = Code_annots.find stmt in
    let filter e l acc =
      let e = Emitter.Usable_emitter.get e in
      match filter with
      | None -> List.map (fun a -> a, e) l @ acc
      | Some f ->
	let rec aux acc = function
	  | [] -> acc
	  | x :: l -> aux (if f e x then (x, e) :: acc else acc) l
	in
	aux acc l
    in
    Emitter.Usable_emitter.Hashtbl.fold (fun e l acc -> filter e !l acc) tbl []
  with Not_found ->
    []

let populate_spec_ref = Extlib.mk_fun "Annotations.populate_spec"

let populate_spec populate kf spec = match kf.fundec with
  | Definition _ -> false
  | Declaration _ ->
    if populate then begin
      !populate_spec_ref kf spec;
    end else
      false

let merge_assigns ~keep_empty a1 a2 = match a1, a2, keep_empty with
  | WritesAny, a, false | a, WritesAny, false
  | (WritesAny as a), _, true | _, (WritesAny as a), true -> a
  | Writes a1, Writes a2, _ -> Writes (a1 @ a2)

let merge_behavior fresh_bhv bhv =
  assert (fresh_bhv.b_name = bhv.b_name);
  fresh_bhv.b_assumes <- bhv.b_assumes @ fresh_bhv.b_assumes;
  fresh_bhv.b_requires <- bhv.b_requires @ fresh_bhv.b_requires;
  fresh_bhv.b_post_cond <- bhv.b_post_cond @ fresh_bhv.b_post_cond;
  fresh_bhv.b_assigns <-
    merge_assigns ~keep_empty:false fresh_bhv.b_assigns bhv.b_assigns;
  fresh_bhv.b_allocation <-
    Logic_utils.merge_allocation fresh_bhv.b_allocation bhv.b_allocation

let merge_behaviors fresh old =
  let init_fresh_bhvs = fresh.spec_behavior in
  let init_old_bhvs = old.spec_behavior in
(*  let pp_behav fmt b = Format.pp_print_string fmt b.b_name in
  let pp_behavs fmt = Pretty_utils.pp_list ~sep:" " pp_behav fmt in
  Format.printf "##[[ %a + %a ]]@."
    pp_behavs init_fresh_bhvs pp_behavs init_old_bhvs; *)
  let rec merge acc = function
    | [] -> acc
    | b :: tl ->
      (try
         let bhv = List.find (fun x -> x.b_name = b.b_name) init_old_bhvs in
	 merge_behavior b bhv;
       with Not_found ->
	 ());
      merge (b :: acc) tl
  in
  let rec keep acc = function
    | [] -> List.rev acc
    | b :: tl ->
      let acc =
	if List.for_all (fun x -> x.b_name <> b.b_name) init_fresh_bhvs then
          begin
	  (* do not share behaviors *)
	  ({ b with b_assumes = b.b_assumes } :: acc)
	  end else
	  acc
      in
      keep acc tl
  in
  fresh.spec_behavior <-
    merge (keep [] init_old_bhvs) (List.rev init_fresh_bhvs)

let merge_variant fresh old = match fresh.spec_variant, old.spec_variant with
  | _, None -> ()
  | Some _, Some _ -> assert false
  | None, (Some _ as v) -> fresh.spec_variant <- v

let merge_terminates fresh old =
  match fresh.spec_terminates, old.spec_terminates with
  | _, None -> ()
  | Some _, Some _ -> assert false
  | None, (Some _ as v) -> fresh.spec_terminates <- v

let merge_complete fresh old =
  fresh.spec_complete_behaviors <-
    old.spec_complete_behaviors @ fresh.spec_complete_behaviors

let merge_disjoint fresh old =
  fresh.spec_disjoint_behaviors <-
    old.spec_disjoint_behaviors @ fresh.spec_disjoint_behaviors

(* modifies [s1], let [s2] be unchanged. *)
let merge_funspec s1 s2 =
  merge_behaviors s1 s2;
  merge_variant s1 s2;
  merge_terminates s1 s2;
  merge_complete s1 s2;
  merge_disjoint s1 s2

let pre_register_funspec ?tbl ?(emitter=Emitter.end_user) ?(force=false) kf =
  (* Avoid sharing with kf.spec *)
  let spec = { kf.spec with spec_behavior = kf.spec.spec_behavior } in
  let do_it = match tbl with
    | None ->
      if force then begin
	Funspecs.remove kf;
	true
      end else
	not (Funspecs.mem kf)
    | Some _ ->
      true
  in
  if do_it then begin
    let tbl = match tbl with
      | None -> Emitter.Usable_emitter.Hashtbl.create 7
      | Some tbl -> tbl
    in
    Emitter.Usable_emitter.Hashtbl.add
      tbl (Emitter.get emitter) spec;
(*    Kernel.feedback "Registering contract of function %a (%a)" Kf.pretty kf
      Cil_printer.pp_funspec kf.spec;*)
    Funspecs.add kf tbl;
(*    Emitter.Usable_emitter.Hashtbl.iter
      (fun e spec -> Format.printf "Register for function %a, Emitter %a, spec %a@." Kf.pretty kf Emitter.Usable_emitter.pretty e Cil_printer.pp_funspec spec)
      tbl;
*)
    List.iter Property_status.register (Property.ip_of_spec kf Kglobal spec)
  end

let register_funspec ?emitter ?force kf =
  pre_register_funspec ?emitter ?force kf

exception No_funspec of Emitter.t
let generic_funspec merge get ?emitter ?(populate=true) kf =
  let merge tbl =
    (* Kernel.feedback "Getting spec of function %a" Kf.pretty kf; *)
    match emitter with
    | None ->
      let merged_spec () =
	let spec = Cil.empty_funspec () in
	Emitter.Usable_emitter.Hashtbl.iter
          (fun _e s ->
            (*Format.printf "emitter %a(%d):@\n%a@."
              Emitter.Usable_emitter.pretty _e (Obj.magic s) Cil_printer.pp_funspec s; *)
            merge spec s) tbl;
	spec
      in
      let spec = merged_spec () in
      let do_it = populate_spec populate kf spec in
      get (if do_it then merged_spec () else spec)
    | Some e ->
      try
	let s = Emitter.Usable_emitter.Hashtbl.find tbl (Emitter.get e) in
	get s
      with Not_found ->
	raise (No_funspec e)
  in
  try
    let tbl = Funspecs.find kf in
    merge tbl
  with Not_found ->
    let tbl = Emitter.Usable_emitter.Hashtbl.create 7 in
    pre_register_funspec ~tbl kf;
    merge tbl

let funspec ?emitter ?populate kf =
  generic_funspec merge_funspec ?emitter ?populate (fun x -> x) kf

(* Do not share behaviors with outside world if there's a single emitter. *)
let behaviors =
  generic_funspec merge_behaviors 
    (fun x -> List.map (fun b -> { b with b_name = b.b_name }) x.spec_behavior)

let decreases = generic_funspec merge_variant (fun x -> x.spec_variant)

let terminates = generic_funspec merge_terminates (fun x -> x.spec_terminates)

let complete =
  generic_funspec merge_complete (fun x -> x.spec_complete_behaviors)

let disjoint =
  generic_funspec merge_disjoint (fun x -> x.spec_disjoint_behaviors)

let model_fields ?emitter t =
  let rec aux acc t =
    let self_fields =
      try
        let h = Model_fields.find t in
        match emitter with
          | None ->
              Emitter.Usable_emitter.Hashtbl.fold (fun _ m acc-> m @ acc) h acc
          | Some e ->
              let e = Emitter.get e in
              try Emitter.Usable_emitter.Hashtbl.find h e @ acc
              with Not_found -> acc
      with Not_found -> acc
    in
    match t with
      | TNamed (ty,_) -> aux self_fields ty.ttype
      | _ -> self_fields
  in
  aux [] t

(**************************************************************************)
(** {2 Iterating over annotations} *)
(**************************************************************************)

let iter_code_annot f stmt =
  try
    let tbl = Code_annots.find stmt in
    Emitter.Usable_emitter.Hashtbl.iter
      (fun e l -> List.iter (f (Emitter.Usable_emitter.get e)) !l)
      tbl
  with Not_found ->
    ()

let fold_code_annot f stmt acc =
  try
    let tbl = Code_annots.find stmt in
    Emitter.Usable_emitter.Hashtbl.fold
      (fun e l acc ->
	let e = Emitter.Usable_emitter.get e in
	List.fold_left (fun acc x -> f e x acc) acc !l)
      tbl
      acc
  with Not_found ->
    acc

let iter_all_code_annot f =
  Code_annots.iter
    (fun stmt tbl ->
      Emitter.Usable_emitter.Hashtbl.iter
	(fun e l -> List.iter (f stmt (Emitter.Usable_emitter.get e)) !l)
	tbl)

let fold_all_code_annot f =
  Code_annots.fold
    (fun stmt tbl acc ->
      Emitter.Usable_emitter.Hashtbl.fold
	(fun e l acc ->
	  let e = Emitter.Usable_emitter.get e in
	  List.fold_left (fun acc x -> f stmt e x acc) acc !l)
	tbl
	acc)

let iter_global f =
  Globals.iter
    (fun g h ->
      Usable_emitter.Hashtbl.iter
	(fun e () -> f (Emitter.Usable_emitter.get e) g)
	h)

let fold_global f =
  Globals.fold
    (fun g h acc ->
      Usable_emitter.Hashtbl.fold
	(fun e () -> f (Emitter.Usable_emitter.get e) g)
	h
	acc)

let iter_spec_gen get iter f kf =
 try
    let tbl = Funspecs.find kf in
    let treat_one_emitter e spec =
      try
        let e = Emitter.Usable_emitter.get e in
        let orig = get spec in
        iter (f e) orig
      with Not_found -> 
	()
    in
    Usable_emitter.Hashtbl.iter treat_one_emitter tbl
 with Not_found ->
   ()

let iter_behaviors f =
  iter_spec_gen
    (fun s -> s.spec_behavior)
    (fun f l -> List.iter (fun b -> f { b with b_name = b.b_name}) l)
    f

let iter_complete f =
  iter_spec_gen (fun s -> s.spec_complete_behaviors) List.iter f

let iter_disjoint f =
  iter_spec_gen (fun s -> s.spec_disjoint_behaviors) List.iter f

let iter_terminates f = iter_spec_gen (fun s -> s.spec_terminates) Extlib.may f
let iter_decreases f = iter_spec_gen (fun s -> s.spec_variant) Extlib.may f

let iter_bhv_gen get iter f kf b =
  let get spec =
    let bhv = List.find (fun x -> x.b_name = b) spec.spec_behavior in
    get bhv
  in
  iter_spec_gen get iter f kf

let iter_requires f = iter_bhv_gen (fun b -> b.b_requires) List.iter f
let iter_assumes f = iter_bhv_gen (fun b -> b.b_assumes) List.iter f
let iter_ensures f = iter_bhv_gen (fun b -> b.b_post_cond) List.iter f
let iter_assigns f = iter_bhv_gen (fun b -> b.b_assigns) (fun f a -> f a) f
let iter_allocates f = iter_bhv_gen (fun b -> b.b_allocation) (fun f a -> f a) f

let fold_spec_gen get fold f kf acc =
 try
    let tbl = Funspecs.find kf in
    let treat_one_emitter e spec acc =
      try
        let e = Emitter.Usable_emitter.get e in
        let orig = get spec in
        fold (f e) orig acc
      with Not_found -> 
	acc
    in
    Usable_emitter.Hashtbl.fold treat_one_emitter tbl acc
 with Not_found -> 
   acc

let fold_behaviors f =
  fold_spec_gen
    (fun s -> s.spec_behavior)
    (fun f l acc ->
      List.fold_left (fun acc b -> f { b with b_name = b.b_name} acc) acc l)
    f

let fold_complete f =
  fold_spec_gen
    (fun s -> s.spec_complete_behaviors)
    (fun f l acc -> List.fold_left (Extlib.swap f) acc l)
    f

let fold_disjoint f =
  fold_spec_gen
    (fun s -> s.spec_disjoint_behaviors)
    (fun f l acc -> List.fold_left (Extlib.swap f) acc l)
    f

let fold_terminates f =
  fold_spec_gen
    (fun s -> s.spec_terminates) Extlib.opt_fold f

let fold_decreases f =
  fold_spec_gen (fun s -> s.spec_variant) Extlib.opt_fold f

let fold_bhv_gen get fold f kf b acc =
  let get spec =
    let bhv = List.find (fun x -> x.b_name = b) spec.spec_behavior in
    get bhv
  in
  fold_spec_gen get fold f kf acc

let fold_requires f =
  fold_bhv_gen (fun b -> b.b_requires)
    (fun f l acc -> List.fold_left (Extlib.swap f) acc l) f

let fold_assumes f =
  fold_bhv_gen (fun b -> b.b_assumes)
    (fun f l acc -> List.fold_left (Extlib.swap f) acc l) f

let fold_ensures f =
  fold_bhv_gen (fun b -> b.b_post_cond)
    (fun f l acc -> List.fold_left (Extlib.swap f) acc l) f

let fold_assigns f =
  fold_bhv_gen (fun b -> b.b_assigns) (fun f a acc -> f a acc) f

let fold_allocates f =
  fold_bhv_gen (fun b -> b.b_allocation) (fun f a acc -> f a acc) f

(**************************************************************************)
(** {2 Adding annotations} *)
(**************************************************************************)

let extend_name e pred = 
  if Emitter.equal e Emitter.end_user || Emitter.equal e Emitter.kernel 
  then pred
  else
    let names = pred.name in
    let s = Emitter.get_name e in
   if (List.mem s names) ||
     let acsl_identifier_regexp = 
       Str.regexp "^\\([\\][_a-zA-Z]\\|[_a-zA-Z]\\)[0-9_a-zA-Z]*$"
     in not (Str.string_match acsl_identifier_regexp s 0)
   then pred else { pred with name = s :: names }

(** {3 Adding code annotations} *)

let add_code_annot e ?kf stmt ca =
(*  Kernel.feedback "%a: adding code annot %a with stmt %a (%d)"
    Project.pretty (Project.current ())
    Code_annotation.pretty ca
    Stmt.pretty stmt
    stmt.sid;*)
  let convert a = 
    let c = a.annot_content in
    { a with annot_content = match c with
    | AAssert(l, p) -> AAssert(l, extend_name e p)
    | AInvariant(l, b, p) ->
      AInvariant(l, b, extend_name e p)
    | AStmtSpec _ | AVariant _ | AAssigns _ | AAllocation _ | APragma _ -> c }
  in
  let ca = convert ca in
  let e = Emitter.get e in
  let kf = match kf with 
    | None -> Kernel_function.find_englobing_kf stmt
    | Some kf -> kf 
  in
  let ppts = Property.ip_of_code_annot kf stmt ca in
  List.iter Property_status.register ppts;
  let add_emitter tbl = Emitter.Usable_emitter.Hashtbl.add tbl e (ref [ ca ]) in
  try
    let tbl = Code_annots.find stmt in
    try
      let l = Emitter.Usable_emitter.Hashtbl.find tbl e in
      l := ca :: !l;
    with Not_found ->
      add_emitter tbl
  with Not_found ->
    let tbl = Emitter.Usable_emitter.Hashtbl.create 7 in
    add_emitter tbl;
    Code_annots.add stmt tbl

let add_assert e ?kf stmt a =
  let a = Logic_const.new_code_annotation (AAssert ([],a)) in
  add_code_annot e ?kf stmt a

(** {3 Adding globals} *)

let dependencies_of_global annot =
  let c_vars = ref Cil_datatype.Varinfo.Set.empty in
  let logic_vars  = ref Cil_datatype.Logic_info.Set.empty in
  let local_logics = ref Cil_datatype.Logic_info.Set.empty in
  let vis = object
    (* do not use Visitor here, we're above it in link order.
       Anyway, there's nothing Frama-C-specific in the visitor. *)
    inherit Cil.nopCilVisitor
    method! vvrbl vi =
      if vi.vglob then c_vars := Cil_datatype.Varinfo.Set.add vi !c_vars;
      Cil.DoChildren
    method! vlogic_info_use li =
      if not (Cil_datatype.Logic_info.Set.mem li !local_logics) then
        logic_vars := Cil_datatype.Logic_info.Set.add li !logic_vars;
      Cil.DoChildren
    method! vlogic_info_decl li =
      local_logics := Cil_datatype.Logic_info.Set.add li !local_logics;
      Cil.DoChildren
  end
  in
  ignore (Cil.visitCilAnnotation vis annot);
  (!c_vars, !logic_vars)

let rec remove_declared_global_annot logic_vars = function
  | Dfun_or_pred(li,_) | Dinvariant(li,_) | Dtype_annot(li,_) ->
    Cil_datatype.Logic_info.Set.remove li logic_vars
  | Dvolatile _ | Dtype _ | Dlemma _ | Dmodel_annot _ | Dcustom_annot _ ->
    logic_vars
  | Daxiomatic (_,l,_) ->
    List.fold_left remove_declared_global_annot logic_vars l

let remove_declared_global c_vars logic_vars = function
  | GType _ | GCompTag _ | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _
  | GAsm _ | GPragma _ | GText _ ->
    c_vars, logic_vars
  | GVarDecl (_,vi,_) | GVar(vi,_,_) | GFun ({ svar = vi; },_) ->
    Cil_datatype.Varinfo.Set.remove vi c_vars, logic_vars
  | GAnnot (g,_) -> c_vars, remove_declared_global_annot logic_vars g

let insert_global_in_ast annot =
  let glob = GAnnot(annot, Cil_datatype.Global_annotation.loc annot) in
  let file = Ast.get () in
  (* We always put global annotations after types, so there's no need to
     trace their dependencies. *)
  let deps = dependencies_of_global annot in
  let rec insert_after (c_vars, logic_vars as deps) acc l =
    match l with
    | [] ->
      (* Some dependencies might be missing, but we suppose that
         caller knows what s/he's doing. *)
      List.rev (glob :: acc)
    | (GType _ | GCompTag _ | GCompTagDecl _
          | GEnumTag _ | GEnumTagDecl _ as g) :: l ->
      insert_after deps (g :: acc) l
    | g :: l ->
      let c_vars, logic_vars as deps =
	remove_declared_global c_vars logic_vars g
      in
      if Cil_datatype.Varinfo.Set.is_empty c_vars &&
        Cil_datatype.Logic_info.Set.is_empty logic_vars
      then List.rev acc @ g :: glob :: l
      else insert_after deps (g :: acc) l
  in
  let globs = insert_after deps [] file.globals in
  file.globals <- globs

let add_model_field e m =
  let e = Emitter.get e in
  let h =
    try Model_fields.find m.mi_base_type
    with Not_found ->
      let res = Emitter.Usable_emitter.Hashtbl.create 13 in
      Model_fields.add m.mi_base_type res; res
  in
  let l =
    try Emitter.Usable_emitter.Hashtbl.find h e
    with Not_found -> []
  in
  Emitter.Usable_emitter.Hashtbl.replace h e (m::l)

let unsafe_add_global e a =
(*  Kernel.feedback "adding global %a in project %a"
    Cil_printer.pp_annotation a Project.pretty (Project.current ());*)
  let h = Usable_emitter.Hashtbl.create 17 in
  Usable_emitter.Hashtbl.add h (Emitter.get e) ();
  Globals.add a h;
  (match a with
    | Dmodel_annot (m,_) -> add_model_field e m
    | _ -> ());
  List.iter Property_status.register (Property.ip_of_global_annotation a)

let add_global e a =
  unsafe_add_global e a;
  if not (Emitter.equal Emitter.end_user e) then insert_global_in_ast a

(** {3 Adding subparts of a function contract} *)

let mk_spec bhv variant terminates complete disjoint =
  { spec_behavior = bhv;
    spec_variant = variant;
    spec_terminates = terminates;
    spec_complete_behaviors = complete;
    spec_disjoint_behaviors = disjoint; }

let extend_funspec e kf mk_spec set_spec =
  let e = Emitter.get e in
  let add_emitter tbl =
    let spec = mk_spec () in
(*    Kernel.feedback "Creating spec %a" Cil_printer.pp_funspec spec;*)
    Emitter.Usable_emitter.Hashtbl.add tbl e spec
  in
  try
    let tbl = Funspecs.find kf in
    try
      let spec = Emitter.Usable_emitter.Hashtbl.find tbl e in
      set_spec spec tbl
    with Not_found ->
      add_emitter tbl
  with Not_found ->
    let tbl = Emitter.Usable_emitter.Hashtbl.create 7 in
    add_emitter tbl;
    Funspecs.add kf tbl

let add_behaviors ?(register_children=true) e kf bhvs =
  let mk_spec_all () = mk_spec bhvs None None [] [] in
  let set_spec spec _tbl =
    if register_children then
      merge_behaviors spec (mk_spec_all ())
    else
      List.iter
        (fun b ->
          if not (List.exists (fun x -> x.b_name = b.b_name) spec.spec_behavior)
          then
            merge_behaviors spec (mk_spec [b] None None [] []))
        bhvs
  in
  extend_funspec e kf mk_spec_all set_spec;
  (* update ip in property_status: the kernel relies on the behavior stored
     in the ip to determine the validity. If we change something in our
     own tables, this must be reflected there. *)
  List.iter
    (fun b ->
      if List.exists (fun b' -> b'.b_name = b.b_name) bhvs then
        let ip = Property.ip_of_behavior kf Kglobal b in
        Property_status.remove ip;
        Property_status.register ip)
    (behaviors ~populate:false kf);
  if register_children then begin
    List.iter
      (fun bhv ->
        List.iter
          (fun ip ->
            match ip with IPBehavior _ -> () | _ -> Property_status.register ip)
          (Property.ip_all_of_behavior kf Kglobal bhv))
          bhvs
  end


let add_decreases e kf v =
  let mk_spec () = mk_spec [] (Some v) None [] [] in
  let set_spec spec tbl =
    if exists_in_funspec (fun s -> s.spec_variant <> None) tbl then
      Kernel.fatal "already a variant for function %a" Kf.pretty kf;
    spec.spec_variant <- Some v
  in
  extend_funspec e kf mk_spec set_spec;
  Property_status.register (Property.ip_of_decreases kf Kglobal v)

let add_terminates e kf t =
  let mk_spec () = mk_spec [] None (Some t) [] [] in
  let set_spec spec tbl =
    if exists_in_funspec (fun s -> s.spec_terminates <> None) tbl then
      Kernel.fatal "already a terminates clause for function %a" Kf.pretty kf;
    spec.spec_terminates <- Some t
  in
  extend_funspec e kf mk_spec set_spec;
  Property_status.register (Property.ip_of_terminates kf Kglobal t)

let check_bhv_name spec flag name =
  if name = Cil.default_behavior_name then begin
    Kernel.warning
      "Trying to add default behavior in a complete or disjoint clause";
    false
  end
  else if
      List.exists (fun x -> x.b_name = name) spec.spec_behavior
  then flag
  else begin
    Kernel.warning
      "Trying to add a non-existing behavior %s \
        in a complete or disjoint clause"
      name;
    false
  end

let add_complete e kf l =
  let spec = generic_funspec ~populate:false merge_behaviors Extlib.id kf in
  if List.fold_left (check_bhv_name spec) true l then begin
    let mk_spec () = mk_spec [] None None [ l ] [] in
    let set_spec spec _tbl =
      spec.spec_complete_behaviors <- l :: spec.spec_complete_behaviors
    in
    extend_funspec e kf mk_spec set_spec;
    Property_status.register (Property.ip_of_complete kf Kglobal l)
  end

let add_disjoint e kf l =
  let spec = generic_funspec ~populate:false merge_behaviors Extlib.id kf in
  if List.fold_left (check_bhv_name spec) true l then begin
    let mk_spec () = mk_spec [] None None [] [ l ] in
    let set_spec spec _tbl =
      spec.spec_disjoint_behaviors <- l :: spec.spec_disjoint_behaviors
    in
    extend_funspec e kf mk_spec set_spec;
    Property_status.register (Property.ip_of_disjoint kf Kglobal l)
  end

let extend_behavior e kf bhv_name set_bhv =
  (* Kernel.feedback "Function %a, behavior %s" Kf.pretty kf bhv_name;*)
  let e = Emitter.get e in
  let mk_bhv () =
    let bhv = Cil.mk_behavior ~name:bhv_name () in
    set_bhv bhv;
    bhv
  in
  let add_emitter_contract tbl =
    let bhv = mk_bhv () in
    let spec = mk_spec [ bhv ] None None [] [] in
    Emitter.Usable_emitter.Hashtbl.add tbl e spec;
    bhv
  in
  let my_bhv =
    try
      let tbl = Funspecs.find kf in
      try
        let spec = Emitter.Usable_emitter.Hashtbl.find tbl e in
        try
	  let bhv =
            List.find (fun b -> b.b_name = bhv_name) spec.spec_behavior
          in
	  (* this emitter already creates this behavior *)
	  set_bhv bhv;
	  bhv
        with Not_found (* List.find *) ->
	(* unexisting behavior for this emitter *)
	let bhv = mk_bhv () in
	spec.spec_behavior <- bhv :: spec.spec_behavior;
	bhv
      with Not_found (* Emitter.Usable_emitter.Hashtbl.find *) ->
      (* this emitter never adds a spec to this contract *)
        add_emitter_contract tbl
        with Not_found (* Funspecs.find *) ->
    (* no function contract *)
          let tbl = Emitter.Usable_emitter.Hashtbl.create 7 in
          Funspecs.add kf tbl;
          add_emitter_contract tbl
  in
  let bhv =
    List.find (fun b -> b.b_name = bhv_name)
      (behaviors ~populate:false kf)
  in
  (* Property_status uses bhv to determine the validity of [ip]. We
     must update that accordingly... *)
  let ip = Property.ip_of_behavior kf Kglobal bhv in
  Property_status.remove ip;
  Property_status.register ip;
  my_bhv

let add_requires e kf bhv_name l =
  let bhv =
    extend_behavior e kf bhv_name (fun b -> b.b_requires <- l @ b.b_requires)
  in
  List.iter
    (fun p ->
      Property_status.register (Property.ip_of_requires kf Kglobal bhv p))
    l

let add_assumes e kf bhv_name l =
  if bhv_name = Cil.default_behavior_name then begin
    match l with
      | [] -> () (* adding an empty list is a no-op. *)
      | [_] ->
        Kernel.warning "Trying to add an assumes clause to default behavior"
      | _ ->
        Kernel.warning "Trying to add assumes clauses to default behavior"
  end else begin
    let bhv =
      extend_behavior e kf bhv_name (fun b -> b.b_assumes <- l @ b.b_assumes)
    in
    List.iter
      (fun p ->
        Property_status.register (Property.ip_of_assumes kf Kglobal bhv p))
      l
  end

let add_ensures e kf bhv_name l =
  let bhv =
    extend_behavior
      e kf bhv_name (fun b -> b.b_post_cond <- l @ b.b_post_cond)
  in
  List.iter
    (fun a ->
      Property_status.register (Property.ip_of_ensures kf Kglobal bhv a))
    l

let add_assigns ~keep_empty e kf bhv_name a =
  let bhv =
    extend_behavior
      e kf bhv_name
      (fun b ->
	let keep_empty =
	  keep_empty
	  &&
	    let bhvs = behaviors ~populate:false kf in
	    List.for_all
	      (fun b -> b.b_name <> bhv_name || b.b_assigns = WritesAny)
	      bhvs
	in
	b.b_assigns <- merge_assigns ~keep_empty b.b_assigns a)
  in
  (match a with
    | WritesAny -> ()
    | Writes l ->
        List.iter
          (fun f ->
            let ip =
              Property.ip_of_from kf Kglobal (Property.Id_behavior bhv) f
            in
            Property_status.remove ip;
            Property_status.register ip)
          l);
  Extlib.may
    (fun a ->
      (* All assigns of a same behavior share the property. Thus must remove the
	 previous property before adding the new one *)
      Property_status.remove a;
      Property_status.register a)
    (Property.ip_of_assigns kf Kglobal (Property.Id_behavior bhv) a)

let add_allocates e kf bhv_name a =
  let bhv =
    extend_behavior
      e kf bhv_name
      (fun b -> b.b_allocation <- Logic_utils.merge_allocation b.b_allocation a)
  in
  Extlib.may
    Property_status.register
    (Property.ip_of_allocation kf Kglobal (Property.Id_behavior bhv) a)

(**************************************************************************)
(** {2 Removing annotations} *)
(**************************************************************************)

(* use unicity: more efficient than using [List.filter ((!=) x)] *)
let filterq ?(eq = ( == )) x l =
  let rec aux acc = function
    | [] -> List.rev acc
    | y :: l ->
      if eq x y then
	(* equivalent but more efficient than List.rev acc @ l *)
	List.fold_left (fun l x -> x :: l) l acc
      else aux (y :: acc) l
  in
  aux [] l

let remove_code_annot e ?kf stmt ca =
  (*  Kernel.feedback "%a: removing code annot %a of stmt %a (%d)"
      Project.pretty (Project.current ())
      Code_annotation.pretty ca
      Stmt.pretty stmt
      stmt.sid;*)
  kf_ref := kf;
  let e = Emitter.get e in
  Code_annots.apply_hooks_on_remove e stmt (ref [ ca ]);
  kf_ref := None;
  try
    let tbl = Code_annots.find stmt in
    try
      let l = Emitter.Usable_emitter.Hashtbl.find tbl e in
      (* [JS 2012/11/08] (==) is not compatible with the equality over code
	 annot *)
      l := filterq ~eq:Code_annotation.equal ca !l;
    with Not_found ->
      ()
  with Not_found ->
    ()

(* If this function gets exported, please turn e into an Emitter.t *)
let remove_model_field (e:Usable_emitter.t) m =
  try
    let ty = m.mi_base_type in
    let h = Model_fields.find ty in
    let l = Usable_emitter.Hashtbl.find h e in
    let l' =
      List.filter (fun x -> not (Cil_datatype.Model_info.equal x m)) l
    in
    Usable_emitter.Hashtbl.replace h e l';
    Model_fields.apply_hooks_on_remove e ty l'
  with Not_found -> 
    ()

let remove_global e a =
  try
    let e = Emitter.get e in
    let h = Globals.find a in
    Usable_emitter.Hashtbl.iter
      (fun e' () ->
	if Emitter.Usable_emitter.equal e e' then begin
	  Globals.remove a;
	  (match a with
            | Dmodel_annot (m,_) -> remove_model_field e m
            | _ -> ()); 
          let file = Ast.get () in
	  file.globals <-
	    List.filter
	    (fun a' ->
	      not (Global.equal (GAnnot(a, Global_annotation.loc a)) a'))
	    file.globals;
	  Globals.apply_hooks_on_remove e a ()
	end)
      h;
  with Not_found ->
    ()

let remove_in_funspec e kf set_spec =
  try
    let tbl = Funspecs.find kf in
    let e = Emitter.get e in
    try
      let spec = Emitter.Usable_emitter.Hashtbl.find tbl e in
      (* Format.printf "Known specs for %a@." Kf.pretty kf;*)
      (* Emitter.Usable_emitter.Hashtbl.iter
        (fun e spec ->
          Format.printf "For emitter %a: %a@."
            Emitter.Usable_emitter.pretty e
            Cil_printer.pp_funspec spec) tbl; *)
      set_spec spec tbl
    with Not_found -> ()
  with Not_found ->
    assert false

let remove_behavior ?(force=false) e kf bhv =
  let set_spec spec tbl =
    (* Kernel.feedback "Current spec is %a@." Cil_printer.pp_funspec spec; *)
    (* do not use physical equality since the behaviors are almost always copied
       at some points *)
    let eq b1 b2 = b1.b_name = b2.b_name in
    spec.spec_behavior <- filterq ~eq bhv spec.spec_behavior;
    let name = bhv.b_name in
    let check get =
      if not force &&
	exists_in_funspec
	  (fun s -> List.exists (List.exists ((=) name)) (get s))
	  tbl
      then
	Kernel.fatal
	  "trying to remove a behavior used in a complete or disjoint clause"
    in
    check (fun s -> s.spec_complete_behaviors);
    check (fun s -> s.spec_disjoint_behaviors);
    (* Kernel.feedback "Removing behavior %s@." bhv.b_name; *)
    (* Kernel.feedback "New spec is %a@." Cil_printer.pp_funspec spec; *)
    List.iter Property_status.remove
      (Property.ip_all_of_behavior kf Kglobal bhv)
  in
  remove_in_funspec e kf set_spec

let remove_decreases e kf =
  let set_spec spec _tbl =
    match spec.spec_variant with
      | None -> ()
      | Some d ->
          Property_status.remove (Property.ip_of_decreases kf Kglobal d);
          spec.spec_variant <- None
  in
  remove_in_funspec e kf set_spec

let remove_terminates e kf =
  let set_spec spec _tbl =
    match spec.spec_terminates with
    | None -> ()
    | Some t ->
      Property_status.remove (Property.ip_of_terminates kf Kglobal t);
      spec.spec_terminates <- None
  in
  remove_in_funspec e kf set_spec

let remove_complete e kf l =
  let set_spec spec _tbl =
    spec.spec_complete_behaviors <- filterq l spec.spec_complete_behaviors
  in
  remove_in_funspec e kf set_spec;
  Property_status.remove (Property.ip_of_complete kf Kglobal l)

let remove_disjoint e kf l =
  let set_spec spec _tbl =
    spec.spec_disjoint_behaviors <- filterq l spec.spec_disjoint_behaviors
  in
  remove_in_funspec e kf set_spec;
  Property_status.remove (Property.ip_of_disjoint kf Kglobal l)

let remove_requires e kf p =
  let set_spec spec _tbl =
    List.iter
      (fun b ->
        if List.memq p b.b_requires then begin
          b.b_requires <- filterq p b.b_requires;
          Property_status.remove (Property.ip_of_requires kf Kglobal b p)
        end)
      spec.spec_behavior
  in
  remove_in_funspec e kf set_spec

let remove_assumes e kf p =
  let set_spec spec _tbl =
    List.iter
      (fun b ->
        if List.memq p b.b_assumes then begin
          b.b_assumes <- filterq p b.b_assumes;
          Property_status.remove (Property.ip_of_assumes kf Kglobal b p)
        end)
      spec.spec_behavior
  in
  remove_in_funspec e kf set_spec

let remove_ensures e kf p =
  let set_spec spec _tbl =
    List.iter
      (fun b ->
        if List.memq p b.b_post_cond then begin
          b.b_post_cond <- filterq p b.b_post_cond;
          Property_status.remove (Property.ip_of_ensures kf Kglobal b p)
        end)
      spec.spec_behavior
  in
  remove_in_funspec e kf set_spec

let remove_allocates e kf p =
  let set_spec spec _tbl =
    List.iter
      (fun b ->
        if b.b_allocation == p then begin
          b.b_allocation <- FreeAllocAny;
          Extlib.may Property_status.remove
            (Property.ip_of_allocation kf Kglobal (Id_behavior b) p)
        end)
      spec.spec_behavior
  in
  remove_in_funspec e kf set_spec

let remove_assigns e kf p =
  let set_spec spec _tbl =
    List.iter
      (fun b ->
        if b.b_assigns == p then begin
          b.b_assigns <- WritesAny;
          Extlib.may Property_status.remove
            (Property.ip_of_assigns kf Kglobal (Id_behavior b) p);
          (match p with
            | WritesAny -> ()
            | Writes l ->
                List.iter
                  (fun f ->
                    Property_status.remove
                      (Property.ip_of_from kf Kglobal (Id_behavior b) f)) l)
        end)
      spec.spec_behavior
  in
  remove_in_funspec e kf set_spec

let remove_behavior_components e kf b =
  List.iter (remove_requires e kf) b.b_requires;
  List.iter (remove_assumes e kf) b.b_assumes;
  List.iter (remove_ensures e kf) b.b_post_cond;
  remove_assigns e kf b.b_assigns;
  remove_allocates e kf b.b_allocation

(**************************************************************************)
(** {2 Other useful functions} *)
(**************************************************************************)

let has_code_annot ?emitter stmt =
  match emitter with
  | None -> Code_annots.mem stmt
  | Some e ->
    try
      let tbl = Code_annots.find stmt in
      Emitter.Usable_emitter.Hashtbl.mem tbl (Emitter.get e)
    with Not_found ->
      false

exception Found of Emitter.t
let emitter_of_global a =
  let h = Globals.find a in
  try
    Emitter.Usable_emitter.Hashtbl.iter
      (fun e () -> raise (Found (Emitter.Usable_emitter.get e)))
      h;
    assert false
  with Found e -> e

let logic_info_of_global s =
  let check_logic_info li acc =
    if li.l_var_info.lv_name = s then li::acc else acc
  in
  let rec check_one acc = function
    | Dfun_or_pred(li,_) | Dinvariant(li,_) | Dtype_annot(li,_) ->
      check_logic_info li acc
    | Daxiomatic (_,l,_) -> List.fold_left check_one acc l
    | Dtype _ | Dvolatile _ | Dlemma _ | Dmodel_annot _ | Dcustom_annot _
      -> acc
  in
  fold_global (fun _ g acc -> check_one acc g) []

let behavior_names_of_stmt_in_kf kf = match kf.fundec with
  | Definition(def, _) ->
    List.fold_left
      (fun known_names stmt ->
        List.fold_left
          (fun known_names (_bhv,spec) ->
            (List.map (fun x -> x.b_name) spec.spec_behavior) @ known_names)
          known_names
          (Logic_utils.extract_contract (code_annot stmt)))
      []
      def.sallstmts
  | Declaration _ ->
    []

let spec_function_behaviors kf =
  List.map (fun x -> x.b_name) (behaviors ~populate:false kf)

let all_function_behaviors kf =
  behavior_names_of_stmt_in_kf kf @ spec_function_behaviors kf

(* [JS 2012/06/01] TODO: better way to generate fresh name *)
let fresh_behavior_name kf name =
  let existing_behaviors = all_function_behaviors kf in
  let rec aux i =
    let name = name ^ "_" ^ (string_of_int i) in
    if List.mem name existing_behaviors then aux (i+1)
    else name
  in
  if List.mem name existing_behaviors then aux 0 else name

let code_annot_of_kf kf = match kf.fundec with
  | Definition(f, _) ->
    List.fold_left
      (fun acc stmt ->
        fold_code_annot (fun _ a acc -> (stmt, a) :: acc) stmt acc)
      []
      f.sallstmts
  | Declaration _ ->
    []

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
