(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Smart constructors for the logic.
    @plugin development guide *)

(** {1 Identification Numbers} *)

module AnnotId =
  State_builder.SharedCounter(struct let name = "annot_counter" end)
module PredicateId =
  State_builder.SharedCounter(struct let name = "predicate_counter" end)
module TermId =
  State_builder.SharedCounter(struct let name = "term_counter" end)


let new_code_annotation annot =
  { annot_content = annot ; annot_id = AnnotId.next () }

let fresh_code_annotation = AnnotId.next

let new_predicate p =
  { ip_id = PredicateId.next ();
    ip_content = p.content; ip_loc = p.loc; ip_name = p.name }

let fresh_predicate_id = PredicateId.next

let pred_of_id_pred p =
  { name = p.ip_name; loc = p.ip_loc; content = p.ip_content }

let refresh_predicate p = { p with ip_id = PredicateId.next () }

let new_identified_term t =
  { it_id = TermId.next (); it_content = t }

let fresh_term_id = TermId.next

let refresh_identified_term d = new_identified_term d.it_content

let refresh_identified_term_list = List.map refresh_identified_term

let refresh_deps = function
  | FromAny -> FromAny
  | From l ->
    From(refresh_identified_term_list l)

let refresh_from (a,d) = (new_identified_term a.it_content, refresh_deps d)

let refresh_allocation = function
    | FreeAllocAny -> FreeAllocAny
    | FreeAlloc(f,a) -> 
	FreeAlloc((refresh_identified_term_list f),refresh_identified_term_list a)

let refresh_assigns = function
    | WritesAny -> WritesAny
    | Writes l ->
      Writes(List.map refresh_from l)

let refresh_behavior b =
  { b with
    b_requires = List.map refresh_predicate b.b_requires;
    b_assumes = List.map refresh_predicate b.b_assumes;
    b_post_cond =
      List.map (fun (k,p) -> (k, refresh_predicate p)) b.b_post_cond;
    b_assigns = refresh_assigns b.b_assigns;
    b_allocation = refresh_allocation b.b_allocation;
    b_extended =
      List.map (fun (s,n,p) -> (s,n,List.map refresh_predicate p)) b.b_extended
  }

let refresh_spec s =
  { spec_behavior = List.map refresh_behavior s.spec_behavior;
    spec_variant = s.spec_variant;
    spec_terminates = Extlib.opt_map refresh_predicate s.spec_terminates;
    spec_complete_behaviors = s.spec_complete_behaviors;
    spec_disjoint_behaviors = s.spec_disjoint_behaviors;
  }

let refresh_code_annotation annot =
  let content =
    match annot.annot_content with
      | AAssert _ | AInvariant _ | AAllocation _ | AVariant _ | APragma _ as c -> c
      | AStmtSpec(l,spec) -> AStmtSpec(l, refresh_spec spec)
      | AAssigns(l,a) -> AAssigns(l, refresh_assigns a)
      
  in
  new_code_annotation content

(** {1 Smart constructors} *)

(** {2 pre-defined logic labels} *)
(* empty line for ocamldoc *)

let pre_label = LogicLabel (None, "Pre")

let post_label = LogicLabel (None, "Post")

let here_label = LogicLabel (None, "Here")

let old_label = LogicLabel (None, "Old")

let loop_current_label = LogicLabel (None, "LoopCurrent")

let loop_entry_label = LogicLabel (None, "LoopEntry")

(** {2 Types} *)

let is_set_type = function
  | Ltype ({lt_name = "set"},[_]) -> true
  | _ -> false

(** [set_conversion ty1 ty2] returns a set type as soon as [ty1] and/or [ty2]
    is a set. Elements have type [ty1], or the type of the elements of [ty1] if
    it is itself a set-type ({i.e.} we do not build set of sets that way).*)
let set_conversion ty1 ty2 =
  match ty1,ty2 with
  | Ltype ({lt_name = "set"},[_]),_ -> ty1
  | ty1, Ltype({lt_name = "set"} as lt,[_]) -> Ltype(lt,[ty1])
  | _ -> ty1

(** converts a type into the corresponding set type if needed. *)
let make_set_type ty =
  set_conversion ty (Ltype(Logic_env.find_logic_type "set",[Lvar "_"]))

(** returns the type of elements of a set type.
    @raise Failure if the input type is not a set type. *)
let type_of_element ty = match ty with
  | Ltype ({lt_name = "set"},[t]) -> t
  | _ -> failwith "not a set type"

(** [plain_or_set f t] applies [f] to [t] or to the type of elements of [t]
    if it is a set type *)
let plain_or_set f = function
  | Ltype ({lt_name = "set"},[t]) -> f t
  | t -> f t

let transform_element f t = set_conversion (plain_or_set f t) t

let is_plain_type = function
  | Ltype ({lt_name = "set"},[_]) -> false
  | _ -> true

let is_boolean_type = function
  | Ltype ({ lt_name = s }, []) when s = Utf8_logic.boolean -> true
  | _ -> false

(** {2 Offsets} *)

let rec lastTermOffset (off: term_offset) : term_offset =
   match off with
   | TNoOffset | TField(_,TNoOffset) | TIndex(_,TNoOffset) 
   | TModel(_,TNoOffset)-> off
   | TField(_,off) | TIndex(_,off) | TModel(_,off) -> lastTermOffset off

let rec addTermOffset (toadd: term_offset) (off: term_offset) : term_offset =
  match off with
  | TNoOffset -> toadd
  | TField(fid', offset) -> TField(fid', addTermOffset toadd offset)
  | TIndex(t, offset) -> TIndex(t, addTermOffset toadd offset)
  | TModel(m,offset) -> TModel(m,addTermOffset toadd offset)

let addTermOffsetLval toadd (b, off) : term_lval =
  b, addTermOffset toadd off


(** {2 Terms} *)
(* empty line for ocamldoc *)

(** @plugin development guide *)
let term ?(loc=Cil_datatype.Location.unknown) term typ =
  { term_node = term;
    term_type = typ;
    term_name = [];
    term_loc = loc }

let taddrof ?(loc=Cil_datatype.Location.unknown) lv typ =
  match lv with
    | TMem h, TNoOffset -> h
    | _ -> term ~loc (TAddrOf lv) typ

(** range of integers *)
let trange ?(loc=Cil_datatype.Location.unknown) (low,high) =
  term ~loc (Trange(low,high))
    (Ltype(Logic_env.find_logic_type "set",[Linteger]))

(** An integer constant (of type integer). *)
let tinteger ?(loc=Cil_datatype.Location.unknown) i =
  term ~loc (TConst (Integer (Integer.of_int i,None))) Linteger

(** An integer constant (of type integer) from an int64 . *)
let tinteger_s64
    ?(loc=Cil_datatype.Location.unknown) i64 =
  term ~loc (TConst (Integer (Integer.of_int64 i64,None))) Linteger

let tint ?(loc=Cil_datatype.Location.unknown) i =
  term ~loc (TConst (Integer (i,None))) Linteger

(** A real constant (of type real) from a Caml float . *)
let treal ?(loc=Cil_datatype.Location.unknown) f =
  let s = Pretty_utils.to_string Floating_point.pretty f in
  let r = {
    r_literal = s ;
    r_upper = f ; r_lower = f ; r_nearest = f ; 
  } in
  term ~loc (TConst (LReal r)) Lreal

let treal_zero ?(loc=Cil_datatype.Location.unknown) ?(ltyp=Lreal) () = 
  let zero = { r_nearest = 0.0 ; r_upper = 0.0 ; r_lower = 0.0 ; r_literal = "0." } in
  term ~loc (TConst (LReal zero)) ltyp

let tat ?(loc=Cil_datatype.Location.unknown) (t,label) =
  term ~loc (Tat(t,label)) t.term_type

let told ?(loc=Cil_datatype.Location.unknown) t = tat ~loc (t,old_label)

let tvar ?(loc=Cil_datatype.Location.unknown) lv =
  term ~loc (TLval(TVar lv,TNoOffset)) lv.lv_type

let tresult ?(loc=Cil_datatype.Location.unknown) typ =
  term ~loc (TLval(TResult typ,TNoOffset)) (Ctype typ)

(* needed by Cil, upon which Logic_utils depends.
   TODO: some refactoring of these two files *)
(** true if the given term is a lvalue denoting result or part of it *)
let rec is_result t = match t.term_node with
  | TLval (TResult _,_) -> true
  | Tat(t,_) -> is_result t
  | _ -> false

let rec is_exit_status t = match t.term_node with
  | TLval (TVar n,_) when n.lv_name = "\\exit_status" -> true
  | Tat(t,_) -> is_exit_status t
  | _ -> false

(** {2 Predicate constructors} *)
(* empty line for ocamldoc *)

let unamed ?(loc=Cil_datatype.Location.unknown) p =
  {content = p ; loc = loc; name = [] }

let ptrue = unamed Ptrue
let pfalse = unamed Pfalse

let pold ?(loc=Cil_datatype.Location.unknown) p = match p.content with
  | Ptrue | Pfalse -> p
  | _ -> {p with content = Pat(p, old_label); loc = loc}

let papp ?(loc=Cil_datatype.Location.unknown) (p,lab,a) =
  unamed ~loc (Papp(p,lab,a))

let pand ?(loc=Cil_datatype.Location.unknown) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, _ -> p2
  | _, Ptrue -> p1
  | Pfalse, _ -> p1
  | _, Pfalse -> p2
  | _, _ -> unamed ~loc (Pand (p1, p2))

let por ?(loc=Cil_datatype.Location.unknown) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, _ -> p1
  | _, Ptrue -> p2
  | Pfalse, _ -> p2
  | _, Pfalse -> p1
  | _, _ -> unamed ~loc (Por (p1, p2))

let pxor ?(loc=Cil_datatype.Location.unknown) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, Ptrue -> unamed ~loc Pfalse
  | Ptrue, _ -> p1
  | _, Ptrue -> p2
  | Pfalse, _ -> p2
  | _, Pfalse -> p1
  | _,_ -> unamed ~loc (Pxor (p1,p2))

let pnot ?(loc=Cil_datatype.Location.unknown) p2 = match p2.content with
  | Ptrue -> {p2 with content = Pfalse; loc = loc }
  | Pfalse ->  {p2 with content = Ptrue; loc = loc }
  | Pnot p -> p
  | _ -> unamed ~loc (Pnot p2)

let pands l = List.fold_right (fun p1 p2 -> pand (p1, p2)) l ptrue
let pors l = List.fold_right (fun p1 p2 -> por (p1, p2)) l pfalse

let plet ?(loc=Cil_datatype.Location.unknown) p = match p.content with
  | (_, ({content = Ptrue} as p)) -> p
  | (v, p) -> unamed ~loc (Plet (v, p))

let pimplies ?(loc=Cil_datatype.Location.unknown) (p1,p2) =
  match p1.content, p2.content with
  | Ptrue, _ | _, Ptrue -> p2
  | Pfalse, _ -> { name = p1.name; loc = loc; content = Ptrue }
  | _, _ -> unamed ~loc (Pimplies (p1, p2))

let pif ?(loc=Cil_datatype.Location.unknown) (t,p2,p3) =
  match (p2.content, p3.content) with
  | Ptrue, Ptrue  -> ptrue
  | Pfalse, Pfalse -> pfalse
  | _,_ -> unamed ~loc (Pif (t,p2,p3))

let piff ?(loc=Cil_datatype.Location.unknown) (p2,p3) =
  match p2.content, p3.content with
  | Pfalse, Pfalse -> ptrue
  | Ptrue, _  -> p3
  | _, Ptrue -> p2
  | _,_ -> unamed ~loc (Piff (p2,p3))

(** @plugin development guide *)
let prel ?(loc=Cil_datatype.Location.unknown) (a,b,c) =
  unamed ~loc (Prel(a,b,c))

let pforall ?(loc=Cil_datatype.Location.unknown) (l,p) = match l with
  | [] -> p
  | _ :: _ ->
    match p.content with
    | Ptrue -> p
    | _ -> unamed ~loc (Pforall (l,p))

let pexists ?(loc=Cil_datatype.Location.unknown) (l,p) = match l with
  | [] -> p
  | _ :: _ -> match p.content with
    | Pfalse -> p
    | _ -> unamed ~loc (Pexists (l,p))

let pfresh ?(loc=Cil_datatype.Location.unknown) (l1,l2,p,n) = unamed ~loc (Pfresh (l1,l2,p,n))
let pallocable ?(loc=Cil_datatype.Location.unknown) (l,p) = unamed ~loc (Pallocable (l,p))
let pfreeable ?(loc=Cil_datatype.Location.unknown) (l,p) = unamed ~loc (Pfreeable (l,p))
let pvalid_read ?(loc=Cil_datatype.Location.unknown) (l,p) = unamed ~loc (Pvalid_read (l,p))
let pvalid ?(loc=Cil_datatype.Location.unknown) (l,p) = unamed ~loc (Pvalid (l,p))
(* the index should be an integer or a range of integers *)
let pvalid_index ?(loc=Cil_datatype.Location.unknown) (l,t1,t2) =
  let ty1 = t1.term_type in
  let ty2 = t2.term_type in
  let t, ty =(match t1.term_node with
		| TStartOf lv -> 
		    TAddrOf (addTermOffsetLval (TIndex(t2,TNoOffset)) lv)
		| _ -> TBinOp (PlusPI, t1, t2)),
    set_conversion ty1 ty2 in
  let t = term ~loc t ty in
    pvalid ~loc (l,t)
(* the range should be a range of integers *)
let pvalid_range ?(loc=Cil_datatype.Location.unknown) (l,t1,b1,b2) =
  let t2 = trange ((Some b1), (Some b2)) in
    pvalid_index ~loc (l,t1,t2)
let pat ?(loc=Cil_datatype.Location.unknown) (p,q) = unamed ~loc (Pat (p,q))
let pinitialized ?(loc=Cil_datatype.Location.unknown) (l,p) =
  unamed ~loc (Pinitialized (l,p))
let psubtype ?(loc=Cil_datatype.Location.unknown) (p,q) =
  unamed ~loc (Psubtype (p,q))

let pseparated  ?(loc=Cil_datatype.Location.unknown) seps =
  unamed ~loc (Pseparated seps)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
