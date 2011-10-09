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

let refresh_deps = function
  | FromAny -> FromAny
  | From l ->
    From (List.map (fun d -> new_identified_term d.it_content) l)

let refresh_from (a,d) = (new_identified_term a.it_content, refresh_deps d)

let refresh_assigns a =
  match a with
      WritesAny -> WritesAny
    | Writes l ->
      Writes(List.map refresh_from l)

let refresh_behavior b =
  { b with
    b_requires = List.map refresh_predicate b.b_requires;
    b_assumes = List.map refresh_predicate b.b_assumes;
    b_post_cond =
      List.map (fun (k,p) -> (k, refresh_predicate p)) b.b_post_cond;
    b_assigns = refresh_assigns b.b_assigns;
    b_extended =
      List.map (fun (s,n,p) -> (s,n,List.map refresh_predicate p)) b.b_extended
  }

let refresh_variant (t,s) = (new_identified_term t.it_content,s)

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
      | AAssert _ | AInvariant _ | AVariant _ | APragma _ as c -> c
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

(** {2 Predicate constructors} *)
(* empty line for ocamldoc *)

let unamed ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p =
  {content = p ; loc = loc; name = [] }

let ptrue = unamed Ptrue
let pfalse = unamed Pfalse

let pold ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = match p.content with
  | Ptrue | Pfalse -> p
  | _ -> {p with content = Pat(p, old_label); loc = loc}

let papp ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,lab,a) =
  unamed ~loc (Papp(p,lab,a))

let pand ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, _ -> p2
  | _, Ptrue -> p1
  | Pfalse, _ -> p1
  | _, Pfalse -> p2
  | _, _ -> unamed ~loc (Pand (p1, p2))

let por ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, _ -> p1
  | _, Ptrue -> p2
  | Pfalse, _ -> p2
  | _, Pfalse -> p1
  | _, _ -> unamed ~loc (Por (p1, p2))

let pxor ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
  match p1.content, p2.content with
  | Ptrue, Ptrue -> unamed ~loc Pfalse
  | Ptrue, _ -> p1
  | _, Ptrue -> p2
  | Pfalse, _ -> p2
  | _, Pfalse -> p1
  | _,_ -> unamed ~loc (Pxor (p1,p2))

let pnot ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p2 = match p2.content with
  | Ptrue -> {p2 with content = Pfalse; loc = loc }
  | Pfalse ->  {p2 with content = Ptrue; loc = loc }
  | Pnot p -> p
  | _ -> unamed ~loc (Pnot p2)

let pands l = List.fold_right (fun p1 p2 -> pand (p1, p2)) l ptrue
let pors l = List.fold_right (fun p1 p2 -> por (p1, p2)) l pfalse

let plet ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = match p.content with
  | (_, ({content = Ptrue} as p)) -> p
  | (v, p) -> unamed ~loc (Plet (v, p))

let pimplies ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1,p2) =
  match p1.content, p2.content with
  | Ptrue, _ | _, Ptrue -> p2
  | Pfalse, _ -> { name = p1.name; loc = loc; content = Ptrue }
  | _, _ -> unamed ~loc (Pimplies (p1, p2))

let pif ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (t,p2,p3) =
  match (p2.content, p3.content) with
  | Ptrue, Ptrue  -> ptrue
  | Pfalse, Pfalse -> pfalse
  | _,_ -> unamed ~loc (Pif (t,p2,p3))

let piff ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p2,p3) =
  match p2.content, p3.content with
  | Pfalse, Pfalse -> ptrue
  | Ptrue, _  -> p3
  | _, Ptrue -> p2
  | _,_ -> unamed ~loc (Piff (p2,p3))

(** @plugin development guide *)
let prel ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (a,b,c) =
  unamed ~loc (Prel(a,b,c))

let pforall ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
  | [] -> p
  | _ :: _ ->
    match p.content with
    | Ptrue -> p
    | _ -> unamed ~loc (Pforall (l,p))

let pexists ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
  | [] -> p
  | _ :: _ -> match p.content with
    | Pfalse -> p
    | _ -> unamed ~loc (Pexists (l,p))

let pfresh ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pfresh p)
let pvalid ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pvalid p)
let pat ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) = unamed ~loc (Pat (p,q))
let pvalid_index ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Pvalid_index (p,q))
let pvalid_range ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q,r) =
  unamed ~loc (Pvalid_range (p,q,r))
let pinitialized ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p =
  unamed ~loc (Pinitialized p)
let psubtype ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Psubtype (p,q))

let pseparated  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) seps =
  unamed ~loc (Pseparated seps)

(** {2 Types} *)

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

(** {2 Terms} *)
(* empty line for ocamldoc *)


(** @plugin development guide *)
let term ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) term typ =
  { term_node = term;
    term_type = typ;
    term_name = [];
    term_loc = loc }

let taddrof ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) lv typ =
  match lv with
  | TMem h, TNoOffset -> h
  | _ -> term ~loc (TAddrOf lv) typ

(** range of integers *)
let trange ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) (low,high) =
  term ~loc (Trange(low,high))
    (Ltype(Logic_env.find_logic_type "set",[Linteger]))

(** An integer constant (of type integer). *)
let tinteger ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) ?(ikind=ILongLong) i =
  term ~loc (TConst (CInt64 (My_bigint.of_int i,ikind,None))) Linteger

(** An integer constant (of type integer) from an int64 . *)
let tinteger_s64
    ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) ?(ikind=ILongLong) i64 =
  term ~loc (TConst (CInt64 (My_bigint.of_int64 i64,ikind,None))) Linteger

let tat ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (t,label) =
  term ~loc (Tat(t,label)) t.term_type

let told ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) t = tat ~loc (t,old_label)

let tvar ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) lv =
  term ~loc (TLval(TVar lv,TNoOffset)) lv.lv_type

let tresult ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) typ =
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

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
