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

(**/**)
let __annot_count = ref 0
let __pred_count = ref 0
let __term_count = ref 0
(**/**)

let new_code_annotation annot =
  incr __annot_count; { annot_content = annot ; annot_id = !__annot_count }

let fresh_code_annotation () =
  incr __annot_count; !__annot_count

let refresh_code_annotation annot = new_code_annotation annot.annot_content

let new_predicate p =
  incr __pred_count;
  { ip_id = !__pred_count;
    ip_content = p.content; ip_loc = p.loc; ip_name = p.name }

let fresh_predicate_id () =
  incr __pred_count; !__pred_count

let pred_of_id_pred p =
  { name = p.ip_name; loc = p.ip_loc; content = p.ip_content }

let new_identified_term t =
  incr __term_count; { it_id = !__term_count; it_content = t }

let fresh_term_id () =
  incr __term_count; !__term_count

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
| _ -> {p with content = Pold p; loc = loc}

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
      Ptrue, Ptrue -> unamed ~loc Pfalse
    | Ptrue, _ -> p1
    | _, Ptrue -> p2
    | Pfalse, _ -> p2
    | _, Pfalse -> p1
    | _,_ -> unamed ~loc (Pxor (p1,p2))

let pnot ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p2 = match p2.content with
| Ptrue -> {p2 with content = Pfalse; loc = loc }
| Pfalse ->  {p2 with content = Ptrue; loc = loc }
| _ -> unamed ~loc (Pnot p2)

let pands l = List.fold_right (fun p1 p2 -> pand (p1, p2)) l ptrue

let pors l = List.fold_right (fun p1 p2 -> por (p1, p2)) l pfalse

let plet ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = match p.content with
| (_, ({content = Ptrue} as p)) -> p
| (v, p) -> unamed ~loc (Plet (v, p))

let pimplies ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1,p2) =
match p1.content,p2.content with
| Ptrue, _ | _, Ptrue -> p2
| Pfalse, _ ->
    {name = p1.name ; loc = loc; content = Ptrue }
| _, _ -> unamed ~loc (Pimplies (p1, p2))

let pif ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (t,p2,p3) =
match (p2.content,p3.content) with
| Ptrue, Ptrue  -> ptrue
| Pfalse, Pfalse -> pfalse
| _,_ -> unamed ~loc (Pif (t,p2,p3))

let piff ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p2,p3) =
match (p2.content,p3.content) with
| Pfalse, Pfalse -> ptrue
| Ptrue, _  -> p3
| _, Ptrue -> p2
| _,_ -> unamed ~loc (Piff (p2,p3))

(** @plugin development guide *)
let prel ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (a,b,c) =
  unamed ~loc (Prel(a,b,c))

let pforall ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
| [] -> p
| _ -> match p.content with
  | Ptrue -> p
  | _ -> unamed ~loc (Pforall (l,p))

let pexists ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
| [] -> p
| _ -> match p.content with
  | Pfalse -> p
  | _ -> unamed ~loc (Pexists (l,p))

let pfresh ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pfresh p)
let pvalid ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pvalid p)
let pat ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) = unamed ~loc (Pat (p,q))
let pvalid_index ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Pvalid_index (p,q))
let pvalid_range ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q,r) =
  unamed ~loc (Pvalid_range (p,q,r))
let psubtype ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Psubtype (p,q))

let pseparated  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) seps =
  unamed ~loc (Pseparated seps)

(** {2 Types} *)

(** [set_conversion ty1 ty2] returns a set type as soon as [ty1] and/or [ty2]
    is a set. Elements have type [ty1], or the type of the elements of [ty1] if
    it is itself a set-type ({i.e.} we do not build set of sets that way).
*)
let set_conversion ty1 ty2 =
  match ty1,ty2 with
      Ltype ({lt_name = "set"},[_]),_ -> ty1
    | ty1,Ltype({lt_name = "set"} as lt,[_]) -> Ltype(lt,[ty1])
    | _ -> ty1

(** converts a type into the corresponding set type if needed. *)
let make_set_type ty =
  set_conversion ty
    (Ltype(Logic_env.find_logic_type "set",[Lvar "_"]))

(** returns the type of elements of a set type.
    @raise Failure if the input type is not a set type.
 *)
let type_of_element ty = match ty with
  | Ltype ({lt_name = "set"},[t]) -> t
  | _ -> failwith "not a set type"

(** [plain_or_set f t] applies [f] to [t] or to the type of elements of [t]
    if it is a set type
 *)
let plain_or_set f = function
    | Ltype ({lt_name = "set"},[t]) -> f t
    | t -> f t

let is_plain_type = function
    | Ltype ({lt_name = "set"},[_]) -> false
    | _ -> true

(** {2 Terms} *)
(* empty line for ocamldoc *)

let taddrof ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) lv typ =
  match lv with
    | TMem h, TNoOffset -> h
    | _ -> { term_node = TAddrOf lv;
             term_type = typ;
             term_name = [];
             term_loc = loc}

(** @plugin development guide *)
let term ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) term typ =
  { term_node = term;
    term_type = typ;
    term_name = [];
    term_loc = loc }

(** range of integers *)
let trange ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) (low,high) =
  term ~loc (Trange(low,high))
    (Ltype(Logic_env.find_logic_type "set",[Linteger]))

(** An integer constant (of type integer). *)
let tinteger ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) i =
  term ~loc (TConst (CInt64 (Int64.of_int i,IUInt,None))) Linteger

(** An integer constant (of type integer) from an int64 . *)
let tinteger_s64 ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) i64 =
  term ~loc (TConst (CInt64 (i64,IInt,None))) Linteger

let tat ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (t,label) =
  term ~loc (Tat(t,label)) t.term_type

let tvar ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) lv =
  term ~loc (TLval(TVar lv,TNoOffset)) lv.lv_type

let tresult ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) typ =
  term ~loc (TLval(TResult typ,TNoOffset)) (Ctype typ)

(* needed by Cil, upon which Logic_utils depends.
   TODO: some refactoring of these two files *)
(** true if the given term is a lvalue denoting result or part of it *)
let rec is_result t = match t.term_node with
    TLval (TResult _,_) -> true
  | Tat(t,_) -> is_result t
  | Told t -> is_result t
  | _ -> false

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
