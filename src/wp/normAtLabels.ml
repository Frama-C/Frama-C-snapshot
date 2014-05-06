(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Cil_types

type label_mapping = Cil_types.logic_label -> Cil_types.logic_label


(** push the Tat down to the 'data' operations.
* This can be useful in cases like \at (x + \at(y, Ly), Lx) because
* it gives \at(x, Lx) + \at(y, Ly) so there is no more \at imbrications.
  * Also try to "normalize" label :
  * - remove Here because its meaning change when propagating,
  * - remove Old because its meaning depend on where it comes from.
* *)
class norm_at label_map = object(self)
  inherit Visitor.generic_frama_c_visitor (Cil.copy_visit (Project.current ()))

  val mutable current_label = None

  method private change_label label =
    let label = label_map label in
    let old_label = current_label in
    current_label <- Some label; old_label

  method private restore_term old_label x =
    current_label <- old_label;
    let x = match x.term_node with
      | Ttypeof x -> (* Ttypeof is used as a dummy unary construct *) x
      | _ -> assert false
    in x

  method private restore_pred old_label x =
    current_label <- old_label;
    let x = match x.content with
      | Pnot x -> (* Pnot is used as a dummy unary construct *) x
      | _ -> assert false
    in x


  method! vterm t =
    match t.term_node with
      | Tat (t, l) ->
          let old_label = self#change_label l in
          let new_t = {t with term_node = Ttypeof t} in
            Cil.ChangeDoChildrenPost (new_t, self#restore_term old_label)
      | TAddrOf (h, _) | TLval (h, _) | TStartOf (h, _)  ->
          let old_label = current_label in
          let at_label = match h with
            | TResult _ -> Some Logic_const.post_label
            | _ -> old_label
          in
            current_label <- None;
          let post t =
            current_label <- old_label;
            match at_label with
            | Some label -> {t with term_node = Tat (t, label)}
            | None -> t
          in Cil.ChangeDoChildrenPost (t, post)
      | Tapp _ ->
          let post = function
            | {term_node=Tapp(predicate,labels,args)} as t ->
                let new_labels =
                  List.map
                    (fun (logic_lab, stmt_lab) -> logic_lab, label_map stmt_lab)
                    labels
                in { t with term_node=Tapp(predicate,new_labels,args) }
            | _ -> assert false
          in
          Cil.ChangeDoChildrenPost (t,post)
      | _ -> Cil.DoChildren

  method! vpredicate_named p = match p.content with
    | Pat (p, l) ->
        let old_label = self#change_label l in
        let new_p = {p with content = Pnot p} in
        Cil.ChangeDoChildrenPost (new_p, self#restore_pred old_label)
    | Papp _ ->
        let post = function
          | {content=Papp(predicate,labels,args)} as p ->
              let new_labels =
                List.map
                  (fun (logic,stmt) -> logic, label_map stmt)
                  labels
              in { p with content=Papp(predicate,new_labels,args) }
          | _ -> assert false
        in
        Cil.ChangeDoChildrenPost (p,post)
    | _ -> Cil.DoChildren
end

exception LabelError of logic_label

let labels_empty l = raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Function Contracts                                                 --- *)
(* -------------------------------------------------------------------------- *)

let labels_fct_pre = function
  | LogicLabel (None, ("Pre" | "Here")) -> Logic_const.pre_label
  | l -> raise (LabelError l)


let labels_fct_post = function
  | LogicLabel (None, ("Pre" | "Old"))  -> Logic_const.pre_label
  | LogicLabel (None, ("Post" | "Here")) -> Logic_const.post_label
  | l -> raise (LabelError l)

let labels_fct_assigns = function
  | LogicLabel (None, "Post")  -> Logic_const.post_label
  | LogicLabel (None, ("Pre" | "Old")) -> Logic_const.pre_label
  | l -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Statements Contracts                                               --- *)
(* -------------------------------------------------------------------------- *)
let labels_stmt_pre s = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label (* fct pre-state *)
  | LogicLabel (None, "Here") -> Clabels.mk_logic_label s
  | LogicLabel (Some s, _) -> Clabels.mk_logic_label s
  | StmtLabel rs -> Clabels.mk_logic_label !rs
  | l -> raise (LabelError l)

let labels_stmt_post s l_post = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label (* fct pre-state *)
  | LogicLabel (None, "Old") ->  Clabels.mk_logic_label s (* contract pre-state *)
  | LogicLabel (None, ("Here" | "Post")) as l ->
      begin match l_post with Some l -> l
        | None -> (* TODO ? *) raise (LabelError l)
      end
  | LogicLabel (Some s, _) -> Clabels.mk_logic_label s
  | StmtLabel rs -> Clabels.mk_logic_label !rs
  | l -> raise (LabelError l)

let labels_stmt_assigns s l_post = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label (* fct pre-state *)
  | LogicLabel (None, ("Here" | "Old")) ->  (* contract pre-state *)
      Clabels.mk_logic_label s
  | LogicLabel (None, "Post") -> labels_stmt_post s l_post Logic_const.post_label
  | LogicLabel (Some s, _) -> Clabels.mk_logic_label s
  | StmtLabel rs -> Clabels.mk_logic_label !rs
  | l -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- User Assertions in Functions Code                                  --- *)
(* -------------------------------------------------------------------------- *)

let labels_assert_before s = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label
  | LogicLabel (None, "Here") -> Clabels.mk_logic_label s
  | LogicLabel (Some s, _) -> Clabels.mk_logic_label s
  | StmtLabel rs -> Clabels.mk_logic_label !rs
  | l -> raise (LabelError l)

let labels_assert_after s l_post = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label
  | LogicLabel (None, "Here") ->
      labels_stmt_post s l_post Logic_const.post_label
  | LogicLabel (Some s, _) -> Clabels.mk_logic_label s
  | StmtLabel rs -> Clabels.mk_logic_label !rs
  | l -> raise (LabelError l)

let labels_loop_inv s = function
  | LogicLabel (None, "Pre") -> Logic_const.pre_label
  | LogicLabel (None, "Here") -> Logic_const.here_label
  | LogicLabel (None, "LoopEntry") -> Clabels.mk_logic_label s
  | LogicLabel (None, ("Old" | "Post")) as l -> raise (LabelError l)
  | l -> l

let labels_loop_assigns s l = labels_loop_inv s l

(* -------------------------------------------------------------------------- *)
(* --- User Defined Predicates                                            --- *)
(* -------------------------------------------------------------------------- *)

let labels_predicate lab_pairs = fun l ->
  try List.assoc l lab_pairs
  with Not_found -> l

let labels_axiom = function
    | LogicLabel (None, ("Pre"|"Old"|"Post")) as l -> raise (LabelError l)
    | LogicLabel (None, _) as l -> l
    | l -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Apply Normalization                                                --- *)
(* -------------------------------------------------------------------------- *)

(** @raise LabelError if there is a label in [p] that is incompatible
* with the [labels] translation *)
let preproc_annot labels p =
  let visitor = new norm_at labels in
  Visitor.visitFramacPredicateNamed visitor p

(** @raise LabelError if there is a label in [p] that is incompatible
* with the [labels] translation *)
let preproc_assigns labels asgns =
  let visitor = new norm_at labels in
  List.map (Visitor.visitFramacFrom visitor) asgns

let preproc_label labels l = labels l

let catch_label_error ex txt1 txt2 = match ex with
  | LabelError lab ->
      Wp_parameters.warning
        "Unexpected label %a in %s : ignored %s"
        Wp_error.pp_logic_label lab txt1 txt2
  | _ -> raise ex
