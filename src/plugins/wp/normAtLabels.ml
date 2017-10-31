(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type label_mapping = Cil_types.logic_label -> Clabels.c_label

(** push the Tat down to the 'data' operations.
 * This can be useful in cases like \at (x + \at(y, Ly), Lx) because
 * it gives \at(x, Lx) + \at(y, Ly) so there is no more \at imbrications.
  * Also try to "normalize" label :
  * - remove Here because its meaning change when propagating,
  * - remove Old because its meaning depend on where it comes from.
 * *)
class norm_at (mapping : label_mapping) =
  object(self)
    inherit Visitor.generic_frama_c_visitor (Cil.copy_visit (Project.current ()))

    val mutable current_label = None

    method private change_label label =
      let label = mapping label in
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
      let x = match x.pred_content with
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
            | TResult _ -> Some Clabels.post
            | _ -> old_label
          in
          current_label <- None;
          let post t =
            current_label <- old_label;
            match at_label with
            | Some label -> {t with term_node = Tat (t, Clabels.to_logic label)}
            | None -> t
          in Cil.ChangeDoChildrenPost (t, post)
      | Tapp _ ->
          let post = function
            | {term_node=Tapp(predicate,labels,args)} as t ->
                let normalize l = mapping l |> Clabels.to_logic in
                let new_labels = List.map normalize labels in
                { t with term_node=Tapp(predicate,new_labels,args) }
            | _ -> assert false
          in
          Cil.ChangeDoChildrenPost (t,post)
      | _ -> Cil.DoChildren

    method! vpredicate p = match p.pred_content with
      | Pat (p, l) ->
          let old_label = self#change_label l in
          let new_p = {p with pred_content = Pnot p} in
          Cil.ChangeDoChildrenPost (new_p, self#restore_pred old_label)
      | Papp _ ->
          let post = function
            | {pred_content=Papp(predicate,labels,args)} as p ->
                let normalize l = mapping l |> Clabels.to_logic in
                let new_labels = List.map normalize labels in
                { p with pred_content=Papp(predicate,new_labels,args) }
            | _ -> assert false
          in
          Cil.ChangeDoChildrenPost (p,post)
      | _ -> Cil.DoChildren
  end

exception LabelError of logic_label

let option l = function Some l -> l | None -> raise (LabelError l)

let labels_empty l = raise (LabelError l)

let enclosing_loop ?kf ?at l =
  match kf , at with
  | Some kf , Some stmt -> Kernel_function.find_enclosing_loop kf stmt
  | _ -> raise (LabelError l)

let labels_fct ?kf ?at l =
  match l with
  | BuiltinLabel Init -> Clabels.init
  | BuiltinLabel Pre -> Clabels.pre
  | StmtLabel at -> Clabels.stmt !at
  | BuiltinLabel LoopEntry -> Clabels.loop_entry (enclosing_loop ?kf ?at l)
  | BuiltinLabel LoopCurrent -> Clabels.loop_current (enclosing_loop ?kf ?at l)
  | _ -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Function Contracts                                                 --- *)
(* -------------------------------------------------------------------------- *)

let labels_fct_pre = function
  | BuiltinLabel Init -> Clabels.init
  | BuiltinLabel (Pre|Here) -> Clabels.pre
  | l -> raise (LabelError l)

let labels_fct_post = function
  | BuiltinLabel Init -> Clabels.init
  | BuiltinLabel (Pre | Old)  -> Clabels.pre
  | BuiltinLabel (Post | Here) -> Clabels.post
  | l -> raise (LabelError l)

let labels_fct_assigns = function
  | BuiltinLabel Init -> Clabels.init
  | BuiltinLabel (Here | Pre | Old) -> Clabels.pre
  | BuiltinLabel Post -> Clabels.post
  | l -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Statements Contracts                                               --- *)
(* -------------------------------------------------------------------------- *)

let labels_stmt_pre ~kf s = function
  | BuiltinLabel Here -> Clabels.stmt s
  | l -> labels_fct ~kf ~at:s l

let labels_stmt_post ~kf s l_post = function
  | BuiltinLabel Old -> Clabels.stmt s
  | BuiltinLabel (Here | Post) as l -> option l l_post
  | l -> labels_fct ~kf ~at:s l

let labels_stmt_assigns ~kf s l_post = function
  | BuiltinLabel (Here | Old) -> Clabels.stmt s
  | BuiltinLabel Post as l -> option l l_post
  | l -> labels_fct ~kf ~at:s l

(* -------------------------------------------------------------------------- *)
(* --- User Assertions in Functions Code                                  --- *)
(* -------------------------------------------------------------------------- *)

let labels_assert_before ~kf s = function
  | BuiltinLabel Here -> Clabels.stmt s
  | l -> labels_fct ~kf ~at:s l

let labels_assert_after ~kf s l_post = function
  | BuiltinLabel Old -> Clabels.stmt s
  | BuiltinLabel Here as l -> option l l_post
  | l -> labels_fct ~kf ~at:s l

let labels_loop_inv ~established s = function
  | BuiltinLabel Here -> Clabels.here
  | BuiltinLabel LoopEntry -> Clabels.loop_entry s
  | BuiltinLabel LoopCurrent ->
      if established
      then Clabels.loop_entry s
      else Clabels.loop_current s
  | FormalLabel wplabel -> Clabels.formal wplabel
  | l -> labels_fct ?kf:None ?at:None l (* current loop is handled above *)

let labels_loop_assigns s l = labels_loop_inv ~established:false s l

(* -------------------------------------------------------------------------- *)
(* --- User Defined Predicates                                            --- *)
(* -------------------------------------------------------------------------- *)

let labels_predicate lab_pairs l =
  try List.assoc l lab_pairs |> Clabels.of_logic
  with Not_found -> Clabels.of_logic l

let labels_axiom = function
  | FormalLabel a -> Clabels.formal a
  | l -> raise (LabelError l)

(* -------------------------------------------------------------------------- *)
(* --- Apply Normalization                                                --- *)
(* -------------------------------------------------------------------------- *)

(** @raise LabelError if there is a label in [p] that is incompatible
 * with the [labels] translation *)
let preproc_annot labels p =
  let visitor = new norm_at labels in
  Visitor.visitFramacPredicate visitor p

(** @raise LabelError if there is a label in [p] that is incompatible
 * with the [labels] translation *)
let preproc_assigns labels asgns =
  let visitor = new norm_at labels in
  List.map (Visitor.visitFramacFrom visitor) asgns

let catch_label_error ex txt1 txt2 = match ex with
  | LabelError lab ->
      Wp_parameters.warning
        "Unexpected label %a in %s : ignored %s"
        Wp_error.pp_logic_label lab txt1 txt2
  | _ -> raise ex
