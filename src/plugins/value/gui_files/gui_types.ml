(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types

type gui_callstack =
  | GC_Filtered (* Some results have been hidden by a filter *)
  | GC_Consolidated (* Join of all possible callstacks *)
  | GC_Single of Value_types.callstack (* Only one callstack possible here *)
  | GC_Callstack of Value_types.callstack (* One of multiple callstacks *)

let hash_gui_callstack = function
  | GC_Filtered -> 0
  | GC_Consolidated -> 1
  | GC_Single cs -> 2 * Value_types.Callstack.hash cs
  | GC_Callstack cs -> 4 * Value_types.Callstack.hash cs

let compare_gui_callstack cs1 cs2 = match cs1, cs2 with
  | GC_Filtered, GC_Filtered -> 0
  | GC_Consolidated, GC_Consolidated -> 0
  | GC_Single cs1, GC_Single cs2 | GC_Callstack cs1, GC_Callstack cs2 ->
    Value_types.Callstack.compare cs1 cs2
  | _, GC_Filtered -> 1
  | GC_Filtered, _ -> -1
  | _, GC_Consolidated -> 1
  | GC_Consolidated, _ -> -1
  | _, GC_Single _ -> 1
  | GC_Single _, _ -> -1

module GCallstackMap = FCMap.Make(struct
    type t = gui_callstack
    let compare = compare_gui_callstack
  end)

type gui_selection =
  | GS_TLVal of term | GS_LVal of lval | GS_AbsoluteMem
  | GS_Expr of exp | GS_Term of term
  | GS_Predicate of Cil_types.predicate

let pretty_gui_selection fmt = function
  | GS_TLVal t | GS_Term t -> Printer.pp_term fmt t
  | GS_LVal l -> Printer.pp_lval fmt l
  | GS_AbsoluteMem -> Format.pp_print_string fmt "NULL"
  | GS_Expr e -> Printer.pp_exp fmt e
  | GS_Predicate p -> Printer.pp_predicate_node fmt p.pred_content

let gui_selection_equal e1 e2 = match e1, e2 with
  | GS_TLVal t1, GS_TLVal t2 | GS_Term t1, GS_Term t2 ->
    Cil_datatype.Term.equal t1 t2
  | GS_LVal lv1, GS_LVal lv2 -> Cil_datatype.Lval.equal lv1 lv2
  | GS_AbsoluteMem, GS_AbsoluteMem -> true
  | GS_Expr e1, GS_Expr e2 -> Cil_datatype.Exp.equal e1 e2
  | GS_Predicate p1, GS_Predicate p2 ->
    (* Cil_datatype.Predicate.equal not implemented *)
    p1.pred_content == p2.pred_content
  | (GS_TLVal _ | GS_LVal _ | GS_AbsoluteMem | GS_Expr _ | GS_Term _ |
     GS_Predicate _) , _ -> false

type gui_offsetmap_res =
  | GO_Bottom (* Bottom memory state *)
  | GO_Empty (* Location with Empty validity (e.g. empty struct) *)
  | GO_Top (* State or size was Top *)
  | GO_InvalidLoc (* Location is always invalid *)
  | GO_Offsetmap of Cvalue.V_Offsetmap.t (* Normal result *)

let equal_gui_offsetmap_res r1 r2 = match r1, r2 with
  | GO_Bottom, GO_Bottom -> true
  | GO_Empty, GO_Empty -> true
  | GO_Top, GO_Top -> true
  | GO_InvalidLoc, GO_InvalidLoc -> true
  | GO_Offsetmap o1, GO_Offsetmap o2 -> Cvalue.V_Offsetmap.equal o1 o2
  | (GO_Bottom | GO_Empty | GO_Top | GO_InvalidLoc | GO_Offsetmap _), _ -> false

let pretty_gui_offsetmap_res ?typ fmt r =
  match r with
  | GO_Bottom ->  Format.pp_print_string fmt "<BOTTOM>"
  | GO_Empty ->  Format.pp_print_string fmt "<EMPTY>"
  | GO_InvalidLoc -> Format.pp_print_string fmt "<INVALID LOCATION>"
  | GO_Top -> Format.pp_print_string fmt "<NO INFORMATION>"
  | GO_Offsetmap off ->
    Cvalue.V_Offsetmap.pretty_generic ?typ () fmt off;
    match typ with
    | None -> ()
    | Some typ -> Eval_op.pretty_stitched_offsetmap fmt typ off

(* Some cases are impossible because of conflicting sizes *)
let join_gui_offsetmap_res r1 r2 = match r1, r2 with
  | GO_Top, _ | _, GO_Top -> GO_Top
  | GO_Bottom, x | x, GO_Bottom -> x
  | GO_InvalidLoc, x | x, GO_InvalidLoc -> x
  | GO_Empty, x | x, GO_Empty -> x
  | GO_Offsetmap o1, GO_Offsetmap o2 ->
    GO_Offsetmap (Cvalue.V_Offsetmap.join o1 o2)

type 'a gui_res =
  | GR_Empty
  | GR_Offsm of gui_offsetmap_res * Cil_types.typ option
  | GR_Value of 'a Eval.flagged_value * Cil_types.typ option
  | GR_Status of Eval_terms.predicate_status
  | GR_Zone of Locations.Zone.t

type 'a gui_after =
  | GA_After of 'a gui_res
  | GA_Bottom
  | GA_NA
  | GA_Unchanged

module type S = sig
  type value

  val pretty_gui_res : Format.formatter -> value gui_res -> unit
  val equal_gui_res : value gui_res -> value gui_res -> bool
  val vars_in_gui_res : value gui_res -> Cil_types.varinfo list

  val pretty_gui_after : Format.formatter -> value gui_after -> unit
  val equal_gui_after : value gui_after -> value gui_after -> bool
end

module Make (V: Abstractions.Value) = struct

  let pretty_gui_res fmt = function
    | GR_Empty -> ()
    | GR_Offsm (offsm, typ) -> pretty_gui_offsetmap_res ?typ fmt offsm
    | GR_Value (v, typ) -> Eval.Flagged_Value.pretty (V.pretty_typ typ) fmt v
    | GR_Status s -> Eval_terms.pretty_predicate_status fmt s
    | GR_Zone z -> Locations.Zone.pretty fmt z

  let equal_gui_res r1 r2 = match r1, r2 with
    | GR_Empty, GR_Empty -> true
    | GR_Offsm (o1, typ1), GR_Offsm (o2, typ2) ->
      equal_gui_offsetmap_res o1 o2 &&
      Extlib.opt_equal Cil_datatype.Typ.equal typ1 typ2
    | GR_Value (v1, typ1), GR_Value (v2, typ2) ->
      Eval.Flagged_Value.equal V.equal v1 v2 &&
      Extlib.opt_equal Cil_datatype.Typ.equal typ1 typ2
    | GR_Status s1, GR_Status s2 -> Extlib.compare_basic s1 s2 = 0
    | GR_Zone z1, GR_Zone z2 -> Locations.Zone.equal z1 z2
    | (GR_Empty | GR_Offsm _ | GR_Value _  | GR_Status _ | GR_Zone _), _ -> false

  let pretty_gui_after fmt = function
    | GA_After r -> Format.fprintf fmt "%a" pretty_gui_res r
    | GA_Bottom -> Format.fprintf fmt "BOTTOM"
    | GA_NA -> Format.fprintf fmt "n/a"
    | GA_Unchanged -> Format.fprintf fmt "unchanged"

  let equal_gui_after a1 a2 = match a1, a2 with
    | GA_NA, GA_NA | GA_Unchanged, GA_Unchanged | GA_Bottom, GA_Bottom -> true
    | GA_After r1, GA_After r2 -> equal_gui_res r1 r2
    | (GA_After _ | GA_NA | GA_Unchanged | GA_Bottom), _ -> false

  let get_cvalue = V.get Main_values.CVal.key
  let from_cvalue v = V.set Main_values.CVal.key v V.top

  let var_of_base base acc =
    try
      let vi = Base.to_varinfo base in
      (* if it is a function, do not add it *)
      if Cil.isFunctionType vi.vtype then acc else vi :: acc
    with Base.Not_a_C_variable -> acc

  (* [vars_in_gui_res r] returns a list of non-function C variables
     present in [r]. *)
  let vars_in_gui_res r =
    let rev_vars = match r with
      | GR_Offsm (m_res, _) ->
        begin
          match m_res with
          | GO_Offsetmap m ->
            Cvalue.V_Offsetmap.fold_on_values (fun vu acc ->
                Cvalue.V.fold_bases var_of_base
                  (Cvalue.V_Or_Uninitialized.get_v vu) acc
              ) m []
          | _ -> []
        end
      | GR_Value (value, _) ->
        begin
          match value.Eval.v with
          | `Bottom -> []
          | `Value v ->
            match get_cvalue with
            | None -> []
            | Some get -> Cvalue.V.fold_bases var_of_base (get v) []
        end
      | GR_Zone z -> Locations.Zone.fold_bases var_of_base z []
      | GR_Status _ | GR_Empty -> []
    in
    (* inverse the list to preserve the order of the offsetmap *)
    List.rev rev_vars

end

type gui_loc =
  | GL_Stmt of kernel_function * stmt
  | GL_Pre of kernel_function (* pre-state of a function *)
  | GL_Post of kernel_function (* post-state of a function *)

let gui_loc_equal lm1 lm2 =
  match lm1, lm2 with
  | GL_Stmt (_, s1), GL_Stmt (_, s2) -> Cil_datatype.Stmt.equal s1 s2
  | GL_Pre kf1, GL_Pre kf2
  | GL_Post kf1, GL_Post kf2 -> Kernel_function.equal kf1 kf2
  | (GL_Stmt _ | GL_Pre _ | GL_Post _), _ -> false

let gui_loc_loc = function
  | GL_Stmt (_, stmt) -> Cil_datatype.Stmt.loc stmt
  | GL_Pre kf | GL_Post kf -> Kernel_function.get_location kf

let kf_of_gui_loc = function
  | GL_Stmt (kf, _) | GL_Pre kf | GL_Post kf -> kf

(* This pretty-printer drops the toplevel kf, which is always the function
   in which we are pretty-printing the expression/term *)
let pretty_callstack fmt cs =
  match cs with
  | [_, Kglobal] -> ()
  | (_kf_cur, Kstmt callsite) :: q -> begin
      let rec aux callsite = function
        | (kf, callsite') :: q -> begin
            Format.fprintf fmt "%a (%a%t)"
              Kernel_function.pretty kf
              Cil_datatype.Location.pretty (Cil_datatype.Stmt.loc callsite)
              (fun fmt ->
                 if Gui_parameters.debug_atleast 1 then
                   Format.fprintf fmt ", %d" callsite.sid);
            match callsite' with
            | Kglobal -> ()
            | Kstmt callsite' ->
              Format.fprintf fmt " ←@ ";
              aux callsite' q
          end
        | _ -> assert false
      in
      Format.fprintf fmt "@[<hv>%a" Value_types.Callstack.pretty_hash cs;
      aux callsite q;
      Format.fprintf fmt "@]"
    end
  | _ -> assert false

(* This pretty-printer prints only the lists of the functions, not
   the locations *)
let pretty_callstack_short fmt cs =
  match cs with
  | [_, Kglobal] -> ()
  | (_kf_cur, Kstmt _callsite) :: q ->
    Format.fprintf fmt "%a" Value_types.Callstack.pretty_hash cs;
    Pretty_utils.pp_flowlist ~left:"@[" ~sep:" ←@ " ~right:"@]"
      (fun fmt (kf, _) -> Kernel_function.pretty fmt kf) fmt q
  | _ -> assert false


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
