(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

exception Alignment_error of string
let align_error s = raise (Alignment_error s)

(* Returns true if the list of attributes [attrs] contains an [align]
 * attribute of [algn] or greater. Returns false otherwise.
 * Throws an exception if
 *  - [attrs] contains several [align] attributes specifying different
 *    alignment
 *  - [attrs] has a single align attribute with a value which is less than [algn] *)
let sufficiently_aligned attrs algn =
  let alignment = List.fold_left (fun acc attr ->
    match attr with
    | Attr("align", [AInt i]) ->
      let alignment = Integer.to_int i in
      if acc <> 0 && acc <> alignment then
        (* Multiple align attributes with different values *)
        align_error "Multiple alignment attributes"
      else if alignment < algn then
        (* If there is an alignment attribute it should be greater
          * or equal to [algn] *)
        align_error "Insufficient alignment"
      else
        alignment
    | Attr("align", _) ->
      (* Align attribute with an argument other than a single number,
      should not happen really *)
      assert false
    | _ -> acc
  ) 0 attrs in alignment > 0

(* Given the type and the list of attributes of [varinfo] ([fieldinfo]) return
 * true if that [varinfo] ([fieldinfo]) requires to be aligned at the boundary
 * of [algn] (i.e., less than [algn] bytes and has no alignment attribute *)
let require_alignment typ attrs algn =
  Cil.bitsSizeOf typ < algn*8 && not (sufficiently_aligned attrs algn)

class prepare_visitor prj = object (self)
  inherit Visitor.frama_c_copy prj

  (* Add align attributes to local variables (required by temporal analysis) *)
  method !vblock _ =
    if Temporal.is_enabled () then
      Cil.DoChildrenPost (fun blk ->
        List.iter (fun vi ->
          (* 4 bytes alignment is required to allow sufficient space for storage
             of 32-bit timestamps in a 1:1 shadow. *)
          if require_alignment vi.vtype vi.vattr 4; then begin
            vi.vattr <- Attr("aligned",[AInt Integer.four]) :: vi.vattr
          end)
          blk.blocals;
        blk)
    else
      Cil.DoChildren

  (* IMPORTANT: for keeping property statuses, we must preserve the ordering of
     translation, see function [Translate.translate_pre_spec] and
     [Translate.translate_post_spec]: be careful when modifying it. *)

  method private push_pre_spec s =
    let kf = Extlib.the self#current_kf in
    let kinstr = self#current_kinstr in
    let open Keep_status in
    Extlib.may
      (fun v -> push kf K_Decreases (Property.ip_of_decreases kf kinstr v))
      s.spec_variant;
    Extlib.may
      (fun t -> push kf K_Terminates (Property.ip_of_terminates kf kinstr t))
      s.spec_terminates;
    List.iter
      (fun l ->
        push kf K_Complete (Property.ip_of_complete kf kinstr ~active:[] l))
      s.spec_complete_behaviors;
    List.iter
      (fun l ->
        push kf K_Disjoint (Property.ip_of_disjoint kf kinstr ~active:[] l))
      s.spec_disjoint_behaviors;
    List.iter
      (fun b ->
        List.iter
          (fun p -> push kf K_Requires (Property.ip_of_requires kf kinstr b p))
          b.b_requires)
      s.spec_behavior

  method private push_post_spec spec =
    let do_behavior b =
      let kf = Extlib.the self#current_kf in
      let ki = match self#current_stmt with
        | None -> Kglobal
        | Some stmt -> Kstmt stmt
      in
      let open Keep_status in
      Extlib.may
        (push kf K_Assigns)
        (Property.ip_of_assigns
           kf
           ki
           (Property.Id_contract (Datatype.String.Set.empty (* TODO *), b))
           b.b_assigns);
      List.iter
        (fun p -> push kf K_Ensures (Property.ip_of_ensures kf ki b p))
        b.b_post_cond
    in
    (* fix ordering of behaviors' iterations *)
    let bhvs =
      List.sort
        (fun b1 b2 -> String.compare b1.b_name b2.b_name)
        spec.spec_behavior
    in
    List.iter do_behavior bhvs

  method private push_pre_code_annot a =
    let kf = Extlib.the self#current_kf in
    let stmt = Extlib.the self#current_stmt in
    let push_single k a =
      Keep_status.push kf k (Property.ip_of_code_annot_single kf stmt a)
    in
    let open Keep_status in
    match a.annot_content with
    | AAssert _ -> push_single K_Assert a
    | AStmtSpec(_ (* TODO *), s) -> self#push_pre_spec s
    | AInvariant _ -> push_single K_Invariant a
    | AVariant v ->
      push kf K_Variant (Property.ip_of_decreases kf (Kstmt stmt) v)
    | AAssigns _ ->
      (* TODO: should be a postcondition, but considered as a unhandled
         precondition in translate.ml right now; and we need to preserve the
         same ordering *)
      Extlib.may
        (push kf K_Assigns)
        (Property.ip_assigns_of_code_annot kf (Kstmt stmt) a)
    | AAllocation(_ (* TODO *), alloc) ->
      Extlib.may
        (push kf K_Allocation)
        (Property.ip_of_allocation kf (Kstmt stmt) (Property.Id_loop a) alloc)
    | APragma _ -> () (* not yet translated *)
    | AExtended _ -> () (* never translate extensions *)

  method private push_post_code_annot a = match a.annot_content with
  | AStmtSpec(_ (* TODO *), s) -> self#push_post_spec s
  | AAssert _
  | AInvariant _
  | AVariant _
  | AAssigns _
  | AAllocation _
  | APragma _
  | AExtended _ -> ()

  (* Move variable declared in the body of a switch statement to the outer
     scope *)
  method !vstmt_aux init_stmt =
    Annotations.iter_code_annot
      (fun _ a -> self#push_pre_code_annot a)
      init_stmt;
    Cil.DoChildrenPost
      (fun stmt ->
        Annotations.iter_code_annot
          (fun _ a -> self#push_post_code_annot a)
          init_stmt;
        match stmt.skind with
        | Switch(_,sw_blk,_,_) ->
          let new_blk = Cil.mkBlock [ stmt ] in
          let new_stmt = Cil.mkStmt (Block new_blk) in
          new_blk.blocals <- sw_blk.blocals;
          sw_blk.blocals <- [];
          new_stmt
        | _ -> stmt)

  method private is_unvariadic_function vi =
    match Cil.unrollType vi.vtype with
    | TFun(_, _, variadic, _) -> not variadic
    | _ -> false

  method !vglob_aux = function
  | GVarDecl(vi, loc) | GFunDecl(_, vi, loc) | GFun({ svar = vi }, loc)
      when self#is_unvariadic_function vi
        && not (Misc.is_library_loc loc)
        && not (Cil.is_builtin vi)
        ->
    let kf = Extlib.the self#current_kf in
    let s = Annotations.funspec ~populate:false kf in
    Cil.DoChildrenPost
      (fun f ->
        self#push_pre_spec s;
        self#push_post_spec s;
        f)
  | _ ->
    Cil.DoChildren

  initializer Project.copy ~selection:(Parameter_state.get_selection ()) prj

 end

let prepare () =
  Options.feedback ~level:2 "prepare AST for E-ACSL transformations";
  File.create_project_from_visitor
    "e_acsl_prepare_ast"
    (new prepare_visitor)
