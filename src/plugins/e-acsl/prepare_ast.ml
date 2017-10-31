(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

class prepare_visitor prj = object (_)
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

  (* Move variable declared in the body of a switch statement to the outer
     scope *)
  method !vstmt_aux _ =
    Cil.DoChildrenPost (fun stmt ->
      match stmt.skind with
      | Switch(_,sw_blk,_,_) ->
        let new_blk = Cil.mkBlock [ stmt ] in
        let new_stmt = Cil.mkStmt (Block new_blk) in
        new_blk.blocals <- sw_blk.blocals;
        sw_blk.blocals <- [];
        new_stmt
      | _ -> stmt)

  initializer
    Project.copy ~selection:(Parameter_state.get_selection ()) prj
end

let prepare () =
  Options.feedback ~level:2 "prepare AST for E-ACSL transformations";
  File.create_project_from_visitor
    "e_acsl_prepare_ast"
    (new prepare_visitor)
