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

open Cil_types
open Cil
open Locations
module Ki = Cil_datatype.Kinstr


module Location_list = Datatype.List (Locations.Location)

module Non_linear_assignments =
  Cil_state_builder.Varinfo_hashtbl
    (Cil_datatype.Kinstr.Hashtbl.Make(Location_list))
    (struct
      let name = "Non linear assignments"
      let size = 37
      let dependencies = [ Ast.self ]
    end)

module Loc_hashtbl = Hashtbl.Make (Location_Bits)

class do_non_linear_assignments = object(self)
  inherit Visitor.frama_c_inplace as super
  val mutable current_locs = None
  val mutable assigns_table =
    (Ki.Hashtbl.create 17 : Location_list.t Ki.Hashtbl.t)

  method result = assigns_table

  method! vstmt s =
    current_locs <- None;
    match s.skind with
      | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore (visitCilStmt (self:>cilVisitor) stmt))
            seq;
          SkipChildren (* do not visit the additional lvals *)
      | _ -> super#vstmt s

  method! vlval lv =
    match current_locs with
      None -> SkipChildren
    | Some current_locs ->
        begin match lv with
          Mem _e, _ -> DoChildren
        | Var v, NoOffset ->
            let loc = Locations.loc_of_varinfo v in
            ignore (Loc_hashtbl.find current_locs loc.loc);
            SkipChildren
        | Var _v, (Index _ | Field _) -> DoChildren
        end


  method! vcode_annot _ = SkipChildren

  method private visit_addr lv =
    begin match lv with
      Var v, offset ->
        let offset' = visitCilOffset (self :> cilVisitor) offset in
        let v' = Cil.get_varinfo self#behavior v in
        if offset' == offset && v == v'
        then SkipChildren
        else ChangeTo (Var v', offset')
    | Mem e, offset ->
        let e' = visitCilExpr (self :> cilVisitor) e in
        let offset' = visitCilOffset (self :> cilVisitor) offset in
        if offset' == offset && e == e'
        then SkipChildren
        else ChangeTo (Mem e', offset')
    end;

  method! vinst i =
    match i with
    | Set (lv,exp,_) ->
        current_locs <- Some (Loc_hashtbl.create 7);
        begin match lv with
          Var _, offset ->
            ignore (self#voffs offset);
        | Mem e, offset ->
            ignore (self#vexpr e);
            ignore (self#voffs offset);
        end;
        ignore (self#vexpr exp);
        (* TODO: do some stuff with self#current_stmt *)
        SkipChildren
    | _ -> SkipChildren

  method! vexpr exp =
    match exp.enode with
    | AddrOf _lv | StartOf _lv ->
        SkipChildren (* TODO: do better stuff *)
    | _ -> DoChildren

end

let compute_non_linear_assignments f =
  let vis = new do_non_linear_assignments in
  ignore (Visitor.visitFramacFunction (vis:>Visitor.frama_c_visitor) f);
  vis#result

let find fundec =
  let var = fundec.svar in
  try Non_linear_assignments.find var
  with Not_found ->
    let nl = compute_non_linear_assignments fundec in
    Non_linear_assignments.replace var nl;
    nl

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
