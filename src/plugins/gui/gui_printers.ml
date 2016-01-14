(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(* Mapping from vid to varinfos whose name have been printed in the
   annotation window *)
module ResolveVid =
  State_builder.Hashtbl(Datatype.Int.Hashtbl)(Cil_datatype.Varinfo)
    (struct
      let name = "Design.ResolveVid"
      let size = 67
      let dependencies = [Ast.self]
    end)

(* Maps Cil_types.typ to unique IDs (necessary for the type links). *)
module ResolveTypId =
  State_builder.Hashtbl(Cil_datatype.TypNoUnroll.Hashtbl)(Datatype.Int)
    (struct
      let name = "Design.ResolveTypId"
      let size = 67
      let dependencies = [Ast.self]
    end)
(* Maps unique IDs back to Cil_types.typ. *)
module ResolveTyp =
  State_builder.Hashtbl(Datatype.Int.Hashtbl)(Cil_datatype.TypNoUnroll)
    (struct
      let name = "Design.ResolveTyp"
      let size = 67
      let dependencies = [Ast.self]
    end)

(* Maps Cil_types.location to unique IDs. *)
module ResolveLocId =
  State_builder.Hashtbl(Cil_datatype.Location.Hashtbl)(Datatype.Int)
    (struct
      let name = "Design.ResolveLocId"
      let size = 67
      let dependencies = [Ast.self]
    end)
(* Maps unique IDs back to Cil_types.location. *)
module ResolveLoc =
  State_builder.Hashtbl(Datatype.Int.Hashtbl)(Cil_datatype.Location)
    (struct
      let name = "Design.ResolveLoc"
      let size = 67
      let dependencies = [Ast.self]
    end)

(* Returns the ID associated to a linked [typ]
   (adding it to the maps if needed).
   Only typedefs, composite types and enumerations are linked. *)
let tid_of_typ typ =
  match typ with
  | TNamed _ | TComp _ | TEnum _ ->
    (try
       Some (ResolveTypId.find typ)
     with
     | Not_found ->
       let nextId = ResolveTypId.length () in
       ResolveTypId.replace typ nextId;
       ResolveTyp.replace nextId typ;
       Some nextId)
  | _ -> None

(* Returns the ID associated to a location (adding it to the maps if needed). *)
let lid_of_loc loc =
  try
    ResolveLocId.find loc
  with
  | Not_found ->
    let nextId = ResolveLocId.length () in
    ResolveLocId.replace loc nextId;
    ResolveLoc.replace nextId loc;
    nextId

(* Returns the base type for a pointer/array, otherwise [t] itself.
   E.g. for [t = int***], returns [int]. *)
let rec get_type_specifier (t:typ) =
  match t with
  | TPtr (bt, _) | TArray (bt, _, _, _) -> get_type_specifier bt
  | _ -> t

let pp_tcomp_unfolded fmt comp attrs =
  (* uses GCompTag pretty-printer to expand the composite type *)
  let cattrs = Cil.addAttributes attrs comp.cattr in
  let comp = {comp with cattr = cattrs} in
  Printer.pp_global fmt (GCompTag (comp, Cil_datatype.Location.unknown))

let pp_enum_unfolded fmt enum attrs =
  (* use GEnumTag pretty-printer to expand the enum *)
  let eattrs = Cil.addAttributes attrs enum.eattr in
  let enum = {enum with eattr = eattrs} in
  Printer.pp_global fmt (GEnumTag (enum, Cil_datatype.Location.unknown))

(* This function is intended to be used in a class extended by {!LinkPrinter}
   below, as otherwise the sub-types won't be clickable. Doing it differently
   is difficult, because we want to unroll only one level of types
   (hence we cannot say that this function is the method [typ] itself),
   and we cannot add new public methods in extensible printers. *)
let pp_typ_unfolded fmt (t : typ) =
  match t with
  | TNamed (ty, attrs) ->
    begin
      (* unfolds the typedef, and one step further if it is a TComp/TEnum *)
      match ty.ttype with
      | TComp (comp, _, cattrs) ->
        pp_tcomp_unfolded fmt comp (Cil.addAttributes attrs cattrs)
      | TEnum (enum, eattrs) ->
        pp_enum_unfolded fmt enum (Cil.addAttributes attrs eattrs)
      | _ ->
        Printer.pp_typ fmt (Cil.typeAddAttributes attrs ty.ttype)
    end
  | TComp (comp, _, attrs) -> pp_tcomp_unfolded fmt comp attrs
  | TEnum (enum, attrs) -> pp_enum_unfolded fmt enum attrs
  | _ -> Printer.pp_typ fmt t

let pp_typ fmt typ =
  match tid_of_typ typ with
  | None -> Format.fprintf fmt "@{%a@}" Printer.pp_typ typ
  | Some tid ->
    Format.fprintf fmt "@{<link:typ%d>%a@}" tid Printer.pp_typ typ

(* Override the default printer to add <link> tags around types and
   some l-values *)
module LinkPrinter(X: Printer.PrinterClass) = struct
  class printer = object
    inherit X.printer as super

    method! typ ?fundecl nameOpt fmt t =
      match tid_of_typ t with
      | None -> Format.fprintf fmt "@{%a@}" (super#typ ?fundecl nameOpt) t
      | Some tid ->
        Format.fprintf fmt "@{<link:typ%d>%a@}"
          tid (super#typ ?fundecl nameOpt) t

    method! varinfo fmt vi =
      ResolveVid.replace vi.vid vi;
      Format.fprintf fmt "@{<link:vid%d>%a@}" vi.vid super#varinfo vi

    method! location fmt loc =
      let lid = lid_of_loc loc in
      Format.fprintf fmt "@{<link:loc%d>%a@}" lid super#location loc

  end
end

exception NoMatch

let varinfo_of_link s =
  try
    let vid = Scanf.sscanf s "vid%d" (fun id -> id) in
    ResolveVid.find vid
  with Scanf.Scan_failure _ | Not_found (* should not happen *) ->
    raise NoMatch

let typ_of_link s =
  try
    let tid = Scanf.sscanf s "typ%d" (fun id -> id) in
    ResolveTyp.find tid
  with Scanf.Scan_failure _ | Not_found (* should not happen *) ->
    raise NoMatch

let loc_of_link s =
  try
    let tid = Scanf.sscanf s "loc%d" (fun id -> id) in
    ResolveLoc.find tid
  with Scanf.Scan_failure _ | Not_found (* should not happen *) ->
    raise NoMatch
