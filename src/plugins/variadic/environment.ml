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

module Table = Datatype.String.Hashtbl

type env =
{
  globals: varinfo Table.t;
  functions: varinfo Table.t;
  typedefs: typeinfo Table.t;
  structs: compinfo Table.t;
  unions: compinfo Table.t;
  enums: enuminfo Table.t;
}

let empty () : env =
{
  globals = Table.create 17;
  functions = Table.create 17;
  typedefs = Table.create 17;
  structs = Table.create 17;
  unions = Table.create 17;
  enums = Table.create 17;
}

let add_global (env : env) (vi : varinfo) : unit  =
  Table.add env.globals vi.vname vi

let add_function (env : env) (vi : varinfo) : unit  =
  Table.add env.functions vi.vname vi

let add_typeinfo (env : env) (typeinfo : typeinfo) : unit =
  Table.add env.typedefs typeinfo.torig_name typeinfo

let add_compinfo (env : env) (compinfo : compinfo) : unit  =
  let table = if compinfo.cstruct then env.structs else env.unions in
  Table.add table compinfo.corig_name compinfo

let add_enuminfo (env : env) (enuminfo : enuminfo) : unit  =
  Table.add env.enums enuminfo.eorig_name enuminfo

let find_global (env : env) (vname : string) : varinfo  =
  Table.find env.globals vname

let find_function (env : env) (vname : string) : varinfo =
  Table.find env.functions vname

let find_typedef (env : env) (tname : string) : typeinfo=
  Table.find env.typedefs tname

let find_struct (env : env) (tname : string) : compinfo =
  Table.find env.structs tname

let find_union (env : env) (tname : string) : compinfo =
  Table.find env.unions tname

let find_enum (env : env) (tname : string) : enuminfo =
  Table.find env.enums tname

let find_type (env : env) (namespace : Logic_typing.type_namespace)
    (tname : string) : typ =
  match namespace with
  | Logic_typing.Typedef ->
    TNamed (find_typedef env tname, [])
  | Logic_typing.Struct ->
    TComp (find_struct env tname, {scache=Not_Computed}, [])
  | Logic_typing.Union ->
    TComp (find_union env tname, {scache=Not_Computed}, [])
  | Logic_typing.Enum ->
    TEnum (find_enum env tname, [])

let from_file (file : file) : env =
  let env = empty () in
  let v = object inherit Cil.nopCilVisitor
    method! vglob glob =
      begin match glob with
      | GFunDecl(_,vi,_) | GFun ({svar = vi}, _) ->
        add_function env vi
      | GVarDecl (vi,_) | GVar (vi, _, _) ->
        add_global env vi
      | GType (typeinfo,_) ->
        add_typeinfo env typeinfo
      | GCompTag (compinfo,_) ->
        add_compinfo env compinfo
      | GEnumTag (enuminfo,_) ->
        add_enuminfo env enuminfo
      | _ -> ()
      end;
      Cil.SkipChildren         
  end in
  Cil.visitCilFile v file;
  env
