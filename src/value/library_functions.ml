(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
open Abstract_interp

module Retres =
  Cil_computation.VarinfoHashtbl
    (Cil_datatype.Varinfo)
      (struct
	 let name = "retres_variable"
	 let size = 9
	 let dependencies = [Ast.self]
       end)

let get f_vi =
  try
    Retres.find f_vi
  with Not_found ->
    let typ = Cil.getReturnType f_vi.vtype in
    let rv = makeVarinfo false false "__retres" typ in
    Retres.add f_vi rv;
    rv


let add_dependency = Project.Computation.add_dependency Db.Value.self

let () = add_dependency Retres.self

let add_retres_to_state f_vi offsetmap state =
  let retres_vi = get f_vi in
  let retres_base = Base.create_varinfo retres_vi in
  let loc = Location_Bits.inject retres_base Ival.zero in
  let size = 
    try 
      Int.of_int (bitsSizeOf retres_vi.vtype) 
    with SizeOfError _ -> 
      Value_parameters.abort "library function return type size unknown. Please report"
  in
  Some retres_vi,
  Relations_type.Model.paste_offsetmap offsetmap loc Int.zero size state

