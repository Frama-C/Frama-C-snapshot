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

module Retres =
  Kernel_function.Make_Table
    (Datatype.Option(Cil_datatype.Varinfo))
    (struct
      let name = "Value.Library_functions.Retres"
      let size = 9
      let dependencies = [Ast.self]
    end)
let () = Ast.add_monotonic_state Retres.self

let () =
  State_dependency_graph.add_dependencies ~from:Retres.self [ Db.Value.self ]

let get_retres_vi = Retres.memo
    (fun kf ->
       let vi = Kernel_function.get_vi kf in
       let typ = Cil.getReturnType vi.vtype in
       if Cil.isVoidType typ then
         None
       else
         try
           ignore (Cil.bitsSizeOf typ);
           let name = Format.asprintf "\\result<%a>" Kernel_function.pretty kf in
           Some (Cil.makeVarinfo false false name typ)
         with Cil.SizeOfError _ ->
           Value_parameters.abort ~current:true
             "function %a returns a value of unknown size. Aborting"
             Kernel_function.pretty kf
    )

let returned_value kf =
  let return_type = Cil.unrollType (Kernel_function.get_return_type kf) in
  match return_type with
  | TComp _ when Cil.is_fully_arithmetic return_type -> Cvalue.V.top_int
  | TPtr _ | TComp _ -> Cvalue.V.inject Base.null Ival.zero
  | TInt _ | TEnum _ ->  Cvalue.V.top_int
  | TFloat (FFloat, _) -> Cvalue.V.top_single_precision_float
  | TFloat (FDouble, _)
  | TFloat (FLongDouble, _) -> Cvalue.V.top_float
  | TBuiltin_va_list _ ->
    Value_parameters.error ~current:true ~once:true
      "functions returning variadic arguments must be stubbed%t"
      Value_util.pp_callstack;
    Cvalue.V.top_int
  | TVoid _ -> Cvalue.V.top (* this value will never be used *)
  | TFun _ | TNamed _ | TArray _ -> assert false


let unsupported_specifications =
  [ "glob", "glob.c";
    "globfree", "glob.c";
    "getaddrinfo", "netdb.c";
    "getline", "stdio.c";
    "strerror", "string.c";
    "strdup", "string.c";
    "strndup", "string.c";
    "getenv", "stdlib.c";
    "putenv", "stdlib.c";
    "setenv", "stdlib.c";
    "unsetenv", "stdlib.c"
  ]

let unsupported_specs_tbl =
  let tbl = Hashtbl.create 10 in
  List.iter
    (fun (name, file) -> Hashtbl.replace tbl name file)
    unsupported_specifications;
  tbl

let warn_unsupported_spec name =
  try
    let header = Hashtbl.find unsupported_specs_tbl name in
    Value_parameters.warning ~once:true ~current:true
      ~wkey:Value_parameters.wkey_libc_unsupported_spec
      "@[The specification of function '%s' is currently not supported by Eva.@ \
       Consider adding %s@ to the analyzed source files.@]"
      name (Config.datadir ^ "/libc/" ^ header)
  with Not_found -> ()


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
