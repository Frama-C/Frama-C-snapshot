(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

module type Parameter_input = sig
  include Plugin.Parameter_input
  val module_name: string
end

module type Parameter_input_with_arg = sig
  include Plugin.Parameter_input_with_arg
  val module_name: string
end

module type COMPLEX_VALUE = sig
  include Plugin.COMPLEX_VALUE
  val module_name: string
end

let () = Plugin.register_kernel ()

module P = Plugin.Register
  (struct
     let name = ""
     let shortname = ""
     let module_name = ""
     let descr = "General options of Frama-C"
   end)

include (P: Plugin.S)

module Bool(X:sig include Parameter_input val default: bool end) =
  P.Bool(struct let () = Plugin.set_module_name X.module_name include X end)

module False(X: Parameter_input) = 
  P.False(struct let () = Plugin.set_module_name X.module_name include X end)

module True(X: Parameter_input) = 
  P.True(struct let () = Plugin.set_module_name X.module_name include X end)

module Int (X: sig val default: int include Parameter_input_with_arg end) =
  P.Int(struct let () = Plugin.set_module_name X.module_name include X end)

module Zero(X:Parameter_input_with_arg) =
  P.Zero(struct let () = Plugin.set_module_name X.module_name include X end)

module String
  (X: sig include Parameter_input_with_arg val default: string end) =
  P.String(struct let () = Plugin.set_module_name X.module_name include X end)

module EmptyString(X: Parameter_input_with_arg) =
  P.EmptyString
    (struct let () = Plugin.set_module_name X.module_name include X end)

module StringSet(X: Parameter_input_with_arg) =
  P.StringSet
    (struct let () = Plugin.set_module_name X.module_name include X end)

module StringList(X: Parameter_input_with_arg) =
  P.StringList
    (struct let () = Plugin.set_module_name X.module_name include X end)

module IndexedVal (V:COMPLEX_VALUE) = 
  P.IndexedVal
    (struct let () = Plugin.set_module_name V.module_name include V end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
