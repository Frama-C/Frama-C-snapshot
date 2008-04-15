(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* $Id: register.ml,v 1.7 2008/11/19 13:38:25 uid568 Exp $ *)

let load =
  Journal.register
    ~use_apply:true
    "Journal_loader.load"
    (Type.func Type.string Type.unit)
    (fun file ->
	 try
	   (* Assume MyDynlink.init was called earlier *)
           (*Format.printf "STARTHERE %s@.@.@." file ;*)
	   MyDynlink.loadfile (Extlib.adapt_filename file)
           (*Format.printf "HERE@.@.@."*)
	 with 
	 | MyDynlink.Unsupported_Feature msg -> 
	     raise (Journal.LoadingError msg)
	 | MyDynlink.Error err -> 
	     raise (Journal.LoadingError (MyDynlink.error_message err))
	 | Sys_error s ->
	     raise (Journal.LoadingError s))

(** @plugin development guide *)
let () = 
  Dynamic.register "Journal_loader.load" (Type.func Type.string Type.unit) load

(** @plugin development guide *)
module LoadFile = Cmdline.Dynamic.Register.EmptyString
  (struct let name = "Journal_loader.load" end)

let () = Options.add_plugin
  ~name:"journal loading"
  ~descr:"Dynamic journal loading"
  ["-load-journal",
   Arg.String LoadFile.set,
   "filename : file name of the journal to load"]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
