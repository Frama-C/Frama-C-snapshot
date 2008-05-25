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

class type t =  object 
  method model : GTree.model_filter
  method set_file_attribute: 
    ?strikethrough:bool -> ?visible:bool -> ?text:string -> string -> unit
  method set_global_attribute: 
    ?strikethrough:bool -> ?visible:bool -> ?text:string -> Cil_types.varinfo -> unit
  method add_select_function : 
    (was_activated:bool -> activating:bool -> Cil_types.global list -> unit) -> unit
  method select_global : Cil_types.varinfo -> unit
  method view : GTree.view
  method reset : unit -> t
end

val make : GTree.view -> t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
