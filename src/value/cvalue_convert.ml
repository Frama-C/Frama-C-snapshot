(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

open Abstract_interp

let offsetmap_of_value ~typ v =
  Cvalue.V_Offsetmap.update_ival
       ~with_alarms:CilE.warn_none_mode
       ~validity:Base.All
       ~offsets:Ival.zero
       ~exact:true
       ~size:(Int.of_int (Cil.bitsSizeOf typ))
       Cvalue.V_Offsetmap.empty
       (Cvalue.V_Or_Uninitialized.initialized v)

let wrap_int i = Some (offsetmap_of_value ~typ:Cil.intType i)
let wrap_ptr p = Some (offsetmap_of_value ~typ:Cil.intPtrType p)
let wrap_double d = Some (offsetmap_of_value ~typ:Cil.doubleType d)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
