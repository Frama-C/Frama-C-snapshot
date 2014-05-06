(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Ctypes
open Lang.F
open Memory

module type Code =
sig
  type loc
  val equal_obj : c_object -> loc value -> loc value -> pred
end

module type Logic =
sig
  type loc
  val vars : loc Memory.sloc list -> Vars.t
  val pp_logic : Format.formatter -> loc Memory.logic -> unit
  val pp_sloc : Format.formatter -> loc Memory.sloc -> unit
  val pp_region : Format.formatter -> loc Memory.sloc list -> unit
end

module Make
  ( M : Memory.Model )
  ( C : Code with type loc = M.loc )
  ( L : Logic with type loc = M.loc ) :
sig

  open M
  open Memory

  type region = (c_object * loc sloc list) list

  val vars : region -> Vars.t
  val domain : region -> Heap.set
  val assigned : sigma sequence -> region -> pred list

end
