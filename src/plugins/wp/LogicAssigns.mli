(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
