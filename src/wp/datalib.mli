(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
open Ctypes
open Formula


module Cvalues(M : Mvalues.Model) : Mvalues.Values
  with type loc = M.loc
  and module F = M.F
  and module A = M.A
  and module R = M.R


module Collector : sig
  type t

  val push : unit -> t
  val pop : t -> Wpo.warning list * Property.t list

  (**
   * When adding a warning, one has to provide :
   * - the source of the warning (for instance "model M"),
   * - the effect of the warning (for instance "stop computation")
   * - and a formated message about why this warning is emited.
  *)
  val add_warning : ?severe:bool -> ?source:string -> reason:string ->
    ('a, Format.formatter, unit) format -> 'a

  val add_depend : Property.t -> unit

end





module Create (V:Mvalues.Values) : Formula.Logic with module F = V.F



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
