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

(* -------------------------------------------------------------------------- *)
(** Memory Model for functional variables.                                    *)
(** Optimisation for variables which address is never taken.                  *)
(* -------------------------------------------------------------------------- *)

(** The interaction between M and funvar depends on criteria.
    For instance, the Store model discharges the traitment of
    functional variables to funvar and traits the variables
    which addresses are taken.
    For the Hoare model thing are different :
    the traitment of functional variables is discharged to funvar.
    A variable which address is taken has to be represented by
    two locations :
         1) One for itself traits by funvar
         2) Another for its address traits by M.
   Hence, to drive the set of variables manage by M (mem) of
   funvar (fun), use the module Criteria.
*)



(** Define the criteria of variables trait by funvar of by M *)
module type Criteria =
sig
  val isHoare : bool
end

module Create (Crit:Criteria)(M:Mwp.S) : Mwp.S with module F=M.F


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
