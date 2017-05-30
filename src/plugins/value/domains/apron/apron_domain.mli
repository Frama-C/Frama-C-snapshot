(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Experimental binding for the numerical abstract domains provided by
    the APRON library: http://apron.cri.ensmp.fr/library
    For now, this binding only processes scalar integer variables. *)

(** Are apron domains available? *)
val ok : bool

(** Signature of an Apron domain in EVA. *)
module type S = Abstract_domain.Internal
  with type value = Main_values.Interval.t
   and type location = Precise_locs.precise_location


(** Apron domains available for Eva. *)

(** Octagons abstract domain. *)
module Octagon : S

(** Intervals abstract domain. *)
module Box : S

(** Loose polyhedra of the NewPolka library.
    Cannot have strict inequality constraints. Algorithmically more efficient. *)
module Polka_Loose : S

(** Strict polyhedra of the NewPolka library. *)
module Polka_Strict : S

(** Linear equalities. *)
module Polka_Equalities : S


(** Domain keys for the Apron domains in Eva. *)

val octagon_key : Octagon.t Abstract_domain.key
val box_key : Box.t Abstract_domain.key
val polka_loose_key : Polka_Loose.t Abstract_domain.key
val polka_strict_key : Polka_Strict.t Abstract_domain.key
val polka_equalities_key : Polka_Equalities.t Abstract_domain.key


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
