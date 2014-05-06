(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Describe items of Source and Properties. 
    @since Nitrogen-20111001 *)

open Cil_types

val pp_stmt : bool -> Format.formatter -> stmt -> unit
(** prints "<instruction>" or "<instruction> (<file,line>)" *)

val pp_kinstr : bool -> Format.formatter -> kinstr -> unit
(** prints nothing for global, or " at <stmt>" *)

val pp_idpred : bool -> Format.formatter -> identified_predicate -> unit
(** prints the "'<labels>'" or the "(<location>)" of the predicate *)

val pp_region : bool -> Format.formatter -> identified_term from list -> unit
(** prints message "nothing" or the "'<names>'" or the "(<location>)" of the
    relation *) 

val pp_named: Format.formatter -> 'a named -> unit
(** prints the name of a named logic structure (if any), separated by ','. *)

val pp_for : Format.formatter -> string list -> unit
(** prints nothing or " for 'b1,...,bn'" *)

val pp_bhv : Format.formatter -> funbehavior -> unit
(** prints nothing for default behavior, and " for 'b'" otherwize *)

val pp_property : Format.formatter -> Property.t -> unit
(** prints an identified property *)

type kf = [ `Always | `Never | `Context of kernel_function ]

val pp_localized : kf:kf -> ki:bool -> kloc:bool -> Format.formatter -> Property.t -> unit
(** prints more-or-less localized property *)

val pp_local : Format.formatter -> Property.t -> unit
(** completely local printer *)

val pp_compare : Property.t -> Property.t -> int
(** Computes a partial order compatible with pretty printing *)

val full_compare : Property.t -> Property.t -> int
(** Completes [pp_compare] with [Property.compare] *)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
