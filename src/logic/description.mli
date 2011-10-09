(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Module for Describing items of Source and Properties. 
    @since Nitrogen-20111001 *)

open Cil_types

val pp_loc : Format.formatter -> location -> unit
(** prints "<file,line>" or "generated" *)

val pp_stmt : Format.formatter -> stmt -> unit
(** prints "<instruction> (<file,line>)" *)

val pp_kinstr : Format.formatter -> kinstr -> unit
(** prints nothing for global, or " at <stmt>" *)

val pp_idpred : Format.formatter -> identified_predicate -> unit
(** prints the "'<labels>'" or the "(<location>)" of the predicate *)

val pp_froms : Format.formatter -> identified_term from list -> unit
(** prints message "nothing" or the "'<names>'" or the "(<location>)" of the
    relation *) 

val pp_for : Format.formatter -> string list -> unit
(** prints nothing or " for 'b1,...,bn'" *)

val pp_bhv : Format.formatter -> funbehavior -> unit
(** prints nothing for default behavior, and " for 'b'" otherwize *)

val pp_property : Format.formatter -> Property.t -> unit
(** prints an identified property *)

type kf = [ `Always | `Never | `Context of kernel_function ]

val pp_localized : kf:kf -> ki:bool -> Format.formatter -> Property.t -> unit
(** prints more-or-less localized property *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
