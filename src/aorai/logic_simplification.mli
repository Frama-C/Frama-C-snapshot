(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

(** Basic simplification over {!Promelaast.typed_condition} *)

open Promelaast

(** {2 smart constructors for typed conditions} *)
val tand: typed_condition -> typed_condition -> typed_condition
val tor: typed_condition -> typed_condition -> typed_condition
val tnot: typed_condition -> typed_condition


(** {2 simplifications} *)

(** Given a condition, this function does some logical simplifications
    and returns an equivalent DNF form together with the simplified version *)
val simplifyCond: 
  Promelaast.typed_condition -> 
  Promelaast.typed_condition *(Promelaast.typed_condition list list)

(** Given a transition list, this function returns the same transition list with
    simplifyCond done on each cross condition. Uncrossable transition are
    removed. *) 
val simplifyTrans: 
  Promelaast.typed_condition Promelaast.trans list -> 
  (Promelaast.typed_condition Promelaast.trans list)*
    (Promelaast.typed_condition list list list)

val dnfToCond : 
  (Promelaast.typed_condition list list) -> Promelaast.typed_condition

val simplifyDNFwrtCtx : 
  Promelaast.typed_condition list list -> Cil_types.kernel_function -> 
  Promelaast.funcStatus -> Promelaast.typed_condition

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
