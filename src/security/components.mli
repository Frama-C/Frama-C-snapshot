(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: components.mli,v 1.13 2008/06/05 08:31:45 uid568 Exp $ *)

type t
  (** Type of a security component *)

val compare: t -> t -> int
val hash: t -> int
val equal: t -> t -> bool

module Make(X:sig val use_ctrl_dependencies: bool end) : sig

  val init: unit -> unit

  val is_concerned_by_security: Cil_types.stmt -> bool
    (** Return [true] if the given statement belongs to one security
	component. *)

  val fold_fold:
    ('b -> t -> 'a -> 'b) -> ('a -> Cil_types.stmt -> 'a) -> 'b -> 'a -> 'b
    (** [fold_fold f g init_f init_g] folds [g] on each statement [s] of each
	security component [c]. This folding is	initialized with [init_g] and
	returns [c_result]. Next, [f] is folded on each security component [c]
	using [c_result]. This second folding is initialized with [init_f]. *)

  val slice: unit -> Project.t
    (** Slice the application according to the security components:
	all the resulting statement belongs to a security components. *)

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
