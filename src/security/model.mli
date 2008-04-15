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

(* $Id: model.mli,v 1.23 2008/04/01 09:25:22 uid568 Exp $ *)

open Cil_types
open Db
open Db_types
open Locations

val state_name: string

module Make(S : Lattice.S) : sig

  (** State of the analysis *)
  module State : sig
    type t
    val reset : unit -> t
    val combine : old:t -> t -> t
    val is_included : t -> t -> bool
    val pretty : Format.formatter -> t -> unit
    val clear_leaks : t -> t
    val push_call : Db.Value.state -> kernel_function -> exp list -> t -> t (* * S.t list*)
    val pop_call : kinstr -> fundec -> exp list -> old:t -> t -> t
    val last_stmt : stmt ref
    val print_results : t -> unit
  end

  (** How to register variables *)
  module Register : sig
    val globals : State.t -> State.t
    val formals : 
      varinfo list -> exp list -> Db.Value.state -> State.t -> 
      State.t * Db.Value.state
    val locals : Db.Value.state -> fundec -> State.t -> State.t
    val clean: kernel_function -> State.t -> State.t
  end

  (** Logic model *)
  module Logic : sig
    val affect: Db.Value.state -> State.t -> lval -> exp -> State.t
    val return: Db.Value.state -> State.t -> lval option -> exp -> State.t
    val requires: 
      Db.Value.state -> kinstr -> State.t -> predicate named -> State.t
    val ensures: Db.Value.state -> State.t -> predicate named -> State.t
    val warn_todo: unit -> unit
  end

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
