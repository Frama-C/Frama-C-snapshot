(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: hook.mli,v 1.4 2008-11-04 10:05:05 uid568 Exp $ *)

(** Hook builder. A hook is a bunch of functions which can be extended and
    applied at any program point. *)

(** Output signature. *)
module type S = sig

  type param
    (** Type of the parameter of the functions registered in the hook. *)

  val extend: (param -> unit) -> unit
    (** Add a new function to the hook. *)

  val apply: param -> unit
    (** Apply all the functions of the hook on the given parameter. 
	These functions are applied from the least recently entered to the most
	recently entered. *)

  val is_empty: unit -> bool
    (** Is no function already registered in the hook? *)

  val clear: unit -> unit
    (** Clear the hook. *)

  val length: unit -> int
    (** Number of registered functions. *)

end

(** Make a new empty hook from a given type of parameters. *)
module Build(P:sig type t end) : S with type param = P.t

(** Make a new empty hook from [unit]. *)
module Make(X:sig end) : S with type param = unit

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
