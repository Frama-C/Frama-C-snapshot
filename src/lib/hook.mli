(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, either version 3 of the License, or (at your option)      *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public Licence version 3 for more details  *)
(*  (enclosed in the file licences/LGPLv3).                               *)
(*                                                                        *)
(**************************************************************************)

(* $Id: hook.mli,v 1.2 2008/03/25 15:54:05 uid568 Exp $ *)

(** Hook builder. A hook is a bunch of functions which can be extended and
    applied at any program point. *)

(** Output signature. *)
module type S = sig

  type param
    (** Type of the parameter of the functions registered in the hook. *)

  val extend: (param -> unit) -> unit
    (** Add a new function to the hook. *)

  val apply: param -> unit
    (** Apply all the functions of the hook on the given parameter. *)

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
