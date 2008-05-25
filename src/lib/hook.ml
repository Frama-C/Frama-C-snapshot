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

(* $Id: hook.ml,v 1.2 2008/03/25 15:54:05 uid568 Exp $ *)

module type S = sig
  type param
  val extend: (param -> unit) -> unit
  val apply: param -> unit
  val is_empty: unit -> bool
  val clear: unit -> unit
  val length: unit -> int
end

module Build(P:sig type t end) = struct
  type param = P.t
  let hooks = Queue.create ()
  let extend f = Queue.add f hooks
  let apply arg = Queue.iter (fun f -> f arg) hooks
  let is_empty () = Queue.is_empty hooks
  let clear () = Queue.clear hooks
  let length () = Queue.length hooks
end

module Make(X:sig end) = Build(struct type t = unit end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
