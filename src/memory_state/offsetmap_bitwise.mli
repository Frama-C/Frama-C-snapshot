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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

open Abstract_interp
open Abstract_value

type itv = Int.t * Int.t

module Make(V:sig include Abstract_interp.Lattice val tag: t -> int end) : sig

  include Datatype.S_no_copy
  val degenerate : V.t -> t
  val pretty_with_type : Cil_types.typ option -> Format.formatter -> t -> unit

  val empty : t
  val is_empty : t -> bool

  val find : (Int.t -> Int.t -> V.t) -> itv -> t -> V.t
  val find_intervs : (Int.t -> Int.t -> V.t) ->
    Int_Intervals.t -> t -> V.t

  val add : itv -> V.t -> t -> t
  val add_approximate :
    itv -> V.t -> t -> t
  val collapse : t -> V.t
  val find_iset :
    (Int.t -> Int.t -> V.t) -> V.t ->
    Int_Intervals.t -> t -> V.t

  val add_iset : exact:bool -> Int_Intervals.t -> V.t -> t -> t
  val join : t -> t -> t
  val joindefault :  t -> t
  val is_included_exn : t -> t -> unit
  val map_and_merge : (V.t -> V.t) -> t -> t -> t
  val map :  (bool * V.t -> bool * V.t) -> t -> t
  val map2 :
    ((bool * V.t) option -> (bool * V.t) option -> bool * V.t)
    -> t -> t -> t
  val fold :
    (Int_Intervals.t -> bool * V.t -> 'a -> 'a) -> t -> 'a -> 'a

  val copy_paste :
    f:((bool*V.t -> bool*V.t) * (Int.t -> Int.t -> V.t)) option ->
    t -> Int.t -> Int.t -> Int.t -> t -> t

  val copy_merge : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy :
    f:((bool*V.t -> bool*V.t) * (Int.t -> Int.t -> V.t)) option ->
    t -> Int.t -> Int.t -> t

  val tag: t -> int

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
