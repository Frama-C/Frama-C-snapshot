(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

(** Wrapper for [Map] compatible with all OCaml versions. 
    @since Nitrogen-20111001 *)

module type S = sig
  include Map.S
  val merge: 
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** @since Oxygen-20120901 *)

  val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** @since Oxygen-20120901 *)

  val max_binding: 'a t -> (key * 'a)
  (** @raise Not_found if the map is empty
      @since Oxygen-20120901 *)

  val min_binding: 'a t -> (key * 'a)
  (** @raise Not_found if the map is empty
      @since Oxygen-20120901 *)

  val choose: 'a t -> (key * 'a)
  (** @raise Not_found if the map is empty
      @since Oxygen-20120901 *)

  val cardinal: 'a t -> int
  (** @since Oxygen-20120901 *)

end

module Make(Ord:Map.OrderedType): S with type key = Ord.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
