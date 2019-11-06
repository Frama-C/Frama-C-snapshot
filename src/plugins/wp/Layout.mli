(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Region Utilities *)

open Pretty_utils
open Cil_types

module type Data =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : t formatter
end

(* -------------------------------------------------------------------------- *)
(** {2 L-Path} *)
(* -------------------------------------------------------------------------- *)

type offset =
  | Field of fieldinfo
  | Index of typ * int

type lvalue = (** Generalized l-values *)
  | Eval of exp
  | Tval of term
  | Assigned of stmt

module Offset :
sig
  include Data with type t = offset
  val index : typ -> offset
  val field : fieldinfo -> offset
  val typeof : offset -> typ
  val typeof_chain : typ -> offset list -> typ
  val pp_chain : typ -> offset list formatter

  type cache
  val cache : unit -> cache
  val field_offset : cache -> fieldinfo -> int * int (* in bits *)
  val range : cache -> offset -> (int * int) * int (* in bits *)
  val sizeof : offset -> int (* in bits *)
end

module Lvalue : Data with type t = lvalue

(* -------------------------------------------------------------------------- *)
(** {2 Access} *)
(* -------------------------------------------------------------------------- *)

type alias = NotUsed | NotAliased | Aliased
type usage = Value | Deref | Array
type deref = usage * typ

module Alias :
sig
  val use : alias -> alias
  val merge : alias -> alias -> alias
  val alias : alias -> alias -> alias
  val is_aliased : alias -> bool
  val pretty : alias formatter
end

module Usage :
sig
  val pretty : usage formatter
  val merge : usage -> usage -> usage
  val is_shifted : usage -> bool
  val is_aliased : usage -> bool
end

module Deref : Data with type t = deref

(* -------------------------------------------------------------------------- *)
(** {2 R-Values} *)
(* -------------------------------------------------------------------------- *)

type 'a value =
  | Int of Ctypes.c_int
  | Float of Ctypes.c_float
  | Pointer of 'a

module Value :
sig

  val compare : ('a -> 'a -> int) -> 'a value -> 'a value -> int
  val equal : ('a -> 'a -> bool) -> 'a value -> 'a value -> bool
  val pretty : 'a formatter -> 'a value formatter

  val sizeof : 'a value -> int
  val pointed : 'a value -> 'a option

  val merge : ('a -> 'a -> 'a) -> 'a value -> 'a value -> 'a value option

end

module Matrix :
sig

  val gcd : int -> int -> int
  val pretty : int list formatter
  val sizeof : int -> int list -> int
  val merge : int list -> int list -> int list

end

(* -------------------------------------------------------------------------- *)
(** {2 Overlays} *)
(* -------------------------------------------------------------------------- *)

type dim = Raw of int | Dim of int * int list

type 'a range = private {
  ofs : int ; (* in bits, start from 0 *)
  len : int ;
  reg : 'a ;
  dim : dim ;
}

type 'a overlay = 'a range list

type 'a merger = raw:bool -> 'a -> 'a -> 'a

module Range :
sig

  val pretty : 'a formatter -> 'a range formatter
  val overlap : 'a formatter -> 'a merger -> 'a range -> 'a range -> 'a range
  val included : int -> int -> 'a range -> bool

end

module Overlay :
sig

  val pretty : ?title:(Format.formatter -> unit) ->
    'a formatter -> 'a overlay formatter
  val merge : 'a formatter ->
    'a merger -> 'a overlay -> 'a overlay -> 'a overlay

  val once : 'a -> 'a overlay -> bool

end

(* -------------------------------------------------------------------------- *)
(** {2 Compound Layout} *)
(* -------------------------------------------------------------------------- *)

type 'a layout = {
  sizeof : int ;
  layout : 'a overlay ;
}

module Compound :
sig

  val garbled : Offset.cache -> offset -> 'a -> 'a layout
  val reshape : eq:('a -> 'a -> bool) -> flat:bool -> pack:bool ->
    'a layout -> 'a layout

end

(* -------------------------------------------------------------------------- *)
(** {2 Clustering} *)
(* -------------------------------------------------------------------------- *)

type 'a cluster =
  | Empty
  | Garbled
  | Chunk of 'a value
  | Layout of 'a layout

module Cluster :
sig

  val pretty : 'a formatter -> 'a cluster formatter
  val deref : pointed:'a Lazy.t -> deref -> 'a cluster
  val shift : Offset.cache -> 'a formatter ->
    offset -> 'a -> inline:bool -> 'a cluster -> 'a layout
  val merge : 'a formatter -> 'a merger -> 'a cluster -> 'a cluster -> 'a cluster
  val is_empty : 'a cluster -> bool
  val is_garbled : 'a cluster -> bool
  val reshape : eq:('a -> 'a -> bool) -> flat:bool -> pack:bool ->
    'a cluster -> 'a cluster

end

(* -------------------------------------------------------------------------- *)
(** {2 Roots} *)
(* -------------------------------------------------------------------------- *)

type 'a from =
  | Fvar of varinfo
  | Ffield of 'a * int
  | Findex of 'a
  | Fderef of 'a
  | Farray of 'a

type root =
  | Rnone
  | Rfield of varinfo * int (* static offset *)
  | Rindex of varinfo (* any offset rooted at var *)
  | Rtop

module Root :
sig

  val pretty : root formatter
  val from : root:('a -> root) -> 'a from -> root
  val merge : root -> root -> root
  val indexed : root -> bool
  val framed : root -> bool

end

(* -------------------------------------------------------------------------- *)
(** {2 Chunks} *)
(* -------------------------------------------------------------------------- *)

type chunks = Qed.Intset.t

type 'a chunk =
  | Mref of 'a (** Constant pointers to region *)
  | Mmem of root * 'a value (** Aliased values *)
  | Mraw of root * 'a option (** Bits that may points-to *)
  | Mcomp of chunks * 'a overlay (** Aliased chunks & overlay *)

module Chunk :
sig
  val empty : chunks
  val singleton : int -> chunks
  val union : chunks -> chunks -> chunks
  val disjoint : chunks -> chunks -> bool
  val union_map : ('a -> chunks) -> 'a list -> chunks
  val mem : int -> chunks -> bool
  val pretty : int formatter -> chunks formatter
end

(* -------------------------------------------------------------------------- *)
(** {2 Options} *)
(* -------------------------------------------------------------------------- *)

(** Read-Write access *)
module RW :
sig
  val default : unit -> bool
  val merge : bool -> bool -> bool
end

(** Flatten arrays *)
module Flat :
sig
  val default : unit -> bool
  val merge : bool -> bool -> bool
end

(** Pack fields *)
module Pack :
sig
  val default : unit -> bool
  val merge : bool -> bool -> bool
end

(* -------------------------------------------------------------------------- *)
