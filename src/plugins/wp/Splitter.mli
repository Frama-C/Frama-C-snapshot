(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

open Cil_types

type tag =
  | MARK of stmt
  | THEN of stmt
  | ELSE of stmt
  | CALL of stmt * kernel_function
  | CASE of stmt * int64 list
  | DEFAULT of stmt
  | ASSERT of identified_predicate * int * int (* part / Npart *)

val loc : tag -> location
val pretty : Format.formatter -> tag -> unit

val mark : stmt -> tag
val if_then : stmt -> tag
val if_else : stmt -> tag
val switch_cases : stmt -> int64 list -> tag
val switch_default : stmt -> tag
val cases : identified_predicate -> (tag * predicate named) list option
val call : stmt -> kernel_function -> tag

type 'a t

val empty : 'a t
val singleton : 'a -> 'a t
val group : tag -> ('a list -> 'a) -> 'a t -> 'a t

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val merge :
  left:('a -> 'c) ->
  both:('a -> 'b -> 'c) ->
  right:('b -> 'c) ->
  'a t -> 'b t -> 'c t

val merge_all : ('a list -> 'a) -> 'a t list -> 'a t

val length : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t
val iter : (tag list -> 'a -> unit) -> 'a t -> unit
val fold : (tag list -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val filter : ('a -> bool) -> 'a t -> 'a t

