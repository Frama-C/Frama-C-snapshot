(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** This file contains functions that uses features that are deprecated in
    current OCaml version, but whose replacing feature is not available in
    the oldest OCaml version officially supported by Frama-C. Be sure to
    update it when support for a given version is dropped.

    Functions are grouped according to the module of the stdlib they
    emulate. The mentioned OCaml version indicate when the function was
    introduced in the stdlib (i.e. when Frama-C requires a version higher
    than that, it can safely be removed from Transitioning).
*)

(** {1 OCaml} *)

(** In OCaml 4.03, many functions [f] from String have been deprecated
    in favor of [f_ascii], which operate only on the ASCII charset, while
    the deprecated [f] knew about iso-8859-1.
    We use the new names here, so that when support of 4.02.3 is dropped,
    client code will just have to erase [Transitioning.] to use directly
    the stdlib version
*)
module String: sig
  val uppercase_ascii: string -> string (** 4.03 *)
  val capitalize_ascii: string -> string (** 4.03 *)
  val uncapitalize_ascii: string -> string (** 4.03 *)
  val lowercase_ascii: string -> string (** 4.03 *)
  val split_on_char: char -> string -> string list (** 4.04 *)
end

(** See above documentation for [String] *)
module Char: sig
  val uppercase_ascii: char -> char (** 4.03 *)
  val lowercase_ascii: char -> char (** 4.03 *)
end

module Stack: sig
  val fold: ('a -> 'b -> 'a) -> 'a -> 'b Stack.t -> 'a (** 4.03 *)
end

module List: sig
  val nth_opt: 'a list -> int -> 'a option (** 4.05 *)
  val find_opt: ('a -> bool) -> 'a list -> 'a option (** 4.05 *)
  val assoc_opt: 'a -> ('a * 'b) list -> 'b option (** 4.05 *)
  val assq_opt: 'a -> ('a * 'b) list -> 'b option (** 4.05 *)
end

(** {1 Zarith} *)

(** Function [Q.to_float] was introduced in Zarith 1.5 *)
module Q: sig
  val to_float : Q.t -> float
end
