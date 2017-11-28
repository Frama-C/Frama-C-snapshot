(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

    Functions are grouped according to the version in which the replacing
    feature did appear.
*)

(** {1 4.03.0} *)

(** In OCaml 4.03, many functions [f] from String have been deprecated
    in favor of [f_ascii], which operate only on the ASCII charset, while
    the deprecated [f] knew about iso-8859-1.
    We use the new names here, so that when support of 4.02.3 is dropped,
    client code will just have to erase [Transitioning.] to use directly
    the stdlib version
*)
module String: sig
  val uppercase_ascii: string -> string
  val capitalize_ascii: string -> string
  val uncapitalize_ascii: string -> string
  val lowercase_ascii: string -> string
end

(** See above documentation for [String] *)
module Char: sig
  val uppercase_ascii: char -> char
  val lowercase_ascii: char -> char
end

(** {1 Zarith 1.5} *)

(** Function [Q.to_float] was introduced in Zarith 1.5 *)
module Q: sig
  val to_float : Q.t -> float
end
