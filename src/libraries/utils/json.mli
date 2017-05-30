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

(** Json Library 

    Remarks:
     - UTF-8 escaping is not supported;
     - Parsers are less {i strict} than Json format;
     - Printers are supposed to {i strictly} conforms to Json format;
     - [Number] can be used to encode non OCaml-primitive numbers,
       for instance Zarith.
*)

(** Json Objects *)
type t =
  | Null
  | True | False
  | String of string
  | Number of string
  | Int of int
  | Float of float
  | Array of t list
  | Assoc of (string * t) list

val equal : t -> t -> bool (** Pervasives *)
val compare : t -> t -> int (** Pervasives *)
val pp : Format.formatter -> t -> unit

exception Error of string * int * string
(** file, line, message *)

(** {2 Constructors} *)

val of_bool : bool -> t
val of_int : int -> t
val of_string : string -> t
val of_float : float -> t
val of_list : t list -> t
val of_array : t array -> t
val of_fields : (string * t) list -> t

(** {2 Parsers} Parsing raise [Error] in case of error. *)

val load_lexbuf : Lexing.lexbuf -> t (** Consumes the entire buffer. *)
val load_channel : in_channel -> t (** Parses the stream until EOF. *)
val load_string : string -> t (** Parses the Json in the string. *)
val load_file : string -> t (** May also raise system exception. *)

(** {2 Printers} Printers use formatting unless [~pretty:false]. *)

val save_string : ?pretty:bool -> t -> string
val save_buffer : ?pretty:bool -> Buffer.t -> t -> unit
val save_channel : ?pretty:bool -> out_channel -> t -> unit
val save_file : ?pretty:bool -> string -> t -> unit

(** {2 Accessors}
    Accessors raise exception [Invalid_argument] in case of wrong
    format. *)

val bool : t -> bool
(** Extract [True] and [False] only. 
    @raise Invalid_argument when the conversion fails. *)

val int : t -> int
(** Convert [Null], [Int], [Float], [Number] and [String] to an [int]. 
    Floats are truncated with [int_of_float] and [Null] to 0.
    @raise Invalid_argument when the conversion fails. *)

val string : t -> string
(** Convert [Null], [Int], [Float], [Number] and [String] to a [string].
    Floats are truncated with [string_of_float] and [Null] to [""].
    @raise Invalid_argument when the conversion fails. *)

val float : t -> float
(** Convert [Null], [Int], [Float], [Number] and [String] to [float] and [Null] to [0.0].
    @raise Invalid_argument when the conversion fails. *)

val array : t -> t array
(** Extract the array of an [Array] object. 
    [Null] is considered an empty array.
    @raise Invalid_argument if the object is not an array. *)

val list : t -> t list
(** Extract the list of an [Array] object. 
    [Null] is considered an empty list.
    @raise Invalid_argument if the object is not a list. *)
    
val fold : (string -> t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over all fields of the object. 
    [Null] is considered an empty object.
    Typical usage is [fold M.add t M.empty] where [M=Map.Make(String)]. 
    @raise Invalid_argument if the object is not an [Assoc] or [Null] object. *)

val field : string -> t -> t
(** Lookup a field in an object.
    [Null] is considered an empty object.
    @raise Not_found if the field is absent from the object.
    @raise Invalid_argument if the object is not an [Assoc] or [Null] object. *)
