(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* -------------------------------------------------------------------------- *)
(** Data Encoding *)
(* -------------------------------------------------------------------------- *)

type json = Json.t

val pretty : Format.formatter -> json -> unit

module type S =
sig
  type t
  val syntax : Syntax.t
  val of_json : json -> t
  val to_json : t -> json
end


(** Datatype registration.

    Name and page must be consistent with each other:
    - The name must be lowercase, dash-separated list of identifiers
    - Protocol data must start with ["<server>-*"]
    - Plugin data must start with ["<plugin>-*"]
*)
module type Info =
sig
  val page : Doc.page
  val name : string
  val descr : Markdown.text
end

type 'a data = (module S with type t = 'a)

(* -------------------------------------------------------------------------- *)
(** {2 Collections} *)
(* -------------------------------------------------------------------------- *)

module type S_collection =
sig
  include S
  module Joption : S with type t = t option
  module Jlist : S with type t = t list
  module Jarray : S with type t = t array
end

module Collection(A : S) : S_collection with type t = A.t

(* -------------------------------------------------------------------------- *)
(** {2 Constructors} *)
(* -------------------------------------------------------------------------- *)

module Joption(A : S) : S with type t = A.t option
module Jpair(A : S)(B : S) : S with type t = A.t * B.t
module Jtriple(A : S)(B : S)(C : S) : S with type t = A.t * B.t * C.t
module Jlist(A : S) : S with type t = A.t list
module Jarray(A : S) : S with type t = A.t array

(* -------------------------------------------------------------------------- *)
(** {2 Atomic Data} *)
(* -------------------------------------------------------------------------- *)

module Junit : S with type t = unit
module Jany : S with type t = json
module Jbool : S_collection with type t = bool
module Jint : S_collection with type t = int
module Jfloat : S_collection with type t = float
module Jstring : S_collection with type t = string
module Jident : S_collection with type t = string (** Syntax is {i ident}. *)
module Jtext : S with type t = json (** Rich text encoding, see [Jbuffer] *)

(* -------------------------------------------------------------------------- *)
(** {2 Records} *)
(* -------------------------------------------------------------------------- *)

type 'a record (** Records of type 'a *)
type 'a signature  (** Opened signature for record of type ['a] *)
type ('a,'b) field (** Field of type ['b] for a record of type ['a] *)

(** Record factory.

    You shall start by declaring a (ghost) type [r] and call
    [Record.signature] to create a signature of type [r].
    Then, populate the record with [Record.field] or [Record.option].
    Finally, you shall call [Record.publish] to obtain a new data module
    of type [Record with type r = r], which gives you a [Data] with an opaque
    type [t = r record] with fields of type [(r,a) field].

    {[
      (* ---- Exemple of Record Data --- *)
      type r
      let s = Record.signature ~page ~kind ~name ~descr () in
      let fd_a = Record.field s ~name:"a" ~descr:"..." (module A) in
      let fd_b = Record.field s ~name:"b" ~descr:"..." (module B) in

      module M = (val (Record.publish s) : Record with type r = r)

      let make a b = M.default |> M.set fd_a a |> M.set fd_b b
    ]}
*)
module Record :
sig

  (** Data with [type t = r record].
      Also contains getters and setters for fields. *)
  module type S =
  sig
    type r
    include S with type t = r record
    val default : t
    val has : (r,'a) field -> t -> bool
    val get : (r,'a) field -> t -> 'a
    val set : (r,'a) field -> 'a -> t -> t
  end

  (** Create a new, opened record type *)
  val signature : page:Doc.page -> name:string -> descr:Markdown.text ->
    unit -> 'a signature

  (** Adds a field to an opened record *)
  val field : 'r signature ->
    name:string -> descr:Markdown.text -> ?default:'a -> 'a data ->
    ('r,'a) field

  (** Adds a optional field to an opened record *)
  val option : 'r signature ->
    name:string -> descr:Markdown.text -> 'a data ->
    ('r,'a option) field

  (** Publish and close an opened record *)
  val publish : 'a signature -> (module S with type r = 'a)

end

(* -------------------------------------------------------------------------- *)
(** {2 Indexed Values} *)
(* -------------------------------------------------------------------------- *)

(** Simplified [Map.S] *)
module type Map =
sig
  type 'a t
  type key
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
end

module type Index =
sig
  include S_collection
  val get : t -> int
  val find : int -> t (** @raise Not_found if not registered *)
  val clear : unit -> unit
  (** Clear index tables. Use with extreme care. *)
end

(** Builds an indexer that {i does not} depend on current project. *)
module Static(M : Map)(I : Info) : Index with type t = M.key

(** Builds a {i projectified} index. *)
module Index(M : Map)(I : Info) : Index with type t = M.key

(* -------------------------------------------------------------------------- *)
(** {2 Identified Types} *)
(* -------------------------------------------------------------------------- *)

module type IdentifiedType =
sig
  type t
  val id : t -> int
  include Info
end

(** Builds a {i projectified} index on types with {i unique} identifiers *)
module Identified(A : IdentifiedType) : Index with type t = A.t

(* -------------------------------------------------------------------------- *)
(** {2 Dictionary} *)
(* -------------------------------------------------------------------------- *)

module type Enum =
sig
  type t
  val values : (t * string * Markdown.text) list
  include Info
end

module Dictionary(E : Enum) : S_collection with type t = E.t

(* -------------------------------------------------------------------------- *)
(** {2 Error handling} *)
(* -------------------------------------------------------------------------- *)

(** Exception thrown during the decoding of a request's inputs *)
exception InputError of string

val failure : ?json:json -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** @raise InputError with provided message *)

val failure_from_type_error : string -> json -> 'a
(** @raise InputError from Yojson.Basic.Util.Type_error arguments *)

(* -------------------------------------------------------------------------- *)
