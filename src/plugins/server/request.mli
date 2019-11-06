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
(** Request Registry *)
(* -------------------------------------------------------------------------- *)

type json = Data.json
type kind = [ `GET | `SET | `EXEC ]

module type Input =
sig
  type t
  val syntax : Syntax.t
  val of_json : json -> t
end

module type Output =
sig
  type t
  val syntax : Syntax.t
  val to_json : t -> json
end

type 'a input = (module Input with type t = 'a)
type 'b output = (module Output with type t = 'b)

(** {2 Simple Requests Registration} *)

(** Register a simple request of type [(a -> b)].

    Name, page and kind must be consistent with each others:
    - No publication on [`Protocol] pages
    - Kernel requests shall starts with ["Kernel.*"]
    - Plugin requests shall starts with ["<Plugin>.*"]
    - SET requests must contain ["set"] (case insensitive)
    - GET requests must contain ["get"] or ["print"] (case insensitive)
    - EXEC requests must contain ["exec"] or ["compute"] (case insensitive)

*)
val register :
  page:Doc.page ->
  kind:kind ->
  name:string ->
  descr:Markdown.text ->
  ?details:Markdown.block ->
  input:'a input ->
  output:'b output ->
  ('a -> 'b) -> unit

(** {2 Requests with Named Parameters}

    The API below allows for creating requests with
    named and optional parameters. Although such requests
    could be defined with simple registration and {i record} datatypes,
    the helpers below allow more flexibility and a better correspondance
    between optional parameters and OCaml option types.

    To register a request with named parameters and/or named results,
    you first create a {i signature}. Then you define named
    parameters and results, and finally you {i register} the processing
    function:

    {[
      (* ---- Exemple of Request Registration --- *)
      let () =
        let s = Request.signature ~page ~kind ~name ~descr () in
        let get_a = Request.param s ~name:"a" ~descr:"..." (module A) in
        let get_b = Request.param s ~name:"b" ~descr:"..." (module B) in
        let set_c = Request.result s ~name:"c" ~descr:"..." (module C) in
        let set_d = Request.result s ~name:"d" ~descr:"..." (module D) in
        Request.register_sig s
          (fun rq () ->
             let (c,d) = some_job (get_a rq) (get_b rq) in
             set_c rq c ; set_d rq d)
    ]}

*)

(** Under definition request signature. *)
type ('a,'b) signature

(** Create an opened request signature.
    Depending on whether [~input] and [~output] datatype are provided,
    you shall define named parameters and results before registering the
    request processing function. *)
val signature :
  page:Doc.page ->
  kind:kind ->
  name:string ->
  descr:Markdown.text ->
  ?details:Markdown.block ->
  ?input:'a input ->
  ?output:'b output ->
  unit -> ('a,'b) signature

(** Request JSON parameters. *)
type rq

(** Named input parameter. *)
type 'a param = rq -> 'a

(** Named output parameter. *)
type 'b result = rq -> 'b -> unit

(** Register the request JSON processing function.
    This call finalize the signature definition and shall be called
    once on the signature. *)
val register_sig : ('a,'b) signature -> (rq -> 'a -> 'b) -> unit

(** {2 Named Parameters and Results}

    The functions bellow must be called on a freshly created signature
    {i before} its final registration. The obtained getters and setters
    shall be only used within the registered process.

    The correspondance between input/output JSON syntax and OCaml values
    is summarized in the tables below.Abstract_domain

    For named input parameters:
    [

        API:                    Input JSON   OCaml Getter
        -----------------------------------------------------------------------
        Request.param            { f: a  }    'a (* might raise an exception *)
        Request.param ~default   { f: a? }    'a (* defined by default *)
        Request.param_opt        { f: a? }    'a option

    ]


    For named output parameters:
    [

        API:                    Input JSON   OCaml Setter
        ----------------------------------------------------------------------
        Request.result           { f: a  }    'a (* shall be set by process *)
        Request.result ~default  { f: a  }    'a (* defined by default *)
        Request.result_opt       { f: a? }    'a option

    ]

*)


(** Named input parameter. If a default value is provided,
    the JSON input field becomes optional. Otherwized, it is required. *)
val param : (unit,'b) signature ->
  name:string ->
  descr:Markdown.text ->
  ?default:'a ->
  'a input -> 'a param

(** Named optional input parameter. *)
val param_opt : (unit,'b) signature ->
  name:string ->
  descr:Markdown.text ->
  'a input -> 'a option param

(** Named output parameter. If a default value is provided,
    the JSON output field is initialized with it.
    Otherwized, it shall be set at each invocation of the request processing
    funciton. *)
val result : ('a,unit) signature ->
  name:string ->
  descr:Markdown.text ->
  ?default:'b ->
  'b output -> 'b result

(** Named optional output parameter. The initial value is set to [None]. *)
val result_opt : ('a,unit) signature ->
  name:string ->
  descr:Markdown.text ->
  'b output -> 'b option result

(* -------------------------------------------------------------------------- *)
