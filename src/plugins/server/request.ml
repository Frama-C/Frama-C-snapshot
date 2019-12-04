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

module Senv = Server_parameters
module Jutil = Yojson.Basic.Util

(* -------------------------------------------------------------------------- *)
(* --- Request Registry                                                   --- *)
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
type 'a output = (module Output with type t = 'a)

(* -------------------------------------------------------------------------- *)
(* --- Sanity Checks                                                      --- *)
(* -------------------------------------------------------------------------- *)

let re_name = Str.regexp_case_fold "[a-zA-Z0-9._]+$"

let wpage = Senv.register_warn_category "inconsistent-page"
let wkind = Senv.register_warn_category "inconsistent-kind"

let check_name name =
  if not (Str.string_match re_name name 0) then
    Senv.warning ~wkey:Senv.wname
      "Request %S is not a dot-separated list of (camlCased) identifiers" name

let check_plugin plugin name =
  let p = String.lowercase_ascii plugin in
  let n = String.lowercase_ascii name in
  let k = String.length plugin in
  if not (String.length name > k &&
          String.sub n 0 k = p &&
          String.get n k = '.')
  then
    Senv.warning ~wkey:wpage
      "Request '%s' shall be named « %s.* »"
      name (String.capitalize_ascii plugin)

let check_page page name =
  match Doc.chapter page with
  | `Kernel -> check_plugin "kernel" name
  | `Plugin plugin -> check_plugin plugin name
  | `Protocol ->
    Senv.warning ~wkey:wkind
      "Request '%s' shall not be published in protocol pages" name

(* -------------------------------------------------------------------------- *)
(* --- Multiple Fields Requests                                           --- *)
(* -------------------------------------------------------------------------- *)

module Fmap = Map.Make(String)

type rq = {
  mutable param : json Fmap.t ;
  mutable result : json Fmap.t ;
}

let fmap_of_json r js =
  List.fold_left
    (fun r (fd,js) -> Fmap.add fd js r)
    r (Jutil.to_assoc js)

let fmap_to_json r =
  `Assoc (Fmap.fold (fun fd js r -> (fd,js)::r) r [])

type 'a param = rq -> 'a
type 'a result = rq -> 'a -> unit

(* -------------------------------------------------------------------------- *)
(* --- Input/Output Request Processing                                    --- *)
(* -------------------------------------------------------------------------- *)

type _ rq_input =
  | Pnone
  | Pdata : 'a input -> 'a rq_input
  | Pfields : Syntax.field list -> unit rq_input

type _ rq_output =
  | Rnone
  | Rdata : 'a output -> 'a rq_output
  | Rfields : Syntax.field list -> unit rq_output

(* json input syntax *)
let sy_input (type a) (input : a rq_input) : Syntax.t =
  match input with
  | Pnone -> assert false
  | Pdata d -> let module D = (val d) in D.syntax
  | Pfields _ -> Syntax.record []

(* json output syntax *)
let sy_output (type b) (output : b rq_output) : Syntax.t =
  match output with
  | Rnone -> assert false
  | Rdata d -> let module D = (val d) in D.syntax
  | Rfields _ -> Syntax.record []

(* json input documentation *)
let doc_input (type a) (input : a rq_input) =
  match input with
  | Pnone -> assert false
  | Pdata _ -> []
  | Pfields fs -> [Syntax.fields ~title:"Input" (List.rev fs)]

(* json output syntax *)
let doc_output (type b) (output : b rq_output) =
  match output with
  | Rnone -> assert false
  | Rdata _ -> []
  | Rfields fs -> [Syntax.fields ~title:"Output" (List.rev fs)]

(* -------------------------------------------------------------------------- *)
(* --- Multi-Parameters Requests                                          --- *)
(* -------------------------------------------------------------------------- *)

type ('a,'b) signature = {
  page : Doc.page ;
  kind : kind ;
  name : string ;
  descr : Markdown.text ;
  details : Markdown.block ;
  mutable defined : bool ;
  mutable defaults : json Fmap.t ;
  mutable required : string list ;
  mutable input : 'a rq_input ;
  mutable output : 'b rq_output ;
}

let failure_missing fmap name =
  Data.failure ~json:(fmap_to_json fmap) "Missing parameter '%s'" name

let check_required fmap fd =
  if not (Fmap.mem fd fmap) then failure_missing fmap fd

(* -------------------------------------------------------------------------- *)
(* --- Named Input Parameters Definitions                                 --- *)
(* -------------------------------------------------------------------------- *)

(* current input fields *)
let fds_input s : Syntax.field list =
  if s.defined then Senv.failure "Request '%s' has been finalized." s.name ;
  match s.input with
  | Pdata _ ->
    Senv.fatal "Can not define named parameters for request '%s'" s.name
  | Pnone -> []
  | Pfields fds -> fds

let param (type a b) (s : (unit,b) signature) ~name ~descr
    ?default (input : a input) : a param =
  let module D = (val input) in
  let syntax = if default = None then D.syntax else Syntax.option D.syntax in
  let fd = Syntax.{ name ; syntax ; descr } in
  s.input <- Pfields (fd :: fds_input s) ;
  fun rq ->
    try D.of_json (Fmap.find name rq.param)
    with Not_found ->
    match default with
    | None -> failure_missing rq.param name
    | Some v -> v

let param_opt (type a b) (s : (unit,b) signature) ~name ~descr
    (input : a input) : a option param =
  let module D = (val input) in
  let fd = Syntax.{ name ; syntax = Syntax.option D.syntax ; descr } in
  s.input <- Pfields (fd :: fds_input s) ;
  fun rq ->
    try Some(D.of_json (Fmap.find name rq.param))
    with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Named Output Parameters Definitions                                --- *)
(* -------------------------------------------------------------------------- *)

(* current output fields *)
let fds_output s : Syntax.field list =
  if s.defined then Senv.failure "Request '%s' has been finalized." s.name ;
  match s.output with
  | Rdata _ -> Senv.fatal "Can not define named results request '%s'" s.name
  | Rnone -> []
  | Rfields fds -> fds

let result (type a b) (s : (a,unit) signature) ~name ~descr
    ?default (output : b output) : b result =
  let module D = (val output) in
  let fd = Syntax.{ name ; syntax = D.syntax ; descr } in
  s.output <- Rfields (fd :: fds_output s) ;
  begin
    match default with
    | None -> s.required <- name :: s.required
    | Some v -> s.defaults <- Fmap.add name (D.to_json v) s.defaults
  end ;
  fun rq v -> rq.result <- Fmap.add name (D.to_json v) rq.result

let result_opt (type a b) (s : (a,unit) signature) ~name ~descr
    (output : b output) : b option result =
  let module D = (val output) in
  let fd = Syntax.{ name ; syntax = option D.syntax ; descr } in
  s.output <- Rfields (fd :: fds_output s) ;
  fun rq opt ->
    match opt with None -> () | Some v ->
      rq.result <- Fmap.add name (D.to_json v) rq.result

(* -------------------------------------------------------------------------- *)
(* --- Opened Signature Definition                                        --- *)
(* -------------------------------------------------------------------------- *)

let signature
    ~page ~kind ~name ~descr ?(details=[]) ?input ?output () =
  check_name name ;
  check_page page name ;
  let input = match input with None -> Pnone | Some d -> Pdata d in
  let output = match output with None -> Rnone | Some d -> Rdata d in
  {
    page ; kind ; name ; descr ; details ;
    defaults = Fmap.empty ; required = [] ;
    input ; output ; defined = false ;
  }

(* -------------------------------------------------------------------------- *)
(* --- Opened Signature Process                                           --- *)
(* -------------------------------------------------------------------------- *)

(* json input processing *)
let mk_input (type a) name defaults (input : a rq_input) : (rq -> json -> a) =
  match input with
  | Pnone -> Senv.fatal "No input defined for request '%s'" name
  | Pdata d ->
    let module D = (val d) in
    begin fun rq js ->
      rq.result <- defaults ;
      try D.of_json js
      with Jutil.Type_error (msg, js) -> Data.failure_from_type_error msg js
    end
  | Pfields _ ->
    begin fun rq js ->
      try rq.param <- fmap_of_json rq.param js
      with Jutil.Type_error (msg, js) -> Data.failure_from_type_error msg js
    end

(* json output processing *)
let mk_output (type b) name required (output : b rq_output) : (rq -> b -> json) =
  match output with
  | Rnone -> Senv.fatal "No output defined for request '%s'" name
  | Rdata d ->
    let module D = (val d) in (fun _rq v -> D.to_json v)
  | Rfields _ ->
    (fun rq () ->
       List.iter (check_required rq.result) required ;
       fmap_to_json rq.result)

let register_sig (type a b) (s : (a,b) signature) (process : rq -> a -> b) =
  let open Markdown in
  if s.defined then
    Senv.fatal "Request '%s' is defined twice" s.name ;
  let input = mk_input s.name s.defaults s.input in
  let output = mk_output s.name s.required s.output in
  let processor js =
    let rq = { param = Fmap.empty ; result = Fmap.empty } in
    js |> input rq |> process rq |> output rq
  in
  let skind = Main.string_of_kind s.kind in
  let title =  Printf.sprintf "`%s` %s" skind s.name in
  let header = [ plain "Input", Center; plain "Output", Center] in
  let content =
    [[ Syntax.text @@ sy_input s.input ;
       Syntax.text @@ sy_output s.output ]]
  in
  let synopsis = Table { caption=None ; header; content } in
  let description =
    [ Block [Text s.descr ] ; synopsis ; Block s.details] @
    doc_input s.input @
    doc_output s.output
  in
  let _ = Doc.publish ~page:s.page ~name:s.name ~title description [] in
  Main.register s.kind s.name processor ;
  s.defined <- true

(* -------------------------------------------------------------------------- *)
(* --- Request Registration                                               --- *)
(* -------------------------------------------------------------------------- *)

let register ~page ~kind ~name ~descr ?details ~input ~output process =
  register_sig
    (signature ~page ~kind ~name ~descr ?details ~input ~output ())
    (fun _rq v -> process v)

(* -------------------------------------------------------------------------- *)
