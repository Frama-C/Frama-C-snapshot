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

module Senv = Server_parameters

let check_plugin plugin name =
  let p = String.lowercase_ascii plugin in
  let n = String.lowercase_ascii name in
  let k = String.length plugin in
  if not (String.length name > k &&
          String.sub n 0 k = p &&
          String.get n k = '-')
  then
    Senv.warning ~wkey:Senv.wpage
      "Data %S shall be named « %s-* »"
      name plugin

let check_page page name =
  match Doc.chapter page with
  | `Kernel -> ()
  | `Plugin plugin -> check_plugin plugin name
  | `Protocol -> check_plugin "server" name

let re_name = Str.regexp "[a-z0-9-]+$"

let check_name name =
  if not (Str.string_match re_name name 0) then
    Senv.warning ~wkey:Senv.wname
      "Data name %S is not a dash-separated list of lowercase identifiers" name

(* -------------------------------------------------------------------------- *)

type t = { atomic:bool ; text:Markdown.text }

let atom md = { atomic=true ; text=md }
let flow md = { atomic=false ; text=md }
let text { text } = text

let protect a =
  if a.atomic then a.text else Markdown.(plain "(" @ a.text @ plain ")")

let publish ~page ~name ~descr ~synopsis ?(details = []) () =
  check_name name ;
  check_page page name ;
  let id = Printf.sprintf "data-%s" name in
  let title = Printf.sprintf "`DATA` %s" name in
  let dref = Doc.href page id in
  let dlink = Markdown.href ~text:(Markdown.emph name) dref in
  let syntax = Markdown.(glue [
      plain "<" ; dlink ; plain ">" ; plain ":=" ; synopsis.text ]) in
  let content = Markdown.(Block ( text descr @ text syntax ) :: details) in
  let _href = Doc.publish ~page ~name:id ~title ~index:[name] content [] in
  atom dlink

let unit = atom @@ Markdown.plain "-"
let any = atom @@ Markdown.emph "any"
let int = atom @@ Markdown.emph "int"
let ident = atom @@ Markdown.emph "ident"
let string = atom @@ Markdown.emph "string"
let number = atom @@ Markdown.emph "number"
let boolean = atom @@ Markdown.emph "boolean"

let escaped name =
  Markdown.code (Printf.sprintf "'%s'" @@ String.escaped name)

let tag name = atom @@ escaped name
let array a = atom @@ Markdown.(code "[" @ protect a @ code  ", … ]")

let tuple ts =
  atom @@
  Markdown.(
    code "[" @
    glue ~sep:(code ",") (List.map protect ts) @
    code "]"
  )

let union ts = flow @@ Markdown.(glue ~sep:(plain "|") (List.map protect ts))

let option t = atom @@ Markdown.(protect t @ code "?")

let field (a,t) = Markdown.( escaped a @ code ":" @ t.text )

let record fds =
  let fields =
    if fds = [] then Markdown.plain "…" else
      Markdown.(glue ~sep:(code ";") (List.map field fds))
  in atom @@ Markdown.(code "{" @ fields @ code "}")

type field = {
  name : string ;
  syntax : t ;
  descr : Markdown.text ;
}

let fields ~title (fds : field list) =
  let open Markdown in
  let header = [
    plain title, Left;
    plain "Format", Center;
    plain "Description", Left
  ] in
  let column f = [ code f.name ; f.syntax.text ; f.descr ] in
  Markdown.Table {
    caption = None ; header ; content = List.map column fds ;
  }

(* -------------------------------------------------------------------------- *)
