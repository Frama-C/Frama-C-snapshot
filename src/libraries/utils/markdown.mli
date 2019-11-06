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

(** {2 Markdown Document}
    Structured representation of Markdown content. *)

(** Table columns alignment *)
type align = Left | Center | Right

(** Local refs and URLs *)
type href =
  | URL of string
  (** URL href is printed as it is. *)

  | Page of string
  (** URL relative to a common root.
      During pretty-printing, if given the path of the current
      document, the string will be modified accordingly. For instance,
      when writing to [foo/bar.md], [Page "foo/bla.md"] will be output as
      [(bla.md)].
  *)

  | Section of string * string
  (** URL of an anchor within a [Page], see above. *)

type inline =
  | Plain of string (** Printed as it is *)
  | Emph of string (** Printed as ["_……_"] *)
  | Bold of string (** Printed as ["**……**"] *)
  | Inline_code of string (** Printed as ["`……`"] *)
  | Link of text * href (** Hyperlink with text and URL *)
  | Image of string * string (** [Image(alt,path)] with alternative text and image file *)

and text = inline list (** Inline elements separated by spaces *)

type block_element =
  | Text of text (** Single paragraph of text. *)
  | Block_quote of element list
  | UL of block list
  | OL of block list
  | DL of (text * text) list (** definition list *)
  | EL of (string option * text) list (** example list *)
  | Code_block of string * string list

and block = block_element list

and table = {
  caption: text option;
  header: (text * align) list;
  content: text list list;
}

and element =
  | Comment of string (** markdown comment, printed <!-- like this --> *)
  | Block of block
  | Table of table
  | Raw of string list
  (** Each element of the list is printed as-is on its own line.
      A blank line separates the [Raw] node from the next one. *)
  | H1 of text * string option
  | H2 of text * string option
  | H3 of text * string option
  | H4 of text * string option
  | H5 of text * string option
  | H6 of text * string option

and elements = element list

type pandoc_markdown =
  { title: text;
    authors: text list;
    date: text;
    elements: elements
  }

(** {2 Formatting Utilities}

    Remark: [text] values are list of [inline] values, hence
    you may combined with the [(@)] operator or with the [glue ?sep] utility
    function (see below).
*)

(** Plain markdown *)
val plain: string -> text

(** Emph text *)
val emph: string -> text

(** Bold text *)
val bold: string -> text

(** Inline code *)
val code: string -> text

(** Image *)
val image: alt:string -> file:string -> text

(** Href link *)
val href: ?text:text -> href -> text

(** Local links *)
val link: ?text:text -> ?page:string -> ?name:string -> unit -> text

(** URL links *)
val url: ?text:text -> string -> text

(** Plain markdown content of the formatted string *)
val format: ('a, Format.formatter, unit, text) format4 -> 'a

(** {2 Blocks Utilities}

    Remark: [block] values are list of [block_element] values, hence
    you may combined with the [(@)] operator or with the [glue ?sep] utility
    function (see below).
*)

(** Text Block *)
val text : text -> block

(** Itemized list *)
val list : block list -> block

(** Enumerated list *)
val enum : block list -> block

(** Description list *)
val description : (text * text) list -> block

(** [codeblock lang "...."] returns a [Code_block] for [code],
    written in [lang] with the given formatted content.
    The code block content placed inside an englobing hv-box, trimed
    and finally splitted into lines. *)
val codeblock : ?lang:string -> ('a,Format.formatter,unit,block) format4 -> 'a

(** {2 Document Elements}

    Remark: [elements] values are list of [element] values, hence
    you may combined with the [(@)] operator or with the [glue ?sep] utility
    function (see below).
*)

(** Single Paragraph element *)
val par : text -> elements

(** Block element *)
val block : block -> elements

(** Get the content of a file as raw markdown.
    @raise Sys_error if there's no such file.
*)
val rawfile: string -> elements

(** {2 Document Structure} *)

(** Creates a document from a list of elements and optional metadatas.
    Defaults are:
    - title: empty
    - authors: empty list
    - date: current day, in ISO format
*)
val pandoc:
  ?title:text -> ?authors: text list -> ?date: text -> elements ->
  pandoc_markdown

(** Adds a [H1] header with the given [title] on top of the given elements.
    If name is not explicitly provided,
    the header will have as associated anchor [id title]
*)
val section: ?name:string -> title:string -> elements -> elements

(** [subsections header body] returns a list of [element]s where the [body]'s
    headers have been increased by one (i.e. [H1] becomes [H2]).
    [H5] stays at [H5], though.
*)
val subsections: elements -> elements list -> elements

(** {2 Other Utilities} *)

(** Glue fragments, typically used for combining [text], [block]
    and [elements].
    Default separator is empty. The function is tail-recursive. *)
val glue: ?sep:'a list -> 'a list list -> 'a list

(** Transforms a string into an anchor name, roughly following
    pandoc's conventions. This function is automatically used
    by pretty-printers and smart constructors to normalize section names
    and local links. *)
val label: string -> string

(** {2 Pretty-printers} *)

val pp_inline: ?page:string -> Format.formatter -> inline -> unit

val pp_text: ?page:string -> Format.formatter -> text -> unit

val pp_block_element: ?page:string -> Format.formatter -> block_element -> unit

val pp_block: ?page:string -> Format.formatter -> block -> unit

val pp_element: ?page:string -> Format.formatter -> element -> unit

val pp_elements: ?page:string -> Format.formatter -> elements -> unit

val pp_pandoc: ?page:string -> Format.formatter -> pandoc_markdown -> unit
