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

module Pango = struct
  open Wutil_once

  let small_font =
    once (fun (f: GPango.font_description) ->
        let f = f#copy in
        let size = f#size - 2 in
        f#modify ~size (); f)

  let bold_font =
    once (fun (f: GPango.font_description) ->
        let f = f#copy in
        let weight = `BOLD in
        f#modify ~weight (); f)

  let modify_font phi (widget: #GObj.widget) =
    widget#misc#modify_font (phi widget#misc#pango_context#font_description)

  let set_small_font w = modify_font small_font w
  let set_bold_font w = modify_font bold_font w
end

let get_toolbar_index toolbar item = toolbar#get_item_index item#as_tool_item

let window = GWindow.window
