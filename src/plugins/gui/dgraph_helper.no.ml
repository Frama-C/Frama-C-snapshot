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

let window_msg_unavailable () =
  let buttons = GWindow.Buttons.ok in
  let message_type = `WARNING in
  let message =
    "Frama-C has not been compiled against a library with \
     working graph visualization. Property dependencies graph can't be shown."
  in
  let dialog =
    GWindow.message_dialog ~buttons ~show:true ~message_type ~message ()
  in
  let callback _ = dialog#destroy () in
  ignore (dialog#connect#response ~callback)

let graph_window ~parent:_ ~title:_ _ =
  window_msg_unavailable ()

let graph_window_through_dot ~parent:_ ~title:_ _ =
  window_msg_unavailable ()
