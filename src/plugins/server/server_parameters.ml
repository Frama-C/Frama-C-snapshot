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
(* --- Server Plugin & Options                                            --- *)
(* -------------------------------------------------------------------------- *)

module P = Plugin.Register
    (struct
      let name = "Server"
      let shortname = "server"
      let help = "Frama-C Request Server"
    end)

include P

module Idle = P.Int
    (struct
      let option_name = "-server-idle"
      let arg_name = "ms"
      let default = 10
      let help = "Waiting time (in milliseconds) when idle"
    end)

module Rate = P.Int
    (struct
      let option_name = "-server-rate"
      let arg_name = "n"
      let default = 100
      let help = "Number of analysis steps between server communications"
    end)

module Doc = P.String
    (struct
      let option_name = "-server-doc"
      let arg_name = "dir"
      let default = ""
      let help = "Output a markdown documentation of the server in <dir>"
    end)

module Log = P.False
    (struct
      let option_name = "-server-logs"
      let help = "Start (or stop) monitoring logs"
    end)

let wpage = register_warn_category "inconsistent-page"
let wkind = register_warn_category "inconsistent-kind"
let wname = register_warn_category "invalid-name"

(* -------------------------------------------------------------------------- *)
