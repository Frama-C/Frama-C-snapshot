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

include Plugin.S

(** Value of [-mdr-out]. *)
module Output: Parameter_sig.String

(** Value of [-mdr-gen]. *)
module Generate: Parameter_sig.String

(** Value of [-mdr-remarks]. *)
module Remarks: Parameter_sig.String

(** Value of [-mdr-flamegraph]. *)
module FlameGraph: Parameter_sig.String

(** Value of [-mdr-authors]. *)
module Authors: Parameter_sig.String_list

(** Value of [-mdr-title]. *)
module Title: Parameter_sig.String

(** Value of [-mdr-date]. *)
module Date: Parameter_sig.String

(** Value of [-mdr-stubs]. *)
module Stubs: Parameter_sig.String_list
