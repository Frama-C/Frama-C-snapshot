(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

module Enabled: Parameter_sig.With_output
(** Activate metrics *)

module ByFunction: Parameter_sig.Bool
(** Activate metrics by function *)

module ValueCoverage: Parameter_sig.With_output
(** Give an estimation about value analysis code penetration.
    Only works on CIL AST. *)

module AstType: Parameter_sig.String
(** Set the ASTs on which the metrics should be computetd *)

module OutputFile: Parameter_sig.String
(** Pretty print metrics to the given file.
    The output format will be recognized through the extension.
    Supported extensions are:
    "html" or "htm" for HTML
    "txt" or "text" for text
*)

module SyntacticallyReachable: Parameter_sig.String_set
(** List of functions for which we compute the functions they may call *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
