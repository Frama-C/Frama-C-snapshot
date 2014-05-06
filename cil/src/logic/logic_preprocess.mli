(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(** adds another pre-processing step in order to expand macros in
    annotations.
*)

(** [file suffix cpp file] takes the file to preprocess,
    and the pre-processing directive, and returns the name of the file
    containing the completely pre-processed source. suffix will be appended
    to the name of intermediate files generated for pre-processing annotations
    (gcc pre-processing differs between .c and .cxx files)

    @raises Sys_error if the file cannot be opened. 

    @modifies Oxygen-20120901: added suffix argument

*)

val file: string -> (string -> string -> string) -> string -> string
