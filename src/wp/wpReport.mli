(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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


type fcstat

val fcstat : unit -> fcstat

val export : fcstat -> string -> unit

  (** Export Statistics.

      Patterns for formatting:
      - ["%{cmd:arg}"] or "%cmd:arg"
      - ["%{cmd}"] or ["%cmd"]

      Patterns in [fct]:
      - ["%kf"] or ["%kf:name"] the name of the function.
      - ["%kf:<s>"] the stats in format [<s>] for the function.
      - ["%<p>:<s>"] the stats in format [<s>] for prover [<p>].
      
      Patterns in [main]:
      - "%<s>" the global statistics with format [<s>].

      Prover strings are ["wp"], ["ergo"], ["coq"] , ["z3"] and ["simplify"].
      Format strings are "100" (percents of valid upon total, default), 
        ["total"], ["valid"] and ["failed"] 
        for respective number of verification conditions.
      Zero is printed as [zero]. Percentages are printed in decimal ["dd.d"].

  *)

      
      
