(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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



