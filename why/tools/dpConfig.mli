(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)




type prover_id = 
    Simplify | Harvey | Cvcl | Zenon | Rvsat | Yices | Ergo | ErgoSelect
  | Cvc3 | Graph | Z3 | Coq

type prover_data =
  {
    name : string;
    is_interactive : bool;
    mutable version: string;
    version_switch : string;
    version_regexp : string;
    mutable command : string;
    command_switches : string;
    valid_regexp : string;
    mutable valid_cregexp : Str.regexp option;
    undecided_regexp : string;
    mutable undecided_cregexp : Str.regexp option;
  }
    

val alt_ergo : prover_data

val simplify : prover_data

val z3 : prover_data

val yices : prover_data

val cvc3 : prover_data

val coq : prover_data

val prover_list : (prover_id * (prover_data * string list)) list

val load_rc_file : unit -> unit

val save_rc_file : unit -> unit
