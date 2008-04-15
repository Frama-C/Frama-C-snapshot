(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: kernel_type.ml,v 1.9 2008/12/03 13:50:49 uid562 Exp $ *)

(** An extension of Type library for Frama-C usage *)

let () = Journal.List.register_printer Type.string

(* ****************************************************************************)
(** {2 Frama-C types} *)
(* ****************************************************************************)

open Cil_types

let lexing_pos_dummy = Lexing.dummy_pos

(* used to register varinfo type in the journal mechanism. Do not attempt
   to use it in real AST.
*)
let varinfo_dummy = {
  vorig_name = "dummy";
  vname = "dummy";
  vtype = TVoid [];
  vattr = [];
  vstorage = NoStorage;
  vglob = false;
  vdefined = false;
  vformal = false;
  vinline = false;
  vdecl = lexing_pos_dummy,lexing_pos_dummy;
  vid = 0;
  vaddrof = false;
  vreferenced = false;
  vdescr = None;
  vdescrpure = false;
  vghost = false;
  vlogic = false;
  vlogic_var_assoc = None
}

let big_int = Type.make "Big_int.big_int" Big_int.zero_big_int
let () =
  Journal.register_printer
    big_int
    (fun fmt bi -> Format.fprintf fmt "%s" (Big_int.string_of_big_int bi))

let varinfo = Type.make "Cil_types.varinfo" varinfo_dummy
let () =
  Journal.register_printer
    varinfo
    (fun fmt v -> Format.fprintf fmt "@[(Cil.varinfo_from_vid %d)@]" v.vid)

let kinstr = Type.make "Cil_types.kinstr" Kglobal
let lval = Type.make "Cil_types.lval" (Var varinfo_dummy, NoOffset)

let string_set = Type.make "Cilutil.StringSet.t" Cilutil.StringSet.empty
let () =
  Journal.register_printer
    string_set
    (fun fmt s ->
       Format.fprintf fmt
	 "@[%s@]"
	 (Cilutil.StringSet.fold
	    (fun elt acc ->
	       Format.sprintf
		 "@[(Cilutil.StringSet.add %S %s)@]"
		 elt
		 acc)
	    s
            "Cilutil.StringSet.empty"))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
