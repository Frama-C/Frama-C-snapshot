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

(* $Id: kernel_computation.ml,v 1.6 2008/04/01 09:25:21 uid568 Exp $ *)

module StmtSetRef = 
  Computation.Make_SetRef(Cilutil.StmtSet)(Kernel_datatype.Stmt)

module IntHashtbl = Computation.Make_Hashtbl(Inthash)
module InstrHashtbl = Computation.Make_Hashtbl(Cil.InstrHashtbl)
module StmtHashtbl = Computation.Make_Hashtbl(Cilutil.StmtHashtbl)
module VarinfoHashtbl = Computation.Make_Hashtbl(Cilutil.VarinfoHashtbl)

module CodeAnnotationHashtbl = 
  Computation.Hashtbl
    (struct
       include Kernel_datatype.CodeAnnotation
       open Cil_types
       let hash a = Hashtbl.hash a.annot_id
       let equal a b = a.annot_id = b.annot_id
     end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
