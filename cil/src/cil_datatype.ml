(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

(* $Id: cil_datatype.ml,v 1.3 2008/10/28 16:16:11 uid570 Exp $ *)

open Cil_types
open Cilutil

module IntHashtbl = Datatype.Make_Hashtbl(Inthash)
module InstrHashtbl = Datatype.Make_Hashtbl(InstrHashtbl)
module StmtHashtbl = Datatype.Make_Hashtbl(StmtHashtbl)
module VarinfoHashtbl = Datatype.Make_Hashtbl(VarinfoHashtbl)

module Stmt = struct
  include Project.Datatype.Imperative
    (struct type t = stmt let copy _ = assert false let name = "stmt" end)
  let hash = StmtComparable.hash
  let equal = StmtComparable.equal
  let compare = StmtComparable.compare
  let () = register_comparable ~hash ~equal ~compare ()
end

module StmtSet = Datatype.Make_Set(Cilutil.StmtSet)(Stmt)
module StmtSetRef = Datatype.Make_SetRef(Cilutil.StmtSet)(Stmt)
module StmtList = Datatype.List(Stmt)

module Varinfo = struct
  include Project.Datatype.Imperative
    (struct 
       type t = varinfo 
       let copy _ = assert false 
       let name = "varinfo" 
     end)
  let hash = VarinfoComparable.hash
  let equal = VarinfoComparable.equal
  let compare = VarinfoComparable.compare
  let () = register_comparable ~hash ~equal ~compare ()
end

module Location = struct
  include Project.Datatype.Imperative
    (struct
       type t = location
       let copy (l1, l2) = 
	 { l1 with Lexing.pos_fname = String.copy l1.Lexing.pos_fname },
	 { l2 with Lexing.pos_fname = String.copy l2.Lexing.pos_fname }
       let name = "location"
     end)
  let hash (b, _e) = Hashtbl.hash (b.Lexing.pos_fname, b.Lexing.pos_lnum)
  let () = register_comparable ~hash ()
end

module File = 
  Project.Datatype.Imperative
    (struct type t = file let copy _ = assert false let name = "file" end)

(* [Cabs.file] is mutable but sharing between project is not a problem here. *)
module UntypedFile = 
  Project.Datatype.Persistent
    (struct type t = Cabs.file let name = "Cabs.file" end)

module UntypedFiles = Datatype.List(UntypedFile)

module InitInfo =
  Project.Datatype.Imperative
    (struct
       type t = initinfo
       let copy _ = assert false (* TODO: deep copy *)
       let name = "initinfo"
     end)

(** {3 Annotations} *)

module Code_Annotation = struct
  include Project.Datatype.Imperative
    (struct
       type t = code_annotation
       let copy _ = assert false (* TODO *)
       let name = "code_annotation"
     end)
  let hash x = x.annot_id
  let equal x y = x.annot_id = y.annot_id
  let compare x y = Pervasives.compare x.annot_id y.annot_id
  let () = register_comparable ~hash ~equal ~compare ()
end

module Logic_Info = 
  Project.Datatype.Imperative
    (struct
       type t = logic_info
       let copy c = { c with l_name = c.l_name}
       let name = "logic_info"
     end)

module Logic_Type_Info = 
  Project.Datatype.Imperative
    (struct
       type t = logic_type_info
       let copy c = {nb_params = c.nb_params}
       let name = "logic_type_info"
     end)

module Logic_Ctor_Info = 
  Project.Datatype.Imperative
    (struct
       type t = logic_ctor_info
       let copy c = { c with ctor_name = c.ctor_name}
       let name = "logic_ctor_info"
     end)

(*
module Predicate_Info =
  Project.Datatype.Imperative
    (struct 
       type t = Cil_types.predicate_info 
       let copy c = { c with p_name = c.p_name } 
       let name = "predicate_info"
     end)
*)

module Lval = struct
  include Project.Datatype.Imperative
    (struct 
       type t = lval 
       let copy _ = assert false (* todo *) 
       let name = "lval"
     end)
  let () = register_comparable ~compare:LvalComparable.compare ()
end

module Kinstr = struct
  include Project.Datatype.Imperative
    (struct 
       type t = kinstr 
       let copy _ = assert false (* todo *) 
       let name = "kinstr"
     end)
  let hash = function
    | Kglobal -> -1
    | Kstmt s -> s.sid
  let compare x y = match x,y with
    | Kglobal, Kglobal -> 0
    | Kglobal, Kstmt _ -> -1
    | Kstmt _, Kglobal -> 1
    | Kstmt s1, Kstmt s2 -> StmtComparable.compare s1 s2
  let () = register_comparable ~hash ~compare ()
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
