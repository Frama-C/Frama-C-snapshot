(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
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
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

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

module Block =
  Project.Datatype.Persistent (struct type t = block let name = "block" end)

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
module VarinfoSet = Datatype.Make_Set(
  struct
    include Cilutil.VarinfoSet
    let descr = Unmarshal.t_set_unchangedcompares Varinfo.descr
  end)
  (Varinfo)
module VarinfoList = Datatype.List(Varinfo)

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

module Initinfo =
  Project.Datatype.Imperative
    (struct
       type t = initinfo
       let copy _ = assert false (* TODO: deep copy *)
       let name = "initinfo"
     end)

module InitInfo = Initinfo

module Enuminfo =
  Project.Datatype.Imperative
    (struct
       type t = enuminfo
       let name = "enuminfo"
       let copy _ = assert false
     end)
module EnuminfoSet = Datatype.Make_Set(
  struct include Cilutil.EnuminfoSet
         let descr = Unmarshal.t_set_unchangedcompares Enuminfo.descr
  end)
  (Enuminfo)

module Typeinfo =
  Project.Datatype.Imperative
    (struct
       type t = typeinfo
       let name = "typeinfo"
       let copy _ = assert false
     end)
module TypeinfoSet = Datatype.Make_Set(
  struct include Cilutil.TypeinfoSet
         let descr = Unmarshal.t_set_unchangedcompares Typeinfo.descr
  end)
  (Typeinfo)

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

module Logic_Var =
  Project.Datatype.Imperative
    (struct
       type t = logic_var
       let copy c = { c with lv_name = c.lv_name}
       let name = "logic_var"
     end)

module Logic_Info =
  Project.Datatype.Imperative
    (struct
       type t = logic_info
       let copy c = { c with l_var_info = Logic_Var.copy c.l_var_info}
       let name = "logic_info"
     end)

module Builtin_Logic_Info =
  Project.Datatype.Imperative
    (struct
       type t = builtin_logic_info
       let copy c = { c with bl_name = c.bl_name }
       let name = "builtin_logic_info"
     end)

module Logic_Type_Info =
  Project.Datatype.Imperative
    (struct
       type t = logic_type_info
       let copy c = {c with lt_params = c.lt_params}
       let name = "logic_type_info"
     end)

module Logic_Ctor_Info =
  Project.Datatype.Imperative
    (struct
       type t = logic_ctor_info
       let copy c = { c with ctor_name = c.ctor_name}
       let name = "logic_ctor_info"
     end)

module Lval =
  Project.Datatype.Imperative
    (struct
       type t = lval
       let copy _ = assert false (* todo *)
       let name = "lval"
     end)
let () = Lval.register_comparable ~compare:LvalComparable.compare ()

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

module Annotation_Status =
  Project.Datatype.Imperative
    (struct
       type t = annotation_status
       let name = "annotation_status"
       let copy _ = assert false
     end)

module Annot_Status =
  Project.Datatype.Imperative
    (struct
       type t = annot_status
       let name = "annot_status"
       let copy _ = assert false
     end)

module Annot_Status_List = Datatype.List(Annot_Status)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
