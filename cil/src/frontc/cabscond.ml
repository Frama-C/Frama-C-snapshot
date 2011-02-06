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

(* -------------------------------------------------------------------------- *)
(* --- Original Conditions                                                --- *)
(* -------------------------------------------------------------------------- *)

open Cabs

type cond =
  | And  of cond * cond
  | Or   of cond * cond
  | Not  of cond
  | Atom of Cil_types.exp
  | Blob

type kind =
  | IF
  | FOR
  | WHILE
  | DOWHILE

type info = {
  id : int ;
  kind : kind ;
  file : string ;
  line : int ;
  cond : cond ;
}

module A = Cabs

type lazy_cond =
  | LzAnd  of lazy_cond * lazy_cond
  | LzOr   of lazy_cond * lazy_cond
  | LzNot  of lazy_cond
  | LzAtom of A.expression

let rec lazy_cond e = match e.expr_node with
  | A.BINARY(A.AND,e1,e2) -> LzAnd(lazy_cond e1,lazy_cond e2)
  | A.BINARY(A.OR,e1,e2) -> LzOr(lazy_cond e1,lazy_cond e2)
  | A.UNARY(A.NOT,e) -> LzNot(lazy_cond e)
  | _ -> LzAtom e

type binding =
  | Lazy of int * lazy_cond
  | Info of info

let c_info = ref 0
let active = ref false
let c_stack = ref []

let inconsistent from =
  match !c_stack with
    | (_,_,loc)::_ ->
	Cilmsg.warning
	  "[%s] Inconsistent state when binding condition at %a"
	  from Cabshelper.d_cabsloc loc ;
	active := false
    | _ ->
	Cilmsg.warning
	  "[%s] Inconsistent condition stack (no condition expression stacked)"
	  from ;
	active := false

module M = Hashtbl.Make
  (struct
     type t = A.expression
     let equal = (==)
     let hash = Hashtbl.hash
   end)

let atoms : Cil_types.exp M.t = M.create 371
let conditions : (int,binding ref) Hashtbl.t = Hashtbl.create 371

let rec cond = function
  | LzAnd(x,y) -> And(cond x,cond y)
  | LzOr(x,y) -> Or(cond x,cond y)
  | LzNot x -> Not(cond x)
  | LzAtom a -> try Atom(M.find atoms a) with Not_found -> Blob

let push_condition descr loc a =
  if !active then
    let k = let k = !c_info in incr c_info ; k in
    let binder = ref (Lazy(k,lazy_cond a)) in
    c_stack := (binder,descr,loc) :: !c_stack ;
    true
  else
    false

let pop_condition () =
  if !active then
    match !c_stack with
      | ({contents=Lazy(id,lzc)} as binder,descr,loc) :: stk ->
	  begin
	    try
	      c_stack := stk ;
	      binder := Info {
		id = id ;
		kind = descr ;
		file = (fst loc).Lexing.pos_fname ;
		line = (fst loc).Lexing.pos_lnum ;
		cond = cond lzc ;
	      } ;
	    with Not_found -> inconsistent "pop-condition"
	  end
      | _ -> inconsistent "pop-condition"

let top_binder () =
  match !c_stack with
    | (binder,_,_) :: _ when !active -> binder
    | _ -> raise Not_found

let id = function Lazy(id,_) | Info {id=id} -> id

let bind (a : Cabs.expression) (e : Cil_types.exp) =
  try
    let b = top_binder () in
    begin
      M.replace atoms a e ;
      Hashtbl.replace conditions e.Cil_types.eid b ;
    end
  with Not_found -> ()

let lookup e =
  try
    match Hashtbl.find conditions e.Cil_types.eid with
      | {contents=Info c} -> Some c
      | _ -> None
  with Not_found -> None

let pp_kind fmt kd =
  Format.pp_print_string fmt
    (match kd with
       | IF -> "IF"
       | FOR -> "FOR"
       | WHILE -> "WHILE"
       | DOWHILE -> "DO WHILE")

let pp_where fmt (name,e,cond) =
  let rec pp fmt = function
    | And(x,y) -> Format.fprintf fmt "(%a && %a)" pp x pp y
    | Or(x,y) -> Format.fprintf fmt "(%a || %a)" pp x pp y
    | Not x -> Format.fprintf fmt "!(%a)" pp x
    | Atom a ->
	if a.Cil_types.eid = e.Cil_types.eid
	then Format.pp_print_string fmt name
	else Format.pp_print_char fmt '_'
    | Blob -> Format.pp_print_char fmt '_'
  in pp fmt cond

open Cil_types

let pp_comment fmt s =
  if !active then
    match s.skind with
      | If(e,_,_,_) ->
	  begin
	    match lookup e with
	      | Some info ->
		  Format.fprintf fmt "/*[CID:%d EXP:%d] %a @[%a@] */@ "
		    info.id e.Cil_types.eid
		    pp_kind info.kind pp_where ("here",e,info.cond)
	      | None -> ()
	  end
      | _ -> ()
