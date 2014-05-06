(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

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

type lazy_cond =
  | LzAnd  of lazy_cond * lazy_cond
  | LzOr   of lazy_cond * lazy_cond
  | LzNot  of lazy_cond
  | LzAtom of Cabs.expression

let rec lazy_cond e = match e.expr_node with
  | Cabs.BINARY(Cabs.AND,e1,e2) -> LzAnd(lazy_cond e1,lazy_cond e2)
  | Cabs.BINARY(Cabs.OR,e1,e2) -> LzOr(lazy_cond e1,lazy_cond e2)
  | Cabs.UNARY(Cabs.NOT,e) -> LzNot(lazy_cond e)
  | _ -> LzAtom e

type binding =
  | Lazy of int * lazy_cond
  | Info of info

type context = {
  c_kind : kind ;
  c_loc : Cabs.cabsloc ;
  mutable c_binder : binding ;
  mutable c_if : string option ;
  mutable c_then : string option ;
  mutable c_else : string option ;
}

let c_info = ref 0
let active = ref false
let c_stack : context list ref = ref []

let inconsistent from =
  match !c_stack with
    | context::_ ->
	Kernel.warning
	  "[%s] Inconsistent state when binding condition at %a"
	  from Cabshelper.d_cabsloc context.c_loc ;
	active := false
    | _ ->
	Kernel.warning
	  "[%s] Inconsistent condition stack (no condition expression stacked)"
	  from ;
	active := false

module Emap = Hashtbl.Make
  (struct
     type t = Cabs.expression
     let equal = (==)
     let hash = Hashtbl.hash
   end)

let atoms : Cil_types.exp Emap.t = Emap.create 371
let conditions : (int,context) Hashtbl.t = Hashtbl.create 371

let rec cond = function
  | LzAnd(x,y) -> And(cond x,cond y)
  | LzOr(x,y) -> Or(cond x,cond y)
  | LzNot x -> Not(cond x)
  | LzAtom a -> try Atom(Emap.find atoms a) with Not_found -> Blob

let push_condition kind loc a =
  if !active then
    let k = !c_info in 
    incr c_info ;
    let context = {
      c_loc = loc ; c_kind = kind ;
      c_binder = Lazy(k,lazy_cond a) ;
      c_if = None ; c_then = None ; c_else = None ;
    } in
    c_stack := context :: !c_stack ;
    true
  else
    false

let pop_condition () =
  if !active then
    match !c_stack with
      | ({ c_binder=Lazy(id,lzc) } as context) :: stk ->
	  begin
	    c_stack := stk ;
	    context.c_binder <- Info {
	      id = id ;
	      kind = context.c_kind ;
	      file = (fst context.c_loc).Lexing.pos_fname ;
	      line = (fst context.c_loc).Lexing.pos_lnum ;
	      cond = cond lzc ;
	    } ;
	  end
      | _ -> inconsistent "pop-condition"

let top_context () =
  match !c_stack with
    | context :: _ when !active -> context
    | _ -> raise Not_found

let bind (a : Cabs.expression) (e : Cil_types.exp) =
  try
    let context = top_context () in
    begin
      Emap.replace atoms a e ;
      Hashtbl.replace conditions e.Cil_types.eid context ;
    end
  with Not_found -> ()

(* -------------------------------------------------------------------------- *)
(* --- Retrieving Conditions                                              --- *)
(* -------------------------------------------------------------------------- *)

let lookup e =
  try
    match Hashtbl.find conditions e.Cil_types.eid with
      | {c_binder=Info info} -> Some info
      | _ -> None
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Pretty-Print                                                       --- *)
(* -------------------------------------------------------------------------- *)

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
		  Format.fprintf fmt "/*[CID:%d] %a @[%a@] */@ "
		    info.id pp_kind info.kind pp_where ("here",e,info.cond)
	      | None -> ()
	  end
      | _ -> ()
