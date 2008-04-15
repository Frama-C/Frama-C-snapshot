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

(* explanations *)

let program_locs = Hashtbl.create 17

(*
let read_in_file f l b e = 
  let ch = open_in f in
  for i = 2 to l do
    ignore (input_line ch)
  done;
  for i = 1 to b do
    ignore (input_char ch)
  done;
  let buf = Buffer.create 17 in
  for i = b to e-1 do
    Buffer.add_char buf (input_char ch)
  done;
  Buffer.contents buf
*)
 
open Format
open Logic
open Logic_decl

  
let raw_loc ?(quote=false) ?(pref="") fmt (f,l,b,e) =
  if quote 
  then
    begin
      fprintf fmt "%sfile = \"%s\" " pref f;
      fprintf fmt "%sline = \"%d\" " pref l;
      fprintf fmt "%sbegin = \"%d\" " pref b;
      fprintf fmt "%send = \"%d\"" pref e
    end
  else
    begin
      fprintf fmt "%sfile = \"%s\"@\n" pref f;
      fprintf fmt "%sline = %d@\n" pref l;
      fprintf fmt "%sbegin = %d@\n" pref b;
      fprintf fmt "%send = %d@\n" pref e
    end
 
let print_formula fmt s =
  if String.length s > 0 then
    fprintf fmt "formula = \"%s\"@\n" s

let print_kind ?(quote=false) fmt (loc,k) =
  (* 
     Option_misc.iter (fun lab ->  fprintf fmt "label = %s@\n" lab) labopt; 
  *)
  if quote then
    begin
      match k with
	| EKOther s -> fprintf fmt "kind = \"Other\"@\ntext = \"%s\"" s
	| EKAbsurd -> fprintf fmt "kind = \"Absurd\""
	| EKAssert -> fprintf fmt "kind = \"Assert\""
	| EKPre s -> fprintf fmt "kind = \"Pre\"@\ntext = \"%s\"" s
	| EKPost -> fprintf fmt "kind = \"Post\""
	| EKWfRel -> fprintf fmt "kind = \"WfRel\""
	| EKVarDecr -> fprintf fmt "kind = \"VarDecr\"" 
	| EKLoopInvInit s -> 
	    fprintf fmt "kind = \"LoopInvInit\"";
	    print_formula fmt s
	| EKLoopInvPreserv s -> 
	    fprintf fmt "kind = \"LoopInvPreserv\"";
	    print_formula fmt s
	| EKLemma -> fprintf fmt "kind = \"Lemma\""
    end
  else
    begin
      raw_loc fmt loc;
      match k with
	| EKOther s -> fprintf fmt "kind = Other@\ntext = \"%s\"@\n" s
	| EKAbsurd -> fprintf fmt "kind = Absurd@\n"
	| EKAssert -> fprintf fmt "kind = Assert@\n"
	| EKPre s -> fprintf fmt "kind = Pre@\ntext = \"%s\"@\n" s
	| EKPost -> fprintf fmt "kind = Post@\n"
	| EKWfRel -> fprintf fmt "kind = WfRel@\n"
	| EKVarDecr -> fprintf fmt "kind = VarDecr@\n" 
	| EKLoopInvInit s -> 
	    fprintf fmt "kind = LoopInvInit@\n";
	    print_formula fmt s
	| EKLoopInvPreserv s -> 
	    fprintf fmt "kind = LoopInvPreserv@\n";
	    print_formula fmt s
	| EKLemma -> fprintf fmt "kind = Lemma@\n"
    end


let print ?(quote=false) fmt  ((*loc,*)e) = 
  print_kind ~quote fmt (e.vc_loc,e.vc_kind)

let msg_of_loopinv = function
  | "" -> " of loop invariant"
  | s -> " of generated loop inv.: " ^ s

let msg_of_kind = 
  function
    | EKPre "PointerDeref" -> "pointer dereferencing"
    | EKPre "IndexBounds" -> "check index bounds"
    | EKPre "ArithOverflow" -> "check arithmetic overflow"
    | EKPre "DivByZero" -> "check division by zero"
    | EKPre "AllocSize" -> "check allocation size"
    | EKPre "UserCall" -> "precondition for user call"
    | EKPre "" -> "precondition"
    | EKPre s -> "unclassified precondition `" ^ s ^ "'"
    | EKOther s -> "unclassified obligation `" ^ s ^ "'"
    | EKAbsurd -> "unreachable code"
    | EKAssert -> "assertion"
    | EKPost -> "postcondition"
    | EKWfRel -> "well-foundness of relation"
    | EKVarDecr -> "variant decrease" 
    | EKLoopInvInit s -> "initialization" ^ msg_of_loopinv s
    | EKLoopInvPreserv s -> "preservation" ^ msg_of_loopinv s
    | EKLemma -> "lemma"
