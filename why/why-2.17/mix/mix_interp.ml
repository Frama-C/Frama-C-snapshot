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

(* Mix to Why *)

open Format
open Mix_ast
open Mix_seq.X
open Mix_seq

type term = 
  | Tvar of string
  | Tconst of string
  | Tapp of string * term list

type why_stmt = 
  | Wvoid
  | Wassume of predicate
  | Wassert of predicate
  | Wseq of why_stmt * why_stmt
  | Wassign of string * term

type why_code = { why_pre : predicate option; why_code : why_stmt }

let index a = function
  | None -> a
  | Some i -> Tapp ("add_int", [a; Tvar ("!i" ^ i)])

let rec value_addr loc = function
  | PAself -> error loc UnsupportedInstruction
  | PAconst s -> Tconst s
  | PAident s -> Tvar s
  | PAplus (a1, a2) -> Tapp ("add_int", [value_addr loc a1; value_addr loc a2])
  | PAminus (a1, a2) -> Tapp ("sub_int",[value_addr loc a1; value_addr loc a2])
  | PAuminus a -> Tapp ("neg_int", [value_addr loc a])

let value_op loc op = match op.pop_address, op.pop_index, op.pop_field with
  | Some a, i, None -> index (value_addr loc a) i
  | _ -> assert false (*TODO*)

let value_at loc op = Tapp ("access", [Tvar "!mem"; value_op loc op])

let register_value = function
  | A -> "!a" | X -> "!x"
  | I1 -> "!i1" | I2 -> "!i2" | I3 -> "!i3" 
  | I4 -> "!i4" | I5 -> "!i5" | I6 -> "!i6" 

let interp_instr loc i op = match i with
  | Ld r -> 
      Wassign (register_name r, value_at loc op)
  | Ldn r -> 
      Wassign (register_name r, Tapp ("neg_int", [value_at loc op]))
  | St r ->
      Wassign ("mem", 
	       Tapp ("update", 
		     [Tvar "!mem"; value_op loc op; Tvar (register_value r)]))
  | Stz ->
      Wassign ("mem", 
	       Tapp ("update", [Tvar "!mem"; value_op loc op; Tconst "0"]))
  | Stj -> 
      error loc UnsupportedInstruction
  | Add | Sub | Mul | Div -> (* TODO: actually incorrect for Mul and Div *)
      let aop = match i with
	| Add -> "add_int" | Sub -> "sub_int"
	| Mul -> "mul_int" | Div -> "div_int" | _ -> assert false
      in
      Wassign ("a", Tapp (aop, [Tvar "!a"; value_at loc op]))
  | Srb ->
      begin match op.pop_address, op.pop_index, op.pop_field with
	| Some (PAconst "1"), None, None -> 
	    Wassign ("a", Tapp ("div_int", [Tvar "!a"; Tconst "2"]))
	| _ -> error loc UnsupportedInstruction
      end
  | Cmp r -> 
      Wassign ("cmp", 
	       Tapp ("sub_int", [Tvar (register_value r); value_at loc op]))
  | Ent r ->
      Wassign (register_name r, value_op loc op)
  | Enn r ->
      Wassign (register_name r, Tapp ("neg_int", [value_op loc op]))
  | Inc r ->
      Wassign (register_name r, 
	      Tapp ("add_int", [Tvar (register_value r); value_op loc op]))
  | Dec r ->
      Wassign (register_name r, 
	       Tapp ("sub_int", [Tvar (register_value r); value_op loc op]))
  | Nop ->
      Wvoid
  | Jmp | Jsj 
  | Jl | Je | Jg | Jge | Jne | Jle
  | Jn _ | Jz _ | Jp _ | Jnn _ | Jnz _ | Jnp _
  | Hlt -> 
      assert false 

let rec interp_stmt = function
  | Void -> Wvoid
  | Mix { node = PSassert p } -> Wassert p
  | Mix { node = PSinvariant _ } -> assert false
  | Mix { loc = loc; node = PSinstr (i, op) } -> interp_instr loc i op
  | Assume p -> Wassume p
  | Seq (s1, s2) -> Wseq (interp_stmt s1, interp_stmt s2)

let interp_seq c = { why_pre = c.seq_pre; why_code = interp_stmt c.seq_code }

let interp = List.map interp_seq

(* pretty-printing *)

let counter = ref 0

let rec term fmt = function
  | Tvar s | Tconst s -> fprintf fmt "%s" s
  | Tapp (f, l) -> fprintf fmt "@[(%s@ %a)@]" f (Pp.print_list Pp.space term) l

let rec print_why fmt = function
  | Wvoid -> fprintf fmt "void"
  | Wassume p -> fprintf fmt "[{} unit {%s}]" p
  | Wassert p -> fprintf fmt "(assert {%s}; void)" p
  | Wseq (s1, s2) -> fprintf fmt "%a;@ %a" print_why s1 print_why s2
  | Wassign (id, t) -> fprintf fmt "%s := %a" id term t

let print_why_code fmt c =
  incr counter;
  fprintf fmt "let seq%d () =@\n" !counter;
  begin match c.why_pre with
    | Some p -> fprintf fmt "  @[<hov 2>{ %s }@]@\n" (X.string_of_predicate p)
    | None -> ()
  end;
  fprintf fmt "  @[<hv>%a@]@\n@\n" print_why c.why_code


