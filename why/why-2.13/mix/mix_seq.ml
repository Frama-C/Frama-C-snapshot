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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* Turn a MIX program into a set of sequential MIX programs *)

open Format
open Mix_ast

module Label = struct

  type t = 
    { lab_name : string; 
      lab_user : bool;
      lab_addr : int;
    }

  let equal = (=)

  let hash = Hashtbl.hash

  let create = 
    let r = ref 0 in 
    fun () -> 
      incr r; 
      { lab_name = "L" ^ string_of_int !r;
	lab_user = false;
	lab_addr = 0 }

  let user id addr = 
    { lab_name = id; lab_user = true; lab_addr = addr }

  let anon addr = 
    let lab = create () in { lab with lab_addr = addr }

  let to_string l = l.lab_name

end

module X = struct

  module Label = Label

  type predicate = string
    
  let ptrue = "true"
  let string_of_predicate p = p

  type statement = 
    | Void
    | Mix of pstmt
    | Assume of predicate
    | Seq of statement * statement
    
  let void_stmt = Void
  let append_stmt s1 s2 = Seq (s1, s2)
  let assert_stmt p = Mix { node = PSassert p; loc = Lexing.dummy_pos }

  let rec string_of_stmt = function
    | Void -> "void"
    | Mix { node = PSassert p } -> "assert ... "
    | Mix _ -> "<mix>"
    | Assume s -> "assume " ^ s
    | Seq (s1, s2) -> string_of_stmt s1 ^ "; " ^ string_of_stmt s2

end

include Mix_cfg.Make(X)

(* error reporting *)

type error = 
  | UnboundLabel of string
  | IllegalCodeAddress of int
  | ClashEqu of string
  | UnsupportedInstruction

let report fmt = function
  | UnboundLabel s -> fprintf fmt "unbound label %s" s
  | IllegalCodeAddress n -> fprintf fmt "illegal address %d" n
  | ClashEqu id -> fprintf fmt "clash with previous EQU %s" id
  | UnsupportedInstruction -> fprintf fmt "unsupported instruction"

exception Error of loc * error

let error loc s = raise (Error (loc, s))

(* symbol table *)

let labels_by_name = Hashtbl.create 97
let labels_by_addr = Hashtbl.create 97

let declare_label lab =
  Hashtbl.add labels_by_name lab.Label.lab_name lab;
  Hashtbl.add labels_by_addr lab.Label.lab_addr lab

let find_label_by_name loc id = 
  try Hashtbl.find labels_by_name id 
  with Not_found -> error loc (UnboundLabel id)

let find_label_by_addr loc a = 
  try Hashtbl.find labels_by_addr a
  with Not_found -> error loc (IllegalCodeAddress a)

let equ = Hashtbl.create 17 

(* Mixal: we resolve addresses *)

let step s = match s.node with
  | PSinvariant _ | PSassert _ -> 0
  | PSinstr _ -> 1

let eval_address self loc a =
  let rec addr = function
    | PAplus (a1, a2) -> addr a1 + addr a2
    | PAminus (a1, a2) -> addr a1 - addr a2
    | PAuminus a -> - (addr a)
    | PAident id -> let lab = find_label_by_name loc id in lab.Label.lab_addr
    | PAconst n -> int_of_string n
    | PAself -> self
  in
  addr a

let eval_operand self loc op =
  match op.pop_address, op.pop_index, op.pop_field with
    | Some a, None, None -> eval_address self loc a
    | None, _, _ | _, Some _, _ | _, _, Some _ -> assert false (*TODO*)

let address self loc = function
  | { pop_address = Some (PAident id); pop_index = None; pop_field = None } ->
      find_label_by_name loc id
  | op ->
      (* otherwise we eval the address and find the corresponding label *)
      find_label_by_addr loc (eval_operand self loc op)

let register_name = function
  | A -> "a" | X -> "x"
  | I1 -> "i1" | I2 -> "i2" | I3 -> "i3" 
  | I4 -> "i4" | I5 -> "i5" | I6 -> "i6" 

let pos s = s ^ " > 0"
let zero s = s ^ " = 0"
let neg s = s ^ " < 0"
let npos s = s ^ " <= 0"
let nzero s = s ^ " <> 0"
let nneg s = s ^ " >= 0"

(* prev = previous instruction
   lab = label of current instruction *)
let interp_stmt self prev lab s = match s.node with
  | PSinvariant i -> 
      Ainvariant i
  | PSassert a -> 
      Aother (X.assert_stmt a)
  | PSinstr ((Jmp | Jsj), op) -> (* TODO Jsj *)
      Ajump (address self s.loc op)
  | PSinstr (Jn r | Jz r | Jp r | Jnn r | Jnz r | Jnp r as i, op) -> 
      let n = register_name r in
      let tt,tf = match i with
	| Jp _ -> pos, npos | Jz _ -> zero, nzero | Jn _ -> neg, nneg
	| Jnp _ -> npos, pos | Jnz _ -> nzero, zero | Jnn _ -> nneg, neg
	| _ -> assert false
      in
      Acond (address self s.loc op, X.Assume (tt n), X.Assume (tf n))
  | PSinstr (Jl, op) ->
      Acond (address self s.loc op, X.Assume "cmp < 0", X.Assume "cmp >= 0")
  | PSinstr (Jle, op) ->
      Acond (address self s.loc op, X.Assume "cmp <= 0", X.Assume "cmp > 0")
  | PSinstr (Jge, op) ->
      Acond (address self s.loc op, X.Assume "cmp >= 0", X.Assume "cmp < 0")
  | PSinstr (Jg, op) ->
      Acond (address self s.loc op, X.Assume "cmp > 0", X.Assume "cmp <= 0")
  | PSinstr (Je, op) ->
      Acond (address self s.loc op, X.Assume "cmp = 0", X.Assume "cmp <> 0")
  | PSinstr (Jne, op) ->
      Acond (address self s.loc op, X.Assume "cmp <> 0", X.Assume "cmp = 0")
  | PSinstr (Hlt, _) -> 
      Ahalt
  | PSinstr _ -> 
      Aother (X.Mix s)

let declare_equ loc id a =
  if Hashtbl.mem equ id then error loc (ClashEqu id);
  Hashtbl.add equ id a

let mixal (pseudo,asm) init =
  let self = ref 0 in
  (* pseudo *)
  List.iter
    (fun p -> match p.node with
      | Equ_addr (id, a) -> 
	  declare_equ p.loc id (eval_address !self p.loc a)
      | Equ_field _ -> 
	  assert false (*TODO*)
      | Orig (id, a) ->
	  let n = eval_address !self p.loc a in
	  begin match id with 
	    | Some id -> declare_equ p.loc id n 
	    | None -> () 
	  end;
	  self := n
      | Verbatim _ -> 
	  ())
    pseudo;
  (* declare code labels *)
  let asm =
    List.map
      (fun (lo, ps) -> 
	let l = match lo with 
	  | None -> Label.anon !self
	  | Some id -> Label.user id !self 
	in
	declare_label l;
	self := !self + step ps;
	l, ps)
    asm
  in
  (* instruction interpretation *)
  let rec map_instr prev = function
    | [] -> 
	[]
    | (l,i) :: r -> 
	(l, interp_stmt l.Label.lab_addr prev l i) :: map_instr (Some i) r
  in
  let asm = map_instr None asm in
  asm

let sequentialize ~show_graph asm init =
  let asm = mixal asm init in
  let init = find_label_by_name Lexing.dummy_pos init in
  transform ~show_graph asm init
