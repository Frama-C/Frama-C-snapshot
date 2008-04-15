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

(* Statistics on Why goals *)

open Format
open Pp
open Ident
open Logic
open Ptree

let out = ref ""
let prefix = ref "x"

let spec = []
let usage = "why-stat files..."

let files = Queue.create ()

let rec explain_exception fmt = function
  | Lexer.Lexical_error s -> 
      fprintf fmt "Lexical error: %s" s
  | Parsing.Parse_error -> 
      fprintf fmt "Syntax error"
  | Stream.Error s -> 
      fprintf fmt "Syntax error: %s" s
  | Loc.Located (loc, e) ->
      fprintf fmt "%a%a" Loc.report_position loc explain_exception e
  | e ->
      fprintf fmt "Anomaly: %s" (Printexc.to_string e); raise e

let add_file f =
  try
    let c = open_in f in
    let lb = Lexing.from_channel c in
    lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
    let p = Lexer.parse_file lb in
    Queue.add p files;
    close_in c
  with e -> 
    explain_exception err_formatter e;
    pp_print_newline err_formatter ();
    exit 1

let () = Arg.parse spec add_file usage

(* sets of identifiers *)
module S = struct
  include Idset
  let print fmt s =
    let l = Idset.elements s in
    print_list (fun fmt () -> fprintf fmt ",") Ident.print fmt l
end
(* multisets of sets of identifiers *)
module M = struct
  module MS = Map.Make(S)
  let ms = ref MS.empty
  let add s = 
    try incr (MS.find s !ms) 
    with Not_found -> ms := MS.add s (ref 1) !ms
  let print fmt =
    let l = MS.fold (fun s r l -> (!r,s) :: l) !ms [] in
    let l = List.sort (fun (n1,_) (n2,_) -> compare n2 n1) l in
    List.iter (fun (n,s) -> fprintf fmt "%8d %a@\n" n S.print s) l
end

let ident avoid s id = if not (S.mem id avoid) then S.add id s else s

let rec lexpr avoid s p = match p.pp_desc with
  | PPvar id -> 
      ident avoid s id
  | PPapp (id, l) ->
      let s = ident avoid s id in
      List.fold_left (lexpr avoid) s l
  | PPconst _
  | PPtrue 
  | PPfalse ->
      s
  | PPinfix (a, _, b) ->
      lexpr avoid (lexpr avoid s a) b
  | PPif (a, b, c) -> 
      lexpr avoid (lexpr avoid (lexpr avoid s a) b) c
  | PPprefix (_, a) 
  | PPfpi (a, _, _) 
  | PPnamed (_, a) ->
      lexpr avoid s a
  | PPforall (id,_,_,p)
  | PPexists (id,_,p) ->
      let avoid = S.add id avoid in
      lexpr avoid s p


let rec lexpr avoid s p = match p.pp_desc with
  | PPvar id -> 
      ident avoid s id
  | PPapp (id, l) ->
      let s = ident avoid s id in
      List.fold_left (lexpr avoid) s l
  | PPconst _
  | PPtrue 
  | PPfalse ->
      s
  | PPinfix (a, _, b) ->
      lexpr avoid (lexpr avoid s a) b
  | PPif (a, b, c) -> 
      lexpr avoid (lexpr avoid (lexpr avoid s a) b) c
  | PPprefix (_, a) 
  | PPfpi (a, _, _) 
  | PPnamed (_, a) ->
      lexpr avoid s a
  | PPforall (id,_,_,p)
  | PPexists (id,_,p) ->
      let avoid = S.add id avoid in
      lexpr avoid s p


let number_of_literals = ref 0


let rec compute_literal_number  pr = match pr.pp_desc with
  | PPinfix (p1,_, p2) ->
      compute_literal_number  p1 ; 
      compute_literal_number  p2 ; 
  | PPif(_,p1,p2) -> 
      compute_literal_number  p1 ; 
      compute_literal_number  p2 ; 
  | PPapp(_) -> 
      number_of_literals := !number_of_literals + 1
  | PPprefix(_,p)
  | PPforall (_, _, _, p)
  | PPexists (_,_,p) 
  | PPnamed(_,p) 
  | PPfpi(p,_,_) ->
      compute_literal_number  p
  | PPvar(_)   
  | PPconst(_) ->
      number_of_literals := !number_of_literals + 1
  | PPtrue 
  | PPfalse -> ()
      


let decl = function
  | Goal (_, _, p) ->
      let rec intros avoid p = match p.pp_desc with
	| PPinfix (_, PPimplies, p) -> intros avoid p
	| PPforall (id, _, _, p) -> intros (S.add id avoid) p
	| _ -> lexpr avoid S.empty p
      in
      let s = intros S.empty p in
      M.add s ; 
      compute_literal_number p;
  | Program _
  | Parameter _
  | Exception _
  | Logic _
  | Axiom _
  | Predicate_def _
  | Inductive_def _
  | Function_def _
  | TypeDecl _ ->
      ()

let () = 
  Queue.iter (List.iter decl) files;
  M.print std_formatter;
  Printf.printf  "Goal Literal Number : %d \n" !number_of_literals 

