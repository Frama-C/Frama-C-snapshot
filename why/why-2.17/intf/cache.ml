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

open Options
open Marshal
open Digest
open Logic
open Cc

let flags = []
let max_size = ref 5000 (* maximum cache size *)
let cache = ref (Hashtbl.create 97)
let source_file = ref "/tmp/gwhy.cache"
let active = ref true
let obligs = ref true
let ok = ref true

let enable () = active := true
let disable () = active := false
let set_active v = active := v
let swap_active () = active := not !active
let is_enabled () = !active

let swap_hard_proof () = obligs := not !obligs
let set_hard_proof v = obligs := v
let hard_proof () = !obligs

let exists p o = 
  try 
    Queue.iter 
      (fun pr -> if p = pr then raise Exit) 
      (Hashtbl.find !cache o); 
    false
  with Exit -> true

let read_cache () = 
  try
    let in_channel = open_in !source_file in
    cache := from_channel in_channel
  with 
    | Sys_error s -> 
	print_endline ("     [...] Sys_error : "^s); 
	flush stdout
    | End_of_file -> 
	print_endline ("     [...] cache empty !"); 
	flush stdout
    | _ -> 
	print_endline ("     [...] error while loading cache !"); 
	flush stdout

let clean seq =
  let (ctx, p) = seq.Env.scheme_type in
  let rec clean0 = function 
    | Pvar _ | Papp (_, _, _) | Pfpi (_,_,_) | Ptrue | Pfalse as p -> p
    | Pimplies (i, p1, p2) -> 
	Pimplies (i, clean0 p1, clean0 p2)
    | Pif (t, p1, p2) ->
      Pif (t, clean0 p1, clean0 p2)
    | Pand (wp, sym, p1, p2) ->
	Pand (wp, sym, clean0 p1, clean0 p2)
    | Por (p1, p2) ->
	Por (clean0 p1, clean0 p2)
    | Piff (p1, p2) ->
	Piff (clean0 p1, clean0 p2)
    | Pnot p ->
	Pnot (clean0 p)
    | Forall (wp, id1, id2, pt, tl, p) ->
	Forall (wp, id1, id2, pt, tl, clean0 p)
    | Forallb (wp, p1, p2) ->
	Forallb (wp, clean0 p1, clean0 p2)
    | Exists (id1, id2, pt, p) ->
	Exists (id1, id2, pt, clean0 p)
    | Pnamed (_, p) ->
	clean0 p
  and clean1 = function 
    | Svar _ as c -> c
    | Spred (id, p) -> Spred (id, clean0 p)
  in 
  (List.map clean1 ctx, clean0 p)
	  
let fool () = Hashtbl.length !cache > !max_size 
  (* i mean fool ... cache doesn't want to do his job *)
let is_full = ref false

let load_cache source =
  source_file := source;
  if not (Sys.file_exists !source_file) then
    begin
      let xc = Sys.command ("touch " ^ source) in
      if xc <> 0 then
	begin
	  ok := false;
	  print_endline ("     [...] Error : cannot create cache file "^source^" !"); 
	  flush stdout;
	end
    end;
  if !ok then read_cache ();
  is_full := fool ()
    
let save () = 
  let out_channel = open_out_bin !source_file in
  to_channel out_channel !cache flags;
  close_out out_channel

let clear () = 
  Hashtbl.clear !cache;
  save ()

let remove x = Hashtbl.remove !cache x
let in_cache x = Hashtbl.mem !cache x
let find x = Hashtbl.find !cache x
let is_empty () = Hashtbl.length !cache = 0
let o_in_cache o = let (_,_,seq) = o in in_cache seq

let add (seq:Cc.sequent Env.scheme) (prover:string) = 
  let o = clean seq in
  if not !is_full then begin
    if in_cache o then
      begin 
	if not (exists prover o) then
	  Queue.add prover (Hashtbl.find !cache o) 
      end
    else
      begin 
	let q = Queue.create () in
	let _ = Queue.add prover q in
	Hashtbl.add !cache o q;
	is_full := fool ()
      end
  end;
  save () (* actually, must be done when exiting gui *)

