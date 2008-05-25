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

open Cil_types
open Cil
open Cabs

type location = { filename : string; lineno : int }

module Smap = Map.Make(String)

module VTable = Hashtbl.Make(struct type t = varinfo 
                                    let hash v = v.vid 
                                    let equal v v' = v.vid = v'.vid 
                                    let compare v v'= 
                                      Pervasives.compare v.vid v'.vid 
                             end)

let all_vars = VTable.create 79

type data = 
  | Var of varinfo ref
  | Typ of typ
  | Label of string
  | Enum of (exp * typ)
  | Block of int * int

type stack = {
  mutable map : data Smap.t;
  mutable open_scopes : (int * data Smap.t) list;
  mutable all_scopes : (int * data * data Smap.t) list;
}

let stacks = Hashtbl.create 17

let dump_data fmt = function
    | Var vi -> Format.fprintf fmt "%s(%d)" !vi.vname !vi.vid
    | Typ _ -> Format.fprintf fmt "<type>"
    | Label _ -> Format.fprintf fmt "<label>"
    | Enum _ -> Format.fprintf fmt "<enum>"
    | Block _ -> Format.fprintf fmt "<block>"

let dump_map m =
  Smap.iter (fun x vi -> Format.eprintf "%s=%a " x dump_data vi) m;
  Format.eprintf "\n"

let dump_all_scopes v = 
  List.iter 
    (fun (n,_,m) -> Format.eprintf "%d: " n; dump_map m)
    v.all_scopes
  
let dump () =
  Hashtbl.iter
    (fun f s ->
       if f <> "" then begin
	 Format.eprintf "file %s:\n" f;
	 Format.eprintf "current: "; dump_map s.map;
	 dump_all_scopes s;
       end)
    stacks;
  flush stderr

let replace replace_var _replace_typ _replace_enum =
(*  let replace_data data =  
    match data with     
    | Var e ->  
        let new_v = replace_var e in
        (* Format.eprintf "Replacing %s(%d) with %s(%d)@\n" e.vname e.vid
          new_v.vname new_v.vid;*)
        Var new_v
    | Typ e -> Typ (replace_typ e)
    | Enum e -> Enum (replace_enum e)
    | Label _ | Block _ -> data
  in
  let replace_scope (a,b,map) = 
    a,(replace_data b),Smap.fold 
      (fun key data acc ->Smap.add key (replace_data data) acc)
      map
      Smap.empty
  in
  let replace_map map = 
    Smap.fold 
      (fun key data acc ->Smap.add key (replace_data data) acc)
      map
      Smap.empty
  in
  Hashtbl.iter 
    (fun fname data -> 
       data.all_scopes <- List.map replace_scope data.all_scopes;
       data.map <- replace_map data.map;
       data.open_scopes <- 
         List.map (fun (i,tbl) -> i,replace_map tbl)
         data.open_scopes)
    stacks
*)
  VTable.iter (fun _k vr -> vr := replace_var !vr) all_vars

let file_stack f =
  try 
    Hashtbl.find stacks f 
  with Not_found -> 
    let m = Smap.empty in
    let s = { map = m; open_scopes = [0,m]; all_scopes = [0,Block(0,0),m] } in
    Hashtbl.add stacks f s;
    s

let push l =
  (* Format.eprintf "Scope.push file=%s loc=%d@\n" l.filename l.lineno;
  dump (); *)
  let stack = file_stack l.filename in
  stack.open_scopes <- (l.lineno, stack.map) :: stack.open_scopes

let pop l =
  (* Format.eprintf "Scope.pop file=%s loc=%d@\n" l.filename l.lineno;
  dump ();*)
  let stack = file_stack l.filename in
  match stack.open_scopes with
    | (l0, m) :: s -> 
	stack.map <- m; 
	stack.open_scopes <- s;
	stack.all_scopes <- 
	  (l.lineno, Block (l0, l.lineno), m) :: stack.all_scopes
    | _ -> 
	assert false

let add l x d =
(*  Format.eprintf "Scope.add file=%s loc=%d %s->%s@\n" 
    l.filename l.lineno x vi.Cil.vname; 
  dump (); *)
  let stack = file_stack l.filename in
  let m' = Smap.add x d stack.map in
  stack.map <- m';
  stack.all_scopes <- (l.lineno, d, m') :: stack.all_scopes

let find p l x =
  let stack = file_stack l.filename in
  let rec lookup = function
    | (n, _, m) :: s when n <= l.lineno -> 
	let v = Smap.find x m in if p v then v else lookup s
    | _ :: s -> lookup s
    | [] -> raise Not_found
  in
  lookup stack.all_scopes

let find_any = find (fun _ -> true)

let add_var l x v =  add l x 
  (Var(try VTable.find all_vars v 
       with Not_found ->
         let new_v = ref v in VTable.add all_vars v new_v; new_v))

let find_var l x = match find_any l x with Var v -> !v | _ -> raise Not_found
let find_enum_id l x = 
  match find_any l x with Enum (e,t) -> (e,t) | _ -> raise Not_found

let find_glob_var l x = 
  let is_glob_var = function Var v -> !v.vglob | _ -> false in
  match find is_glob_var l x with Var v -> !v | _ -> assert false


let add_type l x t = add l x (Typ t)
let find_typedef l x = 
  match find_any l ("type " ^ x) with Typ t -> t | _ -> raise Not_found
let find_enum l x = 
  match find_any l ("enum " ^ x) with Typ t -> t | _ -> raise Not_found
let find_union l x = 
  match find_any l ("union " ^ x) with Typ t -> t | _ -> raise Not_found
let find_struct l x = 
  match find_any l ("struct " ^ x) with Typ t -> t | _ -> raise Not_found

let add_label l x t = add l x (Label t)
let mem_label l x = 
  try (match find_any l ("label " ^ x) with Label _t -> true | _ -> false)
  with Not_found -> false

let add_enum_id l x t = add l x (Enum t)


let rec first_function (l) = 
  (* Format.eprintf "LENGTH is %d@\n" (List.length l);*)
  match l with
  | [] -> raise Not_found
  | (_, Var vi, _) :: _ when isFunctionType !vi.vtype -> 
      (* Format.eprintf "GOT: %s(%d)" vi.vname vi.vid; *)
      !vi
  (* | (_, Var vi, _) :: r -> 
      Format.eprintf "GOT(NO): %s(%d)" vi.vname vi.vid;
      first_function r*)
  | _ :: r -> first_function r

let find_function_after l =
  let stack = file_stack l.filename in
  let rec lookup acc = function
    | (n, _, _) :: _r when n < l.lineno -> first_function acc
    | d :: s -> lookup (d :: acc) s
    | [] -> assert false
  in
  lookup [] stack.all_scopes

let find_function_before l =
  (* Format.eprintf "Looking for function before %s L%d@\n"
    l.filename l.lineno;*)
  let stack = file_stack l.filename in
  (* dump_all_scopes stack;*)
  let rec lookup = function
    | (n, _, _) :: _ as r when n <= l.lineno -> first_function r
    | _ :: s -> lookup s
    | [] -> assert false
  in
  let f = lookup stack.all_scopes in
  (* Format.eprintf "Found function %s(%d)@\n" f.vname f.vid;*)
  f
  

let find_block_after l =
  let stack = file_stack l.filename in
  let rec lookup = function
    | (_, Block (b, e), _) :: _r when b >= l.lineno -> (b, e)
    | _ :: s -> lookup s
    | [] -> assert false
  in
  lookup stack.all_scopes
