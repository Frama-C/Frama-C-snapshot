(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

module F = Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Lang Iterator                                                      --- *)
(* -------------------------------------------------------------------------- *)

let iter f e =
  let q = Queue.create () in
  Queue.add e q ;
  while not (Queue.is_empty q) do
    let e = Queue.pop q in
    f e ; F.lc_iter (fun e -> Queue.push e q) e
  done

let once f e =
  let q = Queue.create () in
  let m = ref F.Tset.empty in
  let once m e =
    if F.Tset.mem e !m then false else (m := F.Tset.add e !m ; true) in
  Queue.add e q ;
  while not (Queue.is_empty q) do
    let e = Queue.pop q in
    f e ; F.lc_iter (fun e -> if once m e then Queue.push e q) e
  done

(* -------------------------------------------------------------------------- *)
(* --- Head Footprint                                                     --- *)
(* -------------------------------------------------------------------------- *)

let head_fields = function
  | [] -> ""
  | (Lang.Mfield(mdt,_,_,_),_)::_ -> mdt.Lang.ext_debug
  | (Lang.Cfield fd,_):: _ -> let open Cil_types in fd.fcomp.cname

let head e =
  let open Qed.Logic in
  match F.repr e with
  | Kint z -> Z.to_string z
  | Kreal r -> Q.to_string r
  | Fvar x -> Printf.sprintf "$%s" (F.Var.basename x)
  | Bvar(k,_) -> Printf.sprintf "#%d" k
  | True -> "T"
  | False -> "F"
  | And _ -> "&"
  | Or _ -> "|"
  | Not _ -> "!"
  | Imply _ -> ">"
  | Eq _ -> "="
  | Lt _ -> "<"
  | Leq _ -> "<="
  | Neq _ -> "~"
  | Add _ -> "+"
  | Mul _ -> "*"
  | Times(k,_) -> Printf.sprintf ".%s" (Z.to_string k)
  | Div _ -> "/"
  | Mod _ -> "%"
  | If _ -> "?"
  | Aget _ -> "[]"
  | Aset _ -> "[=]"
  | Rget(_,fd) -> Pretty_utils.sfprintf ".%a" Lang.Field.pretty fd
  | Rdef fds -> Pretty_utils.sfprintf "{%s}" (head_fields fds)
  | Fun(f,_) -> Pretty_utils.to_string Lang.Fun.pretty f
  | Apply _ -> "()"
  | Bind(Forall,_,_) -> "\\F"
  | Bind(Exists,_,_) -> "\\E"
  | Bind(Lambda,_,_) -> "\\L"

(* -------------------------------------------------------------------------- *)
(* --- Term Footprint                                                     --- *)
(* -------------------------------------------------------------------------- *)

let pattern e =
  let buffer = Buffer.create 32 in
  (try
     iter (fun e ->
         Buffer.add_string buffer (head e) ;
         if Buffer.length buffer >= 32 then raise Exit)
       e
   with Exit -> ()) ;
  Buffer.contents buffer

(* -------------------------------------------------------------------------- *)
(* --- Term Matching                                                      --- *)
(* -------------------------------------------------------------------------- *)

let _prefix m k m' =
  let n = String.length m in
  let n' = String.length m' in
  k+n' <= n &&
    (try for i = 0 to n'-1 do
         if m.[k+i] != m'.[i] then raise Exit
       done ; true
     with Exit -> false)

let matches fp e =
  let fe = pattern e in
  (*TODO: implement partial mach*)
  fp = fe

(* -------------------------------------------------------------------------- *)
(* --- Occurrence                                                         --- *)
(* -------------------------------------------------------------------------- *)

type occurrence = int * string
exception Located of occurrence
exception Found of F.term

let locate ~select ~inside =
  let k = ref 0 in
  let m = pattern select in
  try
    once
      (fun e ->
         if e == select then raise (Located(!k,m)) ;
         if matches m e then incr k ;
      ) inside ;
    raise Not_found
  with Located fp -> fp

let lookup ~occur ~inside =
  let k = ref (succ (fst occur)) in
  let m = snd occur in
  try
    once
      (fun e ->
         if matches m e then decr k ;
         if !k = 0 then raise (Found e) ;
      ) inside ;
    raise Not_found
  with Found e -> e
