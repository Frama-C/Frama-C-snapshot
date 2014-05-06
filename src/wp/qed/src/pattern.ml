(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Logic

(* -------------------------------------------------------------------------- *)
(* --- Pattern Matching                                                   --- *)
(* -------------------------------------------------------------------------- *)

type ('z,'f,'e) fpattern =
  | Ptrue
  | Pfalse
  | Pint   of 'z
  | Pvar   of int
  | Pguard of int * ('e -> bool)
  | Pfun   of 'f * ('z,'f,'e) fpattern list

let rec alloc n = function
  | Pvar x | Pguard(x,_) -> max n (succ x)
  | Pint _ | Ptrue | Pfalse -> n
  | Pfun(_,ps) -> alloc_list n ps
and alloc_list n ps = List.fold_left alloc n ps

let size p = alloc 0 p
let size_list ps = alloc_list 0 ps

module Make(T : Term) =
struct

  type pattern = (T.Z.t,T.Fun.t,T.t) fpattern

  let assign s i e = match s.(i) with
    | None -> s.(i) <- Some e
    | Some e0 -> if e!=e0 then raise Not_found

  let rec unify_all s ps es = match ps , es with
    | [] , [] -> ()
    | [] , _ | _ , [] -> raise Not_found
    | p::ps , e::es -> unify s p e ; unify_all s ps es

  and unify s p e = 
    match p , T.repr e with
    | Pvar k , _ -> assign s k e
    | Pguard(k,f) , _ when f e -> assign s k e
    | Pint z , Kint c when T.Z.equal z c -> ()
    | Ptrue , True -> ()
    | Pfalse , False -> ()
    | Pfun(f,ps) , Fun(g,es) when T.Fun.equal f g -> unify_all s ps es
    | _ -> raise Not_found

  let extract = function Some e -> e | None -> raise Not_found

  let pmatch p e =
    let s = Array.create (size p) None in
    unify s p e ; Array.map extract s

  let pmatch_all ps es =
    let s = Array.create (size_list ps) None in
    unify_all s ps es ; Array.map extract s

  let rec pretty fmt = function
    | Pvar k -> Format.fprintf fmt "#%d" k
    | Pguard(k,_) -> Format.fprintf fmt "#%d?" k
    | Pint z -> T.Z.pretty fmt z
    | Ptrue -> Format.fprintf fmt "true"
    | Pfalse -> Format.fprintf fmt "false"
    | Pfun(f,ps) -> Plib.pp_call_var (T.Fun.debug f) pretty fmt ps

end
