(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- VList Builtins                                                     --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F
module L = Qed.Logic
module E = Qed.Engine

(* -------------------------------------------------------------------------- *)
(* --- Driver                                                             --- *)
(* -------------------------------------------------------------------------- *)

let library = "vlist"

(*--- Linked Symbols ---*)

let t_list = "\\list"
let l_list = Lang.infoprover "list"
let l_concat = Lang.infoprover (E.F_right "concat")
let l_elt = Lang.(E.({
    altergo = F_subst "cons(%1,nil)" ;
    why3 = F_subst "(cons %1 nil)" ;
    coq = F_subst "(cons %1 nil)" ;
  }))
let l_repeat = Lang.(E.({
    altergo = F_call "repeat_box" ;
    why3 = F_call "repeat" ;
    coq = F_call "repeat" ;
  }))

(*--- Qed Symbols ---*)

let f_cons = Lang.extern_f ~library "cons" (* rewriten in concat(elt) *)
let f_nil  = Lang.extern_f ~library ~category:L.Constructor "nil"
let f_elt = Lang.extern_f ~library ~category:L.Constructor ~link:l_elt "elt"

let concatenation = L.(Operator {
    inversible = true ;
    associative = true ;
    commutative = false ;
    idempotent = false ;
    neutral = E_const f_nil ;
    absorbant = E_none ;
  })

let f_nth    = Lang.extern_f ~library "nth"
let f_length = Lang.extern_f ~library ~sort:L.Sint "length"
let f_concat = Lang.extern_f ~library ~category:concatenation ~link:l_concat "concat"
let f_repeat = Lang.extern_f ~library "repeat" ~link:l_repeat

(*--- ACSL Builtins ---*)

let () =
  let open LogicBuiltins in
  begin
    add_type t_list ~library ~link:l_list () ;
    add_builtin "\\Nil" [] f_nil ;
    add_builtin "\\Cons" [A;A] f_cons ;
    add_builtin "\\nth" [A;Z] f_nth ;
    add_builtin "\\length" [A] f_length ;
    add_builtin "\\concat" [A;A] f_concat ;
    add_builtin "\\repeat" [A;Z] f_repeat ;
  end

(*--- Smart Constructors ---*)

let v_nil = F.e_fun f_nil []
let v_elt e = F.e_fun f_elt [e]
let v_concat es = F.e_fun f_concat es
let v_length l = F.e_fun f_length [l]
let v_repeat s n = F.e_fun f_repeat [s;n]

(* -------------------------------------------------------------------------- *)
(* --- Rewriters                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rewrite_cons a w = v_concat [v_elt a ; w]

let rewrite_length e =
  match F.repr e with
  | L.Fun( nil , [] ) when nil == f_nil -> F.e_zero
  | L.Fun( elt , [_] ) when elt = f_elt -> F.e_one
  | L.Fun( concat , es ) when concat = f_concat ->
      F.e_sum (List.map v_length es)
  | _ -> raise Not_found

let rec get_nth k e =
  match F.repr e with
  | L.Fun( concat , list ) when concat == f_concat -> get_nth_list k list
  | L.Fun( elt , [x] ) when elt == f_elt && k = 0 -> x
  | _ -> raise Not_found

and get_nth_list k = function
  | head::tail ->
      begin
        match F.repr head with
        | L.Fun( elt , [x] ) when elt == f_elt ->
            if k = 0 then x else get_nth_list (k-1) tail
        | _ -> raise Not_found
      end
  | [] -> raise Not_found

let rewrite_nth s k =
  match F.repr k with
  | L.Kint z ->
      let k = try Integer.to_int z with _ -> raise Not_found in
      if 0 <= k then get_nth k s else raise Not_found
  | _ -> raise Not_found

let rewrite_repeat s n =
  if F.equal n e_zero then v_nil else
  if F.equal n e_one then s else
  if Cint.is_positive_or_null n then
    match F.repr s with
    | L.Fun( repeat , [s0 ; n0] ) when (repeat = f_repeat) &&
                                       (Cint.is_positive_or_null n0) ->
        v_repeat s0 (F.e_mul n0 n)
    | _ when F.equal s v_nil -> v_nil
    | _ -> raise Not_found
  else raise Not_found

let () =
  begin
    F.set_builtin_2 f_nth rewrite_nth ;
    F.set_builtin_2 f_cons rewrite_cons ;
    F.set_builtin_2 f_repeat rewrite_repeat ;
    F.set_builtin_1 f_length rewrite_length ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Typing                                                             --- *)
(* -------------------------------------------------------------------------- *)

let f_list = [ f_nil ; f_cons ; f_elt ; f_repeat ; f_concat ]

let check_tau = Lang.is_builtin_type ~name:t_list

let rec check_term e = match F.repr e with
  | L.Fvar x -> check_tau (F.tau_of_var x)
  | L.Bvar(_,t) -> check_tau t
  | L.Fun( f , _ ) ->
      List.memq f f_list ||
      (try check_tau (Lang.tau_of_lfun f) with Not_found -> false)
  | _ -> false

(* -------------------------------------------------------------------------- *)
(* --- Export                                                             --- *)
(* -------------------------------------------------------------------------- *)

class type engine =
  object
    method callstyle : Qed.Engine.callstyle
    method pp_atom : Format.formatter -> Lang.F.term -> unit
    method pp_flow : Format.formatter -> Lang.F.term -> unit
  end

let rec pp_concat (engine : #engine) fmt = function
  | [] ->
      begin match engine#callstyle with
        | E.CallVoid -> Format.pp_print_string fmt "nil()"
        | E.CallVar|E.CallApply -> Format.pp_print_string fmt "nil"
      end
  | e::es ->
      begin match F.repr e with
        | L.Fun( elt , [x] ) when elt == f_elt ->
            pp_apply engine fmt "cons" x es
        | _ ->
            pp_apply engine fmt "concat" e es
      end

and pp_apply (engine : #engine) fmt f x es =
  match engine#callstyle with
  | E.CallVar | E.CallVoid ->
      Format.fprintf fmt "@[<hov 2>%s(@,%a,@,%a)@]"
        f engine#pp_flow x (pp_concat engine) es
  | E.CallApply ->
      Format.fprintf fmt "@[<hov 2>(%s@ %a@ %a)@]"
        f engine#pp_atom x (pp_concat engine) es

(* -------------------------------------------------------------------------- *)
