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

(*--- Typechecking ---*)

let a_list = Lang.builtin_type ~library ~name:t_list ~link:l_list

let ty_nil = function _ -> L.Data(a_list,[L.Tvar 0])

let ty_listelt = function
  | L.Data(_,[t]) -> (t : tau)
  | _ -> raise Not_found

let ty_cons = function
  | [ _ ; Some l ] -> l
  | [ Some e ; _ ] -> L.Data(a_list,[e])
  | _ -> raise Not_found

let ty_elt = function
  | [ Some e ] -> L.Data(a_list,[e])
  | _ -> raise Not_found

let ty_nth = function
  | Some l :: _ -> ty_listelt l
  | _ -> raise Not_found

let rec ty_concat = function
  | Some l :: _ -> l
  | None :: w -> ty_concat w
  | [] -> raise Not_found 

let ty_repeat = function
  | Some l :: _ -> l
  | _ -> raise Not_found

(*--- Qed Symbols ---*)

let f_cons = Lang.extern_f ~library ~typecheck:ty_cons "cons" (* rewriten in concat(elt) *)
let f_nil  = Lang.extern_f ~library ~typecheck:ty_nil ~category:L.Constructor "nil"
let f_elt = Lang.extern_f ~library ~category:L.Constructor ~typecheck:ty_elt ~link:l_elt "elt"

let concatenation = L.(Operator {
    invertible = true ;
    associative = true ;
    commutative = false ;
    idempotent = false ;
    neutral = E_const f_nil ;
    absorbant = E_none ;
  })

let f_nth    = Lang.extern_f ~library ~typecheck:ty_nth "nth"
let f_length = Lang.extern_f ~library ~sort:L.Sint "length"
let f_concat = Lang.extern_f ~library ~category:concatenation ~typecheck:ty_concat ~link:l_concat "concat"
let f_repeat = Lang.extern_f ~library ~typecheck:ty_repeat ~link:l_repeat "repeat" 

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

let v_nil = F.constant (F.e_fun f_nil [])
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
  | L.Fun( elt , [_] ) when elt == f_elt -> F.e_one
  | L.Fun( concat , es ) when concat == f_concat ->
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
  if F.equal s v_nil then v_nil else
    match F.repr s with
    | L.Fun( repeat , [s0 ; n0] )
      when (repeat == f_repeat) &&
           (Cint.is_positive_or_null n) &&
           (Cint.is_positive_or_null n0) -> v_repeat s0 (F.e_mul n0 n)
    | _ -> raise Not_found
             
let rec leftmost a ms =
  match F.repr a with
  | L.Fun( concat , e :: es ) when concat == f_concat ->
      leftmost e (es@ms)
  | L.Fun( repeat , [ u ; n ] ) when repeat == f_repeat && Cint.is_positive_or_null n ->
      leftmost u (v_repeat u (F.e_sub n F.e_one) :: ms)
  | _ -> a , ms

let rec rightmost ms a =
  match F.repr a with
  | L.Fun( concat , es ) when concat == f_concat ->
      begin match List.rev es with
        | [] -> ms , a
        | e::es -> rightmost (ms @ List.rev es) e
      end
  | L.Fun( repeat , [ u ; n ] ) when repeat == f_repeat && Cint.is_positive_or_null n ->
      rightmost (ms @ [v_repeat u (F.e_sub n F.e_one)]) u
  | _ -> ms , a

let leftmost_eq a b =
  let a , u = leftmost a [] in
  let b , v = leftmost b [] in
  if u <> [] || v <> [] then
    match F.is_equal a b with
    | L.Yes -> F.p_equal (v_concat u) (v_concat v)
    | L.No -> F.p_false
    | L.Maybe -> raise Not_found
  else
    raise Not_found

let rightmost_eq a b =
  let u , a = rightmost [] a in
  let v , b = rightmost [] b in
  if u <> [] || v <> [] then
    match F.is_equal a b with
    | L.Yes -> F.p_equal (v_concat u) (v_concat v)
    | L.No -> F.p_false
    | L.Maybe -> raise Not_found
  else
    raise Not_found

let p_is_nil a = F.p_equal a v_nil
let rewrite_is_nil a =
  match F.repr a with
  | L.Fun(concat,es) when concat == f_concat -> F.p_all p_is_nil es
  | L.Fun(elt,[_]) when elt == f_elt -> F.p_false
  | L.Fun(repeat,[u;n]) when repeat == f_repeat ->
      F.p_or (F.p_leq n F.e_zero) (p_is_nil u)
  | _ -> raise Not_found

let rewrite_eq a b =
  match F.repr a , F.repr b with
  | L.Fun(nil,[]) , _ when nil == f_nil -> rewrite_is_nil b
  | _ , L.Fun(nil,[]) when nil == f_nil -> rewrite_is_nil a
  | _ ->
      try leftmost_eq a b with Not_found -> rightmost_eq a b

let () =
  Context.register
    begin fun () ->
      F.set_builtin_2 f_nth rewrite_nth ;
      F.set_builtin_2 f_cons rewrite_cons ;
      F.set_builtin_2 f_repeat rewrite_repeat ;
      F.set_builtin_1 f_length rewrite_length ;
      F.set_builtin_eqp f_repeat rewrite_eq ;
      F.set_builtin_eqp f_nil rewrite_eq ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Typing                                                             --- *)
(* -------------------------------------------------------------------------- *)

let f_list = [ f_nil ; f_cons ; f_elt ; f_repeat ; f_concat ]

let check_tau = Lang.is_builtin_type ~name:t_list

let check_term e =
  try match F.repr e with
  | L.Fvar x -> check_tau (F.tau_of_var x)
  | L.Bvar(_,t) -> check_tau t
    | L.Fun( f , _ ) -> List.memq f f_list || check_tau (Lang.F.typeof e)
  | _ -> false
  with Not_found -> false

(* -------------------------------------------------------------------------- *)
(* --- Export                                                             --- *)
(* -------------------------------------------------------------------------- *)

class type engine =
  object
    method callstyle : Qed.Engine.callstyle
    method pp_atom : Format.formatter -> Lang.F.term -> unit
    method pp_flow : Format.formatter -> Lang.F.term -> unit
  end

let rec export (engine : #engine) fmt = function
  | [] ->
      begin match engine#callstyle with
        | E.CallVoid -> Format.pp_print_string fmt "nil()"
        | E.CallVar|E.CallApply -> Format.pp_print_string fmt "nil"
      end
  | e::es ->
      begin match F.repr e with
        | L.Fun( elt , [x] ) when elt == f_elt ->
            apply engine fmt "cons" x es
        | _ ->
            apply engine fmt "concat" e es
      end

and apply (engine : #engine) fmt f x es =
  match engine#callstyle with
  | E.CallVar | E.CallVoid ->
      Format.fprintf fmt "@[<hov 2>%s(@,%a,@,%a)@]"
        f engine#pp_flow x (export engine) es
  | E.CallApply ->
      Format.fprintf fmt "@[<hov 2>(%s@ %a@ %a)@]"
        f engine#pp_atom x (export engine) es
  
(* -------------------------------------------------------------------------- *)

let rec collect xs = function
  | [] -> List.rev xs , []
  | (e::es) as w ->
      begin match F.repr e with
        | L.Fun( elt , [x] ) when elt == f_elt -> collect (x::xs) es
        | _ -> List.rev xs , w
      end

let list engine fmt xs = Qed.Plib.pp_listsep ~sep:"," engine#pp_flow fmt xs

let elements (engine : #engine) fmt xs =
  Format.fprintf fmt "@[<hov 2>[ %a ]@]" (list engine) xs

let rec pp_concat (engine : #engine) fmt es =
  let xs , es = collect [] es in
  begin
    (if xs <> [] then elements engine fmt xs) ;
    match es with
    | [] -> ()
    | m::ms ->
        if xs <> [] then Format.fprintf fmt " ^@ " ;
        engine#pp_atom fmt m ;
        if ms <> [] then
          ( Format.fprintf fmt " ^@ " ; pp_concat engine fmt ms )
  end

let pretty (engine : #engine) fmt es =
  if es = [] then Format.pp_print_string fmt "[]" else
    Format.fprintf fmt "@[<hov 2>%a@]" (pp_concat engine) es

let pprepeat (engine : #engine) fmt = function
  | [l;n] -> Format.fprintf fmt "@[<hov 2>(%a *^@ %a)@]" engine#pp_flow l engine#pp_flow n
  | es -> Format.fprintf fmt "@[<hov 2>repeat(%a)@]" (list engine) es

let shareable e =
  match F.repr e with
  | L.Fun( f , es ) -> f != f_elt && es != []
  | _ -> true

(* -------------------------------------------------------------------------- *)
