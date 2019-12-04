(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* --- Integer Arithmetics Model                                          --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Qed.Logic
open Lang
open Lang.F
module FunMap = FCMap.Make(Lang.Fun)

(* -------------------------------------------------------------------------- *)
(* --- Kernel Interface                                                   --- *)
(* -------------------------------------------------------------------------- *)

let is_overflow_an_error iota =
  if Ctypes.signed iota
  then Kernel.SignedOverflow.get ()
  else Kernel.UnsignedOverflow.get ()

let is_downcast_an_error iota =
  if Ctypes.signed iota
  then Kernel.SignedDowncast.get ()
  else Kernel.UnsignedDowncast.get ()

(* -------------------------------------------------------------------------- *)
(* --- Library Cint                                                       --- *)
(* -------------------------------------------------------------------------- *)

let is_cint_map = ref FunMap.empty
let to_cint_map = ref FunMap.empty

let is_cint f = FunMap.find f !is_cint_map
let to_cint f = FunMap.find f !to_cint_map

let library = "cint"

let make_fun_int op i =
  Lang.extern_f ~library ~result:Logic.Int "%s_%a" op Ctypes.pp_int i
let make_pred_int op i =
  Lang.extern_f ~library ~result:Logic.Prop "%s_%a" op Ctypes.pp_int i

(* let fun_int op = Ctypes.imemo (make_fun_int op) *) (* unused for now *)
(* let pred_int op = Ctypes.imemo (make_pred_int op) *) (* unused for now *)

(* Signature int,int -> int over Z *)
let ac = {
  associative = true ;
  commutative = true ;
  idempotent = false ;
  invertible = false ;
  neutral = E_none ;
  absorbant = E_none ;
}

(* Functions -> Z *)
let result = Logic.Int

(* -------------------------------------------------------------------------- *)
(* --- Library Cbits                                                      --- *)
(* -------------------------------------------------------------------------- *)

let library = "cbits"
let balance = Lang.Left

let op_lxor = { ac with neutral = E_int 0 ; invertible = true }
let op_lor  = { ac with neutral = E_int 0 ; absorbant = E_int (-1); idempotent = true }
let op_land = { ac with neutral = E_int (-1); absorbant = E_int 0 ; idempotent = true }

let f_lnot = Lang.extern_f ~library ~result "lnot"
let f_lor  = Lang.extern_f ~library ~result ~category:(Operator op_lor) ~balance "lor"
let f_land = Lang.extern_f ~library ~result ~category:(Operator op_land) ~balance "land"
let f_lxor = Lang.extern_f ~library ~result ~category:(Operator op_lxor) ~balance "lxor"
let f_lsl = Lang.extern_f ~library ~result "lsl"
let f_lsr = Lang.extern_f ~library ~result "lsr"

let f_bitwised = [ f_lnot ; f_lor ; f_land ; f_lxor ; f_lsl ; f_lsr ]

(* [f_bit_stdlib] is related to the function [bit_test] of Frama-C StdLib *)
let f_bit_stdlib   = Lang.extern_p ~library ~bool:"bit_testb" ~prop:"bit_test" ()
(* [f_bit_positive] is actually exported in forgoting the fact the position is positive *)
let f_bit_positive = Lang.extern_p ~library ~bool:"bit_testb" ~prop:"bit_test" ()
(* At export, some constructs such as [e & (1 << k)] are written into [f_bit_export] construct *)
let f_bit_export   = Lang.extern_p ~library ~bool:"bit_testb" ~prop:"bit_test" ()

let () = let open LogicBuiltins in add_builtin "\\bit_test_stdlib" [Z;Z] f_bit_stdlib
let () = let open LogicBuiltins in add_builtin "\\bit_test" [Z;Z] f_bit_positive

(* -------------------------------------------------------------------------- *)
(* --- Matching utilities for simplifications                             --- *)
(* -------------------------------------------------------------------------- *)

let is_leq a b = F.is_true (F.e_leq a b)

let match_integer t =
  match F.repr t with
  | Logic.Kint c -> c
  | _ -> raise Not_found

(* integration with qed should be improved! *)
let rec is_positive_or_null e = match F.repr e with
  | Logic.Fun( f , [e] ) when Fun.equal f f_lnot -> is_negative e
  | Logic.Fun( f , es ) when Fun.equal f f_land  -> List.exists is_positive_or_null es
  | Logic.Fun( f , es ) when Fun.equal f f_lor   -> List.for_all is_positive_or_null es
  | Logic.Fun( f , es ) when Fun.equal f f_lxor   -> (match xor_sign es with | Some b -> b | _ -> false)
  | Logic.Fun( f , es ) when Fun.equal f f_lsr || Fun.equal f f_lsl
    -> List.for_all is_positive_or_null es
  | _ -> (* try some improvement first then ask to qed *)
      let improved_is_positive_or_null e = match F.repr e with
        | Logic.Add es -> List.for_all is_positive_or_null es
        | _ -> false
      in if improved_is_positive_or_null e then true
      else match F.is_true (F.e_leq e_zero e) with
        | Logic.Yes -> true
        | Logic.No | Logic.Maybe -> false
and is_negative e = match F.repr e with
  | Logic.Fun( f , [e] ) when Fun.equal f f_lnot -> is_positive_or_null e
  | Logic.Fun( f , es ) when Fun.equal f f_lor  -> List.exists is_negative es
  | Logic.Fun( f , es ) when Fun.equal f f_land -> List.for_all is_negative es
  | Logic.Fun( f , es ) when Fun.equal f f_lxor  -> (match xor_sign es with | Some b -> (not b) | _ -> false)
  | Logic.Fun( f , [k;n] ) when Fun.equal f f_lsr || Fun.equal f f_lsl
    -> is_positive_or_null n && is_negative k
  | _ -> (* try some improvement first then ask to qed *)
      let improved_is_negative e = match F.repr e with
        | Logic.Add es -> List.for_all is_negative es
        | _ -> false
      in if improved_is_negative e then true
      else match F.is_true (F.e_lt e e_zero) with
        | Logic.Yes -> true
        | Logic.No | Logic.Maybe -> false
and xor_sign es = try
    Some (List.fold_left (fun acc e ->
        if is_positive_or_null e then acc (* as previous *)
        else if is_negative e then (not acc) (* opposite sign *)
        else raise Not_found) true es)
  with Not_found -> None

let match_positive_or_null e =
  if not (is_positive_or_null e) then raise Not_found;
  e

let match_power2, _match_power2_minus1 =
  let highest_bit_number =
    let hsb p = if p land 2 = 0 then 0 else 1
    in let hsb p = let n = p lsr 2 in if n = 0 then hsb p else 2 + hsb n
    in let hsb p = let n = p lsr 4 in if n = 0 then hsb p else 4 + hsb n
    in let hsb = Array.init 256 hsb
    in let hsb p = let n = p lsr 8 in if n = 0 then hsb.(p) else 8 + hsb.(n)
    in let hsb p =
         let n = Integer.shift_right p Integer.sixteen in
         Integer.of_int (if Integer.is_zero n
                         then hsb (Integer.to_int p)
                         else 16 + hsb (Integer.to_int n))
    in let rec hsb_aux p =
         let n = Integer.shift_right p Integer.thirtytwo in
         if Integer.is_zero n then hsb p
         else Integer.add Integer.thirtytwo (hsb_aux n)
    in hsb_aux
  in let is_power2 k = (* exists n such that k == 2**n? *)
       (Integer.gt k Integer.zero) &&
       (Integer.equal k (Integer.logand k (Integer.neg k)))
  in let rec match_power2 e = match F.repr e with
      | Logic.Kint z
        when is_power2 z -> e_zint (highest_bit_number z)
      | Logic.Fun( f , [n;k] )
        when Fun.equal f f_lsl && is_positive_or_null k ->
          e_add k (match_power2 n)
      | _ -> raise Not_found
  in let match_power2_minus1 e = match F.repr e with
      | Logic.Kint z when is_power2 (Integer.succ z) ->
          e_zint (highest_bit_number (Integer.succ z))
      | _ -> raise Not_found
  in match_power2, match_power2_minus1

let match_fun op t =
  match F.repr t with
  | Logic.Fun( f , es ) when Fun.equal f op -> es
  | _ -> raise Not_found

let match_ufun uop t =
  match F.repr t with
  | Logic.Fun( f , e::[] ) when Fun.equal f uop -> e
  | _ -> raise Not_found

let match_positive_or_null_integer t =
  match F.repr t with
  | Logic.Kint c when Integer.le Integer.zero c -> c
  | _ -> raise Not_found

let match_binop_arg1 match_f = function (* for binop *)
  | [e1;e2] -> (match_f e1),e2
  | _ -> raise Not_found

let match_binop_arg2 match_f = function (* for binop *)
  | [e1;e2] -> e1,(match_f  e2)
  | _ -> raise Not_found

let match_list_head match_f = function
  | [] -> raise Not_found
  | e::es -> (match_f e), es

let match_binop_one_arg1 binop e = match F.repr e with
  | Logic.Fun( f , [one; e2] ) when Fun.equal f binop && one == e_one -> e2
  | _ -> raise Not_found

let match_list_extraction match_f =
  let match_f_opt n = try Some (match_f n) with Not_found -> None in
  let rec aux rs = function
    | [] -> raise Not_found
    | e::es ->
        match match_f_opt e with
        | Some k -> k, e, List.rev_append rs es
        | None -> aux (e::rs) es
  in aux []

let match_integer_arg1 = match_binop_arg1 match_integer

let match_positive_or_null_arg2 = match_binop_arg2 match_positive_or_null
let match_positive_or_null_integer_arg2 =
  match_binop_arg2 match_positive_or_null_integer

let match_integer_extraction = match_list_head match_integer

let match_power2_extraction = match_list_extraction match_power2
let match_binop_one_extraction binop = match_list_extraction (match_binop_one_arg1 binop)


(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

(* rule A: to_a(to_b x) = to_b x when domain(b) is all included in domain(a) *)
(* rule B: to_a(to_b x) = to_a x when range(b) is a multiple of range(a)
                          AND a is not bool *)

(* to_iota(e) where e = to_iota'(e'), only ranges for iota *)
let simplify_range_comp f iota e conv e' =
  let iota' = to_cint conv in
  let size' = Ctypes.i_bits iota' in
  let size = Ctypes.i_bits iota in
  if size <= size'
  then e_fun f [e']
  (* rule B:
     iota' is multiple of iota -> keep iota(e') *)
  else
  if ((Ctypes.signed iota) || not (Ctypes.signed iota'))
  then e
  (* rule A:
     have iota > iota' check sign to apply rule.
     unsigned iota -> iota' must be unsigned
     signed iota -> iota' can have any sign *)
  else raise Not_found

let simplify_f_to_bounds iota e =
  (* min(ctypes)<=y<=max(ctypes) ==> to_ctypes(y)=y *)
  let lower,upper = Ctypes.bounds iota in
  if (F.decide (F.e_leq e (e_zint upper))) &&
     (F.decide (F.e_leq (e_zint lower) e))
  then e
  else raise Not_found

let f_to_int = Ctypes.i_memo (fun iota -> make_fun_int "to" iota)

let configure_to_int iota =

  let simplify_range f iota e =
    begin
      try match F.repr e with
        | Logic.Kint value ->
            let size = Integer.of_int (Ctypes.i_bits iota) in
            let signed = Ctypes.signed iota in
            F.e_zint (Integer.cast ~size ~signed ~value)
        | Logic.Fun( fland , es )
          when Fun.equal fland f_land &&
               not (Ctypes.signed iota) &&
               List.exists is_positive_or_null es ->
            (* to_uintN(a) == a & (2^N-1) when a >= 0 *)
            let m = F.e_zint (snd (Ctypes.bounds iota)) in
            F.e_fun f_land (m :: es)
        | Logic.Fun( flor , es ) when (Fun.equal flor f_lor)
                                   && not (Ctypes.signed iota) ->
            (* to_uintN(a|b) == (to_uintN(a) | to_uintN(b)) *)
            F.e_fun f_lor (List.map (fun e' -> e_fun f [e']) es)
        | Logic.Fun( flnot , [ e ] ) when (Fun.equal flnot f_lnot)
                                       && not (Ctypes.signed iota) ->
            begin
              match F.repr e with
              | Logic.Fun( f' , w ) when f' == f ->
                  e_fun f [ e_fun f_lnot w ]
              | _ -> raise Not_found
            end
        | Logic.Fun( conv , [e'] ) -> (* unary op *)
            simplify_range_comp f iota e conv e'
        | _ -> raise Not_found
      with Not_found ->
        simplify_f_to_bounds iota e
    end
  in
  let simplify_conv f iota e =
    if iota = Ctypes.CBool then
      match F.is_equal e F.e_zero with
      | Yes -> F.e_zero
      | No -> F.e_one
      | Maybe -> raise Not_found
    else
      simplify_range f iota e
  in
  let simplify_leq f iota x y =
    let lower,upper = Ctypes.bounds iota in
    match F.repr y with
    | Logic.Fun( conv , [_] )
      when (Fun.equal conv f) &&
           (F.decide (F.e_leq x (e_zint lower))) ->
        (* x<=min(ctypes) ==> x<=to_ctypes(y) *)
        e_true
    | _ ->
        begin
          match F.repr x with
          | Logic.Fun( conv , [_] )
            when (Fun.equal conv f) &&
                 (F.decide (F.e_leq (e_zint upper) y)) ->
              (* max(ctypes)<=y ==> to_ctypes(y)<=y *)
              e_true
          | _ -> raise Not_found
        end
  in
  let f = f_to_int iota in
  F.set_builtin_1 f (simplify_conv f iota) ;
  F.set_builtin_leq f (simplify_leq f iota) ;
  to_cint_map := FunMap.add f iota !to_cint_map


let simplify_p_is_bounds iota e =
  let bounds = Ctypes.bounds iota in
  (* min(ctypes)<=y<=max(ctypes) <==> is_ctypes(y) *)
  match F.is_true (F.e_and
                     [F.e_leq (e_zint (fst bounds)) e;
                      F.e_leq e (e_zint (snd bounds))]) with
  | Logic.Yes -> e_true
  | Logic.No  -> e_false
  | _  -> raise Not_found

(* is_<cint> : int -> prop *)
let p_is_int = Ctypes.i_memo (fun iota -> make_pred_int "is" iota)

let configure_is_int iota =
  let f = p_is_int iota in
  let simplify = function
    | [e] ->
        begin
          match F.repr e with
          | Logic.Kint k ->
              let vmin,vmax = Ctypes.bounds iota in
              F.e_bool (Z.leq vmin k && Z.leq k vmax)
          | Logic.Fun( flor , es ) when (Fun.equal flor f_lor)
                                     && not (Ctypes.signed iota) ->
              (* is_uintN(a|b) == is_uintN(a) && is_uintN(b) *)
              F.e_and (List.map (fun e' -> e_fun f [e']) es)
          | _ -> simplify_p_is_bounds iota e
        end
    | _ -> raise Not_found
  in
  F.set_builtin f simplify;
  is_cint_map := FunMap.add f iota !is_cint_map

let convert i a = e_fun (f_to_int i) [a]

(* -------------------------------------------------------------------------- *)

type model =
  | Natural (** Integer arithmetics with no upper-bound *)
  | Machine (** Integer/Module wrt Kernel options on RTE *)

let () =
  Context.register
    begin fun () ->
      Ctypes.i_iter configure_to_int;
      Ctypes.i_iter configure_is_int;
    end

let model = Context.create "Cint.model"
let current () = Context.get model
let configure = Context.set model

let to_integer a = a
let of_integer i a = convert i a
let of_real i a = convert i (Cmath.int_of_real a)

let range i a =
  match Context.get model with
  | Natural ->
      if Ctypes.signed i
      then F.p_true
      else F.p_leq F.e_zero a
  | Machine -> p_call (p_is_int i) [a]

let ensures warn i a =
  if warn i
  then
    (if Lang.has_gamma () && Wp_parameters.get_overflows ()
     then Lang.assume (range i a) ;
     a)
  else e_fun (f_to_int i) [a]

let downcast = ensures is_downcast_an_error
let overflow = ensures is_overflow_an_error

(* -------------------------------------------------------------------------- *)
(* --- Arithmetics                                                        --- *)
(* -------------------------------------------------------------------------- *)

let binop f i x y  = overflow i (f x y)
let unop f i x = overflow i (f x)

(* C Code Semantics *)
let iopp = unop e_opp
let iadd = binop e_add
let isub = binop e_sub
let imul = binop e_mul
let idiv = binop e_div
let imod = binop e_mod

(* -------------------------------------------------------------------------- *)
(* --- Bits                                                               --- *)
(* -------------------------------------------------------------------------- *)

(* smp functions raise Not_found when simplification isn't possible *)

let smp1 zf =  (* f(c1) ~> zf(c1) *)
  function
  | [e] -> begin match F.repr e with
      | Logic.Kint c1 -> e_zint (zf c1)
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

let smp2 f zf = (* f(c1,c2) ~> zf(c1,c2),  f(c1,c2,...) ~> f(zf(c1,c2),...) *)
  function
  | e1::e2::others -> begin match (F.repr e1), (F.repr e2) with
      (* integers should be at the beginning of the list *)
      | Logic.Kint c1, Logic.Kint c2 ->
          let z12 = ref (zf c1 c2) in
          let rec smp2 = function (* look at the other integers *)
            | [] -> []
            | (e::r) as l -> begin match F.repr e with
                | Logic.Kint c -> z12 := zf !z12 c; smp2 r
                | _ -> l
              end
          in let others = smp2 others
          in let c12 = e_zint !z12 in
          if others = [] || F.is_absorbant f c12
          then c12
          else if F.is_neutral f c12
          then e_fun f others
          else e_fun f (c12::others)
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

let bitk_positive k e = F.e_fun f_bit_positive [e;k]
let smp_mk_bit_stdlib = function
  | [ a ; k ] when is_positive_or_null k ->
      (* No need to expand the logic definition of the ACSL stdlib symbol when
         [k] is positive (the definition must comply with that simplification). *)
      bitk_positive k a
  | [ a ; k ] ->
      (* TODO: expand the current logic definition of the ACSL stdlib symbol *)
      F.e_neq F.e_zero (F.e_fun f_land [a; (F.e_fun f_lsl [F.e_one;k])])
  | _ -> raise Not_found

let smp_bitk_positive = function
  | [ a ; k ] -> (* requires k>=0 *)
      begin
        try e_eq (match_power2 a) k
        with Not_found ->
        match F.repr a with
        | Logic.Kint za ->
            let zk = match_integer k (* simplifies constants *)
            in if Integer.is_zero (Integer.logand za
                                     (Integer.shift_left Integer.one zk))
            then e_false else e_true
        | Logic.Fun( f , [e;n] ) when Fun.equal f f_lsr && is_positive_or_null n ->
            bitk_positive (e_add k n) e
        | Logic.Fun( f , [e;n] ) when Fun.equal f f_lsl && is_positive_or_null n ->
            begin match is_leq n k with
              | Logic.Yes -> bitk_positive (e_sub k n) e
              | Logic.No  -> e_false
              | Logic.Maybe -> raise Not_found
            end
        | Logic.Fun( f , es ) when Fun.equal f f_land ->
            F.e_and (List.map (bitk_positive k) es)
        | Logic.Fun( f , es ) when Fun.equal f f_lor ->
            F.e_or (List.map (bitk_positive k) es)
        | Logic.Fun( f , [a;b] ) when Fun.equal f f_lxor ->
            F.e_neq (bitk_positive k a) (bitk_positive k b)
        | Logic.Fun( f , [a] ) when Fun.equal f f_lnot ->
            F.e_not (bitk_positive k a)
        | Logic.Fun( conv , [a] ) (* when is_to_c_int conv *) ->
            let iota = to_cint conv in
            let range = Ctypes.i_bits iota in
            let signed = Ctypes.signed iota in
            if signed then (* beware of sign-bit *)
              begin match is_leq k (e_int (range-2)) with
                | Logic.Yes -> bitk_positive k a
                | Logic.No | Logic.Maybe -> raise Not_found
              end
            else begin match is_leq (e_int range) k with
              | Logic.Yes -> e_false
              | Logic.No -> bitk_positive k a
              | Logic.Maybe -> raise Not_found
            end
        | _ -> raise Not_found
      end
  | _ -> raise Not_found

let introduction_bit_test_positive es b =
  (* introduces bit_test(n,k) only when k>=0 *)
  let k,_,es = match_power2_extraction es in
  let es' = List.map (bitk_positive k) es in
  if b == e_zero then e_not (e_and es')
  else
    try let k' = match_power2 b in e_and ( e_eq k k' :: es' )
    with Not_found ->
      let bs = match_fun f_land b in
      let k',_,bs = match_power2_extraction bs in
      let bs' = List.map (bitk_positive k') bs in
      match F.is_true (F.e_eq k k') with
      | Logic.Yes -> e_eq (e_and es') (e_and bs')
      | Logic.No  -> e_and [e_not (e_and es'); e_not (e_and bs')]
      | Logic.Maybe -> raise Not_found

let smp_land es =
  let introduction_bit_test_positive_from_land es =
    if true then raise Not_found; (* [PB] true: until alt-ergo 0.95.2 trouble *)
    let k,e,es = match_power2_extraction es in
    let t = match es with
      | x::[] -> x
      | _ -> e_fun f_land es
    in e_if (bitk_positive k t) e e_zero
  in
  try let r = smp2 f_land Integer.logand es in
    try match F.repr r with
      | Logic.Fun( f , es ) when Fun.equal f f_land ->
          introduction_bit_test_positive_from_land es
      | _ -> r
    with Not_found -> r
  with Not_found -> introduction_bit_test_positive_from_land es

let smp_shift zf = (* f(e1,0)~>e1, c2>0==>f(c1,c2)~>zf(c1,c2), c2>0==>f(0,c2)~>0 *)
  function
  | [e1;e2] -> begin match (F.repr e1), (F.repr e2) with
      | _, Logic.Kint c2 when Z.equal c2 Z.zero -> e1
      | Logic.Kint c1, Logic.Kint c2 when Z.leq Z.zero c2 ->
          (* undefined when c2 is negative *)
          e_zint (zf c1 c2)
      | Logic.Kint c1, _ when Z.equal c1 Z.zero && is_positive_or_null e2 ->
          (* undefined when c2 is negative *)
          e1
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

let smp_leq_with_land a b =
  let es = match_fun f_land a in
  let a1,_ = match_list_head match_positive_or_null_integer es in
  if F.decide (F.e_leq (e_zint a1) b)
  then e_true
  else raise Not_found

let smp_eq_with_land a b =
  let es = match_fun f_land a in
  try
    let b1 = match_integer b in
    try (* (b1&~a2)!=0 ==> (b1==(a2&e) <=> false) *)
      let a2,_ = match_integer_extraction es in
      if Integer.is_zero (Integer.logand b1 (Integer.lognot a2))
      then raise Not_found ;
      e_false
    with Not_found when b == e_minus_one ->
      (* -1==(a1&a2) <=> (-1==a1 && -1==a2) *)
      F.e_and (List.map (e_eq b) es)
  with Not_found -> introduction_bit_test_positive es b

let smp_eq_with_lor a b =
  let b1 = match_integer b in
  let es = match_fun f_lor a in
  try (* b1==(a2|t22) <==> (b1^a2)==(~a2&e) *)
    let a2,es = match_integer_extraction es in
    let k1 = Integer.logxor b1 a2 in
    let k2 = Integer.lognot a2 in
    e_eq (e_zint k1) (e_fun f_land ((e_zint k2)::es))
  with Not_found when b == e_zero ->
    (* 0==(a1|a2) <=> (0==a1 && 0==a2) *)
    F.e_and (List.map (e_eq b) es)

let smp_eq_with_lxor a b = (* b1==(a2^e) <==> (b1^a2)==e *)
  let b1 = match_integer b in
  let es = match_fun f_lxor a in
  try (* b1==(a2^e) <==> (b1^a2)==e *)
    let a2,es = match_integer_extraction es in
    let k1 = Integer.logxor b1 a2 in
    e_eq (e_zint k1) (e_fun f_lxor es)
  with Not_found when b == e_zero  ->
    (* 0==(a1^a2) <=> (a1==a2) *)
    (match es with
     | e1::e2::[] -> e_eq e1 e2
     | e1::((_::_) as e22) -> e_eq e1 (e_fun f_lxor e22)
     | _ -> raise Not_found)
     | Not_found when b == e_minus_one ->
         (* -1==(a1^a2) <=> (a1==~a2) *)
         (match es with
          | e1::e2::[] -> e_eq e1 (e_fun f_lnot [e2])
          | e1::((_::_) as e22) -> e_eq e1 (e_fun f_lnot [e_fun f_lxor e22])
          | _ -> raise Not_found)

let smp_eq_with_lnot a b = (* b1==~e <==> ~b1==e *)
  let b1 = match_integer b in
  let e = match_ufun f_lnot a in
  let k1 = Integer.lognot b1 in
  e_eq (e_zint k1) e

let two_power_k_minus1 k =
  try Integer.pred (Integer.two_power k)
  with Z.Overflow -> raise Not_found

let smp_eq_with_lsl_cst a0 b0 =
  let b1 = match_integer b0 in
  let es = match_fun f_lsl a0 in
  try (* looks at the sd arg of a0 *)
    let e,a2= match_positive_or_null_integer_arg2 es in
    if not (Integer.is_zero (Integer.logand b1 (two_power_k_minus1 a2)))
    then
      (* a2>=0 && 0!=(b1 & ((2**a2)-1)) ==> ( (e<<a2)==b1 <==> false ) *)
      e_false
    else
      (* a2>=0 && 0==(b1 & ((2**a2)-1)) ==> ( (e<<a2)==b1 <==> e==(b1>>a2) ) *)
      e_eq e (e_zint (Integer.shift_right b1 a2))
  with Not_found -> (* looks at the fistt arg of a0 *)
    let a1,e= match_integer_arg1 es in
    if is_negative e then raise Not_found ;
    (* [PB] can be generalized to any term for a1 *)
    if Integer.le Integer.zero a1 && Integer.lt b1 a1 then
      (* e>=0 && 0<=a1 && b1<a1 ==> ( (a1<<e)==b1 <==> false ) *)
      e_false
    else if Integer.ge Integer.zero a1 && Integer.gt b1 a1 then
      (* e>=0 && 0>=a1 && b1>a1 ==> ( (a1<<e)==b1 <==> false ) *)
      e_false
    else raise Not_found

let smp_cmp_with_lsl cmp a0 b0 =
  if a0 == e_zero then
    let b,_ = match_fun f_lsl b0 |> match_positive_or_null_arg2 in
    cmp e_zero b (* q>=0 ==> ( (0 cmp(b<<q)) <==> (0 cmp b) ) *)
  else if b0 == e_zero then
    let a,_ = match_fun f_lsl a0 |> match_positive_or_null_arg2 in
    cmp a e_zero (* p>=0 ==> ( ((a<<p) cmp 0) <==> (a cmp 0) ) *)
  else
    let a,p = match_fun f_lsl a0 |> match_positive_or_null_arg2 in
    let b,q = match_fun f_lsl b0 |> match_positive_or_null_arg2 in
    if p == q then
      (* p>=0 && q>=0 && p==q ==> ( ((a<<p)cmp(b<<q)) <==> (a cmp b) ) *)
      cmp a b
    else if a == b && (cmp==e_eq || is_positive_or_null a) then
      (* p>=0 && q>=0 && a==b && a>=0 ==> ( ((a<<p)cmp(b<<q)) <==> (p cmp q) ) *)
      cmp p q
    else if a == b && is_negative a then
      (* p>=0 && q>=0 && a==b && a<0 ==> ( ((a<<p)<=(b<<q)) <==> (q cmp p) ) *)
      cmp q p
    else
      let p = match_integer p in
      let q = match_integer q in
      if Z.lt p q then
        (* p>=0 && q>=0 && p>q ==> ( ((a<<p)cmp(b<<q)) <==> (a cmp(b<<(q-p))) ) *)
        cmp a (e_fun f_lsl [b;e_zint (Z.sub q p)])
      else if Z.lt q p then
        (* p>=0 && q>=0 && p<q ==> ( ((a<<p)cmp(b<<q)) <==> ((a<<(p-q)) cmp b) ) *)
        cmp (e_fun f_lsl [a;e_zint (Z.sub p q)]) b
      else
        (* p>=0 && q>=0 && p==q ==> ( ((a<<p)cmp(b<<q)) <==> (a cmp b) ) *)
        cmp a b

let smp_eq_with_lsl a b =
  try smp_eq_with_lsl_cst a b
  with Not_found -> smp_cmp_with_lsl e_eq a b

let smp_leq_with_lsl a0 b0 = smp_cmp_with_lsl e_leq a0 b0

let mk_cmp_with_lsr_cst cmp e x2 x1 =
  (* build (e&~((2**x2)-1)) cmp (x1<<x2) *)
  cmp
    (e_zint (Integer.shift_left x1 x2))
    (e_fun f_land [e_zint (Integer.lognot (two_power_k_minus1 x2));e])

let smp_cmp_with_lsr cmp a0 b0 =
  try
    let b1 = match_integer b0 in
    let e,a2 = match_fun f_lsr a0 |> match_positive_or_null_integer_arg2 in
    (* (e>>a2) cmp b1 <==> (e&~((2**a2)-1)) cmp (b1<<a2)
       That rule is similar to
       e/A2 cmp b2 <==> (e/A2)*A2 cmp b2*A2) with A2==2**a2
       So, A2>0 and (e/A2)*A2 == e&~((2**a2)-1)
    *)
    mk_cmp_with_lsr_cst cmp e a2 b1
  with Not_found ->
    (* This rule takes into acount several cases.
       One of them is
       (a>>p) cmp (b>>(n+p)) <==> (a&~((2**p)-1)) cmp (b>>n)&~((2**p)-1)
       That rule is similar to
       (a/P)cmp(b/(N*P)) <==> (a/P)*P cmp ((b/N)/P)*P
       with P==2**p, N=2**n, q=p+n.
       So, (a/P)*P==a&~((2**p)-1), b/N==b>>n,  ((b/N)/P)*P==(b>>n)&~((2**p)-1)  *)
    let a,p = match_fun f_lsr a0 |> match_positive_or_null_integer_arg2 in
    let b,q = match_fun f_lsr b0 |> match_positive_or_null_integer_arg2 in
    let n = Integer.min p q in
    let a = if Integer.lt n p then e_fun f_lsr [a;e_zint (Z.sub p n)] else a in
    let b = if Integer.lt n q then e_fun f_lsr [b;e_zint (Z.sub q n)] else b in
    let m = F.e_zint (Integer.lognot (two_power_k_minus1 n)) in
    cmp (e_fun f_land [a;m]) (e_fun f_land [b;m])

let smp_eq_with_lsr a0 b0 =
  smp_cmp_with_lsr e_eq a0 b0

let smp_leq_with_lsr a0 b0 =
  try
    let bs = match_fun f_lsr b0 in
    if a0 == e_zero then
      let e,_ = match_positive_or_null_arg2 bs in
      (* b2>= 0 ==> (0<=(e>>b2) <==> 0<=e) (note: invalid for `e_eq`) *)
      e_leq e_zero e
    else
      let a1 = match_integer a0 in
      let e,b2 = match_positive_or_null_integer_arg2 bs in
      (* a1 <= (e>>b2) <==> (e&~((2**b2)-1)) >= (a1<<b2) *)
      mk_cmp_with_lsr_cst (fun a b -> e_leq b a) e b2 a1
  with Not_found ->
    if b0 == e_zero then
      let e,_ = match_fun f_lsr a0 |> match_positive_or_null_arg2 in
      (* a2>= 0 ==> ((e>>a2)<=0 <==> e<=0) (note: invalid for `e_eq`) *)
      e_leq e e_zero
    else
      smp_cmp_with_lsr e_leq a0 b0

(* Rewritting at export *)
let export_eq_with_land a b =
  let es = match_fun f_land a in
  if b == e_zero then
    let k,_,es = match_binop_one_extraction f_lsl es in
    (* e1 & ... & en & (1 << k) = 0   <==> !bit_test(e1 & ... & en, k) *)
    e_not (e_fun f_bit_export [e_fun f_land es ; k ])
  else raise Not_found

(* ACSL Semantics *)
type l_builtin = {
  f: lfun ;
  eq: (term -> term -> term) option ;
  leq: (term -> term -> term) option ;
  smp: term list -> term ;
}

let () =
  Context.register
    begin fun () ->
      if Wp_parameters.Bits.get () then
        begin
          let mk_builtin n f ?eq ?leq smp = n, { f ; eq; leq; smp } in

          (* From [smp_mk_bit_stdlib], the built-in [f_bit_stdlib] is such that there is
             no creation of [e_fun f_bit_stdlib args] *)
          let bi_lbit_stdlib = mk_builtin "f_bit_stdlib" f_bit_stdlib smp_mk_bit_stdlib in
          let bi_lbit = mk_builtin "f_bit" f_bit_positive smp_bitk_positive in
          let bi_lnot = mk_builtin "f_lnot" f_lnot ~eq:smp_eq_with_lnot
              (smp1 Integer.lognot) in
          let bi_lxor = mk_builtin "f_lxor" f_lxor ~eq:smp_eq_with_lxor
              (smp2 f_lxor Integer.logxor) in
          let bi_lor  = mk_builtin "f_lor" f_lor  ~eq:smp_eq_with_lor
              (smp2 f_lor  Integer.logor) in
          let bi_land = mk_builtin "f_land" f_land ~eq:smp_eq_with_land ~leq:smp_leq_with_land
              smp_land in
          let bi_lsl  = mk_builtin "f_lsl" f_lsl ~eq:smp_eq_with_lsl ~leq:smp_leq_with_lsl
              (smp_shift Integer.shift_left) in
          let bi_lsr  = mk_builtin "f_lsr" f_lsr ~eq:smp_eq_with_lsr ~leq:smp_leq_with_lsr
              (smp_shift Integer.shift_right) in

          List.iter
            begin fun (_name, { f; eq; leq; smp }) ->
              F.set_builtin f smp ;
              (match eq with
               | None -> ()
               | Some eq -> F.set_builtin_eq f eq);
              (match leq with
               | None -> ()
               | Some leq -> F.set_builtin_leq f leq)
            end
            [bi_lbit_stdlib ; bi_lbit; bi_lnot; bi_lxor; bi_lor; bi_land; bi_lsl; bi_lsr];

          Lang.For_export.set_builtin_eq f_land export_eq_with_land
        end
    end

(* ACSL Semantics *)
let l_not a   = e_fun f_lnot [a]
let l_xor a b = e_fun f_lxor [a;b]
let l_or  a b = e_fun f_lor  [a;b]
let l_and a b = e_fun f_land [a;b]
let l_lsl a b = e_fun f_lsl [a;b]
let l_lsr a b = e_fun f_lsr [a;b]

(* C Code Semantics *)

(* we need a (forced) conversion to properly encode
   the semantics of C in terms of the semantics in Z(ACSL).
   Typically, lnot(128) becomes (-129), which must be converted
   to obtain an unsigned. *)

let mask_unsigned i m =
  if Ctypes.signed i then m else convert i m

let bnot i x   = mask_unsigned i (l_not x)
let bxor i x y = mask_unsigned i (l_xor x y)

let bor _i  = l_or  (* no needs of range conversion *)
let band _i = l_and (* no needs of range conversion *)
let blsl i x y = overflow i (l_lsl x y) (* mult. by 2^y *)
let blsr _i = l_lsr (* div. by 2^y, never overflow *)

(** Simplifiers *)
let c_int_bounds_ival f  =
  let (umin,umax) = Ctypes.bounds f in
  Ival.inject_range (Some umin) (Some umax)

let max_reduce_quantifiers = 1000

module Dom = struct
  type t = Ival.t Tmap.t
  let is_top_ival = Ival.equal Ival.top

  let top = Tmap.empty

  [@@@ warning "-32"]
  let pretty fmt dom =
    Tmap.iter (fun k v ->
        Format.fprintf fmt "%a: %a,@ " Lang.F.pp_term k Ival.pretty v)
      dom

  let find t dom = Tmap.find t dom

  let get t dom = try find t dom with Not_found -> Ival.top

  let narrow t v dom =
    if Ival.is_bottom v then raise Lang.Contradiction
    else if is_top_ival v then dom
    else Tmap.change (fun _ v old ->
        match old with | None -> Some v
                       | (Some old) as old' ->
                           let v = Ival.narrow v old in
                           if Ival.is_bottom v then raise Lang.Contradiction;
                           if Ival.equal v old then old'
                           else Some v) t v dom

  let add t v dom =
    if Ival.is_bottom v then raise Lang.Contradiction;
    if is_top_ival v then dom else Tmap.add t v dom

  let remove t dom = Tmap.remove t dom

  let assume_cmp =
    let module Local = struct
      type t = Integer of Ival.t | Term of Ival.t option
    end in fun cmp t1 t2 dom ->
      let encode t = match Lang.F.repr t with
        | Kint z -> Local.Integer (Ival.inject_singleton z)
        | _ -> Local.Term (try Some (Tmap.find t dom) with Not_found -> None)
      in
      let term_dom = function
        | Some v -> v
        | None -> Ival.top
      in
      match encode t1, encode t2 with
      | Local.Integer cst1, Local.Integer cst2 -> (* assume cmp cst1 cst2 *)
          if Abstract_interp.Comp.False = Ival.forward_comp_int cmp cst1 cst2
          then raise Lang.Contradiction;
          dom
      | Local.Term None, Local.Term None ->
          dom (* nothing can be collected *)
      | Local.Term opt1, Local.Integer cst2 ->
          let v1 = term_dom opt1 in
          add t1 (Ival.backward_comp_int_left cmp v1 cst2) dom
      | Local.Integer cst1, Local.Term opt2 ->
          let v2 = term_dom opt2 in
          let cmp_sym = Abstract_interp.Comp.sym cmp in
          add t2 (Ival.backward_comp_int_left cmp_sym v2 cst1) dom
      | Local.Term opt1, Local.Term opt2 ->
          let v1 = term_dom opt1 in
          let v2 = term_dom opt2 in
          let cmp_sym = Abstract_interp.Comp.sym cmp in
          add t1 (Ival.backward_comp_int_left cmp v1 v2)
            (add t2 (Ival.backward_comp_int_left cmp_sym v2 v1) dom)

  let assume_literal t dom = match Lang.F.repr t with
    | Eq(a,b) -> assume_cmp Abstract_interp.Comp.Eq a b dom
    | Leq(a,b) -> assume_cmp Abstract_interp.Comp.Le a b dom
    | Lt(a,b) -> assume_cmp Abstract_interp.Comp.Lt a b dom
    | Fun(g,[a]) -> begin try
          let ubound =
            c_int_bounds_ival (is_cint g) (* may raise Not_found *) in
          narrow a ubound dom
        with Not_found -> dom
      end
    | Not p -> begin match Lang.F.repr p with
        | Fun(g,[a]) -> begin try (* just checks for a contraction *)
              let ubound =
                c_int_bounds_ival (is_cint g) (* may raise Not_found *) in
              let v = Tmap.find a dom (* may raise Not_found *) in
              if Ival.is_included v ubound then raise Lang.Contradiction;
              dom
            with Not_found -> dom
          end
        | _ -> dom
      end
    | _ -> dom
end

let is_cint_simplifier =
  let reduce_bound ~add_bonus quant v tv dom t : term =
    (** Returns [new_t] such that [c_bind quant (alpha,t)]
                           equals [c_bind quant v (alpha,new_t)]
        under the knowledge that  [(not t) ==> (var in dom)].
        Note: [~add_bonus] has not effect on the correctness of the transformation.
          It is a parameter that can be used in order to get better results.
        Bonus: Add additionnal hypothesis when we could deduce better constraint
        on the variable *)
    let module Tool = struct
      exception Stop
      exception Empty
      exception Unknown of Integer.t
      type t = { when_empty: unit -> term;
                 add_hyp: term list -> term -> term;
                 when_true: bool ref -> unit;
                 when_false: bool ref -> unit;
                 when_stop: unit -> term;
               }
    end in
    let tools = Tool.(match quant with
        | Forall -> { when_empty=(fun () -> e_true);
                      add_hyp =(fun hyps t -> e_imply hyps t);
                      when_true=(fun bonus -> bonus := add_bonus);
                      when_false=(fun _ -> raise Stop);
                      when_stop=(fun () -> e_false);
                    }
        | Exists  ->{ when_empty= (fun () -> e_false);
                      add_hyp =(fun hyps t -> e_and (t::hyps));
                      when_true=(fun _ -> raise Stop);
                      when_false=(fun bonus -> bonus := add_bonus);
                      when_stop=(fun () -> e_true);
                    }
        | _ -> assert false)
    in
    if Vars.mem v (vars t) then try
        let bonus_min = ref false in
        let bonus_max = ref false in
        let dom = if Ival.cardinal_is_less_than dom max_reduce_quantifiers then
            (* try to reduce the domain when [var] is still in [t] *)
            let red reduced i () =
              match repr (QED.e_subst_var v (e_zint i) t) with
              | True -> tools.Tool.when_true reduced
              | False -> tools.Tool.when_false reduced
              | _ -> raise (Tool.Unknown i) in
            let min_bound = try Ival.fold_int (red bonus_min) dom ();
                raise Tool.Empty with Tool.Unknown i -> i in
            let max_bound = try Ival.fold_int_decrease (red bonus_max) dom ();
                raise Tool.Empty with Tool.Unknown i -> i in
            let red_dom = Ival.inject_range (Some min_bound) (Some max_bound) in
            Ival.narrow dom red_dom
          else dom
        in begin match Ival.min_and_max dom with
          | None, None -> t (* Cannot be reduced *)
          | Some min, None -> (* May be reduced to [min ...] *)
              if !bonus_min then tools.Tool.add_hyp [e_leq (e_zint min) tv] t
              else t
          | None, Some max -> (* May be reduced to [... max] *)
              if !bonus_max then tools.Tool.add_hyp [e_leq tv (e_zint max)] t
              else t
          | Some min, Some max ->
              if Integer.equal min max then (* Reduced to only one value: [min] *)
                QED.e_subst_var v (e_zint min) t
              else if Integer.lt min max then
                let h = if !bonus_min then [e_leq (e_zint min) tv] else []
                in
                let h = if !bonus_max then (e_leq tv (e_zint max))::h else h
                in tools.Tool.add_hyp h t
              else assert false (* Abstract_interp.Error_Bottom raised *)
        end
      with
      | Tool.Stop  -> tools.Tool.when_stop ()
      | Tool.Empty -> tools.Tool.when_empty ()
      | Abstract_interp.Error_Bottom -> tools.Tool.when_empty ()
      | Abstract_interp.Error_Top -> t
    else (* [alpha] is no more in [t] *)
    if Ival.is_bottom dom then tools.Tool.when_empty () else t
  in
  let module Polarity = struct
    type t = Pos | Neg | Both
    let flip = function | Pos -> Neg | Neg -> Pos | Both -> Both
    let from_bool = function | false -> Neg | true -> Pos
  end
  in object (self)

    val mutable domain : Dom.t = Dom.top

    method name = "Remove redundant is_cint"
    method copy = {< domain = domain >}

    method target _ = ()
    method fixpoint = ()

    method assume p =
      Lang.iter_consequence_literals
        (fun p -> domain <- Dom.assume_literal p domain) (Lang.F.e_prop p)

    method private simplify ~is_goal p =
      let pool = Lang.get_pool () in

      let reduce op var_domain base =
        let dom =
          match Lang.F.repr base with
          | Kint z -> Ival.inject_singleton z
          | _ ->
              try Tmap.find base domain
              with Not_found -> Ival.top
        in
        var_domain := Ival.backward_comp_int_left op !var_domain dom
      in
      let rec reduce_on_neg var var_domain t =
        match Lang.F.repr t with
        | _ when not (is_prop t) -> ()
        | Leq(a,b) when Lang.F.equal a var ->
            reduce Abstract_interp.Comp.Le var_domain b
        | Leq(b,a) when Lang.F.equal a var ->
            reduce Abstract_interp.Comp.Ge var_domain b
        | Lt(a,b) when Lang.F.equal a var ->
            reduce Abstract_interp.Comp.Lt var_domain b
        | Lt(b,a) when Lang.F.equal a var ->
            reduce Abstract_interp.Comp.Gt var_domain b
        | And l -> List.iter (reduce_on_neg var var_domain) l
        | Not p -> reduce_on_pos var var_domain p
        | _ -> ()
      and reduce_on_pos var var_domain t =
        match Lang.F.repr t with
        | Neq _ | Leq _ | Lt _ -> reduce_on_neg var var_domain (e_not t)
        | Imply (l,p) -> List.iter (reduce_on_neg var var_domain) l;
            reduce_on_pos var var_domain p
        | Or l -> List.iter (reduce_on_pos var var_domain) l;
        | Not p -> reduce_on_neg var var_domain p
        | _ -> ()
      in
      (* [~term_pol] gives the polarity of the term [t] from the top level.
                     That informs about how should be considered the quantifiers
                     of [t]
      *)
      let rec walk ~term_pol t =
        let walk_flip_pol t = walk ~term_pol:(Polarity.flip term_pol) t
        and walk_same_pol t = walk ~term_pol t
        and walk_both_pol t = walk ~term_pol:Polarity.Both t
        in
        match repr t with
        | _ when not (is_prop t) -> t
        | Bind((Forall|Exists),_,_) ->
            let ctx,t = e_open ~pool ~lambda:false t in
            let ctx_with_dom =
              List.map (fun ((quant,var) as qv)  -> match tau_of_var var with
                  | Int ->
                      let tvar = (e_var var) in
                      let var_domain = ref Ival.top in
                      if quant = Forall
                      then reduce_on_pos tvar var_domain t
                      else reduce_on_neg tvar var_domain t;
                      domain <- Dom.add tvar !var_domain domain;
                      qv, Some (tvar, var_domain)
                  | _ -> qv, None) ctx
            in
            let t = walk_same_pol t in
            let f_close t = function
              | (quant,var), None -> e_bind quant var t
              | (quant,var), Some (tvar,var_domain) ->
                  domain <- Dom.remove tvar domain;
                  (** Bonus: Add additionnal hypothesis in forall when we could deduce
                      better constraint on the variable *)
                  let add_bonus = match term_pol with
                    | Polarity.Both -> false
                    | _ -> (term_pol=Polarity.Pos) = (quant=Forall)
                  in
                  let t = reduce_bound ~add_bonus quant var tvar !var_domain t in
                  e_bind quant var t
            in
            List.fold_left f_close t ctx_with_dom
        | Fun(g,[a]) ->
            (** Here we simplifies the cints which are redoundant *)
            begin try
                let ubound = c_int_bounds_ival (is_cint g) in
                let dom = (Tmap.find a domain) in
                if Ival.is_included dom ubound
                then e_true
                else t
              with Not_found -> t
            end
        | Imply (l1,l2) -> e_imply (List.map walk_flip_pol l1) (walk_same_pol l2)
        | Not p -> e_not (walk_flip_pol p)
        | And _ | Or _ -> Lang.F.QED.f_map walk_same_pol t
        | _ ->
            Lang.F.QED.f_map ~pool ~forall:false ~exists:false walk_both_pol t in
      Lang.F.p_bool (walk ~term_pol:(Polarity.from_bool is_goal) (Lang.F.e_prop p))

    method simplify_exp (e : term) = e

    method simplify_hyp p = self#simplify ~is_goal:false p

    method simplify_goal p = self#simplify ~is_goal:true p

    method simplify_branch p = p

    method infer = []
  end


let mask_simplifier =
  let update x m ctx =
    Tmap.insert (fun _ m old -> if Integer.lt m old then (*better*) m else old)
      x m ctx
  and rewrite ctx e =
    let reduce m x = match F.repr x with
      | Kint v -> F.e_zint (Integer.logand m v)
      | _ -> x
    and collect ctx d x = try
        let m = Tmap.find x ctx in
        match d with
        | None -> Some m
        | Some m0 -> if Integer.lt m m0 then Some m else d
      with Not_found -> d
    in
    match F.repr e with
    | Fun(f,es) when f == f_land ->
        begin
          match List.fold_left (collect ctx) None es with
          | None -> raise Not_found
          | Some m -> F.e_fun f_land (List.map (reduce m) es)
        end
    | _ -> raise Not_found
  in
  object

    (** Must be 2^n-1 *)
    val mutable magnitude : Integer.t Tmap.t = Tmap.empty

    method name = "Rewrite unsigned masks"
    method copy = {< magnitude = magnitude >}

    method target _ = ()
    method infer = []
    method fixpoint = ()

    method assume p =
      Lang.iter_consequence_literals
        (fun p -> match F.repr p with
           | Fun(f,[x]) -> begin
               try
                 let iota = is_cint f in
                 if not (Ctypes.signed iota) then
                   magnitude <- update x (snd (Ctypes.bounds iota)) magnitude
               with Not_found -> ()
             end
           | _ -> ()) (F.e_prop p)

    method simplify_exp e =
      if Tmap.is_empty magnitude then e else
        Lang.e_subst (rewrite magnitude) e

    method simplify_hyp p =
      if Tmap.is_empty magnitude then p else
        Lang.p_subst (rewrite magnitude) p

    method simplify_branch p =
      if Tmap.is_empty magnitude then p else
        Lang.p_subst (rewrite magnitude) p

    method simplify_goal p =
      if Tmap.is_empty magnitude then p else
        Lang.p_subst (rewrite magnitude) p

  end

(* -------------------------------------------------------------------------- *)
