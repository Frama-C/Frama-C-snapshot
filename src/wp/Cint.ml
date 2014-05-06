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

(* -------------------------------------------------------------------------- *)
(* --- Integer Arithmetics Model                                          --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Qed.Logic
open Lang
open Lang.F
module FunMap = FCMap.Make(Lang.Fun)

let is_cint_map = ref FunMap.empty
let to_cint_map = ref FunMap.empty

let is_cint f = FunMap.find f !is_cint_map
let to_cint f = FunMap.find f !to_cint_map

(* -------------------------------------------------------------------------- *)
(* --- Library Cint                                                       --- *)
(* -------------------------------------------------------------------------- *)

let library = "cint"

let make_fun_int op i = 
  Lang.extern_f ~library ~result:Logic.Int "%s_%a" op Ctypes.pp_int i
let make_pred_int op i = 
  Lang.extern_f ~library ~result:Logic.Prop "%s_%a" op Ctypes.pp_int i

(* let fun_int op = Ctypes.imemo (make_fun_int op) *) (* unused for now *)
(* let pred_int op = Ctypes.imemo (make_pred_int op) *) (* unused for now *)

(* is_<cint> : int -> prop *)
let p_is_int = Ctypes.imemo 
    (fun iota ->
       let f = make_pred_int "is" iota in
       let simplify = function
         | [e] -> 
             (match F.repr e with
              | Logic.Kint k ->
                  let vmin,vmax = Ctypes.c_int_bounds iota in
                  F.e_bool (Z.leq vmin k && Z.lt k vmax)
              | _ -> 
                  (let bounds = Ctypes.c_int_bounds iota in
                   (* min(ctypes)<=y<=max(ctypes) <==> is_ctypes(y) *)
                   match F.is_true (F.e_and 
                                      [F.e_leq (e_zint (fst bounds)) e;
                                       F.e_leq e (e_zint (snd bounds))]) with
                   | Logic.Yes -> e_true
                   | Logic.No  -> e_false
                   | _  -> raise Not_found))
         | _ -> raise Not_found
       in 
       F.set_builtin f simplify; 
       is_cint_map := FunMap.add f iota !is_cint_map ; 
       f)

let f_to_int = Ctypes.imemo
    (fun iota ->
       let f = make_fun_int "to" iota in
       let simplify = function
         | [e] -> 
             (match F.repr e with
              | Logic.Kint value ->
                  let size = Integer.of_int (Ctypes.i_bits iota) in
                  let signed = Ctypes.signed iota in
                  F.e_zint (Integer.cast ~size ~signed ~value)
              | _ when let bounds = Ctypes.c_int_bounds iota in
                  (F.decide (F.e_leq e (e_zint (snd bounds)))) &&
                  (F.decide (F.e_leq (e_zint (fst bounds)) e)) ->
                  (* min(ctypes)<=y<=max(ctypes) ==> to_ctypes(y)=y *)
                  e 
              | _  -> raise Not_found)
         | _ -> raise Not_found
       in 
       F.set_builtin f simplify ; 

       let simplify_leq x y = 
         let bounds = Ctypes.c_int_bounds iota in
         match F.repr y with
         | Logic.Fun( conv , [_] ) when (Fun.equal conv f) &&
                                        (F.decide (F.e_leq x (e_zint (fst bounds)))) ->
             (* x<=min(ctypes) ==> x<=to_ctypes(y) *)
             e_true
         | _ -> (match F.repr x with
             | Logic.Fun( conv , [_] ) when (Fun.equal conv f) && 
                                            (F.decide (F.e_leq (e_zint (snd bounds)) y)) ->
                 (* max(ctypes)<=y ==> to_ctypes(y)<=y *)
                 e_true
             | _ -> raise Not_found)
       in
       F.set_builtin_leq f simplify_leq ;
       to_cint_map := FunMap.add f iota !to_cint_map ; 
       f)

let f_truncate = extern_f ~library:"qed" ~result:Logic.Int "truncate"

(* Signature int,int -> int over Z *)
let ac = {
  associative = true ;
  commutative = true ;
  idempotent = false ;
  inversible = false ;
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

let op_lxor = { ac with neutral = E_int 0 ; inversible = true }
let op_lor  = { ac with neutral = E_int 0 ; absorbant = E_int (-1); idempotent = true }
let op_land = { ac with neutral = E_int (-1); absorbant = E_int 0 ; idempotent = true }

let f_lnot = Lang.extern_f ~library ~result "lnot"
let f_lor  = Lang.extern_f ~library ~result ~category:(Operator op_lor) ~balance "lor"
let f_land = Lang.extern_f ~library ~result ~category:(Operator op_land) ~balance "land"
let f_lxor = Lang.extern_f ~library ~result ~category:(Operator op_lxor) ~balance "lxor"
let f_lsl = Lang.extern_f ~library ~result "lsl"
let f_lsr = Lang.extern_f ~library ~result "lsr"
let f_bit = Lang.extern_p ~library ~bool:"bit_testb" ~prop:"bit_test" ()

let () = let open LogicBuiltins in add_builtin "\\bit_test" [Z;Z] f_bit

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let of_real i a = e_fun (f_to_int i) [e_fun f_truncate [a]]
let irange i a = p_call (p_is_int i) [a]
let iconvert i a = e_fun (f_to_int i) [a]
let iconvert_unsigned i x = if Ctypes.signed i then x else iconvert i x

type model =
  | Natural (** Integer arithmetics *)
  | Machine  (** Modulo arithmetics *)

let model = Context.create ~default:Natural "Cint.model"
let configure = Context.set model

let ibinop f i x y  = 
  let z = f x y in 
  match Context.get model with Natural -> z | Machine -> iconvert i z

let iunop f i x =
  let z = f x in
  match Context.get model with Natural -> z | Machine -> iconvert i z

(* -------------------------------------------------------------------------- *)
(* --- Arithmetics                                                        --- *)
(* -------------------------------------------------------------------------- *)
    
(* C Code Semantics *)
let iopp = iunop e_opp
let iadd = ibinop e_add
let isub = ibinop e_sub
let imul = ibinop e_mul  
let idiv = ibinop e_div
let imod = ibinop e_mod

(* -------------------------------------------------------------------------- *)
(* --- Bits                                                               --- *)
(* -------------------------------------------------------------------------- *)

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
      (* integers should be at the begining of the list *)
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

let match_integer t =
  match F.repr t with
  | Logic.Kint c -> c
  | _ -> raise Not_found

let is_to_cint conv =
  try ignore (to_cint conv) ; true
  with Not_found -> false

(* integration with qed should be improved! *) 
let rec is_positive_or_null e = match F.repr e with
  | Logic.Fun( f , [e] ) when Fun.equal f f_lnot -> is_negative e
  | Logic.Fun( f , es ) when Fun.equal f f_land -> List.exists is_positive_or_null es
  | Logic.Fun( f , es ) when Fun.equal f f_lor   -> List.for_all is_positive_or_null es
  | Logic.Fun( f , es ) when Fun.equal f f_lsr || Fun.equal f f_lsl
    -> List.for_all is_positive_or_null es
  | _ -> (* try some improvement first then ask to qed *)
      let improved_is_positive_or_null e = match F.repr e with
        | Logic.Add es -> List.for_all is_positive_or_null es
        | Logic.Var _v -> let _tau = F.tau_of_var _v in false
        | _ -> false
      in if improved_is_positive_or_null e then true
      else match F.is_true (F.e_leq e_zero e) with
        | Logic.Yes -> true
        | Logic.No | Logic.Maybe -> false
and is_negative e = match F.repr e with
  | Logic.Fun( f , [e] ) when Fun.equal f f_lnot -> is_positive_or_null e
  | Logic.Fun( f , es ) when Fun.equal f f_lor  -> List.exists is_negative es
  | Logic.Fun( f , es ) when Fun.equal f f_land -> List.for_all is_negative es
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

let match_power2 =
  let highest_bit_number =
    let hsb p = if p land 2 = 0 then 0 else 1
    in let hsb p = let n = p lsr 2 in if n = 0 then hsb p else 2 + hsb n  
    in let hsb p = let n = p lsr 4 in if n = 0 then hsb p else 4 + hsb n 
    in let hsb = Array.init 256 hsb
    in let hsb p = let n = p lsr 8 in if n = 0 then hsb.(p) else 8 + hsb.(n)
    in let hsb p = let n = Integer.shift_right p Integer.sixteen in
      Integer.of_int (if Integer.is_zero n then hsb (Integer.to_int p) else 16 + hsb (Integer.to_int n))
    in let rec hsb_aux p = 
      let n = Integer.shift_right p Integer.thirtytwo in
      if Integer.is_zero n then hsb p else Integer.add Integer.thirtytwo (hsb_aux n)
    in hsb_aux
  in let is_power2 k = (* exists n such that k == 2**n? *)
    (Integer.gt k Integer.zero) && (Integer.equal k (Integer.logand k (Integer.neg k)))
  in let rec match_power2 e = match F.repr e with
    | Logic.Kint z when is_power2 z -> e_zint (highest_bit_number z)
    | Logic.Fun( f , [n;k] ) when Fun.equal f f_lsl && is_positive_or_null k -> 
        e_add k (match_power2 n)
    | _ -> raise Not_found
  in match_power2      

let match_positive_integer t =
  match F.repr t with
  | Logic.Kint c when Integer.le Integer.zero c -> c
  | _ -> raise Not_found

let match_integer_arg1 = function
  | e1::[e2] -> (match_integer e1),e2
  | _ -> raise Not_found

let match_positive_integer_arg2 = function
  | e1::[e2] -> e1,(match_positive_integer e2)
  | _ -> raise Not_found

let match_integer_extraction = function
  | [] -> raise Not_found
  | e::es -> (match_integer e), es

let is_leq a b = F.is_true (F.e_leq a b)

let bitk_positive k e = e_fun f_bit [e;k]
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
              e_if (e_leq n k) (bitk_positive (e_sub k n) e) e_false
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
              let size = Ctypes.i_bits iota in
              let signed = Ctypes.signed iota in
              if signed then 
                begin match is_leq k (e_int (size-2)) with
                  | Logic.Yes -> bitk_positive k a
                  | Logic.No | Logic.Maybe -> raise Not_found
                end
              else begin match is_leq (e_int size) k with
                | Logic.Yes -> e_false
                | Logic.No -> bitk_positive k a
                | Logic.Maybe -> raise Not_found
              end
          | _ -> raise Not_found
      end
  | _ -> raise Not_found

let match_fun op t =
  match F.repr t with
  | Logic.Fun( f , es ) when Fun.equal f op -> es
  | _ -> raise Not_found

let match_ufun uop t =
  match F.repr t with
  | Logic.Fun( f , e::[] ) when Fun.equal f uop -> e
  | _ -> raise Not_found

let match_power2_extraction =
  let power2_opt n = try Some (match_power2 n) with Not_found -> None in
  let rec aux rs = function
    | [] -> raise Not_found
    | e::es -> 
        match power2_opt e with
        | Some k -> k, e, List.rev_append rs es
        | None -> aux (e::rs) es
  in aux []

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


let smp_eq_with_lxor a b = (* b1==(a2^e) <==> ~(b1^a2)==e *)
  let b1 = match_integer b in
  let es = match_fun f_lxor a in
  try (* b1==(a2^e) <==> ~(b1^a2)==e *)
    let a2,es = match_integer_extraction es in
    let k1 = Integer.lognot (Integer.logxor b1 a2) in
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

let two_power_k_minus1 k = Integer.pred (Integer.two_power k)

let smp_eq_with_lsl a b =
  let b1 = match_integer b in
  let es = match_fun f_lsl a in
  try
    let e,a2= match_positive_integer_arg2 es in
    if not (Integer.is_zero (Integer.logand b1 (two_power_k_minus1 a2))) 
    then e_false
    else e_eq e (e_zint (Integer.shift_right b1 a2))  
  with Not_found -> let a1,e= match_integer_arg1 es in
    if is_negative e then raise Not_found ;
    (* [PB] can be generalized to any term for a1 *)
    if Integer.le Integer.zero a1 && Integer.lt b1 a1 then e_false 
    else if Integer.ge Integer.zero a1  && Integer.gt b1 a1 then e_false 
    else raise Not_found

let smp_eq_with_lsr a b =
  let b1 = match_integer b in
  let es = match_fun f_lsr a in
  let e,a2= match_positive_integer_arg2 es in
  e_eq (e_zint (Integer.shift_left b1 a2))
    (e_fun f_land [e_zint (Integer.lognot (two_power_k_minus1 a2));e])  

(* ACSL Semantics *)
type l_builtin = { f: lfun ; eq: (term -> term -> term) option ; smp: term list -> term}

let install_builtins = Context.once
    begin fun () ->

      if Wp_parameters.Bits.get () then
        begin


          let mk_builtin n f ?eq smp = n, { f ; eq; smp } in

          let bi_lbit = mk_builtin "f_bit" f_bit smp_bitk_positive in
          let bi_lnot = mk_builtin "f_lnot" f_lnot ~eq:smp_eq_with_lnot (smp1 Integer.lognot) in
          let bi_lxor = mk_builtin "f_lxor" f_lxor ~eq:smp_eq_with_lxor (smp2 f_lxor Integer.logxor) in
          let bi_lor  = mk_builtin "f_lor" f_lor  ~eq:smp_eq_with_lor  (smp2 f_lor  Integer.logor) in
          let bi_land = mk_builtin "f_land" f_land ~eq:smp_eq_with_land smp_land in
          let bi_lsl  = mk_builtin "f_lsl" f_lsl ~eq:smp_eq_with_lsl (smp_shift Integer.shift_left) in
          let bi_lsr  = mk_builtin "f_lsr" f_lsr ~eq:smp_eq_with_lsr (smp_shift Integer.shift_right) in

          List.iter (fun (_name, { f; eq; smp }) -> 
              F.set_builtin f smp ;
              match eq with
              | None -> ()
              | Some eq -> 
                  let eq a b = let r = eq a b in 
                    (* Format.printf "xx smp_eq %s: (%a)==(%a) <==> %a @." _name pp_term a pp_term b pp_term r; *)
                    r 
                  in
                  F.set_builtin_eq f eq)
            [bi_lbit; bi_lnot; bi_lxor; bi_lor; bi_land; bi_lsl; bi_lsr]


        end
    end

let l_not a   = install_builtins () ; e_fun f_lnot [a]
let l_xor a b = install_builtins () ; e_fun f_lxor [a;b]
let l_or  a b = install_builtins () ; e_fun f_lor  [a;b]
let l_and a b = install_builtins () ; e_fun f_land [a;b]
let l_lsl a b = install_builtins () ; e_fun f_lsl [a;b]
let l_lsr a b = install_builtins () ; e_fun f_lsr [a;b]

(* C Code Semantics *)
let bnot i x   = iconvert_unsigned i (l_not x)
let bxor i x y = iconvert_unsigned i (l_xor x y)
let bor _i  = l_or  (* no needs of range conversion *)
let band _i = l_and (* no needs of range conversion *)

let blsl i x y = iconvert i (l_lsl x y)
let blsr _i = l_lsr (* no needs of range conversion *)

(* -------------------------------------------------------------------------- *)
