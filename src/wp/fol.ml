(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

open Cil_datatype

(* ------------------------------------------------------------------------ *)
(** {2 Constants} *)

type constant =
  | ConstInt of string
  | ConstBool of bool
  | ConstUnit
  | ConstFloat of string

let c_bool b = ConstBool b
let c_int_of_str s = ConstInt s
let c_int i = ConstInt (string_of_int i)
let c_float_of_str s = ConstFloat s
let c_float f = ConstFloat (string_of_float f)

(* ------------------------------------------------------------------------ *)
(** {2 Variables} *)

module Var = struct

  type t =
    (string * int option * Formula.tau * Cil_types.logic_type option)

  let var_type (_, _, t,_)= t

  let var_name (v,p,_,_)=
     match p with
    | None -> v
    | Some id -> Printf.sprintf "%s_%d" v id

  let basename (x,_,_,_) = x

  let kind_of_var (_,_,t,p)=
   match p with
   | Some ty -> Formula.Acsl(t,ty)
   | None ->  Formula.Model t

  let var_counter = ref 0

  (* ensures that the counter is still ok even if some numbers are used 
  * by external function through [mk] or [ident_named_var] *)
  let check_cpt k = if k > !var_counter then var_counter := k

  let fresh_named_var name t =
    incr var_counter;
    (name, Some !var_counter, t,None)

  let fresh_var (name, _, t,_) = fresh_named_var name t

  let mk x k_opt tau ltype_opt = 
    (match k_opt with None -> () | Some k -> check_cpt k);
    (x, k_opt, tau, ltype_opt)

  let ident_named_var name k t = 
    check_cpt k; (name,Some k,t,None)

  let equal v1 v2 = match v1, v2 with
    | (x,c,_,_) , (y,d,_,_) -> c=d && x=y

  let compare v1 v2 = match v1, v2 with
   | (v1, id1, _,_), (v2, id2, _,_) ->
      let cmp = String.compare v1 v2 in
      if cmp <> 0 then cmp
      else
        begin
          match id1 , id2 with
            | None , None -> 0
            | Some _ , None -> 1
            | None , Some _ -> (-1)
            | Some id1 , Some id2 -> Pervasives.compare id1 id2
        end

end

module Vset = Set.Make(Var)
module Vmap = Map.Make(Var)

(* ------------------------------------------------------------------------ *)
(** {2 Terms} *)
(* ------------------------------------------------------------------------ *)

type term =
  | Tconst of constant
  | Tvar of Var.t
  | Tapp of string * term list
  | Tgetfield of Cil_types.fieldinfo * term
  | Tsetfield of Cil_types.fieldinfo * term * term
  | Taccess of term * term
  | Tupdate of term * term * term
  | Tif of term * term * term
  | Tlet of Var.t * term * term

(* -----------------------------------*)
(** {3 Term comparison} *)

let rec eq_terms e1 e2 =
  match e1, e2 with
    | Tconst c1, Tconst c2 -> Pervasives.compare c1 c2 = 0
    | Tvar v1, Tvar v2 -> Var.equal v1 v2
    | Tapp (f1, args1), Tapp (f2, args2) ->
        f1 = f2 && List.for_all2 eq_terms args1 args2
    | Tif (c1, t1, e1), Tif (c2, t2, e2) ->
        eq_terms c1 c2 && eq_terms t1 t2 && eq_terms e1 e2
    | Tlet (x,v,t),Tlet(x',v',t') ->
        Var.equal x x' && eq_terms v v' && eq_terms t t'
    | Tgetfield (f,r) , Tgetfield(g,s) ->
        Fieldinfo.equal f g && eq_terms r s
    | Tsetfield (f,r,v) , Tsetfield(g,s,w) ->
        Fieldinfo.equal f g && eq_terms r s && eq_terms v w
    | Taccess (t,i) , Taccess (u,j) ->
        eq_terms t u && eq_terms i j
    | Tupdate (t,i,v), Tupdate(u,j,w) ->
        eq_terms t u && eq_terms i j && eq_terms v w
    | _ -> false

(* -----------------------------------*)
(** {3 Term visitors} *)

let rec e_has_var xs e =
  let frec = e_has_var xs in
  match e with
  | Tconst _ -> false
  | Tvar y -> List.exists (Var.equal y) xs
  | Tapp(_,ts) -> List.exists (frec) ts
  | Tgetfield(_,r) -> frec r
  | Tsetfield(_,r,v) -> frec r || frec v
  | Taccess(t,i) -> frec t || frec i
  | Tupdate (t,i,v) -> frec t || frec i || frec v
  | Tif(a,b,c) -> frec a || frec b || frec c
  | Tlet(x,v,t) ->
      frec v ||
        (let xs = List.filter (fun y -> not (Var.equal x y)) xs in
         xs <> [] && e_has_var xs t)

let rec e_closed xs = function
  | Tconst _ -> true
  | Tvar y -> List.exists (Var.equal y) xs
  | Tgetfield(_,r) -> e_closed xs r
  | Tsetfield(_,r,v) -> e_closed xs r && e_closed xs v
  | Taccess(t,i) -> e_closed xs t && e_closed xs i
  | Tupdate(t,i,v) -> e_closed xs t && e_closed xs i && e_closed xs v
  | Tapp(_,ts) -> List.for_all (e_closed xs) ts
  | Tif(a,b,c) -> e_closed xs a && e_closed xs b && e_closed xs c
  | Tlet(x,v,t) -> e_closed xs v && e_closed (x::xs) t

(* -----------------------------------*)
(** {3 Term smart constructors} *)

let e_true = Tconst (c_bool true)
let e_false = Tconst (c_bool false)
let e_int i = Tconst (c_int i)
let e_int64 i = Tconst (ConstInt (Int64.to_string i))
let e_float f = Tconst (c_float f)
let e_cnst c = Tconst c
let e_var v = Tvar v

let e_if c t f = match c with
  | Tconst (ConstBool true) -> t
  | Tconst (ConstBool false) -> f
  | _ -> if eq_terms t f then t else Tif (c, t, f)

let i_compute zop a b =
  let z_a = Big_int.big_int_of_string a in
  let z_b = Big_int.big_int_of_string b in
  Tconst(ConstInt(Big_int.string_of_big_int (zop z_a z_b)))

let i_apply zop a =
  let z_a = Big_int.big_int_of_string a in
  Tconst(ConstInt(Big_int.string_of_big_int (zop z_a)))

let simpl() = Wp_parameters.Simpl.get()


let signed_in_bound b z opt_z def_z =
  let bz = Big_int.big_int_of_string z in
  if (Big_int.le_big_int (Big_int.minus_big_int b) bz && Big_int.lt_big_int  bz b)
  then opt_z else def_z
    
let unsigned_in_bound b z opt_z def_z =
  let bz = Big_int.big_int_of_string z in
  if (Big_int.le_big_int Big_int.zero_big_int bz && Big_int.lt_big_int  bz b)
  then opt_z else def_z 
    
let as_int_format fmt z =
  let def = Tapp ("as_int",[Tapp(fmt,[]) ; Tconst(ConstInt z)]) in
  let opt = Tconst (ConstInt z) in 
  let signed_in_bound b = signed_in_bound b z opt def in 
  let unsigned_in_bound b =unsigned_in_bound b z opt def in
  match fmt with
    | "uint8_format" ->
        let b = Big_int.big_int_of_int 256 in unsigned_in_bound b
    | "sint8_format" ->
        let b = Big_int.big_int_of_int 128 in signed_in_bound b
    | "uint16_format" ->
        let b =  Big_int.big_int_of_int 65536 in unsigned_in_bound b
    | "sint16_format" ->
        let b = Big_int.big_int_of_int 32768 in signed_in_bound b
    | "uint32_format" ->
        let b =  Big_int.big_int_of_string ("4294967296") in
        unsigned_in_bound b
    | "sint32_format" ->
        let b = Big_int.big_int_of_string ("2147483648") in
        signed_in_bound b
    | "uint64_format" ->
        let b = Big_int.big_int_of_string ("18446744073709551616") in
        unsigned_in_bound b
    | "sint64_format" ->
        let b = Big_int.big_int_of_string ("9223372036854775808") in
        signed_in_bound b
    |_ -> def


let as_int s z =
  let def = Tapp (s,[Tconst(ConstInt z)]) in
  let opt = Tconst (ConstInt z) in 
  let signed_in_bound b = signed_in_bound b z opt def in
  let unsigned_in_bound b = unsigned_in_bound b z opt def in
  match s with 
   | "as_uint8" ->
        let b = Big_int.big_int_of_int 256 in unsigned_in_bound b
    | "as_sint8" ->
        let b = Big_int.big_int_of_int 128 in signed_in_bound b
    | "as_uint16" ->
        let b =  Big_int.big_int_of_int 65536 in unsigned_in_bound b
    | "as_sint16" ->
        let b = Big_int.big_int_of_int 32768 in signed_in_bound b
    | "as_uint32" ->
        let b =  Big_int.big_int_of_string ("4294967296") in
        unsigned_in_bound b
    | "as_sint32" ->
        let b = Big_int.big_int_of_string ("2147483648") in
        signed_in_bound b
    | "as_uint64" ->
        let b = Big_int.big_int_of_string ("18446744073709551616") in
        unsigned_in_bound b
    | "as_sint64" ->
        let b = Big_int.big_int_of_string ("9223372036854775808") in
        signed_in_bound b
    |_ -> def    
      


let coercion f g d = 
  let def = Tapp (f , [Tapp(g,[d])]) in 
  match f,g,d with 
    | "data_of_addr", "addr_of_data",_
    | "data_of_uint8", "uint8_of_data",_ 
    | "data_of_sint8", "sint8_of_data",_ 
    | "data_of_uint16", "uint16_of_data",_
    | "data_of_sint16", "sint16_of_data",_
    | "data_of_uint32", "uint32_of_data",_
    | "data_of_sint32", "sint32_of_data",_
    | "data_of_uint64", "uint64_of_data",_
    | "data_of_sint64", "sint64_of_data",_
    | "data_of_float16", "float16_of_data",_
    | "data_of_float32", "float32_of_data",_
    | "data_of_float64", "float64_of_data",_
    | "data_of_float128", "float128_of_data",_ -> d
    | "uint8_of_data", "data_of_uint8",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_int 256 in unsigned_in_bound b z d def
    | "sint8_of_data", "data_of_sint8",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_int 128 in signed_in_bound b z d def
    | "uint16_of_data", "data_of_uint16",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_int 65536 in unsigned_in_bound b z d def
    | "sint16_of_data", "data_of_sint16",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_int 32768 in signed_in_bound b z d def
    | "uint32_of_data", "data_of_uint32",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_string ("4294967296") in unsigned_in_bound b z d def
    | "sint32_of_data", "data_of_sint32",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_string ("2147483648") in signed_in_bound b z d def
    | "uint64_of_data", "data_of_uint64",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_string ("18446744073709551616") in unsigned_in_bound b z d def
    | "sint64_of_data", "data_of_sint64",Tconst(ConstInt z)  ->
	let b = Big_int.big_int_of_string ("9223372036854775808") in signed_in_bound b z d def
    | _ -> def


let e_app f args =
  if not (simpl()) then Tapp(f,args)
  else
    match f , args with
      | "neg_int" , [ Tapp( "neg_int" , [a] ) ] -> a
      | "neg_real" , [ Tapp( "neg_real" , [a] ) ] -> a
      | "neg_int" , [ Tconst (ConstInt a) ] ->
	  i_apply Big_int.minus_big_int a
      | "add_int" , [ Tconst (ConstInt a) ; Tconst (ConstInt b) ] ->
          i_compute Big_int.add_big_int a b
      | "sub_int" , [ Tconst (ConstInt a) ; Tconst (ConstInt b) ] ->
          i_compute Big_int.sub_big_int a b
      | "mul_int" , [ Tconst (ConstInt a) ; Tconst (ConstInt b) ] ->
                  i_compute Big_int.mult_big_int a b
      | "add_int" , [ Tconst (ConstInt "0") ; x ] -> x
      | "add_int" , [ x ; Tconst (ConstInt "0") ] -> x
      | "sub_int" , [ x ; Tconst (ConstInt "0") ] -> x
      | "mul_int" , [ Tconst (ConstInt "1") ; x ] -> x
      | "mul_int" , [ x ; Tconst (ConstInt "1") ] -> x
      | "mul_int" , [ (Tconst (ConstInt "0")) as z ; _ ] -> z
      | "mul_int" , [ _ ; (Tconst (ConstInt "0")) as z ] -> z
      | "add_int" , [ b ; Tapp("sub_int",[a;c]) ] when eq_terms b c -> a ;
      | "add_int" , [ Tapp("sub_int",[a;b]) ; c ] when eq_terms b c -> a
      | "sub_int" , [ Tapp("add_int",[a;b]) ; c ] when eq_terms b c -> a
      | "encode" , [ Tapp("decode" , [ a ; fmt ]) ; fmt' ] when eq_terms fmt fmt' -> a
      | "decode" , [ Tapp("encode" , [ a ; fmt ]) ; fmt' ] when eq_terms fmt fmt' -> a
      | "as_int" , [Tapp(fmt,[]) ; Tconst(ConstInt z)] -> as_int_format fmt z
      | f , [Tconst(ConstInt z)] -> as_int f z 
      |  f , [Tapp(g,[d])] -> coercion f g d
      | _ -> Tapp (f, args)

let e_let x exp t = if e_has_var [x] t then Tlet (x, exp, t) else t

let case_of = function
  | Tconst(ConstInt s) -> Some s
  | _ -> None

let e_update t i v = Tupdate(t,i,v)
let e_access t i =
    match t with
      | Tupdate(_,j,v) when simpl() && eq_terms i j -> v
      | t -> Taccess(t,i)

let e_setfield f r v = Tsetfield(f,r,v)
let e_getfield f r =
  match r with
    | Tsetfield(g,s,w) when simpl() ->
        if Fieldinfo.equal f g
        then w (* get set same field *)
        else
          if f.Cil_types.fcomp.Cil_types.cstruct  then
            Tgetfield(f,s) (* get set other of record only *)
          else
            Tgetfield(f,r)
    | r -> Tgetfield(f,r)


(* -----------------------------------*)
(** {3 Term transformation} *)

(** Apply [do_var] in term subexpressions.*)
let rec change_in_exp do_var exp =
  let frec = change_in_exp do_var in
  match exp with
  | Tconst c -> e_cnst c
  | Tvar v -> (match do_var v with Some e -> e | None -> e_var v)
  | Tgetfield(f,r) -> e_getfield f (frec r)
  | Tsetfield(f,r,v) -> e_setfield f (frec r) (frec v)
  | Taccess(t,i) -> e_access (frec t) (frec i)
  | Tupdate(t,i,v) -> e_update (frec t) (frec i) (frec v)
  | Tapp (n,tl) -> e_app n (List.map (frec) tl)
  | Tif (t1,t2,t3) -> e_if (frec t1) (frec t2) (frec t3)
  | Tlet (x,v,t) -> e_let x (frec v) (frec t)

let rec term_replace alpha x exp t =
  let frec = term_replace alpha x exp in
    match t with
      | Tconst _ -> t
      | Tvar x0 -> if Var.equal x0 x then exp else t
      | Tapp (f, ts) -> e_app f (List.map (frec) ts)
      | Tif (a, b, c) -> e_if (frec a) (frec b) (frec c)
      | Tgetfield(f,r) -> e_getfield f (frec r)
      | Tsetfield(f,r,v) -> e_setfield f (frec r) (frec v)
      | Taccess(t,i) -> e_access (frec t) (frec i)
      | Tupdate(t,i,v) -> e_update (frec t) (frec i) (frec v)
      | Tlet (x0, a, b) ->
          if e_has_var [x0] exp then
            match alpha x with
              | None -> Tlet(x,exp,t)
              | Some y ->
                  let by = term_replace alpha x0 (Tvar y) b in
                  Tlet(y,frec a,frec by)
          else
            let b' = if Var.equal x0 x then b else frec b in
            Tlet(x0,frec a,b')

let alpha_bound_var alpha v =
  let old = try Some (Vmap.find v alpha) with Not_found -> None in
  old, Vmap.add v v alpha

let alpha_unbound alpha v old = match old with
  | Some v' -> Vmap.add v v' alpha
  | None -> Vmap.remove v alpha

let apply_alpha alpha v =
  try alpha, Vmap.find v alpha
  with Not_found ->
      let v' = Var.fresh_var v in
      let alpha = Vmap.add v v' alpha in
        alpha, v'

let rec term_alpha_cv alpha t =
  let do_term alpha t = term_alpha_cv alpha t in
  let rec do_terms alpha l = terms_alpha_cv alpha l in
    match t with
      | Tconst _ -> alpha, t
      | Tvar v ->
          let alpha, v = apply_alpha alpha v in alpha, Tvar v
      | Tapp (f, ts) ->
          let alpha, ts = do_terms alpha ts in
            alpha, e_app f ts
      | Tgetfield(f,r) ->
          let alpha, r = do_term alpha r in
          alpha,e_getfield f r
      |Tsetfield(f,r,v) ->
         let alpha,r = do_term alpha r in
         let alpha,v = do_term alpha v in
         alpha, e_setfield f r v
      | Taccess(t,i) ->
          let alpha,t = do_term alpha t in
          let alpha,i = do_term alpha i in
          alpha,e_access t i
      | Tupdate(t,i,v) ->
          let alpha,t = do_term alpha t in
          let alpha,i = do_term alpha i in
          let alpha,v = do_term alpha v in
          alpha, e_update t i v
      | Tif (a, b, c) ->
          let alpha, a = do_term alpha a in
          let alpha, b = do_term alpha b in
          let alpha, c = do_term alpha c in
          alpha, e_if a b c
      | Tlet (x, a, b) ->
          let alpha, a = do_term alpha a in (* a doesn't see x *)
          let old_x, alpha = alpha_bound_var alpha x in
          let alpha, b = do_term alpha b in
          let alpha = alpha_unbound alpha x old_x in (* restore old_x *)
            alpha, e_let x a b
and terms_alpha_cv alpha l = match l with [] -> alpha, []
    | t::l ->
        let alpha, t = term_alpha_cv alpha t in
        let alpha, l = terms_alpha_cv alpha l in
          alpha, t::l

(* ------------------------------------------------------------------------ *)
(** {2 Predicates} *)
(* ------------------------------------------------------------------------ *)

type pred =
  | Papp of string * term list
  | Ptrue
  | Pfalse
  | Pimplies of pred * pred
  | Pif of term * pred * pred
  | Pand of pred * pred
  | Por of pred * pred
  | Piff of pred * pred
  | Pnot of pred
  | Pforall of Var.t * pred
  | Pexists of Var.t * pred
  | Plet of Var.t * term * pred
  | Pnamed of string * pred

let rec eq_preds p q =
  match p,q with
    | Papp(f,xs) , Papp(g,ys) ->
        f = g && List.for_all2 eq_terms xs ys
    | Ptrue , Ptrue -> true
    | Pfalse , Pfalse -> true
    | Pnamed(_,p) , q -> eq_preds p q
    | p , Pnamed(_,q) -> eq_preds p q
    | Pnot p , Pnot q -> eq_preds p q
    | _ -> false

let rec p_has_var xs = function
  | Papp(_,ts) -> List.exists (e_has_var xs) ts
  | Ptrue | Pfalse -> false
  | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) ->
      p_has_var xs p || p_has_var xs q
  | Pif(t,p,q) ->
      e_has_var xs t || p_has_var xs p || p_has_var xs q
  | Pnot p | Pnamed(_,p) -> p_has_var xs p
  | Pforall(x,p) | Pexists(x,p) ->
      let xs = List.filter (fun y -> not (Var.equal x y)) xs in
      xs <> [] && p_has_var xs p
  | Plet(x,a,p) ->
      e_has_var xs a ||
        (let xs = List.filter (fun y -> not (Var.equal x y)) xs in
         xs <> [] && p_has_var xs p)

let rec p_closed xs = function
  | Papp(_,ts) -> List.for_all (e_closed xs) ts
  | Ptrue | Pfalse -> true
  | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) ->
      p_closed xs p && p_closed xs q
  | Pif(t,p,q) ->
      e_closed xs t && p_closed xs p && p_closed xs q
  | Pnot p | Pnamed(_,p) -> p_closed xs p
  | Pforall(x,p) | Pexists(x,p) -> p_closed (x::xs) p
  | Plet(x,a,p) ->
      e_closed xs a && p_closed (x::xs) p

(* -----------------------------------*)
(** {3 Predicates smart constructors} *)

let pp_term : (Format.formatter -> term -> unit) ref = ref (fun _ _ -> ())

let i_compare zop a b =
  if zop (Big_int.big_int_of_string a) (Big_int.big_int_of_string b)
  then Ptrue else Pfalse

let p_app name args =
  if not (simpl()) then Papp(name,args)
  else
    match name , args with
      | "eq", [ a ; b ] when eq_terms a b -> Ptrue
      | "neq", [ a ; b ] when eq_terms a b -> Pfalse
      | "le_int" , [a;b] when eq_terms a b -> Ptrue
      | "lt_int" , [a;b] when eq_terms a b -> Pfalse
      | "eq" , [ Tconst(ConstInt a) ; Tconst(ConstInt b) ] ->
          i_compare Big_int.eq_big_int a b
      | "neq" , [ Tconst(ConstInt a) ; Tconst(ConstInt b) ] ->
          i_compare (fun za zb -> not (Big_int.eq_big_int za zb)) a b
      | "le_int" , [ Tconst(ConstInt a) ; Tconst(ConstInt b) ] ->
          i_compare Big_int.le_big_int a b
      | "lt_int" , [ Tconst(ConstInt a) ; Tconst(ConstInt b) ] ->
          i_compare Big_int.lt_big_int a b
      | _ -> Papp (name,args)

let rec val_of = function Pnamed (_,p) -> val_of p | p -> p
let rec cut p = function Pnamed (a,q) -> Pnamed(a,cut p q) | _ -> p

let rec is_true = function
  | Ptrue -> true
  | Pnamed(_,p) -> is_true p
  | _ -> false

let rec is_false = function
  | Pfalse -> true
  | Pnamed(_,p) -> is_false p
  | _ -> false

let p_not p = match val_of p with
  | Ptrue -> cut Pfalse p
  | Pfalse -> cut Ptrue p
  | Papp( "eq" , w ) -> p_app "neq" w
  | Papp( "neq" , w ) -> p_app "eq" w
  | Papp( "le_int" , [a;b] ) -> p_app "lt_int" [b;a]
  | Papp( "lt_int" , [a;b] ) -> p_app "le_int" [b;a]
  | _ -> Pnot p

let p_and p1 p2 = match val_of p1, val_of p2 with
  | Ptrue, _ -> p2
  | _, Ptrue  -> p1
  | Pfalse,_-> cut Pfalse p1
  | _,Pfalse -> cut Pfalse p2
  | _ -> Pand (p1, p2)

let p_or p1 p2 = match val_of p1, val_of p2 with
  | Ptrue, _ -> cut Ptrue p1
  | _ , Ptrue -> cut Ptrue p2
  | Pfalse ,_ -> p2
  | _ ,Pfalse -> p1
  | _ -> Por (p1,p2)

let p_xor p1 p2 = match val_of p1, val_of p2 with
  | Ptrue , Ptrue -> cut (cut Pfalse p2) p1
  | Ptrue ,_ -> cut Ptrue p1
  | _,Ptrue -> cut Ptrue p2
  | Pfalse , _ -> p2
  | _ , Pfalse -> p1
  | _ -> Pnot(Piff(p1,p2))

let p_implies p1 p2 =
  match val_of p1, val_of p2 with
    | Ptrue, _ -> p2
    | Pfalse, _ -> cut Ptrue p1
    | _, Ptrue -> cut Ptrue p2
    | _ -> Pimplies (p1, p2)

let rec p_conj = function
  | [] -> Ptrue
  | [p] -> p
  | p::ps -> p_and p (p_conj ps)

let rec p_disj = function
  | [] -> Pfalse
  | [p] -> p
  | p::ps -> p_or p (p_disj ps)

let p_if c p1 p2 = match c, val_of p1, val_of p2 with
  | (_,Ptrue, Ptrue ) -> cut (cut Ptrue p2) p1
  | (_,Pfalse, Pfalse ) -> cut (cut Pfalse p2) p1
  | (t,_ , _) -> Pif (t,p1,p2)

let p_iff p1 p2 =
  match val_of p1,val_of p2 with
    | Ptrue ,_ -> p2
    | _ ,Ptrue -> p1
    | Pfalse, _ -> p_not p2
    | _ , Pfalse -> p_not p1
    | _ -> Piff (p1,p2)

let p_eq e1 e2 =
  if eq_terms e1 e2 then Ptrue else p_app "eq" [e1; e2]
let p_neq e1 e2 =
  if eq_terms e1 e2 then Pfalse else p_app "neq" [e1; e2]

let p_forall x p =
  match val_of p with
    | Ptrue | Pfalse | Papp ("dummy",_) -> p
    | _ -> if p_has_var [x] p then Pforall(x,p) else p

let p_exists x p =
  match val_of p with
    | Ptrue | Pfalse | Papp ("dummy",_) -> p
    | _ -> if p_has_var [x] p then Pexists(x,p) else p

let p_let x v p =
  match val_of p with
    | Ptrue | Pfalse| Papp ("dummy",_) -> p
    | _ -> if p_has_var [x] p then Plet(x,v,p) else p

let p_named name p = Pnamed(name,p)

(* ------------------------------------------------------------------------ *)
(* ---  Propagation of transformations                                  --- *)
(* ------------------------------------------------------------------------ *)


(** apply [do_exp] on each sub expression of the predicate.
* [quantif_do_exp] is called to change [do_exp] if needed
* when we go under a quantification.
* This version makes possible to have a different flavor of term,
* ie. it can be used for translation.
* TODOopt: we could have another optimized version if the types of terms
* are the same in order to avoid building new terms when there is no
* modification.
* *)
let rec change_exp_in_pred (do_exp:term -> term)
      (quantif_do_exp: (term -> term) -> Var.t ->
                       (term -> term)) p =
  let subst_pred = change_exp_in_pred do_exp quantif_do_exp in
  match p with
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Pif (t,p1,p2) ->
      Pif (do_exp t, subst_pred p1, subst_pred p2)
  | Pnot p -> p_not (subst_pred p)
  | Pforall (v,p') ->
      let f = quantif_do_exp do_exp v in
        Pforall (v,change_exp_in_pred f quantif_do_exp p')
  | Pexists (v,p') ->
      let f = quantif_do_exp do_exp v in
        Pexists (v,change_exp_in_pred f quantif_do_exp p')
  | Plet (x,v,p) ->
      let f =  quantif_do_exp do_exp x in
        Plet (x, do_exp v,change_exp_in_pred f quantif_do_exp p)
  | Pnamed (n,p) -> Pnamed (n,subst_pred p)
  | Pimplies (p1,p2) -> p_implies (subst_pred p1) (subst_pred p2)
  | Pand (p1,p2) -> p_and (subst_pred p1) (subst_pred p2)
  | Por (p1,p2) -> p_or (subst_pred p1) (subst_pred p2)
  | Piff (p1,p2) -> p_iff (subst_pred p1) (subst_pred p2)
  | Papp (n,t) -> p_app n (List.map do_exp t)

let no_quantif_do_exp do_exp qqvar =
  let var_term = e_var qqvar in
    match do_exp var_term with
      | Tvar v when Var.equal qqvar v -> do_exp
      | _ -> assert false
(*
let change_data_in_pred do_data_rec p =
  change_exp_in_pred (change_data_in_exp do_data_rec) no_quantif_do_exp p
*)
(* ------------------------------------------------------------------------ *)
(* ---  Alpha-conversion                                                --- *)
(* ------------------------------------------------------------------------ *)

let rec pred_alpha_cv alpha p =
  let do_pred alpha p = pred_alpha_cv alpha p in
  let rec do_preds alpha l = match l with [] -> alpha, l
    | p::l ->
        let alpha, p = do_pred alpha p in
        let alpha, l = do_preds alpha l in
          alpha, p::l
  in
  match p with
  | Ptrue | Pfalse -> alpha, p
  | Pif (t,p1,p2) ->
      let alpha, t = term_alpha_cv alpha t in
      let alpha, p1 = do_pred alpha p1 in
      let alpha, p2 = do_pred alpha p2 in
        alpha, Pif (t,p1,p2)
  | Pnot p -> let alpha, p = do_pred alpha p in alpha, Pnot p
  | Pforall (x,p) ->
      let old_x, alpha = alpha_bound_var alpha x in
      let alpha, p = do_pred alpha p in
      let alpha = alpha_unbound alpha x old_x in
        alpha, Pforall (x,p)
  | Pexists (x,p) ->
      let old_x, alpha = alpha_bound_var alpha x in
      let alpha, p = do_pred alpha p in
      let alpha = alpha_unbound alpha x old_x in
        alpha, Pexists (x,p)
  | Plet (x,t,p) ->
      let alpha, t = term_alpha_cv alpha t in
      let old_x, alpha = alpha_bound_var alpha x in
      let alpha, p = do_pred alpha p in
      let alpha = alpha_unbound alpha x old_x in
        alpha, Plet (x,t,p)
  | Pnamed (n,p) -> let alpha, p = do_pred alpha p in alpha, Pnamed (n,p)
  | Pimplies (p1,p2) ->
      let alpha, p1 = do_pred alpha p1 in
      let alpha, p2 = do_pred alpha p2 in
        alpha, Pimplies (p1,p2)
  | Pand (p1,p2) ->
      let alpha, p1 = do_pred alpha p1 in
      let alpha, p2 = do_pred alpha p2 in
        alpha, Pand (p1,p2)
  | Por (p1,p2) ->
      let alpha, p1 = do_pred alpha p1 in
      let alpha, p2 = do_pred alpha p2 in
        alpha, Por (p1,p2)
  | Piff (p1,p2) ->
      let alpha, p1 = do_pred alpha p1 in
      let alpha, p2 = do_pred alpha p2 in
        alpha, Piff (p1,p2)
  | Papp (n,lt) ->
      let alpha, lt = terms_alpha_cv alpha lt in alpha, p_app n lt


let p_alpha_cv p =
  let alpha, p = pred_alpha_cv Vmap.empty p in
  let vars = Vmap.fold (fun _v v' acc -> v'::acc) alpha [] in
    vars, p

(* ------------------------------------------------------------------------ *)
(* ---  Translation (data type can be modified)                         --- *)
(* ------------------------------------------------------------------------ *)

let change_exp_in_pred do_exp = change_exp_in_pred do_exp no_quantif_do_exp

(* ------------------------------------------------------------------------ *)
(* ---  Variable substitutions                                              *)
(* ------------------------------------------------------------------------ *)

(** Similar to [change_vars_in_exp] but on predicates.
* Notice that we assume (and check) that [var_subst] only works on free
* variables (and that they are different from bounded ones).
*)
let subst_vars_in_pred var_subst p =
  let rec do_exp exp = change_in_exp var_subst exp
  in change_exp_in_pred do_exp p

(** Specialized version of [subst_vars_in_pred] to substitute one variable [v]
* by and expression [exp] in a predicate [p]. *)
let subst_in_pred x exp p =
  let var_subst v = if Var.equal v x then Some exp else None in
    subst_vars_in_pred var_subst p

       (*
let nb_var_in_pred prop_in_data v p =
  let nb_occ = ref 0 in
  let do_var () var = if Var.equal v var then nb_occ := !nb_occ + 1 in
  let rec do_exp () e = fold_data_in_exp do_var do_data () e
  and do_data () d = ignore (prop_in_data (fun e -> do_exp () e; e) d) in
  let _ = fold_exp_in_pred do_exp () p in
    !nb_occ
*)

(** Build a predicate equivalent to [let v = e in p] but may use the
* substitution in some simple cases (like substitute a variable by another
* variable for instance).
* [fresh] is only meaningfull when the [let] is actually built: it tells if we
* have to build a new variable for [v].
*)
let let_pred ~fresh v e p =
  if p = Ptrue then Ptrue
  else begin
    match e with
    | Tconst _ | Tvar _ -> subst_in_pred v e p
    | _ ->
        (* let nb_occ = nb_var_in_pred prop_in_data v p in
          (* TODOopt : do only one visit *)
          if nb_occ = 0 then p
          (* else if nb_occ = 1 then subst_in_pred prop_in_data v e p *)
          else *)
        if fresh then
          let v' = Var.fresh_var v in
          let p = subst_in_pred v (e_var v') p in
            p_let v' e p
        else p_let v e p
  end

let rec pred_replace alpha x exp p =
  let frec = pred_replace alpha x exp in
    match p with
      | Papp(f,ts) -> p_app f (List.map (term_replace alpha x exp) ts)
      | (Ptrue | Pfalse) as p -> p
      | Pimplies(p,q) -> p_implies (frec p) (frec q)
      | Pif(a,p,q) -> p_if (term_replace alpha x exp a) (frec p) (frec q)
      | Pand(p,q) -> p_and (frec p) (frec q)
      | Por(p,q)  -> p_or (frec p) (frec q)
      | Piff(p,q) -> p_iff (frec p) (frec q)
      | Pnot(p)   -> p_not (frec p)
      | Pforall(x0,p) as p0 ->
          begin
            if Var.equal x x0 then p0
            else if e_has_var [x0] exp then
                match alpha x with
                  | None -> Plet(x,exp,p0)
                  | Some y ->
                      let py = pred_replace alpha x0 (Tvar y) p in
                      let py' = frec py in
                        Pforall( y , py' )
            else Pforall(x0, frec p)
          end
      | Pexists(x0,p) as p0 ->
          begin
            if Var.equal x x0 then p0
            else if e_has_var [x0] exp then
                match alpha x with
                  | None -> Plet(x,exp,p0)
                  | Some y ->
                      let py = pred_replace alpha x0 (Tvar y) p in
                      let py' = frec py in
                        Pexists( y , py' )
            else Pexists(x0,frec p)
          end
      | Plet(x0,t,p) as p0 ->
          begin
            if e_has_var [x0] exp then
              match alpha x with
                | None -> Plet(x,exp,p0)
                | Some y ->
                    let t' = term_replace alpha x exp t in
                    let py = pred_replace alpha x0 (Tvar y) p in
                    let py' = frec py in
                      Plet( y , t' , py' )
            else
              let t' = term_replace alpha x exp t in
              let p' = if Var.equal x0 x then p else frec p in
                Plet(x0,t',p')
          end
      | Pnamed(a,p) -> Pnamed(a,frec p)

(* ------------------------------------------------------------------------ *)
(* ---  Quantification                                                      *)
(* ------------------------------------------------------------------------ *)

let fresh_vars_in_pred vars p =
  let do_var (vars, p) v =
    let v' = Var.fresh_var v in
    let p = subst_in_pred v (e_var v') p in
      v'::vars, p
  in List.fold_left do_var ([], p) vars

let p_forall_vars vars (p: pred) : pred =
    let vars, p = fresh_vars_in_pred vars p in
      List.fold_left (fun p v -> p_forall v p) p vars

let p_exists_vars vars (p: pred) : pred =
    let vars, p = fresh_vars_in_pred vars p in
      List.fold_left (fun p v -> p_exists v p) p vars

let rec term_calls f = function
  | Tconst _ | Tvar _ -> false
  | Tapp(g,ts) -> f=g || List.exists (term_calls f) ts
  | Tgetfield(_,a) -> term_calls f a
  | Tsetfield(_,a,b) | Taccess(a,b) | Tlet(_,a,b) -> 
      term_calls f a || term_calls f b
  | Tupdate(a,b,c) | Tif(a,b,c) ->
      term_calls f a || term_calls f b || term_calls f c

let rec pred_calls f = function
  | Ptrue | Pfalse -> false
  | Papp(g,ts) -> f=g || List.exists (term_calls f) ts
  | Pimplies(a,b) | Pand(a,b) | Por(a,b) | Piff(a,b) -> 
      pred_calls f a || pred_calls f b
  | Pnamed(_,p) | Pnot p | Pforall(_,p) | Pexists(_,p) -> 
      pred_calls f p
  | Pif(a,p,q) -> 
      term_calls f a || pred_calls f p || pred_calls f q
  | Plet(_,a,p) -> 
      term_calls f a || pred_calls f p

(* ------------------------------------------------------------------------ *)
(* ---  Huge                                                                *)
(* ------------------------------------------------------------------------ *)

exception Huge

let rec check_term m t =
  if m < 0 then raise Huge ;
  match t with
    | Tconst _ | Tvar _  -> pred m
    | Tapp(_,ts) -> List.fold_left check_term (pred m) ts
    | Tgetfield(_,r) -> check_term (pred m) r
    | Tsetfield(_,r,v) -> check_term (check_term (pred m) r) v
    | Taccess(t,i) -> check_term (check_term (pred m) t) i
    | Tupdate(t,i,v) -> check_term (check_term (check_term (pred m) t) i) v
    | Tif(a,b,c) -> check_term (check_term (check_term (pred m) a) b) c
    | Tlet(_,a,b) -> check_term (check_term (pred m) a) b

let rec check_pred m p =
  if m < 0 then raise Huge ;
  match p with
    | Ptrue | Pfalse -> pred m
    | Papp(_,ts) -> List.fold_left check_term (pred m) ts
    | Pimplies(a,b) | Pand(a,b) | Por(a,b) | Piff(a,b) -> check_pred (check_pred (pred m) a) b
    | Pnamed(_,p) | Pnot p | Pforall(_,p) | Pexists(_,p) -> check_pred (pred m) p
    | Pif(a,p,q) -> check_pred (check_pred (check_term (pred m) a) p) q
    | Plet(_,t,p) -> check_pred (check_term (pred m) t) p

let huge_term m t = try ignore (check_term m t) ; false with Huge -> true
let huge_pred m p = try ignore (check_pred m p) ; false with Huge -> true

(* ------------------------------------------------------------------------ *)

type decl = (Var.t,term,pred) Formula.declaration

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
