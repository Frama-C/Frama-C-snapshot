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

open Lang
open Lang.F
open Qed.Logic

(** Debug **)

let dkey = Wp_parameters.register_category "cnf"
let debug fmt = Wp_parameters.debug ~dkey fmt
let debugN level fmt = Wp_parameters.debug ~level ~dkey fmt

(** Can be moved into Qed **)

let s_bool p = [p; e_not p]

(* is it an atom for CNF/DNF *)
let is_cnf_dnf_atom_repr = function
  | If(_,x,y) | Eq(x,y) | Neq(x,y) -> 
      not (is_prop x && is_prop y)
  | And _   | Or  _
  | Imply _ | Not _ -> false
  | _ -> true

let is_cnf_dnf_literal_repr = function
  | Not _ -> true
  | _ as repr -> is_cnf_dnf_atom_repr repr
let is_cnf_dnf_literal e =  is_cnf_dnf_literal_repr (repr e)

let is_conj0_literal_repr = function
  | And xs -> List.for_all is_cnf_dnf_literal xs
  | _ as repr -> is_cnf_dnf_literal_repr repr
let is_conj0_literal e = is_conj0_literal_repr (repr e)

let is_disj0_literal_repr = function
  | Or xs -> List.for_all is_cnf_dnf_literal xs
  | _ as repr -> is_cnf_dnf_literal_repr repr
let is_disj0_literal e = is_disj0_literal_repr (repr e)

(* is it already into a Conjunctive Normal Form *)
let is_cnf_repr = 
  function
  | And xs ->  List.for_all is_disj0_literal xs
  | _ as repr -> is_disj0_literal_repr repr
let is_cnf e = is_cnf_repr (repr e)

(* is it already into a Disjunctive Normal Form *)
let is_dnf_repr = function
  | Or xs ->  List.for_all is_conj0_literal xs
  | _ as repr -> is_conj0_literal_repr repr
let is_dnf e = is_dnf_repr (repr e)

(** CNF/DNF tools **)

exception Absorbant
exception TooBig

type xf_t = term list
type xnf_t = xf_t list
type xNf_t = xf_t * xnf_t

let is_true_repr = function | True -> true | _ -> false 
let is_false_repr = function | False -> true | _ -> false

let is_conj_literal_repr = function
  | And xs -> List.for_all is_cnf_dnf_literal xs
  | _ -> false
let is_disj_literal_repr = function
  | Or xs -> List.for_all is_cnf_dnf_literal xs
  | _ -> false

let conj_args e = match repr e with
  | And xs -> xs
  | _ -> [e]
let disj_args e = match repr e with
  | Or xs -> xs
  | _ -> [e]

let normalize_cf ts =
  (* TODO: use something like Qed.Term.conjunction *)
  let c = e_and ts in
  match repr c with
  | False -> raise Absorbant
  | True -> []
  | And cf -> cf
  | _ -> [ c ]

let normalize_df ts =
  (* TODO: use something like Qed.Term.disjunction *)
  let c = e_or ts in
  match repr c with
  | True -> raise Absorbant
  | False -> []
  | Or cf -> cf
  | _ -> [ c ]

(*** one step of CNF/DNF ***) 

let s_cnf_ite c p q = [e_imply [c] p; e_imply [e_not c] q]
let s_dnf_ite c p q = [e_and [c;p]; e_and [e_not c;e_not q]]
let s_cnf_iff p q = [e_imply [p] q; e_imply [q] p]
let s_dnf_iff p q = [e_and [p;q]; e_and [e_not p;e_not q]]
let s_cnf_xor p q = [e_imply [e_not p] q; e_imply [e_not q] p]
let s_dnf_xor p q = [e_and [e_not p; q]; e_and [e_not q; p]]

type repr = QED.repr

type cnf_dnt_tools = { 
  normalize_xf: xf_t -> xf_t ;
  is_neutral_repr: repr -> bool ;
  is_absorbant_repr: repr -> bool ;
  neutral: term ;
  absorbant: term ;
  mk_top: xf_t -> term ;
  mk_sub: xf_t -> term ;
  sub_args: term -> xf_t ;
  is_sub_repr: repr -> bool ;
  s_ite: term -> term -> term -> term list ;
  s_iff: term -> term -> term list ;
  s_xor: term -> term -> term list ;
}

let cnf_record = {
  normalize_xf=normalize_cf;
  is_neutral_repr=is_true_repr; 
  is_absorbant_repr=is_false_repr;
  neutral=e_true;
  absorbant=e_false;
  mk_top=e_and;
  mk_sub=e_or;
  sub_args=disj_args;
  is_sub_repr=is_disj_literal_repr;
  s_ite=s_cnf_ite;
  s_iff=s_cnf_iff;
  s_xor=s_cnf_xor;
}

let dnf_record = {
  normalize_xf=normalize_df;
  is_neutral_repr=is_false_repr;
  is_absorbant_repr=is_true_repr; 
  neutral=e_false;
  absorbant=e_true;
  mk_top=e_or;
  mk_sub=e_and;
  sub_args=conj_args;
  is_sub_repr=is_conj_literal_repr;
  s_ite=s_dnf_ite;
  s_iff=s_dnf_iff;
  s_xor=s_dnf_xor;
}

let neutral:xNf_t = [],[]

(*** Pretty ***) 

let pp_indent ~pol fmt = function
  | x when x <= 0 -> Format.fprintf fmt "xxx * "
  | x -> Format.fprintf fmt "xxx%s * " (String.make (2*(x-1)+(if pol then 0 else 1)) ' ')

let pp_xf ~pol fmt = function
  | [] -> Format.fprintf fmt "%sf [%s neutral)]"
            (if pol then "c" else "d") (if pol then "TRUE" else "FALSE ")
  | xf -> Format.printf "%sf [" (if pol then "c" else "d");
      List.iter (fun x -> Format.fprintf fmt "%s %a " (if pol then "&&" else "||") Lang.F.pp_term x) xf;
      Format.printf "]"

let pp_xNf ~pol ~depth fmt xNf = 
  let pp_xNf fmt = function
    | [] -> Format.fprintf fmt " (%sNF %s absorbant);@?"
              (if pol then "C" else "D") (if pol then "FALSE" else "TRUE ")
    | xf -> List.iter (fun x -> Format.fprintf fmt "%s %a @?" (if pol then "||" else "&&") Lang.F.pp_term x) xf;
  in
  match xNf with
  | [],[] -> Format.fprintf fmt "%sNF %s neutral=[]@?"
               (if pol then "C" else "D") (if pol then "TRUE " else "FALSE")
  | xf,xnf -> Format.fprintf fmt "%sNF [@?" (if pol then "C" else "D") ;
      if xf <> [] then List.iter (fun x -> Format.fprintf fmt "%s (%a) @?" (if pol then "&&" else "||") Lang.F.pp_term x) xf;
      List.iter (fun x -> Format.fprintf fmt "@.%a %s [%a]@?" (pp_indent ~pol) depth (if pol then "&&" else "||") pp_xNf x) xnf;
      Format.fprintf fmt "]@?"

(** Transforms [e] into CNF/DNF  **)

let cnf_dnf ~pol ~depth e =
  let literal (cf,cnf,others) e = (e::cf),cnf,others in
  let normalized (cf,cnf,others) e = cf,(e::cnf),others in
  let unnormalized (cf,cnf,others) e = cf,cnf,(e::others) in
  let tools ~pol = if pol then cnf_record else dnf_record in
  let flat ~tool ~pol =
    let rec flatten acc = List.fold_left flat acc
    and flat acc e =
      match repr e with
      | Eq(x,y) when (F.is_prop x) && (F.is_prop y) ->
          flatten acc (tool.s_iff x y)
      | Neq(x,y) when (F.is_prop x) && (F.is_prop y) -> 
          flatten acc (tool.s_xor x y)
      | If(c,p,q) ->
          flatten acc (tool.s_ite c p q)

      | Imply _ when pol -> unnormalized acc e 
      | Imply (xe,x)  -> 
          flatten acc (x::(List.map (fun x -> e_not x) xe))

      | Or  xs when not pol -> flatten acc xs
      | And xs when     pol -> flatten acc xs
      | repr when tool.is_absorbant_repr  repr -> raise Absorbant
      | repr when tool.is_neutral_repr    repr -> acc
      | repr when is_cnf_dnf_literal_repr repr -> literal acc e
      | repr when tool.is_sub_repr        repr -> normalized acc e

      | And _ 
      | Or  _ -> unnormalized acc e

      | _ -> unnormalized acc e
    in flat ([],[],[])
  in 
  let c_cNf_cNf2cNf ~tool ~pol ~depth ((cf1,cnf1):xNf_t) ((cf2,cnf2):xNf_t) : xNf_t =
    (*[LC] TODO: check ignored variables *)
    ignore pol ;
    ignore depth ;
    match cnf2 with
    | ([]::_) -> raise Absorbant (* @absorbant @ _ = @absorbant *) 
    | _ ->   
        (* TODO: uses Qed.Term.consequence_style *)
        let cf,cnf = List.fold_left 
            (fun (cf,cnf) -> function | [] -> raise Absorbant | [x] -> (x::cf),cnf | df -> cf,(df::cnf))
            neutral cnf1 
        in 
        let cf = if cf1=[] && cf=[] then cf2 else tool.normalize_xf cf@cf1@cf2 in
        cf, (cnf@cnf2) 

  in
  (* distribution for CNF/DNF as literal list list *)
  let dNf2cNf ~tool ~pol ~depth (dNf:xNf_t) : xNf_t =
    let pp_i fmt () = (pp_indent ~pol) fmt depth in
    let df2cNf (df:xf_t) : xNf_t = match df with
      | []  -> raise Absorbant (* #neutral = @absorbant *) 
      | [_]  -> df,[]
      | _ -> [],[df]
    in
    let c_df_cNf2cNf (df:xf_t) (cNf:xNf_t) : xNf_t =  c_cNf_cNf2cNf ~tool ~pol ~depth (df2cNf df) cNf
    in
    (* (d1#...#dm) # (c1@...@cn) = (c1#d1#...#dm) @ ... @ (cn#d1#...#dm)
       (d1#...#dm) # @neutral/#absorbant = @neutral = ([],[]) *)
    let d_df_cf2cNf (df:xf_t) (cf:xf_t) : xNf_t = match df with
      | [] -> cf,[]  (* #neutral # (c1@...@cn) = (c1@...@cn) *)
      | df -> List.fold_left (fun (acc:xNf_t) (x:term) -> c_df_cNf2cNf (x::df) acc) neutral cf
    in
    (* (d1#...#dm) # (D1@...@Dn) = (D1#d1#...#dm) @ ... @ (Dn#d1#...#dm)
       (d1#...#dm) # @neutral/#absorbant = @neutral = ([],[]) *)
    (*[LC] TODO: check function never called *)
    let _d_df_cnf2cNf (df:xf_t) (cnf:xnf_t) : xNf_t = match df with
      | [] -> ([],cnf) (* #neutral # (D1@...@Dn) = (D1@...@Dn) *)
      | df -> List.fold_left (fun (acc:xNf_t) (df':xf_t) -> c_df_cNf2cNf (df'@df) acc) neutral cnf
    in
    (* (c1@...@cn) # (c1'@...@ck'@D1@...@Dm) = ((c1@...@cn)#c1') @ ... @ ((c1@...@cn)#c1k') @ ((c1@...@cn)#D1) @ ...@ ((c1@...@cn)#Dm)
       (c1@...@cn) # @neutral/#absorbant = @neutral *)
    let d_cf_cNf2cNf (cf:xf_t) (cNf':xNf_t) : xNf_t = 
      let r = match cf,cNf' with
        | _,([],[]) -> debugN 4 "%a> d_cf_cNf2cNf cas1/4@." pp_i ();
            cNf'     (* (c1@...@cn) # @neutral/#absorbant= @neutral *)
        | [],_      -> debugN 4 "%a> d_cf_cNf2cNf cas2/4@." pp_i ();
            neutral  (* @neutral/#absorbant # (c1'@...@ck'@D1@...@Dm) = @neutral *)
        | _, (_,[]::_) -> debugN 4 "%a> d_cf_cNf2cNf cas3/4@." pp_i ();
            cf,[]    (* (c1@...@cn) # #neutral/@absorbant= (c1@...@cn) *)
        | _,(cf',cnf') -> debugN 4 "%a> d_cf_cNf2cNf cas4/4  cf(%d) cNf(%d,%d)@." pp_i ()
                            (List.length cf) (List.length cf') (List.length cnf');
            if 2048 < (List.length cf)*((List.length cf')+(List.length cnf')) then raise TooBig ;
            let cNf1 = List.fold_left (fun (acc:xNf_t) (x:term) -> c_cNf_cNf2cNf ~tool ~pol ~depth (d_df_cf2cNf [x] cf) acc) neutral cf' in
            List.fold_left (fun (acc:xNf_t) (df:xf_t) -> c_cNf_cNf2cNf ~tool ~pol ~depth (d_df_cf2cNf df cf) acc) cNf1 cnf'
      in
      debugN 4 "%a> d_cf_cNf2cNf %sNf(%d,%b) %a %a =@.%a> d_cf_cNf2cNf = %a@." pp_i ()
        (if pol then "C" else "D") depth pol (pp_xf ~pol) cf (pp_xNf ~pol ~depth) cNf' 
        pp_i () (pp_xNf ~pol ~depth) r;
      r
    in
    (* (c1@...@ck@D1@...@Dn) # (C1#...#Cm) = (C1#(c1@...@ck@D1@...@Dn)) # (C2#...#Cm)  *)
    let rec d_cNf_dnf2cNf (cNf:xNf_t) (dnf:xnf_t) : xNf_t = 
      debugN 3 "%a> d_cNf_dnff2cNf cNf(%d,%d) dnf(%d)=...@." pp_i ()
        (List.length (fst cNf)) (List.length (snd cNf)) (List.length dnf);
      match dnf with
      | [] -> cNf        (* (c1@...@ck@D1@...@Dn) # @absorbant/#neutral = (D1@...@Dn) *)
      | []::_ -> neutral (* (c1@...@ck@D1@...@Dn) # @neutral/#absorbant = @neutral *)
      | cf::[]-> d_cf_cNf2cNf cf cNf (* (c1@...@ck@D1@...@Dn) # (c11@...@c1k) = (c11@...@c1k) # (c1@...@ck@D1@...@Dn) *)          
      | cf::dnf -> (* (c1@...@ck@D1@...@Dn) # ((c11@...@c1k)#C2#...#Cm) =
                      ((c11@...@c1k)#(c1@...@ck@D1@...#@n)) @ (C2#...#Cm) *)
          d_cNf_dnf2cNf (d_cf_cNf2cNf cf cNf) dnf
    in
    debugN 3 "%a> %sNf->%sNf(%d,%b) %a=...@." pp_i ()
      (if pol then "D" else "C") (if pol then "C" else "D") depth pol (pp_xNf ~pol:(not pol) ~depth) dNf;
    (* (d1#...#dk)#(C1#...#Cm) = (d1#...#dk) # (C1#...#Cm) *)
    let r = match dNf with
      | [],[] -> raise Absorbant (* #neutral = @absorbant *)
      | ([_] as df),dnf -> d_cNf_dnf2cNf (df,[])   dnf
      | df,dnf          -> d_cNf_dnf2cNf ([],[df]) dnf
    in
    debugN 3 "%a> %sNf->%sNf(%d,%b) %a =@.%a> %a@." 
      pp_i () (if pol then "D" else "C") (if pol then "C" else "D") depth pol (pp_xNf ~pol:(not pol) ~depth) dNf 
      pp_i () (pp_xNf ~pol ~depth) r;
    r
  in
  let rec cnf_dnf ~depth ~pol e =
    debugN 2 "@.%a%sNf(%d,%b) %a@." (pp_indent ~pol) depth 
      (if pol then "C" else "D") depth pol pp_term e;
    if depth <> -1 && depth <= 0 then [e],[]
    else 
      let tool = tools ~pol in
      try 
        let c_cf_cnf2cNf (cf:xf_t) (cnf:xf_t) : xNf_t =
          (* TODO: uses Qed.Term.consequence_style *)
          (tool.normalize_xf cf), (List.map tool.sub_args cnf)
        in
        let cf,cnf,cxf = flat ~tool ~pol e in
        (* [cf@cnf] part is into normal form, but the [cxf] part isn't.
           May raise Absorbant.
        *)
        let (cf,cnf) as cNf = c_cf_cnf2cNf cf cnf in
        let depth = if depth <> -1 && (not pol) then depth-1 else depth in
        try 
          let c_cNf_cdf2cNf cNf xf =
            let dNf = cnf_dnf ~depth ~pol:(not pol) xf in
            c_cNf_cNf2cNf ~tool ~pol ~depth cNf (dNf2cNf ~tool ~pol ~depth dNf) 
          in
          List.fold_left c_cNf_cdf2cNf cNf cxf
        with | TooBig -> debug "Too big CNF/DNF@." ; 
          (cf@cxf),cnf
      with | Absorbant -> [],[[]]
  in 
  let tool = tools pol in
  let cNf = cnf_dnf ~depth ~pol e in
  try 
    match cNf with
    | [],[] -> tool.neutral
    | cf,cnf -> 
        let mk_sub = function 
          | [] -> raise Absorbant
          | df ->
              let r = tool.mk_sub df in
              if tool.is_absorbant_repr (F.repr r) then raise Absorbant
              else r
        in
        tool.mk_top (cf@(List.map mk_sub cnf))
  with Absorbant -> tool.absorbant

let cnf_dnf ~pol ?(depth=(-1)) = cnf_dnf ~pol ~depth

let e_cnf = cnf_dnf ~pol:true
let e_dnf = cnf_dnf ~pol:false

(** Register new available transformation at Conditions.closure **)

(* feature at Conditions.closure and also for debugging purposes *)
let () = Conditions.at_closure (fun ((step,goal) as sequent) -> 
    match Wp_parameters.SplitDepth.get () with
    | 0 ->  sequent
    | depth when depth < -1  -> 
        (* Unspecified debug mode checking the correctness of CNF algo:
           `H |- P` is replaced by `H |- P <-> CNF(P)` *)
        let cnf = e_cnf ~depth:(-(depth+3)) (e_prop goal) in
        debug " CNF=%a@." pp_term cnf;
        step, p_equiv goal (F.p_bool cnf)

    | depth ->  
        (* `H |- P` is replaced by `H |- CNF(P)` *)
        let cnf = e_cnf ~depth (e_prop goal) in
        debug " CNF=%a@." pp_term cnf;
        step, p_bool cnf
  )

