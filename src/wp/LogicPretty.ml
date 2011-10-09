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

(* -------------------------------------------------------------------------- *)
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau
open LogicRaw
let dkey = "pretty" (* debugging key *)

type u_printer = Format.formatter -> unit
type 'a printer = Format.formatter -> 'a -> unit
type ('a,'b) printer2 = Format.formatter -> 'a -> 'b -> unit
type 'a fun_printer = Format.formatter -> 'a -> TERM.t list -> unit

(* -------------------------------------------------------------------------- *)
(* --- Tools                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_coma sep pp fmt = function
  | [] -> ()
  | x::xs -> pp fmt x ; List.iter (fun y -> Format.fprintf fmt "%s@ %a" sep pp y) xs
      
let pp_assoc nil op pp fmt = function
  | [] -> Format.pp_print_string fmt nil
  | x::xs -> pp fmt x ; List.iter (fun y -> Format.fprintf fmt "@ %s@ %a" op pp y) xs

let rec pp_fold_op nil op pp fmt = function
  | [] -> Format.pp_print_string fmt nil
  | [x] -> pp fmt x
  | [x;y] -> Format.fprintf fmt "%a@ %s@ %a" pp x op pp y ; 
  | x::xs -> Format.fprintf fmt "%a@ %s@ (%a)" pp x op (pp_fold_op nil op pp) xs
      
let rec pp_fold_call nil op pp fmt = function
  | [] -> Format.pp_print_string fmt nil
  | [x] -> pp fmt x
  | x::xs -> Format.fprintf fmt "@[<hov 1>%s(%a,@,%a)@]" 
      op pp x (pp_fold_call nil op pp) xs

let rec pp_fold_apply nil op pp fmt = function
  | [] -> Format.pp_print_string fmt nil
  | [x] -> pp fmt x
  | x::xs -> Format.fprintf fmt "@[<hov 1>(%s@ %a@ %a)@]" 
      op pp x (pp_fold_apply nil op pp) xs

let pp_tuple pp fmt xs = Format.fprintf fmt "@[<hov 1>(%a)@]" (pp_coma "," pp) xs
      
let pp_string op fmt = Format.pp_print_string fmt op

let pp_tuple_call pp fmt f xs =
  Format.fprintf fmt "@[<hov 1>%s%a@]" f (pp_tuple pp) xs

let pp_apply_call pp fmt f = function
  | [] -> Format.pp_print_string fmt f
  | xs ->
      Format.fprintf fmt "@[<hov 1>(%s" f ;
      List.iter (fun x -> Format.fprintf fmt "@ %a" pp x) xs ;
      Format.fprintf fmt ")@]"

open VAR
open TERM
open PRED
      
let rec apply_labels p = function 
  | [] -> p | label::labels -> Pnamed(label,apply_labels p labels)

let rec fold_and acc labels = function
  | Pnamed(label,p) -> fold_and acc (label::labels) p
  | Pand(p,q) -> fold_and (fold_and acc labels q) labels p
  | Ptrue -> acc
  | p -> apply_labels p labels :: acc

let rec fold_hyp acc labels = function
  | Pnamed(label,p) -> fold_hyp acc (label::labels) p
  | Pand(p,q) -> fold_hyp (fold_hyp acc labels p) labels q
  | Ptrue -> acc
  | p -> apply_labels p labels :: acc
      
let rec fold_or acc labels = function
  | Pnamed(label,p) -> fold_or acc (label::labels) p
  | Por(p,q) -> fold_or (fold_or acc labels q) labels p
  | Pfalse -> acc
  | p -> apply_labels p labels :: acc

let rec fold_implies hs labels = function
  | Pimplies(p,q) -> fold_implies (fold_and hs labels p) [] q
  | Pnamed(label,p) -> fold_implies hs (label::labels) p
  | p -> List.rev hs , apply_labels p labels
      
let collect_and p = fold_and [] [] p
let collect_or p = fold_or [] [] p
let collect_implies p = fold_implies [] [] p

let rec fold_assoc op acc = function
  | Tprim( f , ts ) when op = f -> List.fold_left (fold_assoc op) acc ts
  | e -> e::acc

let associative op es = List.fold_left (fold_assoc op) [] es
  
let rec collect_labels ls = function
  | Pnamed(l,p) -> collect_labels (l::ls) p
  | p -> List.rev ls , p
      
let rec collect_forall xs = function
  | Pforall(x,p) -> collect_forall (x::xs) p
  | p -> List.rev xs , p
      
let rec collect_exists xs = function
  | Pexists(x,p) -> collect_exists (x::xs) p
  | p -> List.rev xs , p

type call_style = 
  | FunCall 
  | VFunCall
  | ApplyCall

let pp_call_style style pp_atom pp_free fmt f es =
  match style , es with
    | (VFunCall|ApplyCall) , [] -> Format.pp_print_string fmt f
    | (VFunCall|FunCall) , _ -> pp_tuple_call pp_free fmt f es
    | ApplyCall , _ -> pp_apply_call pp_atom fmt f es

type operator = 
  | Infix of string 
  | Prefix of string
  | Postfix of string
  | Assoc of string * string
  | Extern of string
  | Call of id

let operator_atomic = function
  | Prefix _ | Postfix _ | Call _ | Extern _ -> true
  | Infix _ | Assoc _ -> false
  
type binder = VAR.t * id

class engine (space:LogicId.space) =
object(self)

  (* -------------------------------------------------------------------------- *)
  (* --- Names                                                              --- *)
  (* -------------------------------------------------------------------------- *)
  
  method id x = LogicId.name space x
  method pp_id fmt x = Format.pp_print_string fmt (self#id x)
    
  (* -------------------------------------------------------------------------- *)
  (* --- Types                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  method pp_tau_int = pp_string "int"
  method pp_tau_real = pp_string "real"
  method pp_tau_bool = pp_string "bool"
  method pp_tau_pointer = pp_string "pointer"
  method pp_tau_set fmt te = Format.fprintf fmt "{%a..}" self#pp_tau te
  method pp_tau_array fmt ta tb =
    match ta with
      | Integer -> Format.fprintf fmt "@[%a[]@]" self#pp_tau tb
      | _ -> Format.fprintf fmt "@[%a[%a]@]" self#pp_tau tb self#pp_tau ta
  method pp_tau_record = self#pp_id
  method pp_tau_adt fmt a ts = 
    match ts with
      | [] -> self#pp_id fmt a
      | [t] -> Format.fprintf fmt "@[%a %a@]" self#pp_tau t self#pp_id a
      | ts -> Format.fprintf fmt "@[%a %a@]" (pp_tuple self#pp_tau) ts self#pp_id a	  
  method pp_tau_alpha fmt k =
    if 0<= k && k < 26 
    then Format.fprintf fmt "'%c" (char_of_int (int_of_char 'a' + k))
    else Format.fprintf fmt "'a%d" k
    
  method pp_tau fmt = function
    | Integer -> self#pp_tau_int fmt
    | Real -> self#pp_tau_real fmt
    | Boolean -> self#pp_tau_bool fmt
    | Pointer -> self#pp_tau_pointer fmt
    | Set te -> self#pp_tau_set fmt te
    | Array(ta,tb) -> self#pp_tau_array fmt ta tb
    | Record r -> self#pp_tau_record fmt r
    | ADT(a,ts) -> self#pp_tau_adt fmt a ts
    | ALPHA n -> self#pp_tau_alpha fmt n
	
  (* -------------------------------------------------------------------------- *)
  (* --- Variables                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  val mutable sigma : id VMAP.t = VMAP.empty
  val mutable alpha : bool = false

  method bind (xs:VAR.t list) (pp:unit -> unit) =
    let mark = LogicId.mark space in
    let sigma0 = sigma in
    sigma <- List.fold_left
      (fun sigma x ->
	 let xid = LogicId.push space (VAR.basename x) in
	 VMAP.add x xid sigma)
      sigma xs ;
    try 
      pp () ;
      sigma <- sigma0 ;
      if not alpha then LogicId.unmark space mark ;
    with error ->
      sigma <- sigma0 ;
      if not alpha then LogicId.unmark space mark ;
      raise error

  method binder (x:VAR.t) (pp:binder -> unit) =
    let xid = LogicId.push space (VAR.basename x) in
    try
      pp (x,xid) ;
      if not alpha then LogicId.pop space xid ;
    with error ->
      if not alpha then LogicId.pop space xid ;
      raise error

  method pp_binder fmt (_,id) = self#pp_id fmt id
  method with_binder : 'a. binder -> 'a printer -> 'a printer =
    fun (x,xid) pp fmt data -> 
      let sigma0 = sigma in
      sigma <- VMAP.add x xid sigma ;
      try
	pp fmt data ;
	sigma <- sigma0 ;
      with error ->
	sigma <- sigma0 ;
	raise error
      
  method alpha : 'a. 'a printer -> 'a printer = fun pp fmt data ->
    if alpha then pp fmt data
    else
      let mark = LogicId.mark space in
      alpha <- true ;
      try
	pp fmt data ;
	alpha <- false ;
	LogicId.unmark space mark ;
      with error ->
	alpha <- false ;
	LogicId.unmark space mark ;
	raise error
	  
  method var_id x = VMAP.find x sigma
  method pp_var fmt x =
    try self#pp_id fmt (VMAP.find x sigma)
    with Not_found -> Format.fprintf fmt "?%a" VAR.pretty x

  method pp_vartype fmt x = self#pp_tau fmt (VAR.tau_of_var x)

  (* -------------------------------------------------------------------------- *)
  (* --- Terms                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  method term_call = VFunCall

  method term_atomic = function
    | Tint s | Treal s -> not ( String.length s > 0 && s.[0] = '-' )
    | Tvar _ | Tcall _ | Tprim(_,[]) | Ttrue | Tfalse -> true
    | Tprim(pi,_) -> operator_atomic (self#term_operator pi)
    | Tgetfield _ -> true
    | Tsetfield _ -> true
    | Taccess _ -> true
    | Tupdate _ -> true
    | Tif _ -> false
    | Tlet _ -> false

  method term_operator = function
    | I_add | R_add -> Assoc("0","+")
    | I_mul | R_mul -> Assoc("1","*")
    | I_div -> Infix("div")
    | I_mod -> Infix("mod")
    | I_sub | R_sub -> Infix("-")
    | R_div -> Infix("/")
    | I_opp | R_opp -> Prefix("-")
    | I_of_R -> Prefix("(int)")
    | R_of_I -> Prefix("")
    | TERM.L_eq -> Infix("=")
    | TERM.L_neq -> Infix("<>")
    | TERM.I_lt | TERM.R_lt -> Infix("<")
    | TERM.I_leq | TERM.R_leq -> Infix("<=")
    | B_not -> Prefix("!")
    | B_and -> Assoc("true","&&")
    | B_or -> Assoc("false","||")
    | I_bnot -> Prefix("~")
    | I_band -> Infix("(&)")
    | I_bor  -> Infix("(|)")
    | I_bxor -> Infix("(+)")
    | I_lsl  -> Infix("(>>)")
    | I_lsr  -> Infix("(<<)")

  method pp_term_int = Format.pp_print_string
  method pp_term_real = Format.pp_print_string
  method pp_term_true = pp_string "true"
  method pp_term_false = pp_string "false"

  method pp_term_call fmt id es = 
    pp_call_style self#term_call self#pp_term_atom self#pp_term fmt (self#id id) es

  method pp_term_extern fmt f es =
    pp_call_style self#term_call self#pp_term_atom self#pp_term fmt f es

  method pp_term_operator fmt op es =
    match op , es with
      | Infix s , [a;b] -> Format.fprintf fmt "%a@ %s@ %a" 
	  self#pp_term_atom a s self#pp_term_atom b
      | Prefix s , [a] -> Format.fprintf fmt "%s%a" s self#pp_term_atom a
      | Postfix s , [a] -> Format.fprintf fmt "%a%s" self#pp_term_atom a s
      | (Infix s | Postfix s | Prefix s) , _ ->
	  Wp_parameters.fatal "Logic:operator(%s) with %d arguments" s 
	    (List.length es)
      | Assoc(nil,op) , _ -> pp_assoc nil op self#pp_term_atom fmt es
      | Call id , _ -> self#pp_term_call fmt id es
      | Extern f , _ -> self#pp_term_extern fmt f es

  method pp_term_primitive fmt pi es =
    let op = self#term_operator pi in
    let es = match op with Assoc _ -> associative pi es | _ -> es in
    self#pp_term_operator fmt op es

  method pp_term_getfield fmt r f =
    Format.fprintf fmt "%a.%a" self#pp_term_atom r self#pp_id f.f_name
  method pp_term_setfield fmt r f v =
    Format.fprintf fmt "@[<hv 2>%a@,.{%a <-@ %a}@]"
      self#pp_term_atom r self#pp_id f.f_name self#pp_term v
  method pp_term_access fmt r k =
    Format.fprintf fmt "%a[%a]" self#pp_term_atom r self#pp_term k
  method pp_term_update fmt r k v =
    Format.fprintf fmt "@[<hv 2>%a@,[%a <-@ %a]@]" 
      self#pp_term_atom r self#pp_term k self#pp_term v

  method pp_term_let fmt x a b =
    self#binder x 
      (fun bind ->
	 Format.fprintf fmt "@[<hv 2>let %a = %a in@ @]%a"
	   self#pp_binder bind
	   self#pp_term a
	   (self#with_binder bind self#pp_term) b)

  method pp_term_cond fmt c a b =
    if self#term_atomic a && self#term_atomic b then
      Format.fprintf fmt "%a?%a:%a"
	self#pp_term_atom c self#pp_term b self#pp_term c
    else
      Format.fprintf fmt "@[<hv 2>if %a@ then %a@ else %a@]"
	self#pp_term c self#pp_term a self#pp_term b

  method pp_term_atom fmt e =
    if self#term_atomic e then self#pp_term fmt e
    else Format.fprintf fmt "@[<hov 1>(%a)@]" self#pp_term e

  method pp_term fmt = function
    | Tvar x -> self#pp_var fmt x
    | Ttrue -> self#pp_term_true fmt
    | Tfalse -> self#pp_term_false fmt
    | Tint z -> self#pp_term_int fmt z
    | Treal z -> self#pp_term_real fmt z
    | Tcall(f,ts) -> self#pp_term_call fmt f ts
    | Tprim(pi,ts) -> self#pp_term_primitive fmt pi ts
    | Tgetfield(r,f) -> self#pp_term_getfield fmt r f
    | Tsetfield(r,f,v) -> self#pp_term_setfield fmt r f v
    | Taccess(r,k) -> self#pp_term_access fmt r k
    | Tupdate(r,k,v) -> self#pp_term_update fmt r k v
    | Tlet(x,a,b) -> self#pp_term_let fmt x a b
    | Tif(c,a,b) -> self#pp_term_cond fmt c a b

  (* -------------------------------------------------------------------------- *)
  (* --- Predicates                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  method pred_atomic = function
    | Ptrue | Pfalse | Prel(_,[]) | Pcall _ -> true
    | Prel(r,_) -> operator_atomic (self#pred_relation r)
    | _ -> false

  method pred_relation = function
    | PRED.I_lt | PRED.R_lt -> Infix("<")
    | PRED.I_leq | PRED.R_leq -> Infix("<=")
    | PRED.L_eq -> Infix("=")
    | PRED.L_neq -> Infix("<>")
    | B_false -> Prefix ""
    | B_true -> Prefix ""

  method pp_pred_true = pp_string "True"
  method pp_pred_false = pp_string "False"
  method pp_pred_relation fmt rel es =
    self#pp_term_operator fmt (self#pred_relation rel) es
  method pp_pred_call fmt id ts = 
    self#pp_term_call fmt id ts

  method pp_pred_and = pp_assoc "True" "/\\" self#pp_pred_atom 
  method pp_pred_or  = pp_assoc "False" "\\/" self#pp_pred_atom
  method pp_pred_not fmt p = Format.fprintf fmt "not %a" self#pp_pred_atom p
  method pp_pred_iff fmt p q = Format.fprintf fmt "%a@ <->@ %a" 
    self#pp_pred_atom p self#pp_pred_atom q

  method pp_pred_cond fmt c p q =
    Format.fprintf fmt "if %a@ then %a@ else %a"
      self#pp_term c self#pp_pred p self#pp_pred q

  method pp_pred_let fmt x a p =
    self#binder x 
      (fun bind ->
	 Format.fprintf fmt "@[<hv 2>let %a = %a in@ @]%a"
	   self#pp_binder bind
	   self#pp_term a
	   (self#with_binder bind self#pp_pred) p)

  method pp_pred_named fmt labels p =
    List.iter
      (fun label -> Format.fprintf fmt "%a:@," self#pp_id label)
      labels ;
    self#pp_pred_atom fmt p

  method pp_pred_implies fmt hs p =
    List.iter
      (fun h -> Format.fprintf fmt "%a ->@ " self#pp_pred_atom h) hs ;
    self#pp_pred fmt p

  method pp_pred_forall fmt xs p =
    Format.fprintf fmt "@[<hov 2>forall " ;
    self#bind xs
      (fun () ->
	 pp_coma "," 
	   (fun fmt x ->
	      Format.fprintf fmt "%a:%a"
		self#pp_var x self#pp_vartype x) 
	   fmt xs ;
	 Format.fprintf fmt ".@]@ %a" self#pp_pred p)

  method pp_pred_exists fmt xs p =
    Format.fprintf fmt "@[<hov 2>exists " ;
    self#bind xs
      (fun () ->
	 pp_coma "," 
	   (fun fmt x ->
	      Format.fprintf fmt "%a:%a"
		self#pp_var x self#pp_vartype x) 
	   fmt xs ;
	 Format.fprintf fmt ".@]@ %a" self#pp_pred p)

  method pp_pred fmt = function
    | Ptrue -> self#pp_pred_true fmt
    | Pfalse -> self#pp_pred_false fmt
    | Prel(r,es) -> self#pp_pred_relation fmt r es
    | Pcall(f,es) -> self#pp_pred_call fmt f es
    | Pand _ as p -> self#pp_pred_and fmt (collect_and p)
    | Por _ as p -> self#pp_pred_or fmt (collect_or p)
    | Pnot p -> self#pp_pred_not fmt p
    | Piff(p,q) -> self#pp_pred_iff fmt p q
    | Pcond(c,p,q) -> self#pp_pred_cond fmt c p q
    | Plet(x,a,p) -> self#pp_pred_let fmt x a p
    | Pnamed(label,p) -> 
	let labels,p = collect_labels [label] p in
	self#pp_pred_named fmt labels p
    | Pimplies _ as p ->
	let hs,p = collect_implies p in
	self#pp_pred_implies fmt hs p
    | Pforall(x,p) ->
	let xs,p = collect_forall [x] p in
	self#pp_pred_forall fmt xs p
    | Pexists(x,p) ->
	let xs,p = collect_exists [x] p in
	self#pp_pred_exists fmt xs p

  method pp_pred_atom fmt p =
    if self#pred_atomic p then self#pp_pred fmt p
    else Format.fprintf fmt "@[<hov 1>(%a)@]" self#pp_pred p

end

(* -------------------------------------------------------------------------- *)

(* WHY names, for informations

(* --- PRELUDE.why --- *)

let neg_int = "neg_int"
let add_int = "add_int"
let sub_int = "sub_int"
let mul_int = "mul_int"
let div_int = "computer_div"
let mod_int = "computer_mod"
let eq_int = "eq"
let ne_int = "neq"
let lt_int = "lt_int"
let le_int = "le_int"

(* --- BOOL.why --- *)

let bool_not = "bool_not"
let bool_and = "bool_and"
let bool_or  = "bool_or"

(* --- INTEGERS.why --- *)

let eq_int_bool = "eq_int_bool"
let ne_int_bool = "neq_int_bool"
let lt_int_bool = "lt_int_bool"
let le_int_bool = "le_int_bool"

(* --- REAL.why --- *)

let neg_real = "neg_real"
let add_real = "add_real"
let sub_real = "sub_real"
let mul_real = "mul_real"
let fract_real = "div_real"

let eq_real_bool  = "eq_real_bool"
let ne_real_bool  = "neq_real_bool"
let lt_real_bool  = "lt_real_bool"
let le_real_bool  = "le_real_bool"

let eq_real = "eq_real"
let ne_real  = "neq_real"
let lt_real  = "lt_real"
let le_real  = "le_real"

let integer_of_real = "truncate_real_to_int"
let real_of_integer = "real_of_int"

*)
