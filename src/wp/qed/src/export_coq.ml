(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
(* --- Exportation Engine for Coq                                         --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Plib
open Linker
open Engine
open Export

module Kreal =
struct
  
  (* -------------------------------------------------------------------------- *)
  (* --- Coq Real Constants                                                 --- *)
  (* -------------------------------------------------------------------------- *)
  
  let error () = raise (Invalid_argument "invalid real constant")
    
  type sign = Positive | Negative
  type state = Integral | Fraction | Exponent
      
  type env = {
    mantiss  : Buffer.t ;
    exponent : Buffer.t ;
    mutable sign  : sign ;
    mutable coma  : int ;  (* number of digits afer '.' *)
    mutable state : state ;
  }
      
  type token = Digit | Plus | Minus | Exp | Dot
      
  let token = function
    | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') -> Digit
    | '.' -> Dot
    | '-' -> Minus
    | '+' -> Plus
    | 'e' | 'E' -> Exp
    | _ -> error ()
	
  let trail m =
    let n = String.length m in
    let rec first k = if k < n && m.[k] = '0' then first (succ k) else k in
    let rec last k = if k >= 0 && m.[k] = '0' then last (pred k) else k in
    let a = first 0 in
    let b = last (n-1) in
    if a <= b then Some(a , n-b-1) else None
      
  let convert r =
    let e = {
      mantiss  = Buffer.create 64 ; sign = Positive ;
      exponent = Buffer.create 64 ;
      coma = 0 ; state = Integral ;
    } in
    String.iter
      (fun c ->
	 let tk = token c in
	 match e.state , tk with
           | _ , Dot -> e.state <- Fraction ;
           | _ , Exp -> e.state <- Exponent ;
           | (Integral|Fraction) , Plus -> e.sign <- Positive
           | (Integral|Fraction) , Minus -> e.sign <- Negative
           | Integral , Digit -> Buffer.add_char e.mantiss c
           | Fraction , Digit -> Buffer.add_char e.mantiss c ; e.coma <- succ e.coma
           | Exponent , (Minus|Digit) -> Buffer.add_char e.exponent c
	   | Exponent , Plus -> ()
      ) r ;
    let m = Buffer.contents e.mantiss in
    begin
      match trail m with
	| None -> "0"
	| Some(a,b) ->
            let digits = String.sub m a (String.length m - a - b) in
            let exp =
              let ex = Buffer.contents e.exponent in
              if ex = "" then b - e.coma else int_of_string ex + b - e.coma
            in
            let size = 4 + String.length m + abs exp in
            let buffer = Buffer.create size in
            let parent = e.sign = Negative || exp <> 0 in
            if parent then Buffer.add_char buffer '(' ;
            if e.sign = Negative then Buffer.add_char buffer '-' ;
            Buffer.add_string buffer digits ;
            if exp > 0 then Buffer.add_string buffer (String.make exp '0') ;
            if exp < 0 then
              (Buffer.add_string buffer "/1" ;
               Buffer.add_string buffer (String.make (-exp) '0')) ;
            if parent then Buffer.add_char buffer ')' ;
            Buffer.contents buffer
    end
      
end

module Make(T : Term) =
struct

  module T = T
  module E = Export.Make(T)

  open T

  type tau = (Field.t,ADT.t) datatype
  type record = (Field.t * term) list
  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  let libraries = [
    "Bool" ; "ZArith" ; "Reals" ;
    "Qed" ; "Cdiv" ;
  ]
    
  class virtual engine =
  object(self)

    inherit E.engine

    initializer
      begin
	self#declare_all ["Z";"Real";"bool";"Prop";"array";"farray"] ;
	for i=1 to 26 do 
	  let c = int_of_char 'A' + i - 1 in
	  self#declare (Printf.sprintf "%c" (char_of_int c))
	done ;
	self#declare_all ["true";"false";"True";"False"] ;
	self#declare_all ["IZT"] ;
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Types                                                              --- *)
    (* -------------------------------------------------------------------------- *)

    method t_int = "Z"
    method t_real = "R"
    method t_bool = "bool"
    method t_prop = "Prop"
    method t_atomic = function
      | Int | Real | Bool | Prop | Tvar _ -> true
      | Array _ -> false
      | Data(_,[]) -> true
      | Data _ -> false
      | Record _ -> true

    method pp_array fmt t = 
      fprintf fmt "array %a" self#pp_subtau t

    method pp_farray fmt a b = 
      fprintf fmt "farray %a %a" self#pp_subtau a self#pp_subtau b

    method pp_tvar fmt k = 
      if 1 <= k && k <= 26 then 
	let c = int_of_char 'A' + (k-1) in
	pp_print_char fmt (char_of_int c)
      else
	fprintf fmt "A%d" k

    method virtual datatype : T.ADT.t -> string

    method pp_datatype adt fmt = function
      | [] -> pp_print_string fmt (self#datatype adt)
      | ts -> Plib.pp_call_apply (self#datatype adt) self#pp_subtau fmt ts

    (* -------------------------------------------------------------------------- *)
    (* --- Primitives                                                         --- *)
    (* -------------------------------------------------------------------------- *)
	  
    method callstyle = CallApply
    method op_scope = function Aint -> Some "%Z" | Areal -> Some "%R"

    method pp_int fmt z = Z.pretty fmt z
    method pp_real fmt r =
      pp_print_string fmt (Kreal.convert (R.to_string r))
	
    method e_true  = function Cterm -> "true"  | Cprop -> "True"
    method e_false = function Cterm -> "false" | Cprop -> "False"

    (* -------------------------------------------------------------------------- *)
    (* --- Arithmetics                                                        --- *)
    (* -------------------------------------------------------------------------- *)
	      
    method op_add (_:amode) = Assoc "+"
    method op_mul (_:amode) = Assoc "*"
    method op_div = function Aint -> Call "Cdiv" | Areal -> Call "Rdiv"
    method op_mod = function Aint -> Call "Cmod" | Areal -> Call "Rmod"
    method op_minus (_:amode) = Op "-"
    method op_real_of_int = Call "IZR"

    method op_eq  (c:cmode) (a:amode) =
      match c , a with
	| Cprop , _ -> Op "="
	| Cterm , Aint  -> Call "Zeq_bool"
	| Cterm , Areal -> Call "Req_bool"

    method op_neq  (c:cmode) (a:amode) =
      match c , a with
	| Cprop , _ -> Op "<>"
	| Cterm , Aint  -> Call "Zneq_bool"
	| Cterm , Areal -> Call "Rneq_bool"

    method op_lt  (c:cmode) (a:amode) =
      match c , a with
	| Cprop , _ -> Op "<"
	| Cterm , Aint  -> Call "Zlt_bool"
	| Cterm , Areal -> Call "Rlt_bool"

    method op_leq  (c:cmode) (a:amode) =
      match c , a with
	| Cprop , _ -> Op "<="
	| Cterm , Aint  -> Call "Zle_bool"
	| Cterm , Areal -> Call "Rle_bool"

    (* -------------------------------------------------------------------------- *)
    (* --- Connectives                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method op_not   = function Cterm -> Call "negb"  | Cprop -> Op "~"
    method op_or    = function Cterm -> Call "orb"   | Cprop -> Assoc "\\/"
    method op_and   = function Cterm -> Call "andb"  | Cprop -> Assoc "/\\"
    method op_imply = function Cterm -> Call "implb" | Cprop -> Assoc "->"
    method op_equiv = function Cterm -> Call "eqb"   | Cprop -> Op "<->"
    method op_equal = function Cterm -> Call "Aeq_bool" | Cprop -> Op "="
    method op_noteq = function Cterm -> Call "Aneq_bool" | Cprop -> Op "<>"

    (* -------------------------------------------------------------------------- *)
    (* --- Conditional                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_conditional fmt a b c =
      match Export.cmode self#mode with
	| Cterm -> 
	    begin
	      fprintf fmt "itep@ " ;
	      self#with_mode Mterm (fun _ -> self#pp_atom fmt a) ;
	      fprintf fmt "@ %a" self#pp_atom b ;
	      fprintf fmt "@ %a" self#pp_atom c ;
	    end
	| Cprop ->
	    begin
	      fprintf fmt "@[<hov 0>if " ;
	      self#with_mode Mterm (fun _ -> self#pp_atom fmt a) ;
	      fprintf fmt "@ then %a" self#pp_atom b ;
	      fprintf fmt "@ else %a" self#pp_atom c ;
	      fprintf fmt "@]" ;
	    end

    (* -------------------------------------------------------------------------- *)
    (* --- Arrays                                                             --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_array_get fmt m k = 
      fprintf fmt "%a@ %a" self#pp_atom m self#pp_atom k

    method pp_array_set fmt m k v =
      fprintf fmt "%a.[ %a <- %a ]" self#pp_atom m self#pp_flow k self#pp_flow v

    (* -------------------------------------------------------------------------- *)
    (* --- Records                                                            --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual field : T.Field.t -> string

    method pp_get_field fmt r f =
      fprintf fmt "%s@ %a" (self#field f) self#pp_atom r

    method pp_def_fields fmt fvs =
      begin
	fprintf fmt "@[<hov 2>{|" ;
	Plib.iteri
	  (fun i (f,v) -> match i with
	     | Ifirst | Imiddle -> 
		 fprintf fmt "@ @[<hov 2>%s := %a ;@]" (self#field f) self#pp_flow v
	     | Isingle | Ilast ->
		 fprintf fmt "@[<hov 2>%s := %a@]" (self#field f) self#pp_flow v
	  ) fvs ;
	fprintf fmt "@ |}@]" ;
      end

    (* -------------------------------------------------------------------------- *)
    (* --- Atomicity                                                          --- *)
    (* -------------------------------------------------------------------------- *)

    method is_atomic e =
      match T.repr e with
	| Kint z -> Z.positive z
	| Kreal _ -> true
	| Apply(_,[]) | Rdef _ -> true
	| Apply _ | Aset _ | Aget _ | Rget _ -> false
	| Eq _ | Neq _ | Lt _ | Leq _
	| And _ | Or _ | Imply _ | Bind _ | Fun _ | If _ -> false
	| _ -> T.is_simple e

    method is_shareable e =  not (T.is_prop e)

    method pp_let fmt x e =
      fprintf fmt "@[<hov 4>let %s := %a in@]@ " x self#pp_flow e

    (* -------------------------------------------------------------------------- *)
    (* --- Higher Order                                                       --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_apply _cmode e fmt es =
      begin
	fprintf fmt "@[<hov 3>(%a" self#pp_atom e ;
	List.iter (fun a -> fprintf fmt "@ %a" self#pp_atom a) es ;
	fprintf fmt ")@]"
      end

    method private pp_param fmt x =
      fprintf fmt "(%a : %a)" self#pp_var x self#pp_tau (T.tau_of_var x)

    method pp_forall tau fmt = function
      | [] -> ()
      | x::xs ->
	  fprintf fmt "@[<hov 2>forall (%a" self#pp_var x ;
	  List.iter (fun y -> fprintf fmt "@ %a" self#pp_var y) xs ;
	  fprintf fmt "@ : %a),@]" self#pp_tau tau 

    method pp_exists tau fmt = function
      | [] -> ()
      | x::xs ->
	  fprintf fmt "@[<hov 2>exists %a : %a@]," 
	    self#pp_var x self#pp_tau tau ;
	  List.iter
	    (fun x ->
	       fprintf fmt "@ @[<hov 2>exists %a : %a@]," 
		 self#pp_var x self#pp_tau tau) xs
	    
    method pp_lambda fmt xs =
      Plib.iteri
	(fun i x -> match i with
	   | Isingle -> fprintf fmt "@[<hov 2>fun %a =>@]@ " self#pp_param x
	   | Ifirst  -> fprintf fmt "@[<hov 2>fun %a" self#pp_param x
	   | Imiddle -> fprintf fmt "@ %a" self#pp_param x
	   | Ilast   -> fprintf fmt "@ %a =>@]@ " self#pp_param x
	) xs
	      
    (* -------------------------------------------------------------------------- *)
    (* --- Declarations                                                       --- *)
    (* -------------------------------------------------------------------------- *)

    method private pp_declare_poly fmt n =
      if n > 0 then
	begin
	  fprintf fmt " (" ;
	  for i=1 to n do fprintf fmt "%a " self#pp_tvar i done ;
	  fprintf fmt " : Set)" ;
	end ;
      
    method declare_type fmt adt n = function
      | Tabs ->
	  begin
	    fprintf fmt "Parameter %s" (self#datatype adt) ;
	    self#pp_declare_poly fmt n ;
	    fprintf fmt " : Set.@\n"
	  end
      | Tdef def ->
	  begin
	    fprintf fmt "@[<hov 2>Definition %s" (self#datatype adt) ;
	    self#pp_declare_poly fmt n ;
	    fprintf fmt " : Set :=@ %a@].@\n" self#pp_tau def ;
	  end
      | Trec fts ->
	  begin
	    fprintf fmt "@[<hv 0>Record %s" (self#datatype adt) ;
	    self#pp_declare_poly fmt n ;
	    fprintf fmt " : Set := {@[<hv 2>" ;
	    Plib.iteri
	      (fun idx (f,t) ->
		 match idx with
		   | Ifirst | Imiddle ->
		       fprintf fmt "@ %s : %a ;" (self#field f) self#pp_tau t
		   | Isingle | Ilast ->
		       fprintf fmt "@ %s : %a" (self#field f) self#pp_tau t
	      ) fts ;
	    fprintf fmt "@]@ }@].@\n" ;
	  end
      | Tsum cases ->
	  begin
	    fprintf fmt "@[<hv 0>Inductive %s" (self#datatype adt) ;
	    self#pp_declare_poly fmt n ;
	    fprintf fmt " : Set :=" ;
	    let result = Data(adt,Kind.type_params n) in
	    List.iter
	      (fun (c,ts) ->
		 fprintf fmt "@ | @[<hov 2>%s : " (self#link_name Cterm c) ;
		 List.iter (fun t -> fprintf fmt "@ %a ->" self#pp_tau t) ts ;
		 fprintf fmt "@ %a.@]" self#pp_tau result ;
	      ) cases ;
	    fprintf fmt ".@]@\n" ;
	  end

    method declare_signature fmt f ts t =
      begin
	let cmode = ctau t in
	fprintf fmt "@[<hov 4>Parameter %s :" (self#link_name cmode f) ;
	List.iter (fun t -> fprintf fmt "@ %a ->" self#pp_tau t) ts ;
	fprintf fmt "@ %a.@]@\n" self#pp_tau t ;
      end

    method declare_definition fmt f xs t e =
      self#global 
	begin fun () ->
	  fprintf fmt "@[<hov 4>Definition %s" (self#link_name (ctau t) f) ;
	  List.iter
	    (fun x -> 
	       self#bind x ;
	       let t = T.tau_of_var x in
	       fprintf fmt "@ (%a : %a)" self#pp_var x self#pp_tau t
	    ) xs ;
	  fprintf fmt "@ : %a :=@ " self#pp_tau t ; 
	  fprintf fmt "@[<hov 2>%a@]@].@\n" (self#pp_expr t) e ;
	end

    method declare_axiom fmt lemma xs (_:trigger list list) p =
      self#global
	begin fun () ->
	  fprintf fmt "@[<hov 2>Hypothesis %s: %a@].@\n"
	    lemma self#pp_prop (T.e_forall xs p)
	end

  end

end
