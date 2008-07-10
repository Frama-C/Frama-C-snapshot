(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: output.ml,v 1.33 2008/07/09 10:32:00 marche Exp $ i*)

open Lexing
open Format
open Pp

type constant =
  | Prim_void
  | Prim_int of string
  | Prim_real of string
  | Prim_bool of bool
(*
  | Prim_string of string
*)

let fprintf_constant form e =
  match e with
    | Prim_void -> fprintf form "void"
    | Prim_int(n) -> fprintf form "(%s)" n
    | Prim_real(f) -> fprintf form "%s" f
    | Prim_bool(b) -> fprintf form "%b" b
(*
    | Prim_string s -> fprintf form "\"%s\"" s
*)

type term = 
  | LConst of constant
  | LApp of string * term list
  | LVar of string
  | LVarAtLabel of string * string     (*r x@L *)
  | Tnamed of string * term
  | TIf of term * term * term

let rec iter_term f t =
  match t with
  | LConst(c) -> ()
  | LApp(id,l) -> f id; List.iter (iter_term f) l
  | LVar(id) -> f id
  | LVarAtLabel(id,l) -> f id
  | Tnamed(_,t) -> iter_term f t
  | TIf(t1,t2,t3) -> 
      iter_term f t1; iter_term f t2; iter_term f t3


let rec fprintf_term form t =
  match t with
  | LConst(c) -> fprintf_constant form c
  | LApp("eq_pointer",[t1;t2]) ->
      fprintf form "@[(%a=%a)@]" 
	fprintf_term t1
	fprintf_term t2
  | LApp("ne_pointer",[t1;t2]) ->
      fprintf form "@[(%a<>%a)@]" 
	fprintf_term t1
	fprintf_term t2
  | LApp(id,t::tl) ->
      fprintf form "@[%s(%a" id fprintf_term t;
      List.iter (fun t -> fprintf form ",@ %a" fprintf_term t) tl;
      fprintf form ")@]"
  | LApp(id,[])
  | LVar(id) -> fprintf form "%s" id
  | LVarAtLabel(id,l) -> fprintf form "%s@@%s" id l
  | Tnamed(lab,t) -> fprintf form "(%s : %a)" lab fprintf_term t
  | TIf(t1,t2,t3) -> 
      fprintf form "@[<hv 1>(if %a@ then %a@ else %a)@]" 
	fprintf_term t1 fprintf_term t2 fprintf_term t3

type logic_type = 
    { logic_type_name : string;
      logic_type_args : logic_type list;
    }
(*r int, float, int list, ... *)

let rec iter_logic_type f t =
  f t.logic_type_name;
  List.iter (iter_logic_type f) t.logic_type_args
  
type assertion = 
  | LTrue | LFalse
  | LAnd of assertion * assertion
  | LOr of assertion * assertion
  | LIff of assertion * assertion
  | LNot of assertion
  | LImpl of assertion * assertion
  | LIf of term * assertion * assertion
  | LLet of string * term * assertion
      (*r warning: only for Coq assertions *)
  | LForall of string * logic_type * assertion
      (*r forall x:t.a *)
  | LExists of string * logic_type * assertion
      (*r exists x:t.a *)
  | LPred of string * term list
  | LNamed of string * assertion
;;

let rec unname a =
  match a with
    | LNamed(_,a) -> unname a
    | _ -> a

let make_or a1 a2 =
  match (unname a1,unname a2) with
    | (LTrue,_) -> LTrue
    | (_,LTrue) -> LTrue
    | (LFalse,_) -> a2
    | (_,LFalse) -> a1
    | (_,_) -> LOr(a1,a2)

let make_and a1 a2 =
  match (unname a1,unname a2) with
    | (LTrue, _) -> a2
    | (_,LTrue) -> a1
    | (LFalse,_) -> LFalse
    | (_,LFalse) -> LFalse
    | (_,_) -> LAnd(a1,a2)

let rec make_and_list l =
  match l with
    | [] -> LTrue
    | f::r -> make_and f (make_and_list r)

let rec make_or_list l =
  match l with
    | [] -> LFalse
    | f::r -> make_or f (make_or_list r)

let make_impl a1 a2 =
  match (unname a1,unname a2) with
    | (LTrue,_) -> a2
    | (_,LTrue) -> LTrue
    | (LFalse,_) -> LTrue
    | (_,LFalse) -> LNot(a1)
    | (_,_) -> LImpl(a1,a2)

let make_equiv a1 a2 =
  match (unname a1,unname a2) with
    | (LTrue,_) -> a2
    | (_,LTrue) -> a1
    | (_,_) -> LIff(a1,a2)

let rec iter_assertion f a =
  match a with
  | LTrue -> ()
  | LFalse -> () 
  | LAnd(a1,a2) -> iter_assertion f a1; iter_assertion f a2 
  | LOr(a1,a2) -> iter_assertion f a1; iter_assertion f a2 
  | LIff(a1,a2) -> iter_assertion f a1; iter_assertion f a2 
  | LNot(a1) -> iter_assertion f a1
  | LImpl(a1,a2) -> iter_assertion f a1; iter_assertion f a2 
  | LIf(t,a1,a2) -> 
      iter_term f t; iter_assertion f a1; iter_assertion f a2 
  | LLet(id,t,a) -> iter_term f t; iter_assertion f a
  | LForall(id,t,a) -> iter_logic_type f t; iter_assertion f a
  | LExists(id,t,a) -> iter_logic_type f t; iter_assertion f a
  | LPred(id,l) -> f id; List.iter (iter_term f) l
  | LNamed (_, a) -> iter_assertion f a
;;

let rec fprintf_logic_type form t =
  match t.logic_type_args with
    | [] -> fprintf form "%s" t.logic_type_name
    | [x] ->
	fprintf form "%a %s" fprintf_logic_type x t.logic_type_name
    | l ->
	fprintf form "(%a) %s" 
	  (print_list comma fprintf_logic_type) l
	  t.logic_type_name

let rec fprintf_assertion form a =
  match a with
  | LTrue -> fprintf form "true"
  | LFalse -> fprintf form "false"
  | LAnd(a1,a2) -> 
      fprintf form "@[(%a@ and %a)@]" 
	fprintf_assertion a1 
	fprintf_assertion a2
  | LOr(a1,a2) -> 
      fprintf form "@[(%a@ or %a)@]" 
	fprintf_assertion a1 
	fprintf_assertion a2
  | LIff(a1,a2) -> 
      fprintf form "@[(%a@ <-> %a)@]" 
	fprintf_assertion a1 
	fprintf_assertion a2
  | LNot(a1) -> 
      fprintf form "@[(not %a)@]" 
	fprintf_assertion a1
  | LImpl(a1,a2) -> 
      fprintf form "@[<hv 1>(%a ->@ %a)@]" 
	fprintf_assertion a1 fprintf_assertion a2
  | LIf(t,a1,a2) -> 
      fprintf form "@[<hv 1>(if %a@ then %a@ else %a)@]" 
	fprintf_term t fprintf_assertion a1 fprintf_assertion a2
  | LLet(id,t,a) -> 
      fprintf form "@[<hv 1>(let @[<hv 1>%s =@ %a in@]@ %a)@]" id
	fprintf_term t fprintf_assertion a
  | LForall(id,t,a) -> 
      fprintf form "@[<hv 1>(forall %s:%a.@ %a)@]" 
	id fprintf_logic_type t fprintf_assertion a
  | LExists(id,t,a) -> 
      fprintf form "@[<hv 1>(exists %s:%a.@ %a)@]" 
	id fprintf_logic_type t fprintf_assertion a
  | LPred("eq",[t1;t2]) ->
      fprintf form "@[(%a = %a)@]" 
	fprintf_term t1
	fprintf_term t2
  | LPred("neq",[t1;t2]) ->
      fprintf form "@[(%a <> %a)@]" 
	fprintf_term t1
	fprintf_term t2
  | LPred(id,t::tl) ->
      fprintf form "@[%s(%a" id fprintf_term t;
      List.iter (fun t -> fprintf form ",@ %a" fprintf_term t) tl;
      fprintf form ")@]"
  | LPred (id, []) -> assert false
  | LNamed (n, a) ->
      fprintf form "@[(%s:@ %a)@]" n fprintf_assertion a
;;

(*s types *)


type why_type = 
  | Prod_type of string * why_type * why_type      (*r (x:t1)->t2 *)
  | Base_type of logic_type
  | Ref_type of why_type
  | Annot_type of 
      assertion * why_type * 
      string list * string list * assertion * ((string * assertion) list)
	(*r { P } t reads r writes w { Q | E => R } *)
;;

let base_type s = Base_type { logic_type_name = s ; logic_type_args = [] }
let int_type = base_type "int"
let bool_type = base_type "bool"
let unit_type = base_type "unit"

let option_iter f x =
  match x with
    | None -> ()
    | Some y -> f y
;;


let rec iter_why_type f t =
  match t with
    | Prod_type(_,t1,t2) ->
	iter_why_type f t1; iter_why_type f t2
    | Base_type b -> iter_logic_type f b
    | Ref_type(t) -> iter_why_type f t 
    | Annot_type (pre,t,reads,writes,post,signals) ->
	iter_assertion f pre;
	iter_why_type f t;
	List.iter f reads;
	List.iter f writes;
	iter_assertion f post;
	List.iter (fun (_,a) -> iter_assertion f a) signals
;;


let rec fprint_comma_string_list form l =
  match l with
    | [] -> ()
    | x::l -> 
	fprintf form ",%s" x;
	fprint_comma_string_list form l
;;

let rec fprintf_type anon form t = 
  match t with
    | Prod_type(id,t1,t2) ->
	if id="" or anon then
	  fprintf form "@[<hv 1>%a ->@ %a@]" 
	    (fprintf_type anon) t1 (fprintf_type anon) t2
	else
	  fprintf form "@[<hv 1>%s:%a ->@ %a@]" id
	    (fprintf_type anon) t1 (fprintf_type anon) t2
    | Base_type t  -> 
	fprintf_logic_type form t
    | Ref_type(t) -> 
	fprintf form "%a ref" (fprintf_type anon) t
    | Annot_type(p,t,reads,writes,q,signals) ->
	begin
	  fprintf form "@[@[<hv 2>{ "; 
	  if p <> LTrue 
	  then fprintf_assertion form p;
	  fprintf form "}@]@ %a@ " (fprintf_type anon) t;
	  begin
	    match List.sort compare reads with
	      | [] -> ()
	      | r::l -> 
		  fprintf form "reads %s%a@ " r fprint_comma_string_list l
	  end;
	  begin
	    match List.sort compare writes with
	      | [] -> ()
	      | r::l -> 
		  fprintf form "writes %s%a@ " r fprint_comma_string_list l
	  end;
	  begin
	    match signals with
	      | [] -> 
		  fprintf form "@[<hv 2>{ %a }@]@]" fprintf_assertion q
	      | l ->
		  fprintf form 
		    "raises%a@ @[<hv 2>{ %a@ | %a }@]@]" 
		    (print_list comma (fun fmt (e,r) -> fprintf fmt " %s" e))
		    l
		    fprintf_assertion q
		    (print_list alt (fun fmt (e,r) -> 
				       fprintf fmt "@[<hv 2>%s =>@ %a@]" e
					 fprintf_assertion r))
		    l
	  end		    
	  
	end
;;


(*s expressions *)

type variant = term * string option

type opaque = bool

type expr =
  | Cte of constant
  | Var of string
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Void
  | Deref of string
  | If of expr * expr * expr
  | While of 
      expr (* loop condition *)
      * assertion (* invariant *) 
      * variant option (* variant *) 
      * expr list (* loop body *)
  | Block of expr list
  | Assign of string * expr
  | Let of string * expr * expr
  | Let_ref of string * expr * expr
  | App of expr * expr
  | Raise of string * expr option
  | Try of expr * string * string option * expr
  | Fun of (string * why_type) list * 
      assertion * expr * assertion * ((string * assertion) list)
  | Triple of opaque * 
      assertion * expr * assertion * ((string * assertion) list)
  | Assert of assertion * expr
  | Label of string * expr
  | BlackBox of why_type
  | Absurd
  | Loc of Lexing.position * expr
;;

let make_or_expr a1 a2 =
  match (a1,a2) with
    | (Cte (Prim_bool true),_) -> Cte (Prim_bool true)
    | (_,Cte (Prim_bool true)) -> Cte (Prim_bool true)
    | (Cte (Prim_bool false),_) -> a2
    | (_,Cte (Prim_bool false)) -> a1
    | (_,_) -> Or(a1,a2)

let make_and_expr a1 a2 =
  match (a1,a2) with
    | (Cte (Prim_bool true),_) -> a2
    | (_,Cte (Prim_bool true)) -> a1
    | (Cte (Prim_bool false),_) -> Cte (Prim_bool false)
    | (_,Cte (Prim_bool false)) ->Cte (Prim_bool false)
    | (_,_) -> And(a1,a2)


let make_app_rec f l = 
  let rec make_rec accu = function
    | [] -> accu
    | e::r -> make_rec (App(accu,e)) r
  in
  match l with
    | [] -> make_rec f [Void]
    | l -> make_rec f l
;;

let make_app id l = make_app_rec (Var id) l

let make_app_e = make_app_rec

let make_while cond inv var e =
  let body = 
    match e with
      | Block(l) -> l
      | _ -> [e]
  in While(cond,inv,var,body)

let make_label label e = Label (label, e)

let make_pre pre e =  Triple(false,pre,e,LTrue,[])

let append e1 e2 =
  match e1,e2 with
    | Void,_ -> e2
    | _,Void -> e1
    | Block(l1),Block(l2) -> Block(l1@l2)
    | Block(l1),_ -> Block(l1@[e2])
    | _,Block(l2) -> Block(e1::l2)
    | _ -> Block [e1;e2]
;;

let rec iter_expr f e =
  match e with
    | Cte(c) -> ()
    | Var(id) -> f id
    | And(e1,e2) -> iter_expr f e1; iter_expr f e2
    | Or(e1,e2) -> iter_expr f e1; iter_expr f e2
    | Not(e1) -> iter_expr f e1
    | Void -> ()
    | Deref(id) -> f id
    | If(e1,e2,e3) ->
	iter_expr f e1; iter_expr f e2; iter_expr f e3
    | While(e1,inv,var,e2) ->
	iter_expr f e1; 
	iter_assertion f inv; 
	option_iter (fun (var,_) -> iter_term f var) var; 
	List.iter (iter_expr f) e2
    | Block(el) -> List.iter (iter_expr f) el
    | Assign(id,e) -> f id; iter_expr f e
    | Let(id,e1,e2) -> iter_expr f e1; iter_expr f e2
    | Let_ref(id,e1,e2) -> iter_expr f e1; iter_expr f e2
    | App(e1,e2) -> iter_expr f e1; iter_expr f e2
    | Raise (_, None) -> ()
    | Raise(id,Some e) -> iter_expr f e
    | Try(e1,exc,id,e2) -> iter_expr f e1; iter_expr f e2
    | Fun(params,pre,body,post,signals) ->
	iter_assertion f pre;
	iter_expr f body;
	iter_assertion f post;
	List.iter (fun (_,a) -> iter_assertion f a) signals
    | Triple(_,pre,e,post,exceps) ->
	iter_assertion f pre;
	iter_expr f e;
	iter_assertion f post;
	List.iter (fun (_,a) -> iter_assertion f a) exceps
    | Assert(p, e) -> iter_assertion f p; iter_expr f e
    | Label (_,e) | Loc (_,e) -> iter_expr f e
    | BlackBox(ty) -> iter_why_type f ty
    | Absurd -> ()


let fprintf_variant form = function
  | None -> ()
  | Some (t, None) -> fprintf form "variant %a" fprintf_term t
  | Some (t, Some r) -> fprintf form "variant %a for %s" fprintf_term t r
	  
let rec fprintf_expr form e =
  match e with
    | Cte(c) -> fprintf_constant form c
    | Var(id) -> fprintf form "%s" id
    | And(e1,e2) ->
	fprintf form "@[(%a && %a)@]" 
	  fprintf_expr e1 fprintf_expr e2
    | Or(e1,e2) ->
	fprintf form "@[(%a || %a)@]" 
	  fprintf_expr e1 fprintf_expr e2
    | Not(e1) ->
	fprintf form "@[(not %a)@]" 
	  fprintf_expr e1 
    | Void -> fprintf form "void"
    | Deref(id) -> fprintf form "!%s" id
    | If(e1,e2,e3) ->
	fprintf form 
	  "@[<hv 0>(if %a@ @[<hv 1>then@ %a@]@ @[<hv 1>else@ %a@])@]" 
	  fprintf_expr e1 fprintf_expr e2 fprintf_expr e3
    | While(e1,inv,var,e2) ->
	fprintf form 
	  "@[<hv 0>while %a do@ @[<hv 1>@[<hv 2>{ @[<hv 2>invariant@ %a@]@ @[<hv 2>%a@] }@]@ %a@]@ done@]" 
	  fprintf_expr e1 
	  fprintf_assertion inv
	  fprintf_variant var
	  fprintf_expr_list e2
    | Block([]) ->
	fprintf form "void"
    | Block(el) ->
	fprintf form "@[<hv 0>begin@ @[<hv 1>  %a@]@ end@]" fprintf_expr_list el
    | Assign(id,e) ->
	fprintf form "@[<hv 1>(%s := %a)@]" 
	  id fprintf_expr e
    | Let(id,e1,e2) ->
	fprintf form "@[<hv 0>(let %s = %a in@ %a)@]" id
	  fprintf_expr e1 fprintf_expr e2
    | Let_ref(id,e1,e2) ->
	fprintf form "@[<hv 0>(let %s = ref %a in@ %a)@]" id
	  fprintf_expr e1 fprintf_expr e2
    | App(e1,e2) ->
	fprintf form "@[<hv 1>(%a %a)@]" fprintf_expr e1 fprintf_expr e2
    | Raise(id,None) ->
	fprintf form "@[<hv 1>(raise@ %s)@]" id
    | Raise(id,Some e) ->
	fprintf form "@[<hv 1>(raise@ (%s@ %a))@]" id fprintf_expr e
    | Try(e1,exc,None,e2) ->
	fprintf form "@[<hv 1>try@ %a@ with@ %s ->@ %a end@]" 
	  fprintf_expr e1 exc fprintf_expr e2
    | Try(e1,exc,Some id,e2) ->
	fprintf form "@[<hv 1>try@ %a@ with@ %s %s ->@ %a end@]" 
	  fprintf_expr e1 exc id fprintf_expr e2
    | Fun(params,pre,body,post,signals) ->
	fprintf form "@[<hv 1>fun @[";
	List.iter 
	  (fun (x,t) -> fprintf form "(%s : %a) " x (fprintf_type false) t) 
	  params;
	fprintf form "@]->@ @[<hv 0>{ "; 
	if pre <> LTrue 
	then fprintf_assertion form pre;
	fprintf form " }@ %a@]@ " fprintf_expr body;
	begin
	  match signals with
	    | [] -> 
		fprintf form "@[<hv 2>{ %a }@]@]" fprintf_assertion post
	     | l ->
		 fprintf form "@[<hv 2>{ %a@ | %a }@]"
		   fprintf_assertion post
		   (print_list alt
		      (fun fmt (e,r) -> 
			 fprintf fmt "@[<hv 2>%s =>@ %a@]" e
			   fprintf_assertion r))
		  l
	end		    

    | Triple(_,pre,e,LTrue,[]) ->
	fprintf form "@[<hv 0>(assert { %a };@ (%a))@]" 
	  fprintf_assertion pre
	  fprintf_expr e
    | Triple(o,pre,e,post,exceps) ->
	fprintf form "@[<hv 0>(assert { %a };@ (%a)@ " 
	  fprintf_assertion pre
	  fprintf_expr e;
	begin
	  match exceps with
	    | [] -> 
		(if o then fprintf form "{{ %a }}" else fprintf form "{ %a }")
		  fprintf_assertion post
	    | l ->
		(if o then 
		   fprintf form "@[<hv 2>{{ %a@ | %a }}@]" 
		 else
		   fprintf form "@[<hv 2>{ %a@ | %a }@]")
		  fprintf_assertion post
		  (print_list alt
		  (fun fmt (e,r) -> 
		     fprintf fmt "@[<hv 2>%s =>@ %a@]" e
		       fprintf_assertion r))
		  l
	end;
	fprintf form ")@]"
    | Assert(p, e) ->
	fprintf form "@[<hv 0>(assert@ { %a };@ %a)@]" 
	  fprintf_assertion p fprintf_expr e
    | Label (s, e) ->
	fprintf form "@[<hv 0>(%s:@ %a)@]" s fprintf_expr e
    | BlackBox(t) ->
	fprintf form "@[<hv 0>[ %a ]@]" 
	  (fprintf_type false) t
    | Absurd ->
	fprintf form "@[<hv 0>absurd@ @]" 
    | Loc (l, e) ->
	fprintf_expr form e
	(*
	fprintf form "@[#%S %d %d#%a@]" l.pos_fname l.pos_lnum 
	  (l.pos_cnum - l.pos_bol) fprintf_expr e
	*)

and fprintf_expr_list form l =
  match l with
    | [] -> ()
    | e::l ->
	fprintf form "%a" fprintf_expr e;
	fprintf_expr_end_list form l

and fprintf_expr_end_list form l =
  match l with
    | [] -> ()
    | e::l ->
	fprintf form ";@ %a" fprintf_expr e;
	fprintf_expr_end_list form l
;;

type why_decl =
  | Param of bool * string * why_type         (*r parameter in why *)
  | Def of string * expr               (*r global let in why *)
  | Logic of bool * string * (string * logic_type) list * logic_type    (*r logic decl in why *)
  | Axiom of string * assertion         (*r Axiom *)
  | Goal of string * assertion         (*r Goal *)
  | Predicate of bool * string * (string * logic_type) list * assertion  
  | Function of bool * string * (string * logic_type) list * logic_type * term
  | Type of string * string list
  | Exception of string * logic_type option



let get_why_id d =
  match d with
    | Param(_,id,_) 
    | Logic(_,id,_,_)
    | Def(id,_) 
    | Axiom(id,_) 
    | Goal(id,_) 
    | Predicate(_,id,_,_) 
    | Function(_,id,_,_,_) 
    | Type (id,_) 
    | Exception(id,_) -> id

let iter_why_decl f d =
  match d with
    | Param(_,_,t) -> iter_why_type f t
    | Def(id,t) -> iter_expr f t
    | Logic(_,id,args,t) -> 
	List.iter (fun (_,t) -> iter_logic_type f t) args;
	iter_logic_type f t
    | Goal(id,t) | Axiom(id,t) -> iter_assertion f t
    | Predicate(_,id,args,p) -> 
	List.iter (fun (_,t) -> iter_logic_type f t) args;
	iter_assertion f p
    | Function(_,id,args,t,p) -> 
	List.iter (fun (_,t) -> iter_logic_type f t) args;
	iter_logic_type f t;
	iter_term f p
    | Type(t,args) -> List.iter f args
    | Exception(_,t) -> Option_misc.iter (iter_logic_type f) t



type state = [`TODO | `RUNNING | `DONE ];;

type 'a decl = { mutable state : state; decl : 'a };;

module StringMap = Map.Make(String);;

(*
exception Recursion;;
*)

let rec do_topo decl_map iter_fun output_fun id d =
  match d.state with
    | `DONE -> ()
    | `RUNNING -> 
	eprintf "Warning: recursive definition of %s in generated file@." id
    | `TODO ->
	d.state <- `RUNNING;
	iter_fun
	  (fun id ->
	     try 
	       let s = StringMap.find id decl_map in
	       do_topo decl_map iter_fun output_fun id s
	     with
		 Not_found -> ())
	  d.decl;	
	output_fun d.decl;
	d.state <- `DONE
;;


let build_map get_id decl_list =
  List.fold_left
    (fun acc decl ->
       let id = get_id decl in
       StringMap.add id { state = `TODO ; decl = decl } acc)
    StringMap.empty
    decl_list
;;

let fprint_logic_arg form (id,t) =
  fprintf form "%s:%a" id fprintf_logic_type t

let fprintf_why_decl form d =
  match d with
    | Param(b,id,t) ->
	fprintf form "@[<hv 1>%sparameter %s :@ %a@]@.@." 
	(if b then "external " else "") id 
	  (fprintf_type false) t
    | Logic(b,id,args,t) ->
	fprintf form "@[<hv 1>%slogic %s: %a -> %a@.@."
	  (if b then "external " else "") id 
	  (print_list comma (fun fmt (id,t) -> fprintf_logic_type fmt t)) args
	  fprintf_logic_type t 
    | Axiom(id,p) ->
	fprintf form "@[<hv 1>axiom %s :@ %a@]@.@." id 
	  fprintf_assertion p
    | Goal(id,p) ->
	fprintf form "@[<hv 1>goal %s :@ %a@]@.@." id 
	  fprintf_assertion p
    | Def(id,e) ->
	fprintf form "@[<hv 1>let %s =@ %a@]@.@." id fprintf_expr e
    | Predicate (b, id, args, p) ->
	fprintf form "@[<hv 1>%spredicate %s(%a) =@ %a@]@.@."
	  (if b then "external " else "") id 
	  (print_list comma fprint_logic_arg) args
	  fprintf_assertion p
    | Function(b,id,args,t,e) ->
	fprintf form "@[<hv 1>%sfunction %s(%a) : %a =@ %a@]@.@."
	  (if b then "external " else "") id 
	  (print_list comma fprint_logic_arg) args
	  fprintf_logic_type t 
	  fprintf_term e
    | Type (id, []) ->
	fprintf form "@[type %s@]@.@." id
    | Type (id, [t]) ->
	fprintf form "@[type '%s %s@]@.@." t id
    | Type (id, t::l) ->
	fprintf form "@[type ('%s" t;
	List.iter (fun t -> fprintf form ", '%s" t) l;
	fprintf form ") %s@]@.@." id
    | Exception(id, None) ->
	fprintf form "@[exception %s@]@.@." id
    | Exception(id, Some t) ->
	fprintf form "@[exception %s of %a@]@.@." id fprintf_logic_type t 
	

let output_decls get_id iter_decl output_decl decls =
  let map = build_map get_id decls in
  StringMap.iter
    (fun id decl ->
       do_topo map iter_decl output_decl id decl)
    map
;;

let fprintf_why_decls form decls =
  (* Why do we need a partition ?
     because one may have a type and a logic/parameter with the same name, 
     and the computation of dependencies is confused in that case
     
     Type may depend on nothing 
     Logic may depend on Type, Logic and Predicate
     Predicate may depend on Type, Predicate and Logic
     Axiom may depend on Type, Predicate and Logic
     Parameter may depend on Type, Predicate and Logic
     Def may depend on Type, Parameter, Predicate, Logic, and Def
     
     - Claude, 16 nov 2006

  *)

(*
  let (types, defs, others) =
    List.fold_left
      (fun (t,d,o) decl ->
	 match decl with
	   | Type _ -> (decl::t,d,o)
	   | Def _ -> (t,decl::d,o)
	   | _ -> (t,d,decl::o))
      ([],[],[]) decls
  in
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) types;
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) others;
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) defs
*)

  (*
    Additional rules :
    
    Exception may depend on Type
    Parameter may depend on Exception
    
    - Nicolas R., 8 nov 2007
    
  *)

  let (types, params, defs, others) =
    List.fold_left
      (fun (t, p, d, o) decl ->
	 match decl with
	   | Type _ -> (decl::t, p, d, o)
	   | Param _ -> (t, decl::p, d, o)
	   | Def _ -> (t, p, decl::d, o)
	   | _ -> (t, p, d, decl::o))
      ([], [], [], []) decls
  in
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) types;
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) others;
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) params;
    output_decls get_why_id iter_why_decl (fprintf_why_decl form) defs


(*s locs table *)

type kind =
  | ArithOverflow
  | DownCast
  | IndexBounds
  | PointerDeref
  | UserCall
  | DivByZero
  | AllocSize
  | Pack
  | Unpack


let locs_table : 
    (string, (kind option * string option * string option * Loc.position)) 
    Hashtbl.t 
    = Hashtbl.create 97
let name_counter = ref 0
let reg_loc prefix ?id ?kind ?name ?formula loc =
  let id = match id with
    | None ->  
	incr name_counter;
	prefix ^ "_" ^ string_of_int !name_counter
    | Some n -> n
  in
  Hashtbl.add locs_table id (kind,name,formula,loc);
  id

let print_kind fmt k =
  fprintf fmt "%s"
    (match k with
       | Pack -> "Pack"
       | Unpack -> "Unpack"
       | DivByZero -> "DivByZero"
       | AllocSize -> "AllocSize"
       | UserCall -> "UserCall"
       | PointerDeref -> "PointerDeref"
       | IndexBounds -> "IndexBounds"
       | DownCast -> "DownCast"
       | ArithOverflow -> "ArithOverflow"
    )

let abs_fname f =
  if Filename.is_relative f then
    Filename.concat (Unix.getcwd ()) f 
  else f

let print_locs fmt =
  Hashtbl.iter 
    (fun id (kind,name,formula,(b,e)) ->
       fprintf fmt "[%s]@\n" id;
       Option_misc.iter
	 (fun k -> fprintf fmt "kind = %a@\n" print_kind k) kind;
       Option_misc.iter
	 (fun n -> fprintf fmt "name = \"%s\"@\n" n) name;
       Option_misc.iter
	 (fun n -> fprintf fmt "formula = \"%s\"@\n" n) formula;
       fprintf fmt "file = \"%s\"@\n" (String.escaped (abs_fname b.Lexing.pos_fname));
       let l = b.Lexing.pos_lnum in
       let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol in
       let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol in
       fprintf fmt "line = %d@\n" l;
       fprintf fmt "begin = %d@\n" fc;
       fprintf fmt "end = %d@\n@\n" lc)
    locs_table

(*
  Local Variables: 
  compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
  End: 
*)
