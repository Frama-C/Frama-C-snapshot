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
(* --- Exportation to Foreign Languages                                   --- *)
(* -------------------------------------------------------------------------- *)

open Format
open Logic
open Plib
open Linker
open Engine

let cmode = function 
  | Mpositive | Mnegative -> Cprop 
  | Mterm | Mterm_int | Mterm_real | Mint | Mreal -> Cterm

let pmode = function 
  | Mpositive -> Positive 
  | Mnegative -> Negative 
  | Mterm | Mterm_int | Mterm_real | Mint | Mreal -> Boolean

let amode = function 
  | Mpositive | Mnegative | Mterm | Mterm_int | Mint -> Aint 
  | Mterm_real | Mreal -> Areal

let tmode = function
  | Prop -> Mpositive
  | Bool -> Mterm
  | Int -> Mterm_int
  | Real -> Mterm_real
  | Tvar _ | Array _ | Record _ | Data _ -> Mterm

let ctau = function
  | Prop -> Cprop
  | _ -> Cterm

let link_compare a b =
  if a==b then 0 else
    match a , b with
      | F_call f , F_call g 
      | F_call2 f , F_call2 g
      | F_assoc f , F_assoc g
	  -> String.compare f g
      | F_call _ , _ -> (-1)
      | _ , F_call _ -> 1
      | F_call2 _ , _ -> (-1)
      | _ , F_call2 _ -> 1

let link_name = function F_call f | F_call2 f | F_assoc f -> f

module Make(T : Term) =
struct
  open T

  (* -------------------------------------------------------------------------- *)
  (* --- Linkers                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  module ADT = T.ADT
  module Field = T.Field
  module Fun = T.Fun
    
  type tau = (Field.t,ADT.t) datatype
  type var = Var.t
  type term = T.term
  type record = (Field.t * term) list
  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  module Mvar   = Map.Make(Var)
  module Ladt   = Link(ADT)
  module Lfield = Link(Field)
  module Lfun   = Link(Fun)
  module Lvar   = Link(Var)
  module STerm  = Link
    (struct
       type t = term
       let hash = T.hash
       let equal = T.equal
       let compare = T.compare
       let pretty = T.pretty
       let id t = Printf.sprintf "E%03d" (T.id t)
     end)

  (* -------------------------------------------------------------------------- *)
  (* --- Pretty Printing Engine                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module TauMap = Map.Make
    (struct
       type t = T.tau
       let compare = Kind.compare_tau Field.compare ADT.compare
     end)

  let add_var x vars =
    let tx = T.tau_of_var x in
    let xs = try TauMap.find tx vars with Not_found -> [] in
    TauMap.add tx (x::xs) vars

  let rec binders q xs p =
    match T.repr p with
      | Bind(q',y,p') when q'=q -> binders q (add_var y xs) p'
      | _ -> xs,p

  let rec lambda xs p =
    match T.repr p with
      | Bind(Lambda,y,p') -> lambda (y::xs) p'
      | _ -> List.rev xs , p

  class virtual engine =
  object(self)

    method virtual datatype : ADT.t -> string
    method virtual field : Field.t -> string

    val mutable global = allocator () 
    val mutable vars = Vars.empty

    method declare = Linker.declare global
    method declare_all = List.iter (Linker.declare global)

    val linker_variable  = Lvar.linker ()
    val linker_shared    = STerm.linker ()

    method private push =
      let gstack = global in
      begin
	global <- copy global ;
	linker_variable#alloc_with global ;
	linker_shared#alloc_with global ;
	gstack
      end

    method private pop gstack =
      begin
	global <- gstack ;
	linker_variable#alloc_with gstack ;
	linker_shared#alloc_with gstack ;
      end

    method local (job : unit -> unit) =
      let gstack = self#push in
      try job () ; self#pop gstack
      with err -> self#pop gstack ; raise err

    method global (job : unit -> unit) =
      let gstack = self#push in
      try
	linker_variable#clear ;
	linker_shared#clear ;
	vars <- Vars.empty ;
	job () ; self#pop gstack
      with err -> self#pop gstack ; raise err
	
    (* -------------------------------------------------------------------------- *)
    (* --- Types                                                              --- *)
    (* -------------------------------------------------------------------------- *)
	    
    method virtual t_int  : string
    method virtual t_real : string
    method virtual t_bool : string
    method virtual t_prop : string
    method virtual t_atomic : tau -> bool
    method virtual pp_tvar : int printer
    method virtual pp_array : tau printer
    method virtual pp_farray : tau printer2
    method virtual pp_datatype : ADT.t -> tau list printer

    method pp_subtau fmt t = 
      if self#t_atomic t
      then self#pp_tau fmt t
      else fprintf fmt "@[<hov 1>(%a)@]" self#pp_tau t

    method pp_tau fmt = function
      | Int  -> pp_print_string fmt self#t_int
      | Real -> pp_print_string fmt self#t_real
      | Bool -> pp_print_string fmt self#t_bool
      | Prop -> pp_print_string fmt self#t_prop
      | Array(Int,d) -> self#pp_array fmt d
      | Array(k,d) -> self#pp_farray fmt k d
      | Record _fts -> failwith "Qed.Export.record"
      | Tvar x -> self#pp_tvar fmt x
      | Data(adt,ts) -> self#pp_datatype adt fmt ts

    (* -------------------------------------------------------------------------- *)
    (* --- Mode                                                               --- *)
    (* -------------------------------------------------------------------------- *)

    val mutable mode = Mpositive
    method mode = mode
    method with_mode m f = 
      let m0 = mode in 
      if m = m0 then f m 
      else
	try mode <- m ; f m0 ; mode <- m0
	with err -> mode <- m0 ; raise err

    (* -------------------------------------------------------------------------- *)
    (* --- Variables                                                          --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_var = linker_variable#print

    (* -------------------------------------------------------------------------- *)
    (* --- Atoms                                                              --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual e_true : cmode -> string
    method virtual e_false : cmode -> string
    method virtual pp_int : Z.t printer
    method virtual pp_real : R.t printer
    method virtual is_atomic : term -> bool

    (* -------------------------------------------------------------------------- *)
    (* --- Calls                                                              --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual callstyle : callstyle
    method virtual link : cmode -> Fun.t -> link
    method link_name m f = match self#link m f with
      | F_call f | F_call2 f -> f
      | F_assoc _ -> assert false

    method private pp_call ~f fmt xs =
      match self#callstyle with
	| CallVar -> Plib.pp_call_var ~f self#pp_flow fmt xs
	| CallVoid -> Plib.pp_call_void ~f self#pp_flow fmt xs
	| CallApply -> Plib.pp_call_apply ~f self#pp_atom fmt xs

    method private pp_unop ~op fmt x =
      match op with
	| Assoc op | Op op ->
	    if is_identop op && self#is_atomic x then
	      fprintf fmt "%s %a" op self#pp_flow x
	    else
	      fprintf fmt "%s%a" op self#pp_atom x
	| Call f -> self#pp_call f fmt [x]
	    
    method private pp_binop ~op fmt x y =
      match op with
	| Assoc op | Op op -> 
	    fprintf fmt "%a %s@ %a" self#pp_atom x op self#pp_atom y
	| Call f -> self#pp_call f fmt [x;y]

    method private pp_binop_term ~op fmt x y =
      self#with_mode Mterm (fun _old -> self#pp_binop ~op fmt x y)

    method private pp_nary ~op fmt xs =
      match op with
	| Assoc op -> Plib.pp_assoc ~e:"?" ~op self#pp_atom fmt xs
	| Op op -> Plib.pp_fold_binop ~e:"?" ~op self#pp_atom fmt xs
	| Call f -> 
	    match self#callstyle with
	      | CallVar | CallVoid -> 
		  Plib.pp_fold_call ~e:"?" ~f self#pp_flow fmt xs
	      | CallApply -> 
		  Plib.pp_fold_apply ~e:"?" ~f self#pp_atom fmt xs

    method pp_fun cmode f fmt xs = 
      match self#link cmode f with
	| F_call f -> self#pp_call ~f fmt xs
	| F_call2 f -> self#pp_nary ~op:(Call f) fmt xs
	| F_assoc op -> Plib.pp_assoc ~e:"?" ~op self#pp_atom fmt xs

    method virtual pp_apply : cmode -> term -> term list printer

    (* -------------------------------------------------------------------------- *)
    (* --- Arithmetics Operators                                              --- *)
    (* -------------------------------------------------------------------------- *)
		    
    method virtual op_scope : amode -> string option
    method virtual op_real_of_int : op
    method virtual op_add : amode -> op
    method virtual op_mul : amode -> op
    method virtual op_div : amode -> op
    method virtual op_mod : amode -> op
    method virtual op_minus : amode -> op

    (* -------------------------------------------------------------------------- *)
    (* --- Comparisons                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual op_equal : cmode -> op
    method virtual op_noteq : cmode -> op
    method virtual op_eq  : cmode -> amode -> op
    method virtual op_neq : cmode -> amode -> op
    method virtual op_lt  : cmode -> amode -> op
    method virtual op_leq : cmode -> amode -> op

    (* -------------------------------------------------------------------------- *)
    (* --- Arithmetics Printers                                               --- *)
    (* -------------------------------------------------------------------------- *)

    method private pp_arith_arg flow fmt e =
      if linker_shared#mem e then self#pp_atom fmt e 
      else
	if mode = Mreal && T.is_int e then
	  self#with_mode Mint
	    (fun _ -> 
	       match self#op_real_of_int with
		 | Op op | Assoc op -> 
		     begin
		       match flow with
			 | Atom -> fprintf fmt "(%s %a)" op self#pp_atom e
			 | Flow -> fprintf fmt "%s %a" op self#pp_atom e
		     end
		 | Call f ->
		     begin
		       match self#callstyle with
			 | CallVar | CallVoid -> 
			     fprintf fmt "%s(%a)" f self#pp_flow e
			 | CallApply -> 
			     match flow with
			       | Atom -> fprintf fmt "(%s %a)" f self#pp_atom e
			       | Flow -> fprintf fmt "%s %a" f self#pp_atom e
		     end)
	else match flow with
	  | Flow -> self#pp_flow fmt e
	  | Atom -> self#pp_atom fmt e

    method private pp_arith_call ~f fmt xs =
      match self#callstyle with
	| CallVar -> Plib.pp_call_var ~f (self#pp_arith_arg Flow) fmt xs
	| CallVoid -> Plib.pp_call_void ~f (self#pp_arith_arg Flow) fmt xs
	| CallApply -> Plib.pp_call_apply ~f (self#pp_arith_arg Atom) fmt xs

    method private pp_arith_unop ~phi fmt a =
      match phi (amode mode) with
	| Assoc op | Op op ->
	    if is_identop op && self#is_atomic a then
	      fprintf fmt "%s %a" op (self#pp_arith_arg Atom) a
	    else
	      fprintf fmt "%s%a" op (self#pp_arith_arg Atom) a
	| Call f -> self#pp_arith_call ~f fmt [a]
	    	      
    method private pp_arith_binop ~phi fmt a b =
      self#with_mode
	(if T.is_real a || T.is_real b then Mreal else Mint)
	begin fun _ ->
	  match phi (amode mode) with
	    | Assoc op | Op op -> 
		Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b
	    | Call f -> self#pp_arith_call ~f fmt [a;b]
	end
		 
    method private pp_arith_nary ~phi fmt xs =
      self#with_mode
	(if List.exists T.is_real xs then Mreal else Mint)
	begin fun _ ->
	  match phi (amode mode) with
	    | Assoc op -> Plib.pp_assoc ~e:"?" ~op (self#pp_arith_arg Atom) fmt xs
	    | Op op -> Plib.pp_fold_binop ~e:"?" ~op (self#pp_arith_arg Atom) fmt xs
	    | Call f ->
		match self#callstyle with
		  | CallVar | CallVoid ->
		      Plib.pp_fold_call ~e:"?" ~f (self#pp_arith_arg Flow) fmt xs
		  | CallApply ->
		      Plib.pp_fold_apply ~e:"?" ~f (self#pp_arith_arg Atom) fmt xs
	end

    method private pp_arith_cmp ~phi fmt a b =
      let is_real = T.is_real a || T.is_real b in
      let amode = if is_real then Areal else Aint in
      let gmode = if is_real then Mreal else Mint in
      match phi (cmode mode) amode with
	| Assoc op | Op op -> 
	    self#with_mode gmode 
	      (fun emode ->
		 let scope = 
		   match emode with
		     | Mpositive | Mnegative 
		     | Mterm | Mterm_int | Mterm_real -> self#op_scope amode
		     | Mint | Mreal -> None
		 in match scope with
		   | None ->
		       begin
			 fprintf fmt "@[<hov 2>" ;
			 Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b ;
			 fprintf fmt "@]" ;
		       end
		   | Some s ->
		       begin
			 fprintf fmt "@[<hov 1>(" ;
			 Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b ;
			 fprintf fmt ")%s@]" s ;
		       end)
	| Call f ->
	    begin
	      fprintf fmt "@[<hov 2>" ;
	      self#with_mode gmode 
		(fun _ -> self#pp_arith_call ~f fmt [a;b]) ;
	      fprintf fmt "@]" ;
	    end

    method pp_times fmt k e =
      if Z.equal k Z.minus_one 
      then self#pp_arith_unop ~phi:(self#op_minus) fmt e
      else self#pp_arith_binop ~phi:(self#op_mul) fmt (T.e_zint k) e

    (* -------------------------------------------------------------------------- *)
    (* --- Arrays                                                             --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual pp_array_get : formatter -> term -> term -> unit
    method virtual pp_array_set : formatter -> term -> term -> term -> unit

    (* -------------------------------------------------------------------------- *)
    (* --- Records                                                            --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual pp_get_field : formatter -> term -> Field.t -> unit
    method virtual pp_def_fields : record printer

    (* -------------------------------------------------------------------------- *)
    (* --- Logical Connectives                                                --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual op_not   : cmode -> op
    method virtual op_and   : cmode -> op
    method virtual op_or    : cmode -> op
    method virtual op_imply : cmode -> op
    method virtual op_equiv : cmode -> op

    (* -------------------------------------------------------------------------- *)
    (* --- Polarity                                                           --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_not fmt p =
      let pp = self#pp_unop ~op:(self#op_not (cmode mode)) in
      match mode with
	| Mpositive -> mode <- Mnegative ; pp fmt p ; mode <- Mpositive
	| Mnegative -> mode <- Mpositive ; pp fmt p ; mode <- Mnegative
	| _ -> pp fmt p

    method private pp_polarity pp fmt (inv,x) =
      match mode with
	| Mpositive when inv -> mode <- Mnegative ; pp fmt x ; mode <- Mpositive
	| Mnegative when inv -> mode <- Mpositive ; pp fmt x ; mode <- Mnegative
	| _ -> pp fmt x
	    
    method pp_imply fmt hs p =
      let op = self#op_imply (cmode mode) in
      let pp_atom = self#pp_polarity self#pp_atom in
      let pp_flow = self#pp_polarity self#pp_flow in
      let xs = List.map (fun h -> true,h) hs @ [false,p] in
      match op with
	| Assoc op -> Plib.pp_assoc ~e:"?" ~op pp_atom fmt xs
	| Op op -> Plib.pp_fold_binop ~e:"?" ~op pp_atom fmt xs
	| Call f -> 
	    match self#callstyle with
	      | CallVar | CallVoid -> 
		  Plib.pp_fold_call ~e:"?" ~f pp_flow fmt xs
	      | CallApply -> 
		  Plib.pp_fold_apply ~e:"?" ~f pp_atom fmt xs

    (* -------------------------------------------------------------------------- *)
    (* --- Equality                                                           --- *)
    (* -------------------------------------------------------------------------- *)

    method pp_equal fmt a b =
      let cm = cmode mode in
      match Kind.merge (T.sort a) (T.sort b) with
	| Sprop | Sbool -> self#pp_binop ~op:(self#op_equiv cm) fmt a b
	| Sdata | Sarray _ -> self#pp_binop_term ~op:(self#op_equal cm) fmt a b
	| Sint  | Sreal    -> self#pp_arith_cmp ~phi:(self#op_eq) fmt a b

    method pp_noteq fmt a b =
      let cm = cmode mode in
      match Kind.merge (T.sort a) (T.sort b) with
	| Sprop | Sbool    -> self#pp_unop ~op:(self#op_not cm) fmt (T.e_equiv a b)
	| Sdata | Sarray _ -> self#pp_binop_term ~op:(self#op_noteq cm) fmt a b
	| Sint  | Sreal    -> self#pp_arith_cmp ~phi:(self#op_neq) fmt a b

    (* -------------------------------------------------------------------------- *)
    (* --- Conditional                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual pp_conditional : formatter -> term -> term -> term -> unit

    (* -------------------------------------------------------------------------- *)
    (* --- Quantifiers                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method virtual pp_forall : tau -> var list printer
    method virtual pp_exists : tau -> var list printer
    method virtual pp_lambda : var list printer

    method private pp_binders fmt p =
      match T.repr p with

	| Bind(Lambda,x,p) ->
	    let xs,p = lambda [x] p in
	    List.iter self#bind xs ;
	    self#pp_lambda fmt xs ;
	    self#pp_binders fmt p 
	    
	| Bind((Forall|Exists) as q,x,p) -> 
	    let vars,p = binders q (add_var x TauMap.empty) p in
	    TauMap.iter
	      (fun t xs ->
		 List.iter self#bind xs ;
		 let xs = List.sort Var.compare xs in
		 match q with
		   | Forall -> fprintf fmt "%a@ " (self#pp_forall t) xs
		   | Exists -> fprintf fmt "%a@ " (self#pp_exists t) xs
		   | Lambda -> assert false 
	      ) vars ;
	    self#pp_binders fmt p

	| _ -> self#pp_shared fmt p
	
    (* -------------------------------------------------------------------------- *)
    (* --- Sharing                                                            --- *)
    (* -------------------------------------------------------------------------- *)

    method bind x =
      let basename = T.base_of_var x in
      ignore (linker_variable#alloc ~basename x) ;
      vars <- Vars.add x vars

    method virtual is_shareable : term -> bool
	
    method virtual pp_let : Format.formatter -> string -> term -> unit

    method private pp_lets fmt xes e =
      begin
	let m0 = mode in
	List.iter 
	  (fun (x,e) ->
	     mode <- Mterm ;
	     self#pp_let fmt x e ;
	     linker_shared#bind_reserved e x ;
	  ) xes ;
	mode <- m0 ;
	self#pp_flow fmt e ;
      end

    method private pp_shared fmt e =
      let atomic e = self#is_atomic e || linker_shared#mem e in
      let shareable e = self#is_shareable e in
      let es = T.shared ~atomic ~shareable ~closed:vars [e] in
      if es <> [] then
	self#local 
	  (fun () ->
	     let xes =
	       List.map
		 (fun e -> 
		    let basename = Kind.basename (T.sort e) in
		    let var = linker_shared#reserve ~basename in
		    var , e
		 ) es 
	     in self#pp_lets fmt xes e)
      else self#pp_flow fmt e

    (* -------------------------------------------------------------------------- *)
    (* --- Expressions                                                        --- *)
    (* -------------------------------------------------------------------------- *)

    method private op_scope_for e =
      match mode with
	| (Mpositive | Mnegative | Mterm) when T.is_int e -> self#op_scope Aint
	| (Mpositive | Mnegative | Mterm) when T.is_real e -> self#op_scope Areal
	| Mterm_int -> self#op_scope Aint
	| Mterm_real -> self#op_scope Areal
	| _ -> None

    method pp_atom fmt e =
      try pp_print_string fmt (linker_shared#find e)
      with Not_found -> 
	if self#is_atomic e
	then self#pp_repr fmt e
	else fprintf fmt "@[<hov 1>(%a)@]" self#pp_repr e ;
	match self#op_scope_for e with
	  | None -> ()
	  | Some s -> pp_print_string fmt s

    method pp_flow fmt e =
      try pp_print_string fmt (linker_shared#find e)
      with Not_found -> 
	match self#op_scope_for e with
	  | None -> self#pp_repr fmt e
	  | Some s -> fprintf fmt "@[<hov 1>(%a)%s@]" self#pp_repr e s

    method private pp_repr fmt e =
      match T.repr e with
	| True -> pp_print_string fmt (self#e_true (cmode mode))
	| False -> pp_print_string fmt (self#e_false (cmode mode))
	| Var x -> 
	    begin
	      match cmode mode with
		| Cterm -> self#pp_var fmt x
		| Cprop -> fprintf fmt "(%a = %s)" 
		    self#pp_var x (self#e_true Cterm)
	    end
	| Not p -> 
	    begin
	      match T.repr p with
		| Var x ->
		    begin
		      match cmode mode with
			| Cterm -> self#pp_not fmt p
			| Cprop -> fprintf fmt "(%a = %s)" 
			    self#pp_var x (self#e_false Cterm)
		    end
		| _ -> self#pp_not fmt p 
	    end
	| Kint x -> self#pp_int fmt x
	| Kreal x -> self#pp_real fmt x
	| Add xs -> 
	    begin
	      let amode = amode mode in
	      match self#op_add amode , self#op_minus amode with
		| Assoc plus , Op minus ->
		    let sxs = List.map
		      (fun x ->
			 match T.repr x with
			   | Kint z when Z.negative z -> (false,T.e_zint (Z.opp z))
			   | Times(k,y) when Z.equal k Z.minus_one -> (false,y)
			   | _ -> (true,x)
		      ) xs 
		    in Plib.iteri
			 (fun i (s,x) ->
			    if not s || i <> Ifirst then
			      fprintf fmt "@ %s " (if s then plus else minus) ;
			    self#pp_arith_arg Atom fmt x) sxs
		| _ -> self#pp_arith_nary ~phi:(self#op_add) fmt xs
	    end
	| Mul xs -> self#pp_arith_nary ~phi:(self#op_mul) fmt xs
	| Div(a,b) -> self#pp_arith_binop ~phi:(self#op_div) fmt a b
	| Mod(a,b) -> self#pp_arith_binop ~phi:(self#op_mod) fmt a b
	| Times(k,a) -> self#pp_times fmt k a
	| Eq(a,b)  -> self#pp_equal fmt a b
	| Neq(a,b) -> self#pp_noteq fmt a b
	| Lt(a,b)  -> self#pp_arith_cmp ~phi:(self#op_lt) fmt a b
	| Leq(a,b) -> self#pp_arith_cmp ~phi:(self#op_leq) fmt a b
	| Aget(a,k) -> self#pp_array_get fmt a k
	| Aset(a,k,v) -> self#pp_array_set fmt a k v
	| Rget(r,f) -> self#pp_get_field fmt r f
	| Rdef fts -> self#pp_def_fields fmt fts
	| If(a,b,c) -> self#pp_conditional fmt a b c
	| And ts -> self#pp_nary ~op:(self#op_and (cmode mode)) fmt ts
	| Or ts -> self#pp_nary ~op:(self#op_or (cmode mode)) fmt ts
	| Imply(hs,p) -> self#pp_imply fmt hs p
	| Apply(e,es) -> self#with_mode Mterm 
	    (fun em -> self#pp_apply (cmode em) e fmt es)
	| Fun(f,ts) -> self#with_mode Mterm 
	    (fun em -> self#pp_fun (cmode em) f fmt ts)
	| Bind _ -> self#local (fun () -> self#pp_binders fmt e)
	    
    (* -------------------------------------------------------------------------- *)
    (* --- Formulae                                                           --- *)
    (* -------------------------------------------------------------------------- *)

    method private pp_expr_mode m fmt e =
      mode <- m ; self#pp_shared fmt e

    method pp_term = self#pp_expr_mode Mterm
    method pp_prop = self#pp_expr_mode Mpositive
    method pp_expr (tau:tau) = self#pp_expr_mode (tmode tau)

  end

end
