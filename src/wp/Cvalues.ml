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
(* --- Lifting Operations over Memory Values                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Qed
open Lang
open Lang.F
open Memory
open Definitions

(* -------------------------------------------------------------------------- *)
(* --- C Constants                                                        --- *)
(* -------------------------------------------------------------------------- *)
	  
let rec constant = function
  | CInt64(z,_,_) -> e_bigint z
  | CChr c -> e_int64 (Ctypes.char c)
  | CReal(f,_,_) -> Cfloat.code_lit f
  | CEnum e -> constant_exp e.eival
  | CStr _ | CWStr _ -> Warning.error "String constants not yet implemented"

and logic_constant = function
  | Integer(z,_) -> e_bigint z
  | LChr c -> e_int64 (Ctypes.char c)
  | LReal r -> Cfloat.acsl_lit r
  | LEnum e -> constant_exp e.eival
  | LStr _ | LWStr _ -> Warning.error "String constants not yet implemented"
      
and constant_exp e =
  let e = Cil.constFold true e in
  match e.enode with
    | Const c -> constant c
    | _ -> Warning.error "constant(%a)" Printer.pp_exp e

and constant_term t =
  let e = Cil.constFoldTerm true t in
  match e.term_node with
    | TConst c -> logic_constant c
    | _ -> Warning.error "constant(%a)" Printer.pp_term t

(* -------------------------------------------------------------------------- *)

(* The type contains C-integers *)
let rec is_constrained ty = 
  match Ctypes.object_of ty with
    | C_int _ -> true
    | C_float _ -> false
    | C_pointer _ -> false
    | C_array a -> is_constrained a.arr_element
    | C_comp c -> is_constrained_comp c

and is_constrained_comp c =
  List.exists (fun f -> is_constrained f.ftype) c.cfields

module type CASES =
sig
  val prefix : string
  val model : Cint.model
    (* Natural : all types are constrained, but only with their natural values *)
    (* Machine : only atomic types are constrained *)
  val is_int : c_int -> term -> pred
  val is_float : c_float -> term -> pred
  val is_pointer : term -> pred
end

module STRUCTURAL(C : CASES) =
struct

  let constrained_elt ty = match C.model with
    | Cint.Natural -> true
    | Cint.Machine -> is_constrained ty

  let constrained_comp c = match C.model with
    | Cint.Natural -> true
    | Cint.Machine -> is_constrained_comp c
	  
  let model_int fmt i = match C.model with
    | Cint.Natural -> Format.pp_print_string fmt "int"
    | Cint.Machine -> Ctypes.pp_int fmt i
	  
  let array_name te ds =
    let dim = List.length ds in
    match te with
      | C_int i -> 
	  Pretty_utils.sfprintf "%sArray%d_%a" C.prefix dim model_int i
      | C_float _ -> 
	  Pretty_utils.sfprintf "%sArray%d_float" C.prefix dim
      | C_pointer _ ->
	  Pretty_utils.sfprintf "%sArray%d_pointer" C.prefix dim 
      | C_comp c ->
	  Pretty_utils.sfprintf "%sArray%d%s" C.prefix dim (Lang.comp_id c)
      | C_array _ ->
	  Wp_parameters.fatal "Unflatten array (%s %a)" C.prefix Ctypes.pretty te

  let rec is_obj obj t = 
    match obj with
      | C_int i -> C.is_int i t
      | C_float f -> C.is_float f t
      | C_pointer _ty -> C.is_pointer t
      | C_comp c -> 
	  if constrained_comp c then is_record c t else p_true
      | C_array a ->
	  if constrained_elt a.arr_element 
	  then
	    let te,ds = Ctypes.array_dimensions a in
	    is_array te ds t
	  else p_true

  and is_typ typ t = is_obj (Ctypes.object_of typ) t

  and is_record c s =
    Definitions.call_pred
      (Lang.generated_p (C.prefix ^ Lang.comp_id c))
      (fun lfun ->
	 let basename = if c.cstruct then "S" else "U" in
	 let s = Lang.freshvar ~basename (Lang.tau_of_comp c) in
	 let def = p_all 
	   (fun f -> is_typ f.ftype (e_getfield (e_var s) (Lang.Cfield f)))
	   c.cfields 
	 in {
	   d_lfun = lfun ; d_types = 0 ; d_params = [s] ;
	   d_cluster = Definitions.compinfo c ;
	   d_definition = Predicate(Def,def) ;
	 })
      [s]

  and is_array te ds t =
    Definitions.call_pred
    (Lang.generated_p (array_name te ds))
       (fun lfun ->
	  let x = Lang.freshvar ~basename:"T" (Matrix.tau te ds) in
	  let ks = List.map (fun _d -> Lang.freshvar ~basename:"k" Logic.Int) ds in
	  let e = List.fold_left (fun a k -> e_get a (e_var k)) (e_var x) ks in
	  let def = p_forall ks (is_obj te e) in
	  {
	    d_lfun = lfun ; d_types = 0 ; d_params = [x] ;
	    d_cluster = Definitions.matrix te ;
	    d_definition = Predicate(Def,def) ;
	  }
       ) [t]

end

(* -------------------------------------------------------------------------- *)
(* --- Null-Values                                                        --- *)
(* -------------------------------------------------------------------------- *)

let null = Context.create "Lang.null"
module NULL = STRUCTURAL
  (struct
     let prefix = "Null"
     let model = Cint.Natural
     let is_int _i = p_equal e_zero
     let is_float _f = p_equal e_zero_real
     let is_pointer p = Context.get null p
   end)

let is_null = NULL.is_obj

module TYPE = STRUCTURAL
  (struct
     let prefix = "Is"
     let model = Cint.Machine
     let is_int = Cint.irange
     let is_float = Cfloat.frange
     let is_pointer _ = p_true
   end)

let has_ctype = TYPE.is_typ

let has_ltype ltype e =
  match Logic_utils.unroll_type ltype with
    | Ctype typ -> has_ctype typ e
    | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> p_true

let is_object obj = function
  | Loc _ -> p_true
  | Val e -> TYPE.is_obj obj e

let cdomain typ =
  if is_constrained typ then Some(has_ctype typ) else None

let ldomain ltype =
  match Logic_utils.unroll_type ltype with
    | Ctype typ -> cdomain typ
    | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> None

(* -------------------------------------------------------------------------- *)
(* --- ACSL Equality                                                      --- *)
(* -------------------------------------------------------------------------- *)

let s_eq = ref (fun _ _ _ -> assert false) (* recursion for equal_object *)

module EQARRAY = Model.Generator(Matrix.NATURAL)
  (struct
     open Matrix
     type key = matrix
     type data = Lang.lfun
     let name = "Cvalues.EqArray"
     let compile (te,ds) =
       let lfun = Lang.generated_f ~sort:Logic.Sprop "EqArray%s_%s" 
	 (Matrix.id ds) (Matrix.natural_id te) 
       in
       let cluster = Definitions.matrix te in
       let denv = Matrix.denv ds in
       let tau = Matrix.tau te ds in
       let xa = Lang.freshvar ~basename:"T" tau in
       let xb = Lang.freshvar ~basename:"T" tau in
       let ta = e_var xa in
       let tb = e_var xb in
       let ta_xs = List.fold_left e_get ta denv.index_val in
       let tb_xs = List.fold_left e_get tb denv.index_val in
       let eq = p_call lfun (denv.size_val @ [ta ; tb]) in
       let property = p_hyps (denv.index_range) (!s_eq te ta_xs tb_xs) in
       let definition = p_forall denv.index_var property in
       (* Definition of the symbol *)
       Definitions.define_symbol {
	 d_lfun = lfun ; d_types = 0 ; 
	 d_params = denv.size_var @ [xa ; xb ] ;
	 d_definition = Predicate(Def,definition) ;
	 d_cluster = cluster ;
       } ;
       (* Extensionnal Definition (with triggers) *)
       let name = Printf.sprintf "EqArrayExt%s_%s" 
	 (Matrix.id ds) (Matrix.natural_id te) 
       in
       Definitions.define_lemma {
	 l_name = name ;
	 l_cluster = cluster ;
	 l_types = 0 ;
	 l_forall = denv.size_var @ [xa ; xb ] @ denv.index_var ;
	 l_assumed = true ;
	 l_triggers = [ 
	   [ Trigger.of_pred eq ; Trigger.of_term ta_xs ] ;
	   [ Trigger.of_pred eq ; Trigger.of_term tb_xs ] 
	 ] ;
	 l_lemma = property ;
       } ; 
       (* Finally return symbol *)
       lfun
   end)

let rec equal_object obj a b =
  match obj with
    | C_int _ | C_float _ | C_pointer _ -> p_equal a b
    | C_array t -> 
	equal_array (Matrix.of_array t) a b
    | C_comp c ->
	equal_comp c a b
    
and equal_typ typ a b = equal_object (Ctypes.object_of typ) a b

and equal_comp c a b =
  Definitions.call_pred
    (Lang.generated_p ("Eq" ^ Lang.comp_id c))
    (fun lfun ->
       let basename = if c.cstruct then "S" else "U" in
       let xa = Lang.freshvar ~basename (Lang.tau_of_comp c) in
       let xb = Lang.freshvar ~basename (Lang.tau_of_comp c) in
       let ra = e_var xa in
       let rb = e_var xb in
       let def = p_all 
	 (fun f -> 
	    let fd = Cfield f in
	    equal_typ f.ftype 
	      (e_getfield ra fd) (e_getfield rb fd))
	 c.cfields
       in {
	 d_lfun = lfun ; d_types = 0 ; d_params = [xa;xb] ;
	 d_cluster = Definitions.compinfo c ;
	 d_definition = Predicate(Def,def) ;
       }
    ) [a;b]

and equal_array m a b = 
  match m with
    | _obj , [None] -> p_equal a b
    | _ -> p_call (EQARRAY.get m) (Matrix.size m @ [a;b])

let () = s_eq := equal_object

(* -------------------------------------------------------------------------- *)
(* --- Lifting Values                                                     --- *)
(* -------------------------------------------------------------------------- *)

let map_value f = function
  | Val t -> Val t
  | Loc l -> Loc (f l)

let map_sloc f = function
  | Sloc l -> Sloc (f l)
  | Sarray(l,obj,s) -> Sarray(f l,obj,s)
  | Srange(l,obj,a,b) -> Srange(f l,obj,a,b)
  | Sdescr(xs,l,p) -> Sdescr(xs,f l,p)

let map_logic f = function
  | Vexp t -> Vexp t
  | Vloc l -> Vloc (f l)
  | Vset s -> Vset s
  | Lset ls -> Lset (List.map (map_sloc f) ls)

(* -------------------------------------------------------------------------- *)
(* --- Int-As-Boolans                                                     --- *)
(* -------------------------------------------------------------------------- *)
	
let bool_eq a b = e_if (e_eq a b) e_one e_zero
let bool_lt a b = e_if (e_lt a b) e_one e_zero
let bool_neq a b = e_if (e_eq a b) e_zero e_one
let bool_leq a b = e_if (e_leq a b) e_one e_zero
let bool_and a b = e_and [e_neq a e_zero ; e_neq b e_zero]
let bool_or  a b = e_or  [e_neq a e_zero ; e_neq b e_zero]
let is_true p = e_if (e_prop p) e_one e_zero
let is_false p = e_if (e_prop p) e_zero e_one
	    
(* -------------------------------------------------------------------------- *)
(* --- Lifting Memory Model to Values                                     --- *)
(* -------------------------------------------------------------------------- *)

module Logic(M : Memory.Model) =
struct

  type logic = M.loc Memory.logic
  type region = M.loc Memory.sloc list

  (* -------------------------------------------------------------------------- *)
  (* --- Projections                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let value = function
    | Vexp e -> e
    | Vloc l -> M.pointer_val l
    | Vset _ -> Warning.error "Set of values not yet implemented"
    | Lset _ -> Warning.error "T-Set of values not yet implemented"

  let loc = function
    | Vloc l -> l
    | Vexp e -> M.pointer_loc e
    | Vset _ -> Warning.error "Set of pointers not yet implemented"
    | Lset _ -> Warning.error "T-Set of regions not yet implemented"

  let rdescr = function
    | Sloc l -> [],l,p_true
    | Sdescr(xs,l,p) -> xs,l,p
    | Sarray(l,obj,s) ->
	let x = Lang.freshvar ~basename:"k" Logic.Int in
	let k = e_var x in
	[x],M.shift l obj k,Vset.in_size k s
    | Srange(l,obj,a,b) ->
	let x = Lang.freshvar ~basename:"k" Logic.Int in
	let k = e_var x in
	[x],M.shift l obj k,Vset.in_range k a b

  let vset_of_sloc sloc = 
    List.map
      (function
	 | Sloc p -> Vset.Singleton (M.pointer_val p)
	 | u ->
	     let xs,l,p = rdescr u in
	     Vset.Descr( xs , M.pointer_val l , p )
      ) sloc
      
  let sloc_of_vset vset = 
    List.map
      (function
	 | Vset.Singleton e -> Sloc (M.pointer_loc e)
	 | w ->
	     let xs,t,p = Vset.descr w in
	     Sdescr(xs,M.pointer_loc t,p)
      ) vset
      
  let vset = function
    | Vexp v -> Vset.singleton v
    | Vloc l -> Vset.singleton (M.pointer_val l)
    | Vset s -> s
    | Lset sloc -> vset_of_sloc sloc

  let sloc = function
    | Vexp e -> [Sloc (M.pointer_loc e)]
    | Vloc l -> [Sloc l]
    | Lset ls -> ls
    | Vset vset -> sloc_of_vset vset
	    
  (* -------------------------------------------------------------------------- *)
  (* --- Morphisms                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let is_single = function (Vexp _ | Vloc _) -> true | (Lset _ | Vset _) -> false

  let map_lift f1 f2 a =
    match a with
      | Vexp e -> Vexp (f1 e)
      | Vloc l -> Vexp (f1 (M.pointer_val l))
      | _ -> Vset(f2 (vset a))

  let apply_lift f1 f2 a b = 
    if is_single a && is_single b then
      Vexp (f1 (value a) (value b))
    else
      Vset (f2 (vset a) (vset b))

  let map f = map_lift f (Vset.map f)
  let map_opp = map_lift e_opp Vset.map_opp

  let apply f = apply_lift f (Vset.lift f)
  let apply_add = apply_lift e_add Vset.lift_add
  let apply_sub = apply_lift e_sub Vset.lift_sub

  let map_loc f lv =
    if is_single lv then Vloc (f (loc lv))
    else Lset
      (List.map
	 (function
	    | Sloc l -> Sloc (f l)
	    | s -> let xs,l,p = rdescr s in Sdescr(xs,f l,p)
	 ) (sloc lv))

  let map_l2t f lv =
    if is_single lv then Vexp (f (loc lv))
    else Vset
      (List.map
	 (function
	    | Sloc l -> Vset.Singleton (f l)
	    | s -> let xs,l,p = rdescr s in Vset.Descr(xs,f l,p)
	 ) (sloc lv))

  let map_t2l f sv =
    if is_single sv then Vloc (f (value sv))
    else Lset
      (List.map
	 (function
	    | Vset.Singleton e -> Sloc (f e)
	    | s -> let xs,l,p = Vset.descr s in Sdescr(xs,f l,p)
	 ) (vset sv))

  (* -------------------------------------------------------------------------- *)
  (* --- Locations                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let field lv f = map_loc (fun l -> M.field l f) lv

  let restrict kset = function
    | None -> kset
    | Some s ->
	match kset with
	  | Vset.Singleton _ | Vset.Set _ -> kset
	  | Vset.Range(a,b) -> 
	      let cap l = function None -> Some l | u -> u in
	      Vset.Range(cap e_zero a,cap (e_int (s-1)) b)
	  | Vset.Descr(xs,k,p) ->
	      let a = e_zero in
	      let b = e_int s in
	      Vset.Descr(xs,k,p_conj [p_leq a k;p_lt k b;p])

  let shift_set sloc obj (size : int option) kset =
    match sloc , kset , size with
      | Sloc l , Vset.Range(None,None) , Some s -> Sarray(l,obj,s)
      | _ ->
	  match sloc , restrict kset size with
	    | Sloc l , Vset.Singleton k -> Sloc(M.shift l obj k)
	    | Sloc l , Vset.Range(a,b) -> Srange(l,obj,a,b)
	    | Srange(l,obj0,a0,b0) , Vset.Singleton k 
		when Ctypes.equal obj0 obj ->
		Srange(l,obj0, Vset.bound_add a0 (Some k), Vset.bound_add b0 (Some k))
	    | Srange(l,obj0,a0,b0) , Vset.Range(a1,b1) 
		when Ctypes.equal obj0 obj ->
		Srange(l,obj0, Vset.bound_add a0 a1, Vset.bound_add b0 b1)
	    | _ ->
		let xs,l,p = rdescr sloc in
		let ys,k,q = Vset.descr kset in
		Sdescr( xs @ ys , M.shift l obj k , p_and p q )
		  
  let shift lv obj ?size kv =
    if is_single kv then
      let k = value kv in map_loc (fun l -> M.shift l obj k) lv
    else
      let ks = vset kv in
      Lset(List.fold_left
	     (fun s sloc ->
		List.fold_left
		  (fun s kset ->
		     shift_set sloc obj size kset :: s
		  ) s ks
	     ) [] (sloc lv))

  (* -------------------------------------------------------------------------- *)
  (* --- Load in Memory                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  type loader = { 
    mutable sloc : M.loc sloc list ; 
    mutable vset : Vset.vset list ;
  }
      
  let flush prefer_loc a = match a with
    | { vset=[] } -> Lset (List.rev a.sloc)
    | { sloc=[] } -> Vset (List.rev a.vset)
    | _ ->
	if prefer_loc then
	  Lset (a.sloc @ sloc_of_vset a.vset)
	else
	  Vset (vset_of_sloc a.sloc @ a.vset)
	    
  let loadsloc a sigma obj = function
    | Sloc l -> 
	begin
	  match M.load sigma obj l with
	    | Val t -> a.vset <- Vset.Singleton t :: a.vset
	    | Loc l -> a.sloc <- Sloc l :: a.sloc
	end
    | (Sarray _ | Srange _ | Sdescr _) as s ->
	let xs , l , p = rdescr s in
	begin
	  match M.load sigma obj l with
	    | Val t -> a.vset <- Vset.Descr(xs,t,p) :: a.vset
	    | Loc l -> a.sloc <- Sdescr(xs,l,p) :: a.sloc
	end
	  
  let load sigma obj lv =
    if is_single lv then 
      let data = M.load sigma obj (loc lv) in
      Lang.assume (is_object obj data) ;
      match data with
	| Val t -> Vexp t
	| Loc l -> Vloc l
    else
      let a = { vset=[] ; sloc=[] } in
      List.iter (loadsloc a sigma obj) (sloc lv) ;
      flush (Ctypes.is_pointer obj) a

  let union t vs =
    let a = { vset=[] ; sloc=[] } in
    List.iter
      (function
	 | Vexp e -> a.vset <- Vset.Singleton e::a.vset
	 | Vloc l -> a.sloc <- Sloc l :: a.sloc
	 | Vset s -> a.vset <- List.rev_append s a.vset 
	 | Lset s -> a.sloc <- List.rev_append s a.sloc
      ) vs ;
    flush (Logic_typing.is_pointer_type t) a

  let inter t vs =
    match List.map (fun v -> Vset.concretize (vset v)) vs with
      | [] -> 
	  if Logic_typing.is_pointer_type t 
	  then Lset [] else Vset []
      | v::vs ->
	  let s = List.fold_left Vset.inter v vs in
	  let t = Lang.tau_of_ltype t in
	  Vset [Vset.Set(t,s)]

  (* -------------------------------------------------------------------------- *)
  (* --- Sloc to Rloc                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rloc obj = function 
    | Sloc l -> Rloc(obj,l)
    | Sarray(l,t,s) -> Rarray(l,t,s)
    | Srange(l,t,a,b) -> Rrange(l,t,a,b)
    | Sdescr _ -> raise Exit

  (* -------------------------------------------------------------------------- *)
  (* --- Separated                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let separated_sloc w (obj1,sloc1) (obj2,sloc2) =
    List.fold_left
      (fun w s1 ->
	 List.fold_left
	   (fun w s2 ->
	      let cond =
		try M.separated (rloc obj1 s1) (rloc obj2 s2)
		with Exit ->
		  let xs,l1,p1 = rdescr s1 in
		  let ys,l2,p2 = rdescr s2 in
		  let se1 = Rloc(obj1,l1) in
		  let se2 = Rloc(obj2,l2) in
		  p_forall (xs@ys) (p_hyps [p1;p2] (M.separated se1 se2))
	      in cond::w
	   ) w sloc2
      ) w sloc1
      
  let rec separated_from w r1 = function
    | r2::rs -> separated_from (separated_sloc w r1 r2) r1 rs
    | [] -> w

  let rec separated_regions w = function
    | r::rs -> separated_regions (separated_from w r rs) rs
    | [] -> w

  let separated regions =
    (* forall i<j, (tau_i,R_i)#(tau_j,R_j) *)
    (* forall i<j, forall p in R_j, forall q in R_j, p#q *)
    p_conj (separated_regions [] regions)

  (* -------------------------------------------------------------------------- *)
  (* --- Included                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let included_sloc obj1 s1 obj2 s2 =
    try M.included (rloc obj1 s1) (rloc obj2 s2)
    with Exit ->
      let xs,l1,p1 = rdescr s1 in
      let ys,l2,p2 = rdescr s2 in
      let se1 = Rloc(obj1,l1) in
      let se2 = Rloc(obj2,l2) in
      p_forall xs (p_imply p1 (p_exists ys (p_and p2 (M.included se1 se2))))

  let included obj1 r1 obj2 r2 =
    p_all (fun s1 -> p_any (fun s2 -> included_sloc obj1 s1 obj2 s2) r2) r1

  (* -------------------------------------------------------------------------- *)
  (* --- Valid                                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let valid_sloc sigma acs obj = function
    | Sloc l -> M.valid sigma acs (Rloc(obj,l))
    | Sarray(l,t,s) -> M.valid sigma acs (Rarray(l,t,s))
    | Srange(l,t,a,b) -> M.valid sigma acs (Rrange(l,t,a,b))
    | Sdescr(xs,l,p) -> p_forall xs (p_imply p (M.valid sigma acs (Rloc(obj,l))))

  let valid sigma acs obj = p_all (valid_sloc sigma acs obj) 

end
