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
(* --- Functional Variable Memory Model Functor                           --- *)
(* -------------------------------------------------------------------------- *)

module WpLog = Wp_parameters
open Cil_types
open Cil_datatype
open Formula
open Ctypes
open Clabels


let debug = WpLog.debug ~dkey:"funvar"
let oracle = WpLog.debug ~dkey:"var_kind"

module type Criteria =
sig
  val isHoare : bool
end


module Create
  (Crit:Criteria)
  (M:Mwp.S)
  =
struct
  (* ------------------------------------------------------------------------ *)
  (* --- Helper to move                                                   --- *)
  (* ------------------------------------------------------------------------ *)

 (* [make_array ty] builts the cil type of an array of element of type [ty]*)
  let make_array ty =
    debug "[make_array] %a" !Ast_printer.d_type ty;
    TArray(ty,None,{scache=Cil_types.Not_Computed},[])

  (*[array_of typ] transforms a pointer type [typ] into an cil array type, 
    recursivly*)
  let rec array_of typ = 
    debug "[object_array_of] %a" !Ast_printer.d_type typ;
    match  Cil.unrollType typ with
      |  TPtr(typ,_) ->
        begin
          match Cil.unrollType typ with
            | TVoid _ -> make_array (TInt (IChar,[]))
            | t -> make_array (array_of t)
	end
      | _ -> typ


(* [pointed_of_arity n typ] makes the cil type : [*^n typ]*)
  let rec pointed_of_arity n typ = 
    debug "[pointed_of_arity] %a,%d" !Ast_printer.d_type typ n ;
    if n > 0
    then  pointed_of_arity (pred n) (Cil.typeOf_pointed typ)
    else typ


(*[brackets_and_stars_typ typ], first computes the stars number of typ [n] and
  the inner type [t] if typ = *^n t returns (n,t). Secondly, 
  computes the number of brackets in [t] [m]. 
  Finally returns the number of stars and the number of brackets [(n,m)].*)
 let brackets_and_stars_typ typ =
  let rec stars_and_elt typ = 
     match Cil.unrollType typ with 
    | TPtr (typ,_) -> 
	let (n,t) = stars_and_elt (Cil.unrollType typ) in
	(n+1),t
    | TInt(_,_) | TFloat(_,_) | TFun _ | TEnum (_,_) | TComp (_,_,_)
    | TArray (_,_,_,_) 
    | TBuiltin_va_list _ | TVoid _ | TNamed _ as t-> (0,t)
  in
  let (n,t) = stars_and_elt typ in (n,Variables_analysis.brackets_typ t)

(* the same in logic type*)
  let brackets_and_stars_lv_typ = function 
    | Ctype t -> brackets_and_stars_typ t | _ -> 0,0

      

(* Path *)
  module Lookup =
  struct

    module F = M.F

    (* In a first intention, a path can be view as a pair 
       of a root and a list of offset use to describe an l-value.*)
 
    (* offset of path*)
    type poffset =
      | Ofield of  fieldinfo  (* a field*)
      | Oindex of  F.integer * Ctypes.c_object (*an index and its type*)

    (*root of a path*)
    type root = 
      | Cvar of varinfo  (*a C variable *)
      | Lvar of logic_var (* a purely logic variable (ie.lv_origin = None)*)

    (* Definition of a path*)

    (* NB : the field p_cvar only occurs if the root is a purely logic by 
       reference parameter when its inner model location it is required 
       for the translation of the predicate or function body. 
       Then and additional formal parameters has to been created. 
       This creation is global to a frame of the translation, then the 
       fol-variable has to been created in translate_prop and carry
       to the memory model to be used in memory made formula. 
    *)
    type path = {
      p_root : root ;            (* root *)
      p_mem : M.mem ;            (* in model memory model *)
      p_arity : int ;            (* arity such as define in by reference arity;
				    0 otherwise*)
      p_off : poffset list ;     (* the list of offset to form the current
				    l-value*)
      p_type : Ctypes.c_object option; 
                                 (* the type of the entire l-value represented
				    by this path*)
      p_cvar : F.var option ;    (* the fol-variable represented the address
				    of the root.*)
    }  

    (* Some smart constructors and helper to manage root and path. *)
	
    (* Pretty-printers *)
    let pp_root fmt = function 
      | Cvar x -> !Ast_printer.d_var fmt x
      | Lvar lv -> !Ast_printer.d_logic_var fmt lv

    let pp_path fmt p =
      Format.fprintf fmt "PATH:%a of %d" 
	pp_root p.p_root (List.length p.p_off)

    (* [object_of_root r] returns the c_object of a root [r].*)
    let object_of_root = function 
	Cvar x -> Some (object_of x.vtype) 
      | Lvar lv ->
	  begin
	    match lv.lv_type with 
	      | Ctype t -> Some (object_of t)
	      | _ -> None
	  end
	  
    (* [object_array_of_root r] transforms the type of the root[r] 
     into an array type,c_object, if it was a pointer type. *)
    let object_array_of_root = function 
	Cvar x -> Some (object_of (array_of x.vtype))  
      | Lvar lv->
	  begin 
	    match lv.lv_type with 
	      | Ctype t -> Some (object_of (array_of t)) 
	      | _ -> None
	  end
	  
    (*[object_of_n_pointed n typ] makes the c_object type of a [n]-ary pointer 
      on inner type [typ] : [*^n typ]*)
    let rec object_of_n_pointed n typ = 
      if n=0 then typ else 
	match typ with 
	  | C_pointer typ -> object_of_n_pointed (pred n) (object_of typ)
	  | t -> WpLog.fatal
	      "[object_of_n_pointed] calls with mismatched arguments :%d and %a"
		n pp_object t


    let object_of_pointed_opt = function 
      | None -> None 
      | Some ty -> Some (Ctypes.object_of_pointed ty)

    let object_of_n_pointed_opt n = function 
      | None -> None 
      | Some ty -> Some (object_of_n_pointed n ty)
	



    (* [pointed_of_path p] makes the deferenced path of [p].*)
    let pointed_of_path p = 
      debug "[pointed_of_path] %a" pp_path p;
	{ p with p_type = object_of_pointed_opt p.p_type }

    (* [pointed_of_n_path n p] makes the [n] time deferenced path of [p].*)
    let pointed_of_n_path n p =
	debug "[pointed_of_n_path] %d %a" n pp_path p;
      { p with p_type = object_of_n_pointed_opt n p.p_type }	

    (* [sizeof_poffset off] returns de C sizeof of an offset.*)
    let sizeof_poffset = function
      | Ofield fd -> F.e_int
          (Int64.to_int (Ctypes.sizeof_object (object_of fd.ftype)))
      | Oindex (i,o) ->
          let typeof_obj = F.e_int64 (Ctypes.sizeof_object o)
          in
          F.e_iop Formula.Imul i typeof_obj

    (* [sizeof_path offs] returns the C sizeof of an offsets list.*)
    let sizeof_path offs  =
      List.fold_left
        (fun acc i -> F.e_iop Iadd acc (sizeof_poffset i)) F.i_zero offs

    (* [access_poffset p off] makes the l-value from the term [p] and 
      the offset [off] : [p.f] or [p[i]] . *)
    let access_poffset p = function
      | Ofield f -> F.acc_field (F.unwrap p) f
      | Oindex (i,_) -> F.acc_index (F.unwrap p) i
	  
    (* [access t offs] makes the l-value from [t] with the offset list [offs]*)
    let access t offs = List.fold_left access_poffset t offs
      
    (* [mcvar m r opt_cv] according to the root [r] calls the approriate 
       function of the inner memory model to translate the root [r] in 
       the memory state [m]. 
       If [r] is a C variable then [cvar] else [lvar ]*)
    let mcvar m r opt_cv= 
      debug "[mcvar] of %a" pp_root r;
      match r with 
      | Cvar x -> 
	  debug "[mcvar] case of mem"; M.cvar m x
      | Lvar lv ->
	  debug "[mcvar] case of logic %a" !Ast_printer.d_logic_var lv;
	  let ty = 
		  match lv.lv_type with 
		    | Ctype ty -> ty 
		    | ty -> Wp_parameters.fatal
			"[mcvar] c type of a pure logic type %a"
			  !Ast_printer.d_logic_type ty 
	  in
	  let obj = Ctypes.object_of ty in
	  match opt_cv with 
	    | None -> (* can't happen*)
		debug "[mcvar] case of none associated cvar";
		let tau = Formula.Pointer (M.tau_of_loc) in
		debug "[mcvar] called fresh";
		let x = M.L.fresh lv.lv_name (Formula.Model tau) in
		debug "[mcvar] binds %a to %a"
		  !Ast_printer.d_logic_var lv F.pp_var x ;
		M.loc_of_term obj (F.var x)
	    | Some cx -> 
		debug "[mcvar] case of associated c varaible :%a" F.pp_var cx ;
		M.loc_of_term obj (F.var cx)

    (* [mloc_of_path p] computes the location of the inner memory model 
       from the path [p].*)
    let mloc_of_path p =
      debug "[mloc_of_path] %a" pp_path p;
      List.fold_left
        (fun loc offset ->
          match offset with
            | Ofield f -> M.field loc f
            | Oindex(k,te) -> M.index loc te k
        ) (mcvar p.p_mem p.p_root p.p_cvar) p.p_off
  
  end

  module Model =
  struct

    open Lookup

    module F = M.F
    module A = M.A
    module R = M.R

    type loc = 
	Path of path  (*Functional Variable*)
      | PRef of path  (*Pointer Reference effective Argument*)
      | ARef of path  (*Array Reference effective Argument*)
      | PRpar of path * int (*Pointer by Reference Formal path and arity*)
      | ARpar of path * int (*Array by Reference Formal path and arity*)
      | Mloc of M.loc (* Memory Location *)

    (* Pretty-printers *)
    let pp_ref  r fmt p opt_arity=
      match opt_arity with 
	| None ->
	    Format.fprintf fmt "%s :%a of %d" r pp_root p.p_root 
	      (List.length p.p_off)
	| Some ari -> 
	    Format.fprintf fmt "%s :(%a,%d) of %d" 
	      r pp_root p.p_root ari (List.length p.p_off)
	
    let pp_loc fmt = function
      | Mloc l -> M.pp_loc fmt l
      | Path p -> pp_path fmt p
      | PRef  p -> pp_ref "Ptr REF" fmt p None 
      | PRpar (p,n) -> pp_ref "Ptr REF PARAM" fmt p (Some n)
      | ARef  p -> pp_ref "Array REF" fmt p None
      | ARpar (p,n) -> pp_ref "Array REF PARAM" fmt p (Some n)

    let cast_loc_to_int ty l ty2 =
      match l with
        | Mloc l -> M.cast_loc_to_int ty l ty2
	| Path p | PRef p | PRpar (p,_) | ARef p | ARpar (p,_) -> 
	    M.cast_loc_to_int ty (Lookup.mloc_of_path p) ty2
        

    let cast_int_to_loc ty i ty2 = Mloc (M.cast_int_to_loc ty i ty2)

    (*[mloc_of_loc l] returns the location of the inner memory model 
    corresponding to the location [l].*)
    let mloc_of_loc = function
      | Path p | PRef p | PRpar (p,_) | ARef p | ARpar (p,_) ->
	  Lookup.mloc_of_path p
      | Mloc loc -> 
	  debug "[mloc_of_loc] already a loc %a" M.pp_loc loc ; loc
      
	  
    let loc_of_term o t = Mloc (M.loc_of_term o t)  
    let term_of_loc l =  M.term_of_loc (mloc_of_loc l)

    let null = Mloc M.null
    let is_null l = M.is_null (mloc_of_loc l)


    let root_equal a b = 
      match a,b with 
	| Cvar x, Cvar y -> Varinfo.equal x y 
	| Lvar l, Lvar p -> Logic_var.equal l p
	| _,_ -> false

    let op_loc mop pop l1 l2=
      match l1,l2 with
        | Mloc l1 , Mloc l2 -> mop l1 l2
        | Path xp, Path yq | PRef xp, PRef yq | ARef xp,ARef yq 
	    when root_equal xp.p_root yq.p_root  ->
            pop (sizeof_path xp.p_off) (sizeof_path yq.p_off)
	| ARpar (xp,xn),ARpar (yq,ym)
	| PRpar (xp,xn), PRpar(yq,ym)  
	    when root_equal xp.p_root yq.p_root && xn = ym ->
	    pop (sizeof_path xp.p_off) (sizeof_path yq.p_off)
        | l1,l2 -> mop (mloc_of_loc l1) (mloc_of_loc l2)

    let minus_loc = op_loc M.minus_loc (F.e_iop Isub)
    let lt_loc = op_loc M.lt_loc (F.p_icmp Clt)
    let le_loc = op_loc M.le_loc (F.p_icmp Cleq)
    let le_loc_bool = op_loc M.le_loc_bool (F.e_icmp Cleq)
    let lt_loc_bool = op_loc M.lt_loc_bool (F.e_icmp Clt)
    let equal_loc_bool = op_loc M.equal_loc_bool (F.e_icmp Ceq)
    let equal_loc = op_loc M.equal_loc (F.p_icmp Ceq)
    let tau_of_loc = M.tau_of_loc


  end

  open Model
  open Lookup

  include Datalib.Cvalues(Model)

  module L = M.L

  type decl = M.F.decl
 
  (* ------------------------------------------------------------------------ *)
  (* ---  Values Coersion                                                 --- *)
  (* ------------------------------------------------------------------------ *)


  (*[value_of_mvalue v] returns the value corresponding to value [v] 
    of the inner memory model*)
  let value_of_mvalue = function
    | M.V_int (i,t) -> V_int (i,t)
    | M.V_float (f,t) -> V_float (f,t)
    | M.V_array (a,t) -> V_array(a,t)
    | M.V_record (c,t) -> V_record (c,t)
    | M.V_union (c,t) -> V_union (c,t)
    | M.V_pointer (te,l) -> V_pointer (te,Mloc l)

  (*[mvalue_of_value v] returns the value of the inner memory model 
    correspondig to the value [v]*)
  let mvalue_of_value = function
    | V_int (i,t) -> M.V_int (i,t)
    | V_float (f,t) -> M.V_float (f,t)
    | V_array (a,t) -> M.V_array(a,t)
    | V_record (c,t) -> M.V_record (c,t)
    | V_union (c,t) -> M.V_union (c,t)
    | V_pointer (te,Mloc l) -> M.V_pointer (te, l)
    | V_pointer (_,Path _) 
    | V_pointer (_,PRef _ )
    | V_pointer (_,PRpar _)
    | V_pointer (_,ARef _ )
    | V_pointer (_,ARpar _) -> 
	WpLog.fatal "[mvalue_of_value] of logical pointer"




  (* ------------------------------------------------------------------------ *)
  (* ---  Env and Memory                                                  --- *)
  (* ------------------------------------------------------------------------ *)

  module Xmap = Cil_datatype.Varinfo.Map
    
  (* translation information associated to a C variable.*)
  type var_info = 
      { v_var : F.var ;     (* the fol-variable associated*) 
	v_arity : int ;     (* the arity as define for by reference*)
	v_is_array : bool ; (* true if it is an array reference*)
	v_type : Ctypes.c_object; (* the C_object type of the variable.*)
      
      }

  (* C-variables translation environment*)
  type vars = var_info Xmap.t

  module Lmap = Cil_datatype.Logic_var.Map
    
  (* kind of a by-reference formal of userdef predicate*)
  type formal = 
      Fpref of int (* pointer by reference formal of arity [n] *) 
    | Faref of int (* array by reference formal of ... .*)
   
  (* translation information associated to a by reference formal 
     of a user-definition*)
  type byrefparam = {
    bref_var : F.var ;    (* the fol-variable associated*)
    bref_formal : formal ; (* the kind of the by-reference formal
			      of user-definition *)
  }

  (* The heap is compounded by 3 elements in the funvar memory model: 
     1) [mem] which is the C inner memory model ; 
     2) [vars] which is the environment of translation of the optimized 
   variable. 
     3) [formals] which is the environment oftranslation of the by-reference 
   formal of user-definition.*)
  type mem = {
    mem : M.mem ; 
    mutable vars : vars ;
    mutable formals : byrefparam Lmap.t ; 
  }

  let mem () = { mem = M.mem () ; vars = Xmap.empty ; formals = Lmap.empty}

 
 (* ------------------------------------------------------------------------ *)
 (* --- Locations                                                        --- *)
 (* ------------------------------------------------------------------------ *)

(* Globals mamagment *) 
  let global_scope _ p = p  
  let global _ = ()

 (* Logic Parameters translation *)
 let get_logic_funvar mem arity lv ap = 
   let s = "[get_logic_funvar]" in 
   try 
     let x = (Lmap.find lv mem.formals).bref_var in
     debug "%s %a already recorded" s !Ast_printer.d_logic_var lv; x 
   with Not_found ->
     debug "%s %a not yet recorded" s !Ast_printer.d_logic_var lv;
     let t = match lv.lv_type with 
       | Ctype t -> t
       | t -> WpLog.fatal "%s c type of pure logic type %a"
	   s !Ast_printer.d_logic_type t
     in
     let typ_logicvar = if ap then array_of t else pointed_of_arity arity t in 
     let obj_logicvar = Ctypes.object_of typ_logicvar in
     let tau_logicvar = tau_of_object obj_logicvar in      
     let var = L.fresh lv.lv_name (Acsl(tau_logicvar,Ctype typ_logicvar)) in
     debug "%s records %a" s F.pp_var var ; 
     let brefparam = {
       bref_var = var ;
       bref_formal = 
	 if ap then Faref arity else Fpref arity ;
     } in
     mem.formals <- Lmap.add lv brefparam mem.formals; var
       
 (* C variable translation *)
  let get_c_funvar mem arity vinfo ap =
    let s = "[get_funvar]" in
    try let x = (Xmap.find vinfo mem.vars).v_var in 
    debug "%s %a as %a already recorded" s !Ast_printer.d_var vinfo F.pp_var x;
    x
    with Not_found ->
      debug "%s %a not yet recorded" s !Ast_printer.d_var vinfo;
      let t = vinfo.vtype in 
      let typ_logicvar = if ap then array_of t else pointed_of_arity arity t in
      let obj_logicvar = Ctypes.object_of typ_logicvar in
      let tau_logicvar = tau_of_object obj_logicvar in      
      let var = L.fresh vinfo.vname (Acsl(tau_logicvar,Ctype typ_logicvar)) in
      let v_info = 
	{v_var = var ; v_arity = arity ;v_is_array = ap ; v_type = obj_logicvar}
      in
      debug "%s (%a,%d,%b,%a)" s F.pp_var var arity ap pp_object obj_logicvar ;
      mem.vars <- Xmap.add vinfo v_info mem.vars ; var

(* Variables translation*)
  let get_funvar  mem arity root ap =
    match root with 
      | Cvar x -> get_c_funvar mem arity x ap
      | Lvar lv -> get_logic_funvar mem arity lv ap
	  
(* [mk_path r m ] makes a path of root [r], 
   with memory [m], arity [0] and type of value. Used for all kind of 
   variable with path excepted by-reference parameter without C variable.*)
  let mk_path r m = 
    { p_root= r ; p_mem = m ; p_off=[] ; 
      p_arity=0 ; p_type=object_of_root r ; p_cvar = None ; }

  (* [mk_pref r m n opt_cv] makes the path of a pointer by-reference parameter 
     with root (r] memory [m], arity [n] and p_cvar [opt_cv].*)
  let mk_pref r m n opt_cv = 
     { p_root= r; p_mem = m ; p_off=[] ; p_arity=n ;
       p_type=object_of_root r ; p_cvar = opt_cv ;} 

  (* [mk_aref r m n opt_cv] makes the path of an array by-reference parameter
     with root [r] memory [m], arity [n] and p_cvar [opt_cv].*)
  let mk_aref  r m n opt_cv = 
    {p_root= r ; p_mem = m; p_off=[] ; p_arity=n ;
     p_type=object_array_of_root r ; p_cvar = opt_cv;}

  let cvar m vi =
    let r = Cvar vi in 
    match Variables_analysis.dispatch_cvar vi with 
      | Variables_analysis.Fvar ->
	  oracle 
	    "%a is a funvar @." !Ast_printer.d_var vi ;
	  Path (mk_path r m.mem)
      | Variables_analysis.Cvar when Crit.isHoare -> 
	  oracle 
	    "%a is a funvar @." !Ast_printer.d_var vi ;
	  Path (mk_path r m.mem)
      | Variables_analysis.Cvar -> 
	  oracle 
	  "%a is a memvar @." !Ast_printer.d_var vi ;
	  Mloc (M.cvar m.mem vi)
      | Variables_analysis.ARarg ->
	  oracle
	    "%a is a array refvar @." !Ast_printer.d_var vi;
	  if vi.vglob then M.global vi;
	  ARef (mk_path r m.mem)
      | Variables_analysis.PRarg -> 
	  oracle
	    "%a is a ptr refvar @." !Ast_printer.d_var vi ;
	  PRef (mk_path r m.mem)
      | Variables_analysis.PRpar n ->
	  oracle
	    "%a is a ptr ref param of arity %d @." !Ast_printer.d_var vi n;
	  PRpar (mk_pref r m.mem n None, n)
      | Variables_analysis.ARpar n ->
	  oracle
	    "%a is a array ref param of arity %d @." !Ast_printer.d_var vi n;
	  let (n0,_) = brackets_and_stars_typ vi.vtype in 
	  if vi.vglob then global vi;
	  ARpar (mk_aref r m.mem n None, n0)

      
  let lvar m lv x= 
    let r = Lvar lv in 
    match Variables_analysis.dispatch_lvar lv with 
      | Variables_analysis.Fvar ->
	  oracle 
	    "%a is a funvar @." !Ast_printer.d_logic_var lv ;
	  Path (mk_path r m.mem)
      | Variables_analysis.Cvar when Crit.isHoare -> 
	  oracle 
	    "%a is a funvar @." !Ast_printer.d_logic_var lv ;
	  Path (mk_path r m.mem)
      | Variables_analysis.Cvar -> 
	  oracle 
	  "%a is a memvar @." !Ast_printer.d_logic_var lv ;
	  Mloc (M.lvar m.mem lv x)
      | Variables_analysis.ARarg ->
	  oracle
	    "%a is a array refvar @." !Ast_printer.d_logic_var lv;
	  ARef (mk_path r m.mem)
      | Variables_analysis.PRarg -> 
	  oracle
	    "%a is a ptr refvar @." !Ast_printer.d_logic_var lv ;
	  PRef (mk_path r m.mem)
      | Variables_analysis.PRpar n ->
	  oracle
	    "%a is a ptr ref param of arity %d @." 
	    !Ast_printer.d_logic_var lv n;
	  PRpar (mk_pref r m.mem n (Some x), n)
      | Variables_analysis.ARpar n ->
	  oracle
	    "%a is a array ref param of arity %d @."
	    !Ast_printer.d_logic_var lv n;
	  let (n0,_) = brackets_and_stars_lv_typ lv.lv_type in 
	  ARpar (mk_aref r m.mem n (Some x), n0) 
	    
  let inner_loc loc = M.term_of_loc (mloc_of_loc loc)

  (* [add_index p i ty] makes the path of the l-value of the path (p] and 
     the index [i] with type [ty].*)
  let add_index p i ty = 
    { p with 
	p_off = p.p_off @ [Lookup.Oindex (i,ty)] ; 
	p_type = Some ty }
      
  let shift l ty i = 
    match l with 
      | Mloc l -> Mloc (M.shift l ty i)
      | PRef p -> Path (add_index p i ty)
      | ARef p -> ARef (add_index p i ty)
      | PRpar (p,0) | Path p ->
	  let loc = Lookup.mloc_of_path p in
	  Mloc (M.shift loc ty i)
      | PRpar  (p,n) -> PRpar (add_index p i ty,n)
      | ARpar (p,n) -> ARpar (add_index p i ty,n) 

  let index l ty i =
    match l with
      | Mloc l -> Mloc (M.index l ty i)
      | Path p   -> Path (add_index p i ty)
      | PRef p    -> PRef (add_index p i ty)
      | ARef p -> ARef (add_index p i ty) 
      | PRpar(p,n)-> PRpar (add_index p i ty,n)
      | ARpar (p,n) -> ARpar (add_index p i ty,n) 

  (*[add_field p finfo] makes the path of the l-value of the path [p]
    and the field access to [finfo].*)
  let add_field p finfo =
    { p with 
	p_off = p.p_off@[Lookup.Ofield finfo] ;
	p_type = Some (object_of finfo.ftype) }

  let field l finfo =
    match l with
      | Mloc l -> Mloc (M.field l finfo)
      | Path p -> Path(add_field p finfo)
      | PRef p -> PRef(add_field p finfo)
      | PRpar (p,n) -> PRpar(add_field p finfo,n)
      | ARef p -> ARef(add_field p finfo)
      | ARpar (p,n) -> ARpar(add_field p finfo,n)

  let startof l ty =
    match l with
      | ARef p -> debug "[startof] %a" pp_path p ; ARef p
      | ARpar (p,n) -> ARpar(p,n)
      | _ -> Mloc (M.startof (mloc_of_loc l) ty)

  (* ------------------------------------------------------------------------ *)
  (* --- Pointers                                                         --- *)
  (* ------------------------------------------------------------------------ *)

  let cast_loc_to_loc t1 t2 = function
    | Mloc l -> Mloc (M.cast_loc_to_loc t1 t2 l)
    | ARef p -> debug "[cast_loc_to_loc %a from %a to %a]"
	pp_path p !Ast_printer.d_type t1 !Ast_printer.d_type t2;
	  index (ARef p) (object_of t2) F.i_zero	  
    | Path _ | PRef _ | PRpar _ | ARpar _ ->
	WpLog.not_yet_implemented
	  "Cast from %a to %a of over a logical-variable (try -wp-no-logicvar)"
	  !Ast_printer.d_type t1 !Ast_printer.d_type t2

  (* ------------------------------------------------------------------------ *)
  (* --- Load                                                             --- *)
  (* ------------------------------------------------------------------------ *)
  (* [fun_load m p ap] returns the load value of the path [p] in the 
     memory state [m] according to the test of being an 
     array by-reference [ap].*)
  let fun_load m p ap =
    let xv = F.var(get_funvar m p.p_arity p.p_root ap) in
      match p.p_off, p.p_type with
        | [],Some ty -> value_of_logic ty xv
        | off,Some ty  -> 
	    let vload = Lookup.access xv off in 
	    value_of_logic ty  vload
	| _ , None -> Wp_parameters.fatal 
                 "[fun_load] offset none null for pure logic type"

  let load m cv l =
    match l with
      | Mloc l -> value_of_mvalue (M.load m.mem cv l)
      | PRef p | Path p -> fun_load m p false
      | ARef p -> fun_load m p true
      | PRpar (p,0) -> fun_load m p false 
      | ARpar (p,0) -> fun_load m p true
      | ARpar (p,n) -> V_pointer(cv,ARpar(pointed_of_path p,n-1))
      | PRpar (p,n) -> V_pointer(cv,PRpar (pointed_of_path p,n-1)) 
      

  (* ------------------------------------------------------------------------ *)
  (* --- Zones                                                            --- *)
  (* ------------------------------------------------------------------------ *)

  let massigned = function
    | F.Aloc(te,l) -> 
	debug "massigned case loc : %a" pp_loc l;
	F.Aloc(te,mloc_of_loc l)
    | F.Arange(te,l,rg) -> 
	debug "massigned case range : %a becomes %a" 
	  pp_loc l M.pp_loc (mloc_of_loc l);
	F.Arange(te,mloc_of_loc l,rg)

  type m_dzone = M.m_dzone
  type dzone = M.dzone
  let tau_of_dzone = M.tau_of_dzone

  let dzone_assigned m z = M.dzone_assigned m.mem (massigned z)
  let dzone_subset = M.dzone_subset
  let dzone_union = M.dzone_union
  let dzone_empty = M.dzone_empty

  let effect_supported = M.effect_supported

  (* ------------------------------------------------------------------------ *)
  (* ---  Pointers Logic Properties                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let base_address m l = Mloc (M.base_address m.mem (mloc_of_loc l))
  let block_length m l = M.block_length m.mem (mloc_of_loc l)
  let valid m z = M.valid m.mem (massigned z)
  let separated m z1 z2 = M.separated m.mem (massigned z1) (massigned z2)

  (* ------------------------------------------------------------------------ *)
  (* ---  By Reference Parameters of User-definitions                     --- *)
  (* ------------------------------------------------------------------------ *)

  let pp_formal_simple fmt = function 
    | Fpref n -> Format.fprintf fmt "Fpref %d" n  
    | Faref n -> Format.fprintf fmt "Faref %d" n 

  let pp_formal (fmt:Format.formatter) (formal,lv) =
    match formal with
      | Fpref n -> 
	  Format.fprintf fmt "%s%s" (String.make n '*') lv.lv_name
      | Faref n ->
	  Format.fprintf fmt "%s%t"
	    lv.lv_name
	    (fun fmt -> for i=1 to n do Format.pp_print_string fmt "[]" done)


  let userdef_ref_has_cvar (lv : logic_var) : bool = 
    Variables_analysis.is_user_formal_in_builtin lv

  (* [userdef_is_ref_param lv] tests if a pure logic variable [lv] is 
     a by reference formal parameter of a user definition. *)
  let userdef_is_ref_param lv =
    match Variables_analysis.dispatch_lvar lv with 
      | Variables_analysis.Fvar | Variables_analysis.Cvar 
      | Variables_analysis.ARarg | Variables_analysis.PRarg -> false
      | Variables_analysis.PRpar _ | Variables_analysis.ARpar _  -> true

  (* [userdef_ref_signature mem] returns the part of the signature 
     of a user definition corresponding to its by-reference parameters.*)
  let userdef_ref_signature mem : ( F.var * logic_var * formal ) list =
    let s = "[userdef_ref_signature]" in
    debug "%s" s;
    Lmap.fold
      (fun lv param signature ->
	 debug "%s of %a" s !Ast_printer.d_logic_var lv ; 
	 (param.bref_var , lv , param.bref_formal) :: signature    
      ) mem.formals []
      
      
  let userdef_ref_apply m fml loc =
    debug "[userdef_ref_apply] calls with formal %a and loc %a"
      pp_formal_simple fml pp_loc loc ; 
    begin
      match fml, loc with 
	| Fpref 1, PRef p -> fun_load m p false
	| Faref 1, ARef p -> fun_load m p true
	| Fpref k, PRpar (p,r) -> 
	    let n = k-r-1 in
	    if n = 0 then fun_load m p false 
	    else 
	      (
		match p.p_type with 
		  | None -> Wp_parameters.fatal 
		      "[userdef_ref_apply] pure type"
		  | Some ty ->
		      let obj = object_of_n_pointed n ty in
		      V_pointer(obj, PRpar(pointed_of_n_path n p,n))) 
	| Faref k, ARpar(p,r) ->
	    let n = k-r-1 in 
	    if n = 0 then fun_load m p true 
	    else 
	      (
		match p.p_type with 
		  | None -> Wp_parameters.fatal 
		      "[userdef_ref_apply] pure type"
		  | Some ty ->
		let obj = object_of_n_pointed n ty in
	       V_pointer(obj, ARpar(pointed_of_n_path n p,n)))
	| f , l -> WpLog.fatal
	    "[userdef_ref_apply] calls with fml:%a and loc:%a"
	    pp_formal_simple f pp_loc l
    end

  (* ------------------------------------------------------------------------ *)
  (* ---  Functional Closure                                              --- *)
  (* ------------------------------------------------------------------------ *)

  type closure =
    | Fclos of int * bool * Cil_types.varinfo (* arity, isArray *)
    | Mclos of M.closure

  let pp_closure fmt = function
    | Mclos cl -> M.pp_closure fmt cl
    | Fclos(k,ap,vinfo) -> 
	if ap then
	  (* array *)
	  Format.fprintf fmt "value of %s%t"
	    vinfo.vname (* C-original name *)
	    (fun fmt -> for i=1 to k do Format.pp_print_string fmt "[]" done)
	else
	  (* ref. *)
	  Format.fprintf fmt "value of %s%s" 
	    (String.make k '*') vinfo.vname

  let userdef_mem_signature m =
    Xmap.fold
      (fun v vi fs -> (vi.v_var,Fclos(vi.v_arity,vi.v_is_array,v))::fs)
      m.vars
      (List.map (fun (y,c) -> y,Mclos c) (M.userdef_mem_signature m.mem))

  let userdef_mem_apply m = function
    | Fclos(k,ap,vinfo) -> F.var (get_c_funvar m k vinfo ap)
    | Mclos  mc -> M.userdef_mem_apply m.mem mc

  (* ------------------------------------------------------------------------ *)
  (* ---  Labels & Quantification                                         --- *)
  (* ------------------------------------------------------------------------ *)

  let update ~(at:mem) ~(here:mem) p =
    Xmap.fold
      (fun v vi p ->
         let x_here = get_c_funvar here vi.v_arity v vi.v_is_array in
         L.subst vi.v_var (F.var x_here) p)
      at.vars
      (M.update at.mem here.mem p)

  let quantify m p =
    let xs = Xmap.fold (fun _ vi xs -> vi.v_var::xs) m.vars [] in
       L.forall xs (M.quantify m.mem p)

  (* ------------------------------------------------------------------------ *)
  (* ---  Assignments                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  (*[update_offset phi current offs] applies the l-value made 
    from the l-value [current] and the list of offset [offs] to the 
    hole-term [phi]. *)
  let rec update_offset phi current offs =
    match offs with
      | [] -> phi current
      | off::m ->
          let v = Lookup.access_poffset current off in
          let r = update_offset phi v m in
          begin
            match off with
              | Lookup.Ofield f ->
                  F.wrap (F.upd_field (F.unwrap current) f r)
              | Lookup.Oindex (i,_obj) ->
                  F.wrap (F.upd_index (F.unwrap current) i r)
          end
	    
  (* [store m p v ap wp] firts, stores in the memory state [m] 
     the value [v] to the path [p] according the by-reference array test [ap] 
     and returns the property [wp] in this new memory  state.*)
  let store m p v ap wp =
    let x = get_funvar m p.p_arity p.p_root ap in
    let v' = update_offset (fun _ -> logic_of_value v) (F.var x) p.p_off in
    L.subst x v' wp

  let subst_lval m obj loc v wp =
    match loc with
      | Mloc l -> M.subst_lval m.mem obj l (mvalue_of_value v) wp
      | Path ph | PRef ph | PRpar (ph,_)| ARef ph ->
	  store m ph v false wp
      | ARpar (ph,_) -> store m ph v true wp

  (* ------------------------------------------------------------------------ *)
  (* ---  Zone Havoc                                                      --- *)
  (* ------------------------------------------------------------------------ *)

  let subst_havoc (m:mem) = function

    | F.Aloc(_,(Path p| PRef p | PRpar(p,_)))
	when p.p_off=[] ->
        let x = get_funvar m p.p_arity p.p_root false in
        let v = L.fresh (F.basename_of_var x) (F.kind_of_var x) in
        [F.Fresh v;F.Update(x,fun _ -> F.var v)]
    | F.Aloc(_, (ARpar(p,_)| ARef p)) when p.p_off=[] ->
        let x = get_funvar m p.p_arity p.p_root true in
        let v = L.fresh (F.basename_of_var x) (F.kind_of_var x) in
        [F.Fresh v;F.Update(x,fun _ -> F.var v)]

    | F.Aloc(_,(Path p| PRef p | PRpar(p,_))) ->
        let x = get_funvar m p.p_arity p.p_root false in
        let v = 
	  match p.p_type with
	    | None -> Wp_parameters.fatal "[subst_havoc] pure logic var"
	    | Some ty ->
		L.fresh "v" (Model (tau_of_object ty))
	in
        let newterm (sigma : (F.var * F.var) list ) : F.abstract =
          F.wrap ( update_offset
                     (fun _ -> F.var v)
                     (L.apply sigma (F.var x)) p.p_off )
        in
        [F.Fresh v;F.Update(x,newterm)]

    | F.Aloc(_, (ARpar(p,_)| ARef p)) ->
      let x = get_funvar m p.p_arity p.p_root true in
      let v = 
	match p.p_type with 
	  | None -> Wp_parameters.fatal "[subst_havoc] of pure logic var"
	  | Some ty -> L.fresh "v" (Model (tau_of_object ty))
      in
      let newterm (sigma : (F.var * F.var) list ) : F.abstract =
        F.wrap ( update_offset
                   (fun _ -> F.var v)
                   (L.apply sigma (F.var x)) p.p_off )
      in
      [F.Fresh v;F.Update(x,newterm)]
	
    | F.Arange(_,(Path p| PRef p | PRpar(p,_)),rg) ->
        let x = get_funvar m p.p_arity p.p_root false in
        let upd_range rg = fun array ->
          F.wrap (F.set_range_index (F.unwrap array) rg)
        in
        let newterm (sigma :(F.var * F.var) list ) : F.abstract =
          F.wrap (update_offset (upd_range rg) (L.apply sigma (F.var x)) 
		    p.p_off)
        in
        [F.Update(x,newterm)]
	  
    | F.Arange(_, (ARpar(p,_)| ARef p),rg) ->
        let x = get_funvar m p.p_arity p.p_root true in
        let upd_range rg = fun array ->
          F.wrap (F.set_range_index (F.unwrap array) rg)
        in
        let newterm (sigma :(F.var * F.var) list ) : F.abstract =
          F.wrap (update_offset (upd_range rg) (L.apply sigma (F.var x))
		    p.p_off)
        in
        [F.Update(x,newterm)]
	  
    | F.Aloc(te,Mloc l) -> 
	M.subst_havoc m.mem (F.Aloc(te,l))

    | F.Arange(te,Mloc l,rg) ->
        M.subst_havoc m.mem (F.Arange(te,l,rg))
	  
  let assigns_goal m1 reg m2 =
    (* Not very usefull, since assigns_supported = false !! *)
    let region =
      List.map
	(function
	   | F.Aloc(_,Path _) | F.Arange(_,Path _,_) 
	   | F.Aloc(_,PRef _) | F.Arange(_,PRef _,_)
	   | F.Aloc(_,PRpar _) | F.Arange(_,PRpar _,_) 
	   | F.Aloc(_,ARef _) | F.Arange(_,ARef _,_)
	   | F.Aloc(_,ARpar _) | F.Arange(_,ARpar _,_) ->
	       WpLog.fatal 
		 "Proof of assigns-clause with hoare-region"
	   | F.Aloc(te,Mloc l) -> F.Aloc(te,l)
	   | F.Arange(te,Mloc l,rg) -> F.Arange(te,l,rg))
	reg
    in
    M.assigns_goal m1.mem region m2.mem

  let assigns_supported = false

  (* ------------------------------------------------------------------------ *)
  (* ---  Local Scope                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  let local_scope m lx b p =
    let xs = List.filter Variables_analysis.is_to_scope lx in
    M.local_scope m.mem xs b p

end


	  
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
