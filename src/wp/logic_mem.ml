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
(* --- Functional Variable Memory Model Functor                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Formula
open Ctypes
module WpMain = Wp_parameters

module Create
  (M:Mwp.S)
  =
struct

  module OFFSET =
  struct
    module F = M.F

    type base = {
      c_mem : M.mem ; (* only used for Runtime ; memory at allocation time *)
      c_var : varinfo ;
      c_kind : Formula.kind ; (* Guarded type of FOL-variable *)
    }

    (* refs for parameters in logic def. *)
    type refkind = Pref | Parray of int * arrayinfo 

    type logicref = {
      lp_var : logic_var ;  (* formal parameter passed by reference *)
      lp_kind : Formula.kind ; (* Guarded type of referenced FOL-value *)
      lp_obj : c_object ;   (* type of pointed value *)
      lp_ref : refkind ;     (* kind of reference *)
      lp_ptr : F.abstract ; (* value of pointer passed by reference *)
    }

    type root =
      | Cbase of base
      | Lpref of logicref

    type offset =
      | Ofield of fieldinfo
      | Oindex of c_object * F.integer

    let pp_root fmt = function
      | Cbase v -> Varinfo.pretty fmt v.c_var
      | Lpref p -> Logic_var.pretty fmt p.lp_var

    let pp_offset fmt = function
      | Ofield f -> Format.fprintf fmt ".%a" Fieldinfo.pretty f
      | Oindex(_,k) -> Format.fprintf fmt "[%a]" F.pp_term k

    let pretty fmt ofs = List.iter (pp_offset fmt) ofs

    let eq_root r1 r2 = match r1,r2 with
      | Cbase {c_var=x} , Cbase {c_var=y} -> Varinfo.equal x y
      | Lpref {lp_var=x} , Lpref {lp_var=y} -> Logic_var.equal x y
      | _ -> false

    (* Access with offset *)
    let offset data = function
      | Ofield f -> F.acc_field (F.unwrap data) f
      | Oindex(_,k) -> F.acc_index (F.unwrap data) k
    let access = List.fold_left offset

    (* Update at offset *)
    let rec update data offset value =
      match offset with
	| [] -> value
	| Ofield f :: off -> 
	    let r = F.unwrap data in
	    let d = update (F.acc_field r f) off value in
	    F.wrap (F.upd_field r f d)
	| Oindex(_,k) :: off ->
	    let a = F.unwrap data in
	    let d = update (F.acc_index a k) off value in
	    F.wrap (F.upd_index a k d)

    (* Convert offset to memory loc *)
    let mloc_offset mloc = function
      | Ofield f -> M.field mloc f
      | Oindex(t,k) -> M.shift mloc t k
    let mloc_root = function
      | Cbase r -> M.cvar r.c_mem r.c_var
      | Lpref p -> M.loc_of_term p.lp_obj p.lp_ptr
    let mloc_shift l (ty,k) = M.shift l ty k
    let mloc_path r ofs = List.fold_left mloc_offset (mloc_root r) ofs
    let mloc_index r sht = List.fold_left mloc_shift (mloc_root r) sht

    (* Convert array shifts to indices *)
    let indices dim sht =
      let ks = Array.create (succ dim) F.i_zero in
      List.iter
	(fun (obj,k) ->
	   match Ctypes.dimension_of_object obj with
	     | None -> ks.(0) <- F.i_add ks.(0) k
	     | Some(d,s) ->
		 let r = dim-d in 
		 if 0 <= r then
		   ks.(r) <- F.i_add ks.(r) k
		 else if Int64.compare s Int64.one = 0 then
		   ks.(0) <- F.i_add ks.(0) k
		 else
		   ks.(0) <- F.i_add ks.(0) (F.i_mult (F.e_int64 s) k)
	) sht ; ks

    let array_index (a:F.abstract) (ks:F.integer array) =
      Array.fold_left (fun a k -> F.acc_index (F.unwrap a) k) a ks

    let array_update (a:F.abstract) (ks:F.integer array) (v:F.abstract) =
      let rec update a v = function
	| [] -> v
	| k::ks ->
	    let a = F.unwrap a in
	    let d = update (F.acc_index a k) v ks in
	    F.wrap (F.upd_index a k d)
      in update a v (Array.to_list ks)

  end

  module LOC =
  struct

    open OFFSET

    module F = M.F
    module A = M.A
    module R = M.R

    type loc =
      | Mloc of M.loc
      | Floc of root * offset list 
      | Fref of root (* A variable accessed by reference *)
      | Farray of root * int * (c_object * F.integer) list (* An array of tau values with indices *)
      | Frefarray of root * int (* A reference to an array with dimensions *)

    let tau_of_loc = M.tau_of_loc

    let mloc_of_loc = function
      | Mloc mloc -> mloc
      | Floc (r,ofs) -> OFFSET.mloc_path r ofs
      | Farray(r,_,sht) -> OFFSET.mloc_index r sht
      | Fref r | Frefarray(r,_) -> OFFSET.mloc_root r

    let term_of_loc loc = M.term_of_loc (mloc_of_loc loc)
    let loc_of_term obj ptr = Mloc (M.loc_of_term obj ptr)

    let pp_loc fmt = function
      | Mloc mloc -> M.pp_loc fmt mloc
      | Floc(r,ofs) -> pp_root fmt r ; OFFSET.pretty fmt ofs
      | Fref r -> Format.fprintf fmt "ref(%a)" pp_root r
      | Frefarray(r,d) -> Format.fprintf fmt "ref(%a,%d)" pp_root r d
      | Farray(r,d,sht) -> 
	  Format.fprintf fmt "array(%a:%d)" pp_root r d ;
	  List.iter (fun (_,k) -> Format.fprintf fmt "[%a]" F.pp_term k) sht

    let null = Mloc M.null
    let is_null = function Mloc l -> M.is_null l | _ -> F.e_false
      
    let m_arith f p q = f (mloc_of_loc p) (mloc_of_loc q)
    let minus_loc = m_arith M.minus_loc
    let le_loc = m_arith M.le_loc
    let lt_loc = m_arith M.lt_loc
    let le_loc_bool = m_arith M.le_loc_bool
    let lt_loc_bool = m_arith M.lt_loc_bool

    let equal_loc p q =
      match p , q with
	| Mloc p , Mloc q -> M.equal_loc p q
	| Fref r1 , Fref r2 -> 
	    if eq_root r1 r2 then F.p_true else F.p_false
	| ( Floc(r1,_) | Farray(r1,_,_) | Frefarray(r1,_) ) ,
	    ( Floc(r2,_) | Farray(r2,_,_) | Frefarray(r2,_) )
	      when eq_root r1 r2 ->
	    M.equal_loc (mloc_of_loc p) (mloc_of_loc q)
	| _ -> F.p_false

    let equal_loc_bool p q =
      match p , q with
	| Mloc p , Mloc q -> M.equal_loc_bool p q
	| Fref r1 , Fref r2 -> 
	    if eq_root r1 r2 then F.e_true else F.e_false
	| ( Floc(r1,_) | Farray(r1,_,_) | Frefarray(r1,_) ) ,
	    ( Floc(r2,_) | Farray(r2,_,_) | Frefarray(r2,_) )
	      when eq_root r1 r2 ->
	    M.equal_loc_bool (mloc_of_loc p) (mloc_of_loc q)
	| _ -> F.e_false

    let cast_int_to_loc ci k ty = Mloc (M.cast_int_to_loc ci k ty)
    let cast_loc_to_int ty l ci = M.cast_loc_to_int ty (mloc_of_loc l) ci

  end

  include Datalib.Cvalues(LOC)
  module L = M.L
  open OFFSET
  open LOC
 
  (* ------------------------------------------------------------------------ *)
  (* ---  Values Coercion                                                 --- *)
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
    | V_pointer (_,_) -> WpMain.fatal "logical pointer"

  (* ------------------------------------------------------------------------ *)
  (* ---  Memory                                                          --- *)
  (* ------------------------------------------------------------------------ *)

  type mem = {
    heap : M.mem ;
    mutable vars : F.var Varinfo.Map.t ;
    mutable refs : (OFFSET.refkind * F.var) Logic_var.Map.t ;
  }

  let mem () = { 
    heap = M.mem () ; 
    vars = Varinfo.Map.empty ; 
    refs = Logic_var.Map.empty ;
  }

  let pp_mem fmt m =
    Varinfo.Map.iter
      (fun v x -> Format.fprintf fmt " * %a => %a@\n" Varinfo.pretty v F.pp_var x)
      m.vars ;
    M.pp_mem fmt m.heap

  let base mem r =
    try Varinfo.Map.find r.c_var mem.vars
    with Not_found ->
      let x = L.fresh r.c_var.vorig_name r.c_kind in
      mem.vars <- Varinfo.Map.add r.c_var x mem.vars ; x

  let pref mem p =
    try snd (Logic_var.Map.find p.lp_var mem.refs)
    with Not_found ->
      let x = L.fresh p.lp_var.lv_name p.lp_kind in
      mem.refs <- Logic_var.Map.add p.lp_var (p.lp_ref,x) mem.refs ; x
	
  let root mem = function
    | Cbase r -> base mem r
    | Lpref p -> pref mem p

  let reroot mem v x0 =
    try Varinfo.Map.find v mem.vars
    with Not_found ->
      let x1 = L.fresh (F.basename_of_var x0) (F.kind_of_var x0) in
      mem.vars <- Varinfo.Map.add v x1 mem.vars ; x1

  (* ------------------------------------------------------------------------ *)
  (* ---  CIL Utilities                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let logic_pointed = function
    | Ctype typ -> Cil.typeOf_pointed typ
    | _ -> WpMain.fatal "pointed of logic-type"

  let rec logic_array ctype n =
    if n > 0 then 
      let carr = TArray(ctype,None,Cil.empty_size_cache(),[]) in
      logic_array carr (pred n)
    else Ctype ctype

  (* ------------------------------------------------------------------------ *)
  (* ---  Memory Model                                                    --- *)
  (* ------------------------------------------------------------------------ *)

  let cvar m vi =
    match VarUsage.of_cvar vi with
      | VarUsage.NotUsed | VarUsage.ByAddress -> Mloc (M.cvar m.heap vi)
      | VarUsage.ByValue -> 
	  let tau = tau_of_object (Ctypes.object_of vi.vtype) in
	  let kind = Formula.Acsl( tau , Ctype vi.vtype ) in
	  Floc( Cbase { c_mem=m.heap ; c_var=vi ; c_kind=kind } ,[] )
      | VarUsage.ByReference ->
	  let tref = Cil.typeOf_pointed vi.vtype in
	  let tau = tau_of_object (Ctypes.object_of tref) in
	  let kind = Formula.Acsl( tau , Ctype tref ) in
	  Fref ( Cbase { c_mem=m.heap ; c_var=vi ; c_kind=kind } )
      | VarUsage.ByArray ds ->
	  let te = VarUsage.type_of_cells vi.vtype in
	  let dim = List.length ds in
	  let tau = tau_of_object_array (Ctypes.object_of te) (succ dim) in
	  let arr = logic_array te (succ dim) in
	  let kind = Formula.Acsl(tau, arr) in
	  Farray( Cbase { c_mem=m.heap ; c_var=vi ; c_kind=kind } , dim , [] )
      | VarUsage.ByRefArray ds ->
	  let tref = Cil.typeOf_pointed vi.vtype in
	  let obj = Ctypes.object_of (VarUsage.type_of_cells tref) in
	  let dim = List.length ds in
	  let tau = tau_of_object_array obj (succ dim) in
	  let arr = logic_array tref (succ dim) in
	  let kind = Formula.Acsl(tau, arr) in
	  Frefarray( Cbase { c_mem=m.heap ; c_var=vi ; c_kind=kind } , dim )

  let lvar _ lv x =
    match VarUsage.of_lvar lv with
      | VarUsage.NotUsed | VarUsage.ByAddress | VarUsage.ByArray _ | VarUsage.ByValue ->
	  WpMain.fatal "unexpected address of logic-variable %a" Logic_var.pretty lv
      | VarUsage.ByReference -> 
	  let te = logic_pointed lv.lv_type in
	  let obj = Ctypes.object_of te in
	  let kind = Formula.Acsl(tau_of_object obj,Ctype te) in
	  Fref ( Lpref { lp_var=lv ; lp_obj=obj ; lp_ptr=F.var x ; 
			 lp_kind=kind ; lp_ref=Pref } )
      | VarUsage.ByRefArray ds ->
	  let tref = logic_pointed lv.lv_type in
	  let tobj = Ctypes.object_of tref in
	  let tcell = VarUsage.type_of_cells tref in
	  let cell = Ctypes.object_of tcell in
	  let dim = List.length ds in
	  let tau = tau_of_object_array cell (succ dim) in
	  let arr = logic_array tcell (succ dim) in
	  let kind = Formula.Acsl(tau,arr) in
	  let ainfo = { arr_element=tref ; arr_flat=None } in
	  Frefarray ( Lpref { lp_var=lv ; lp_obj=tobj ; lp_ptr=F.var x ;
			      lp_kind=kind ; lp_ref=Parray(dim,ainfo) } , dim )

  let shift loc obj k =
    match loc with
      | Mloc l -> Mloc (M.shift l obj k)
      | Floc(r,ofs) -> Floc(r,ofs @ [Oindex(obj,k)])
      | Farray(r,d,sht) -> Farray(r,d,sht@[(obj,k)])
      | Fref _ | Frefarray _ -> WpMain.fatal "Shift on reference %a" pp_loc loc

  let index = shift

  let field loc f =
    match loc with
      | Mloc l -> Mloc (M.field l f)
      | Floc(r,ofs) -> Floc(r,ofs@[Ofield f])
      | Farray _ -> WpMain.fatal "Field on array %a" pp_loc loc
      | Fref _ | Frefarray _ -> WpMain.fatal "Field on reference %a" pp_loc loc

  let load mem obj = function
    | Mloc l -> 
	value_of_mvalue (M.load mem.heap obj l)
    | Floc(r,ofs) -> 
	let v = F.var (root mem r) in
	value_of_logic obj (OFFSET.access v ofs)
    | Farray(r,d,sht) -> 
	let v = F.var (root mem r) in
	let ks = OFFSET.indices d sht in
	value_of_logic obj (OFFSET.array_index v ks)
    | Fref r -> 
	V_pointer(obj,Floc(r,[]))
    | Frefarray(r,d) -> 
	V_pointer(obj,Farray(r,d,[]))

  (* ------------------------------------------------------------------------ *)
  (* ---  Pointer Casts                                                   --- *)
  (* ------------------------------------------------------------------------ *)

  let startof loc ty = 
    match loc with
      | Mloc l -> Mloc (M.startof l ty)
      | floc -> floc

  let cast_loc_to_loc t1 t2 = function
    | Mloc l -> Mloc (M.cast_loc_to_loc t1 t2 l)
    | floc -> floc

  (* ------------------------------------------------------------------------ *)
  (* ---  Assignments                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  let subst_lval mem obj loc value wp =
    match loc with
      | Mloc l -> 
	  M.subst_lval mem.heap obj l (mvalue_of_value value) wp
      | Floc(r,ofs) ->
	  let x = root mem r in
	  let d = logic_of_value value in
	  let v = OFFSET.update (F.var x) ofs d in
	  L.subst x v wp
      | Farray(r,d,sht) -> 
	  let x = root mem r in
	  let ks = OFFSET.indices d sht in
	  let d = logic_of_value value in
	  let v = OFFSET.array_update (F.var x) ks d in
	  L.subst x v wp
      | Fref _ | Frefarray _ -> 
	  WpMain.fatal "assignment to by-reference %a" LOC.pp_loc loc

  (* ------------------------------------------------------------------------ *)
  (* ---  Havoc                                                           --- *)
  (* ------------------------------------------------------------------------ *)

  type delta =
    | Dfield of fieldinfo
    | Dindex of F.integer

  let rec update_range (s:F.abstract) (ds:delta list) rg =
    match ds with
      | [] ->
	  let a = F.unwrap s in
	  F.wrap (F.set_range_index a rg)
      | Dfield f :: ds ->
	  let r = F.unwrap s in
	  let v = update_range (F.acc_field r f) ds rg in
	  F.wrap (F.upd_field r f v)
      | Dindex k :: ds ->
	  let a = F.unwrap s in
	  let v = update_range (F.acc_index a k) ds rg in
	  F.wrap (F.upd_index a k v)

  let delta_offset ofs = 
    List.map (function Oindex (_,k) -> Dindex k | Ofield f -> Dfield f) ofs

  let delta_index dim sht =
    let ks = OFFSET.indices dim sht in
    let ks = Array.sub ks 0 (Array.length ks - 1) in
    Array.to_list (Array.map (fun k -> Dindex k) ks)

  let subst_havoc mem = function
    | F.Aloc(te,Mloc l) -> M.subst_havoc mem.heap (F.Aloc(te,l))
    | F.Arange(te,Mloc l,rg) -> M.subst_havoc mem.heap (F.Arange(te,l,rg))
    | F.Aloc(_,((Fref _|Frefarray _) as loc)) 
    | F.Arange(_,((Fref _|Frefarray _) as loc),_) ->
	WpMain.fatal "assignment to by-reference %a" LOC.pp_loc loc

    | F.Aloc(te,Floc(r,ofs)) ->
	let x = root mem r in
	let v = L.fresh "v" (Model (tau_of_object te)) in
	let newterm sigma =
	  let a = L.apply sigma (F.var x) in
	  OFFSET.update a ofs (F.var v) 
	in
	[F.Fresh v ; F.Update(x,newterm)]

    | F.Aloc(te,Farray(r,dim,sht)) ->
	let x = root mem r in
	let v = L.fresh "v" (Model (tau_of_object te)) in
	let ks = OFFSET.indices dim sht in 
	let newterm sigma =
	  let a = L.apply sigma (F.var x) in
	  OFFSET.array_update a ks (F.var v) 
	in
	[F.Fresh v ; F.Update(x,newterm)]

    | F.Arange(_,Floc(r,ofs),rg) ->
	let x = root mem r in
	let newterm sigma =
	  let a = L.apply sigma (F.var x) in
	  update_range a (delta_offset ofs) rg
	in
	[F.Update(x,newterm)]

    | F.Arange(_,Farray(r,dim,sht),rg) ->
	let x = root mem r in
	let newterm sigma =
	  let a = L.apply sigma (F.var x) in
	  update_range a (delta_index dim sht) rg 
	in
	[F.Update(x,newterm)]

  (* ------------------------------------------------------------------------ *)
  (* --- Zones                                                            --- *)
  (* ------------------------------------------------------------------------ *)

  let massigned = function
    | F.Aloc(te,l) -> F.Aloc(te,mloc_of_loc l)
    | F.Arange(te,l,rg) -> F.Arange(te,mloc_of_loc l,rg)

  type m_dzone = M.m_dzone
  type dzone = M.dzone
  let tau_of_dzone = M.tau_of_dzone

  let dzone_assigned m z = M.dzone_assigned m.heap (massigned z)
  let dzone_subset = M.dzone_subset
  let dzone_union = M.dzone_union
  let dzone_empty = M.dzone_empty

  let effect_supported = M.effect_supported

  (* ------------------------------------------------------------------------ *)
  (* ---  Pointers Logic Properties                                       --- *)
  (* ------------------------g------------------------------------------------ *)

  let base_address m l = Mloc (M.base_address m.heap (mloc_of_loc l))
  let block_length m l = M.block_length m.heap (mloc_of_loc l)
  let valid m z = M.valid m.heap (massigned z)
  let l_rooted = function 
    | Mloc _ -> None
    | Floc(r,_) | Farray(r,_,_) | Fref r | Frefarray(r,_) -> Some r
  let a_rooted = function F.Aloc(_,l) | F.Arange(_,l,_) -> l_rooted l
  let separated m z1 z2 = match z1,z2 with
    | F.Aloc(_,(Fref r1 | Frefarray(r1,_))),
      F.Aloc(_,(Fref r2 | Frefarray(r2,_))) -> 
	if eq_root r1 r2 then F.p_false else F.p_true
    | _ -> 
	match a_rooted z1 , a_rooted z2 with
	  | Some r1 , Some r2 when not (eq_root r1 r2) -> F.p_true
	  | _ -> M.separated m.heap (massigned z1) (massigned z2)
	      
  (* ------------------------------------------------------------------------ *)
  (* ---  Logic Formal Parameters Passed By Reference                     --- *)
  (* ------------------------------------------------------------------------ *)

  type formal = OFFSET.refkind

  let pp_kind fmt = function
    | Pref -> Format.fprintf fmt "ref"
    | Parray(d,_) -> Format.fprintf fmt "%d-array" d

  let pp_formal fmt (formal,lv) = match formal with
    | Pref -> Format.fprintf fmt "*%a" Logic_var.pretty lv
    | Parray(d,_) -> 
	Logic_var.pretty fmt lv ;
	for _i=0 to d do Format.pp_print_string fmt "[]" done

  let userdef_is_ref_param lv =
    match VarUsage.of_lvar lv with
      | VarUsage.NotUsed | VarUsage.ByValue 
      | VarUsage.ByAddress | VarUsage.ByArray _ -> false
      | VarUsage.ByReference | VarUsage.ByRefArray _ -> true

  let userdef_ref_has_cvar = VarUsage.validated_lvar

  let userdef_ref_signature mem : (F.var * logic_var * formal) list =
    Logic_var.Map.fold
      (fun lv (formal,x) signature ->
	 (x,lv,formal)::signature
      ) mem.refs []

  let userdef_ref_apply mem formal obj loc =
    match formal , loc with
      | OFFSET.Pref , _ -> 
	  load mem obj loc
      | OFFSET.Parray(d1,arr) , Farray( r , d2 , [] ) when d1=d2 -> 
	  V_array(arr,F.var (root mem r))
      | _ -> 
	  WpMain.fatal "[Logic] Not yet implemented %a of %a" 
	    pp_kind formal pp_loc loc

  (* ------------------------------------------------------------------------ *)
  (* ---  Logical Closures                                                --- *)
  (* ------------------------------------------------------------------------ *)

  type closure = 
    | Fclosure of varinfo
    | Mclosure of M.closure

  let pp_closure fmt = function
    | Fclosure x -> Varinfo.pretty fmt x
    | Mclosure cc -> M.pp_closure fmt cc

  let userdef_mem_signature m =
    Varinfo.Map.fold
      (fun v x s -> (x,Fclosure v) :: s) m.vars 
      (List.map 
	 (fun (y,cc) -> y,Mclosure cc) 
	 (M.userdef_mem_signature m.heap))

  let userdef_mem_apply m = function
    | Fclosure v -> 
	let tau = tau_of_object (Ctypes.object_of v.vtype) in
	let kind = Acsl( tau , Ctype v.vtype ) in
	F.var (base m { c_mem=m.heap ; c_var=v ; c_kind=kind })
    | Mclosure cc ->
	M.userdef_mem_apply m.heap cc

  (* ------------------------------------------------------------------------ *)
  (* ---  Labels & Quantification                                         --- *)
  (* ------------------------------------------------------------------------ *)

  let update ~(at:mem) ~(here:mem) p =
    Varinfo.Map.fold
      (fun v x_at p ->
	 let x_here = reroot here v x_at in
	 L.subst x_at (F.var x_here) p)
      at.vars
      (M.update ~at:at.heap ~here:here.heap p)

  let quantify m p =
    let xs = Varinfo.Map.fold (fun _ x xs -> x::xs) m.vars [] in
    L.forall xs (M.quantify m.heap p)

  (* ------------------------------------------------------------------------ *)
  (* ---  Assigns                                                         --- *)
  (* ------------------------------------------------------------------------ *)

  let assigns_supported = false
  let assigns_goal _ _ _ = WpMain.fatal "unsupported logic-assigns"

  (* ------------------------------------------------------------------------ *)
  (* ---  Scopes                                                          --- *)
  (* ------------------------------------------------------------------------ *)

  let global _ = ()

  let global_scope _ p = p  

  let local_scope mem locals scope p =
    let locals = List.filter VarUsage.validated_cvar locals in
    M.local_scope mem.heap locals scope p

end
