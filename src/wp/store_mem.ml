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
(** Memory Model with separation                                              *)
(* -------------------------------------------------------------------------- *)

module WpLog = Wp_parameters
open Cil_types
open Formula
open Ctypes
open Clabels

module Create
  (F:Formula.S)
  (A:Mint.S   with module F = F)
  (R:Mfloat.S with module F = F)
 =

struct

  let unsupported = Wp_error.unsupported

  (* ------------------------------------------------------------------------ *)
  (* --- Term Types                                                       --- *)
  (* ------------------------------------------------------------------------ *)

  type decl = F.decl

 
  let t_data: tau = Formula.ADT("data",[])

  type m_memory = m_array
  let t_memory : tau = Formula.ADT("farray",[t_data])
  type store = m_memory F.term

  type m_alloc = m_array
  let t_alloc : tau = Formula.ADT("farray",[Formula.Integer])
  type alloc = m_alloc F.term

  type m_dzone
  type dzone = m_dzone F.term

 
  (* ----------------------------------------------------------------------- *)
  (* --- Extracted specification from store.why                          --- *)
  (* ----------------------------------------------------------------------- *)

  (* Data and basic typed Data conversions *)
  let data_of_int (i: Ctypes.c_int) = 
    F.e_app1 ((Pretty_utils.sfprintf "data_of_%a") Ctypes.pp_int i)
  let int_of_data (i:Ctypes.c_int) = 
     F.e_app1 ((Pretty_utils.sfprintf "%a_of_data") Ctypes.pp_int i)
  let data_of_float (f: Ctypes.c_float) = 
    F.e_app1 ((Pretty_utils.sfprintf "data_of_%a") Ctypes.pp_float f)
  let float_of_data (f:Ctypes.c_float) = 
    F.e_app1 ((Pretty_utils.sfprintf "%a_of_data") Ctypes.pp_float f)
  let addr_of_data  = F.e_app1 "addr_of_data"
  let data_of_addr  = F.e_app1 "data_of_addr"





  (* Address *)

  let model_addr : F.integer -> F.integer -> F.integer = F.e_app2 "addr"
  let model_base : F.integer -> F.integer = F.e_app1 "base"
  let model_offset : F.integer -> F.integer = F.e_app1 "offset"

  let model_addr_shift : F.integer -> F.integer -> F.integer =
    F.e_app2 "addr_shift"


  (* Allocation *)

  let model_valid : alloc -> F.integer -> F.integer -> F.pred =
    F.p_app3 "valid"

  let model_isfresh : store -> alloc -> F.integer -> F.pred =
    F.p_app3 "is_fresh"

  let model_alloc (talloc:alloc) (p:F.integer) (sz:F.integer) : alloc =
    F.e_update talloc p (F.wrap sz)

  let model_free (talloc : alloc) (p: F.integer) : alloc =
    F.e_update talloc p (F.wrap F.i_zero)

  let model_block (talloc : alloc) (p: F.integer) : F.integer =
    F.unwrap (F.e_access talloc p)

   (* Zone *)

  let model_zempty : dzone = F.e_app0 "zempty"
  let model_zunion : dzone -> dzone -> dzone = F.e_app2 "zunion"
  let model_included : dzone -> dzone -> F.pred = F.p_app2 "included"

  let model_zrange : F.integer -> F.integer -> F.integer -> dzone =
    F.e_app3 "zrange"

  let model_zrange_of_addr_range :
      F.integer -> F.integer -> F.integer -> dzone =
    F.e_app3 "zrange_of_addr_range"

  let model_separated : dzone -> dzone -> F.pred = F.p_app2 "separated"

  (* Record, Array acces, update  *)

  let model_update_range: store -> dzone -> F.abstract -> store =
    F.e_app3 "update_range"
  let model_access_range: store -> dzone  -> F.abstract =
    F.e_app2 "access_range"


  let model_update_havoc: store -> dzone -> F.abstract -> store =
    F.e_app3 "update_havoc"
  let model_ishavoc : alloc -> store -> dzone -> store -> F.pred =
    F.p_app4 "is_havoc"

  type mem = {
    x_store : F.var ;
    x_alloc : F.var ;
    store : store ;
    alloc : alloc ;
  }

  let encode fmt v = F.e_app2 "encode" fmt v
  let decode fmt x = F.e_app2 "decode" fmt x

  (* ----------------------------------------------------------------------- *)
  (* --- Instanciation of MVALUE and MLOGIC : Store implemantation       --- *)
  (* ----------------------------------------------------------------------- *)

  let rec sizeof = function
    | C_comp cinfo ->
        List.fold_left
          (fun sz f -> F.i_add sz
	     (sizeof (object_of f.ftype))) F.i_zero cinfo.cfields
    | C_array ainfo ->
        begin
          match ainfo.arr_flat with
            | Some a -> F.i_mult
                (sizeof (object_of a.arr_cell))
                  (F.e_int64 a.arr_cell_nbr)
            | None -> WpLog.not_yet_implemented "[Store] Sizeof unknown-size array"
        end
    | _ -> F.i_one
	
  let n_size n te =  F.i_mult (sizeof te) n
  let add_offset d te k = F.i_add d (n_size k te)
  let cardinal a b = F.i_add F.i_one (F.i_sub b a)

  let offset_of_field f =
    let rec acc sz l f =
      match l with
        | [] -> Wp_parameters.fatal "[offset_of_field] not found %s" f.fname
        | fi::m ->
            if Cil_datatype.Fieldinfo.equal f fi
            then sz
            else  acc (F.i_add sz  (sizeof (object_of fi.ftype))) m f
    in
    acc F.i_zero f.fcomp.cfields f


  module Model =
  struct

    module A=A
    module R=R
    module F=F

    type st_loc = 
	{ base :F.integer ; 
	  off:F.integer ; 
	  obj : Ctypes.c_object}

    let upd_base l b = {base = b; off=l.off; obj = l.obj}
    let upd_off l d = {base = l.base ; off = d ; obj = l.obj}
    let upd_obj l cv = {base = l.base ; off = l.off ; obj = cv}

    type loc =
      | Loc of st_loc
      | Addr of F.integer * Ctypes.c_object (*address,type*)

    let addr = function
      | Loc l -> model_addr l.base l.off
      | Addr (p,_) -> p

    let base = function
      | Loc l -> l.base
      | Addr (p,_) -> model_base p

    let offset = function
      | Loc l -> l.off
      | Addr (p,_) -> model_offset p

    let object_of_loc = function 
      | Loc l -> l.obj
      | Addr (_,cv) -> cv
   
    let sizeof_loc l = 
      let size = Ctypes.sizeof_object (object_of_loc l) in 
      F.e_int64 size


    let loc_of_term ty p = Addr (F.unwrap p,ty)
    let term_of_loc loc = F.wrap (addr loc)

      
      
    let null = 
      let null_obj = Ctypes.C_pointer Cil.charType  in 
      Addr (F.i_zero,null_obj)

    let is_null l = F.e_app2 "addr_eq" (addr l) (F.i_zero)
    let lt_loc l1 l2 = F.p_app2 "addr_lt" (addr l1) (addr l2)
    let le_loc l1 l2 =  F.p_app2 "addr_le" (addr l1) (addr l2)
    let equal_loc l1 l2 = F.p_app2 "addr_eq" (addr l1) (addr l2)
    let lt_loc_bool l1 l2 = F.e_app2 "addr_lt_bool" (addr l1) (addr l2)
    let le_loc_bool l1 l2 =  F.e_app2 "addr_le_bool" (addr l1) (addr l2)
    let equal_loc_bool l1 l2 = F.e_app2 "addr_eq_bool" (addr l1) (addr l2)

    let minus_loc l1 l2 =
      F.e_app2 "minus_addr" (addr l1) (addr l2)


    let cast_loc_to_int _tp loc _ti = addr loc

    let cast_int_to_loc _ti i tp = Addr (i, Ctypes.object_of tp)

    let pp_loc fmt = function
      | Loc l ->
          Format.fprintf fmt "addr(%a,%a)" 
	    F.pp_term l.base F.pp_term l.off
      | Addr(p,_t) ->  F.pp_term fmt p

    let tau_of_loc = Formula.Integer
  end

  module Globals = F.DRegister
    (struct
       include F.Varinfo

       let declare x _ =
         let pool = F.pool () in
         let xa = F.p_fresh pool "ta" (Model t_alloc) in
         let sx = sizeof (Ctypes.object_of x.vtype) in
         let xk = F.Xindex.get_ind x in
         let sa = F.e_access (F.var xa) xk in
         let gta = F.p_app1 "global" (F.var xa) in
         Axiom (F.p_forall [xa]
                  (F.p_implies gta (F.p_eq (F.unwrap sa) sx)))

       let section = S_Model_Prop
       let prefix = "Alloc"
       let basename x = x.vname
       let clear () = ()
       let pp_descr fmt _x =
         Format.fprintf fmt "Global allocation table"
     end)





  module Data =
  struct

    module V = Datalib.Cvalues(Model)
    module L = Datalib.Create(V)
    include V
    open Model

    type m_of_mem = m_memory
    let tau_of_mem = t_memory

      (* ZD : Be carreful, here [dummy_obj] must be useless. *)
    let forall_loc pool =
      let b = F.p_fresh pool "b" (Model Integer) in
      let d = F.p_fresh pool "d" (Model Integer) in
      let dummy_obj = Ctypes.C_int (Ctypes.c_char()) in
      let l = {base = F.var b ; off = F.var d ; obj = dummy_obj} in
      [b;d] , Loc l

    let load_rec = ref (fun _ _ _ -> assert false)
    let store_rec = ref (fun _ _ _ _ -> assert false)



    let define_vinfo x = 
      if x.vglob && (Ctypes.no_infinite_array (Ctypes.object_of x.vtype))
      then Globals.define x

    let global = define_vinfo

    let cvar_of_var vinfo =
      define_vinfo vinfo ;
      Loc 
	{base   = F.Xindex.get_ind vinfo; 
	 off = F.i_zero; 
	 obj    = (Ctypes.object_of (vinfo.vtype))}

    let cvar _mem vinfo = cvar_of_var vinfo
 
    let inner_loc _ = Wp_parameters.fatal "[inner_loc] reserved to funvar"
      
    let lvar _m lv x =
      let ty = 
	match lv.lv_type with 
	  | Ctype ty -> ty 
	  | ty -> Wp_parameters.fatal
	      "[lvar] c type of a pure logic type %a"
		!Ast_printer.d_logic_type ty 
      in
      loc_of_term (Ctypes.object_of ty)(F.var x) 
	
    let offset loc te n =
      if F.equal_terms n F.i_zero then loc else
	match loc with
          | Loc l -> 
	      Loc (upd_obj (upd_off l (add_offset l.off te n)) te)
          | Addr (p,_) ->
	      Addr(model_addr_shift p (n_size n te),te)

    let shift = offset
    let index = offset

    let field loc f =
      if f.fcomp.cstruct then
        let pos = offset_of_field f in
	let cv = Ctypes.object_of f.ftype in 
        match loc with
          | Loc l -> 
	      Loc (upd_obj (upd_off l (F.i_add l.off pos)) cv)
          | Addr (p,_) -> Addr (model_addr_shift p pos,cv)
      else
        unsupported "union field"

    let load_mem m t l = !load_rec m t l
    let store_mem m t l v = !store_rec m t l v

    let mem () =
      let x_m = L.fresh "m" (Formula.Model t_memory) in
      let x_t = L.fresh "ta" (Formula.Model t_alloc) in
      {
        x_store = x_m ;
        x_alloc = x_t ;
        store = F.var x_m ;
        alloc = F.var x_t ;
      }

  end

  module DF = Data_mem.Create(Data)

  include Data
  open Model
  let startof l _cv = l


  let base_address _mem = function
    | Loc l -> Loc (upd_off l F.i_zero)
    | Addr (p,cv) ->   Addr(model_addr (model_base p) F.i_zero,cv)

  let block_length _mem loc = sizeof_loc loc

  let cast_loc_to_loc ty1 ty2 l =
    let o1 = object_of ty1 in 
    let o2 = object_of ty2 in 
   if Ctypes.equal o1 o2 then l else
     match o1,o2 with 
       | C_array ar, C_pointer ty2 ->
	   if Ctypes.equal (Ctypes.object_of ar.arr_element) (Ctypes.object_of ty2) then 
	     l 
	   else 
	     (match  ar.arr_flat with 
		| Some {Ctypes.arr_cell = ty1} ->
		    if Ctypes.equal (Ctypes.object_of ty1) (Ctypes.object_of ty2) 
		    then l 
		    else (unsupported "pointer cast from %a to %a "
			    !Ast_printer.d_type ty1
			    !Ast_printer.d_type ty2)
		| None -> (unsupported "pointer cast from %a to %a "
			     !Ast_printer.d_type ty1
			     !Ast_printer.d_type ty2)
	     )
       | _,_ -> (unsupported "pointer cast from %a to %a "
		   !Ast_printer.d_type ty1
		   !Ast_printer.d_type ty2)
	   
  let zrange loc n =
    match loc with
      | Loc l -> model_zrange l.base l.off n
      | Addr (p,_) -> model_zrange_of_addr_range p F.i_zero n
	  


  (* ------------------------------------------------------------------------ *)
  (* --- Type Coersion Data/Records                                       --- *)
  (* ------------------------------------------------------------------------ *)

  (* record of data *)
  module SofData = F.DRegister
    (struct
       include F.Compinfo
       let prefix = "SofData"
       let section = S_Model_Def
       let clear () = () 
       let pp_title fmt c = 
	 Format.fprintf fmt "SofData for %s '%s'"
           (if c.cstruct then "struct" else "union") c.cname
       let declare comp _ =
         Function ([t_data],Record comp)
     end)
    

  (* data of record *)
  module DataofS = F.DRegister
    (struct
      include F.Compinfo
       let prefix = "DataofS"
       let section = S_Model_Def
       let clear () = () 
       let pp_title fmt c = 
	 Format.fprintf fmt "DataofS for %s '%s'"
           (if c.cstruct then "struct" else "union") c.cname
       let declare comp _ =
         Function ([Record comp],t_data) 
     end)
    
  let s_of_data comp = F.e_app1 (SofData.get_definition comp).d_name
  let data_of_s comp = F.e_app1 (DataofS.get_definition comp).d_name
    
  (* if is record [comp] s -> record of data (data of record s) = s*)
  module Ax2SofData = F.DRegister 
    (struct 
       include F.Compinfo
       let prefix = "SofDataofS"
       let section = S_Model_Prop
       let clear () = () 
       let pp_title fmt c = 
	 Format.fprintf fmt "DataofS for %s '%s'"
           (if c.cstruct then "struct" else "union") c.cname
       let declare comp _ =
	 let xs = F.p_fresh (F.pool()) "s" (Model (Record comp)) in 
	 let s = F.var xs in 
	 Axiom (F.p_forall [xs] 
		  (F.p_implies (L.is_comp comp s)
		  (F.p_eq ((s_of_data comp (data_of_s comp s))) s)))
     end)

  (* is record [comp] (record [comp] of data d) *)
  module Ax3IsSofData = F.DRegister
    (struct
       include F.Compinfo
       let prefix = "ISSofData"
       let section = S_Model_Prop 
       let clear () = () 
       let pp_title fmt c = 
	 Format.fprintf fmt "IsSofData for %s '%s'"
           (if c.cstruct then "struct" else "union") c.cname
       let declare comp _ = 
	 let xd = F.p_fresh (F.pool()) "d" (Model t_data) in 
	 let d = F.var xd in 
	 Axiom (F.p_forall [xd] (L.is_comp comp (s_of_data comp d)))
     end)

  let coerce_comp comp = 
    SofData.define comp; DataofS.define comp; 
     Ax2SofData.define comp; Ax3IsSofData.define comp

  let s_of_data comp = coerce_comp comp ; s_of_data comp 
  let data_of_s comp = coerce_comp comp ; data_of_s comp
    
    
  (* ------------------------------------------------------------------------ *)
  (* --- Type Coersion Data/Arrays                                        --- *)
  (* ------------------------------------------------------------------------ *)

 (* arrayof data *)
  module AofData = F.DRegister
    (struct
       include F.Arrayinfo
       let prefix = "AofData"
       let section = S_Model_Def
       let clear () = () 
       let pp_title fmt arr = 
	 Format.fprintf fmt "AofData for %a "
	    Ctypes.pretty (C_array arr)
       let declare arr _ =
         Function ([t_data],Array arr)
     end)
    

  (* data of array*)
  module DataofA = F.DRegister
    (struct
      include F.Arrayinfo
       let prefix = "DataofA"
       let section = S_Model_Def
       let clear () = () 
       let pp_title fmt arr = 
	 Format.fprintf fmt "DataofA for %a"
            Ctypes.pretty (C_array arr)
       let declare arr _ =
         Function ([Array arr],t_data) 
     end)

 let a_of_data arr = F.e_app1 (AofData.get_definition arr).d_name
 let data_of_a arr = F.e_app1 (DataofA.get_definition arr).d_name

  (* if is array[arr] s -> arrayof data (data of arrays) = s*)
  module Ax2AofData = F.DRegister 
    (struct 
       include F.Arrayinfo
       let prefix = "AofDataofA"
       let section = S_Model_Prop
       let clear () = () 
       let pp_title fmt arr = 
	 Format.fprintf fmt "DataofA for %a"
            Ctypes.pretty (C_array arr)
       let declare arr _ =
	 let xt = F.p_fresh (F.pool()) "t" (Model (Array arr)) in
	 let t = F.var xt in 
	 Axiom (F.p_forall [xt] 
		  (F.p_implies (L.is_array arr t)
		  (F.p_eq ((a_of_data arr (data_of_a arr t))) t)))
     end)

  (* is array[arr] (array[arr] of data d) *)
  module Ax3IsAofData = F.DRegister
    (struct
       include F.Arrayinfo
       let prefix = "IAAofData"
       let section = S_Model_Prop
       let clear () = () 
       let pp_title fmt arr = 
	 Format.fprintf fmt "IsAofData for %a"
           Ctypes.pretty (C_array arr)
       let declare arr _ = 
	 let xd = F.p_fresh (F.pool()) "d" (Model t_data) in 
	 let d = F.var xd in 
	 Axiom (F.p_forall [xd] (L.is_array arr (a_of_data arr d)))
     end)
 

  let coerce_arr arr = 
    AofData.define arr; DataofA.define arr; 
    Ax2AofData.define arr; Ax3IsAofData.define arr

  let a_of_data arr = coerce_arr arr ; a_of_data arr 
  let data_of_a arr = coerce_arr arr ; data_of_a arr
    



  (* ------------------------------------------------------------------------ *)
  (* --- Lod                                                             --- *)
  (* ------------------------------------------------------------------------ *)


  let load_with fmt mem loc =
    decode fmt (F.e_access mem (addr loc))

  let load_mem mem te loc =
    match te with
      | C_pointer ty -> 
	  let cv = Ctypes.object_of ty in
	  V_pointer(cv, Addr(addr_of_data (F.e_access mem (addr loc)),cv))
      | C_int i -> 
	  V_int(i,int_of_data i (F.e_access mem (addr loc)))
      | C_float f -> 
	  V_float(f, float_of_data f (F.e_access mem (addr loc)) )
      | C_comp comp ->
          if comp.cstruct then
            let z = zrange loc (sizeof te) in
            let d = model_access_range mem z in
            V_record(comp,s_of_data comp d)
          else Wp_parameters.not_yet_implemented "load of union"
      | C_array arr ->
          let z = zrange loc  (sizeof te) in
          let d = model_access_range mem z in
          V_array(arr,a_of_data arr d)

  let store_with mem loc fmt v =
    F.e_update mem (addr loc) (encode fmt v)

  let store_mem mem te loc v =
    match v with
      | V_int(i,t) -> F.e_update mem (addr loc) (data_of_int i t)
      | V_float(f,t) -> F.e_update mem (addr loc) (data_of_float f t)
      | V_pointer(_,lv) -> F.e_update mem (addr loc) (data_of_addr (addr lv))
      | V_record (comp,r) ->
          let dr = data_of_s comp r in
          let zp = zrange loc (sizeof te) in
          model_update_range mem zp dr
      | V_union _ -> unsupported "union"
      | V_array (arr,r) ->
          let dr = data_of_a arr r in
          let zp = zrange loc  (sizeof te) in
          model_update_range mem zp dr

  let () =
    begin
      Data.load_rec := load_mem ;
      Data.store_rec := store_mem ;
    end

  let load m te loc =
    DF.loaded te ; load_mem m.store te loc

  (* ------------------------------------------------------------------------ *)
  (* --- Zone                                                             --- *)
  (* ------------------------------------------------------------------------ *)

  (* --- Effect Assigns Method --- *)

  let tau_of_dzone = Formula.ADT("zone",[])

  (* Elementary Zones *)
  type assignable =
    | Xrange of F.integer * F.integer * F.integer (* BASE,OFS,LEN *)
    | Arange of F.integer * F.integer * F.integer (* ADDR,OFS,LEN *)
    | Ablock of F.integer                         (* ADDR,0,1 *)

  let addr_of_assignable = function
    | Xrange(x,ofs,_) -> model_addr x ofs
    | Ablock p -> p
    | Arange(p,ofs,_sz) -> model_addr_shift p ofs

  let zone_of_assignable = function
    | Xrange(x,ofs,sz) -> model_zrange x ofs sz
    | Ablock p -> model_zrange_of_addr_range p F.i_zero F.i_one
    | Arange(p,ofs,sz) -> model_zrange_of_addr_range p ofs sz

  let assignable_loc te loc =
    match loc with
      | Loc l -> Xrange(l.base,l.off,sizeof te)
      | Addr (p,_) -> Ablock p

  let assignable_range mem te loc rg =
    match loc with
      | Loc lc ->
          begin
            match rg with
	      | {F.inf = Some l ; F.sup = Some h} ->
                  Xrange(lc.base,add_offset lc.off te l,n_size (cardinal l h) te)
              | {F.inf = None ; F.sup = Some h} ->
                  Xrange(lc.base,lc.off,n_size (F.i_add h F.i_one) te)
              | {F.inf = Some l;F.sup = None} -> 
		  let h = 
		    F.i_sub (F.unwrap (F.e_access mem.alloc lc.base)) F.i_one
		  in 
		  Xrange(lc.base,add_offset lc.off te l,n_size (cardinal l h) te)
	      | {F.inf = None ; F.sup = None} ->
		  let h = F.unwrap (F.e_access mem.alloc lc.base) in
		  Xrange(lc.base,lc.off,n_size h te)
          end
      | Addr (p,_) ->
          begin
            match rg with
	      | {F.inf = Some l;F.sup = Some h} ->
                  Arange(p,n_size l te,n_size (cardinal l h) te)
              | {F.inf = None;F.sup = Some h} ->
                  Arange(p,F.i_zero,n_size (F.i_add h F.i_one) te)
              | {F.inf = Some l ; F.sup = None} ->
		  let h =
		    F.i_sub (F.unwrap (F.e_access mem.alloc (model_base p)))
		      F.i_one
		  in 
		  Arange(p,n_size l te,n_size (cardinal l h) te)
              |{F.inf = None ; F.sup = None} ->
		 let b = model_base p in 
		 let h = F.unwrap (F.e_access mem.alloc b) in 
		 Xrange(b,F.i_zero,n_size h te)
          end

  let assignable m = function
    | F.Aloc( te , loc ) -> assignable_loc te loc
    | F.Arange( te , loc , rg ) -> assignable_range m te loc rg

  let dzone_assigned m a = zone_of_assignable (assignable m a)
  let dzone_subset = model_included
  let dzone_union = model_zunion
  let dzone_empty () = model_zempty

  let effect_supported = true

  let assignable_sizeof = function
    | F.Aloc( te , _ ) -> sizeof te
    | F.Arange( te , _ , {F.inf=Some l;F.sup=Some h} ) ->
        n_size (cardinal l h) te
    | F.Arange _ -> unsupported "infinite range for array"

  let valid mem a =
    model_valid mem.alloc
      (addr_of_assignable (assignable mem a))
      (assignable_sizeof a)

  let get_zrange_opt = function
    | F.Aloc( te , loc ) -> 
	Some( loc , sizeof te )
    | F.Arange( te , loc , {F.inf=Some a;F.sup=Some b} ) ->
	Some( shift loc te a , n_size (cardinal a b) te )
    | F.Arange( te , loc , {F.inf=None;F.sup=Some b} ) ->
	Some( loc , n_size (cardinal F.i_zero b) te )
    | _ -> None

  let separated mem a1 a2 =
    match get_zrange_opt a1 , get_zrange_opt a2 with
      | Some(p,n) , Some(q,m) -> F.p_app4 "separated_on_addr" (addr p) (addr q) n m
      | _ ->
	  model_separated (dzone_assigned mem a1) (dzone_assigned mem a2)

  (* ----------------------------------------------------------------------- *)
  (* --- Instanciation of MWP : WP Calculus                              --- *)
  (* ----------------------------------------------------------------------- *)

  let update ~(at:mem) ~(here:mem) p =
    L.subst at.x_store here.store
      (L.subst at.x_alloc here.alloc p)

  let quantify m p =
    L.forall [m.x_store;m.x_alloc] p

  let subst_lval m te loc v p =
    DF.stored te ;
    L.subst m.x_store (store_mem m.store te loc v) p

  let alloc_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         let sz_x = sizeof (object_of x.vtype) in
         L.subst m.x_alloc (model_alloc m.alloc v_x sz_x) p
      ) p xs

  let free_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         L.subst m.x_alloc (model_free m.alloc v_x) p
      ) p xs

  let fresh_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         let q = model_isfresh m.store m.alloc v_x in
         F.p_implies q p
      ) p xs

  let notexists_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         let q = F.p_eq (F.unwrap(F.e_access m.alloc v_x)) F.i_zero in
         F.p_implies q p
      ) p xs

  let global_scope m p = 
    if L.has_context_vars [m.x_alloc] p
    then F.p_implies (F.p_app1 "global" m.alloc) p
    else p

  let filter_scope p vars = 
    List.filter (fun x -> F.Xindex.has_ind x p) vars

  let local_scope m vars scope p =
    match scope with
      | Mcfg.SC_Function_frame -> (* nothing to do *) p
      | Mcfg.SC_Block_in | Mcfg.SC_Function_in ->
	  let vars = filter_scope p vars in
          notexists_vars m vars
            (fresh_vars m vars
               (alloc_vars m vars p))
      | Mcfg.SC_Block_out | Mcfg.SC_Function_out  ->
	  let vars = filter_scope p vars in
          free_vars m vars
            (fresh_vars m vars p)
      | Mcfg.SC_Global ->
          if L.has_context_vars [m.x_alloc] p
          then F.p_implies (F.p_app1 "global" m.alloc) p
          else p

  let subst_havoc m a =
    let addr = zone_of_assignable (assignable m a) in
    let v = L.fresh "v" (Formula.Model(ADT("data",[]))) in
    let km sigma =
      let m0 = L.apply sigma m.store in
      F.wrap(model_update_havoc m0 addr (F.var v))
    in
    [F.Fresh v;F.Update(m.x_store,km)]

  let rec region m = function
    | [] -> model_zempty
    | [a] -> dzone_assigned m a
    | a :: others -> model_zunion (dzone_assigned m a) (region m others)

  let assigns_goal m1 assigned m2 =
    model_ishavoc m1.alloc m1.store (region m1 assigned) m2.store

  let assigns_supported = true

  (* ----------------------------------------------------------------------- *)
  (* --- User Predicate environment                                      --- *)
  (* ----------------------------------------------------------------------- *)

  type closure = Mem | Alloc

  let pp_closure fmt  = function
    | Mem -> Format.fprintf fmt "memory store"
    | Alloc -> Format.fprintf fmt "allocation table"

  let userdef_mem_signature m = [ m.x_store,Mem ; m.x_alloc,Alloc ]

  let userdef_mem_apply mem = function
    | Mem -> F.wrap mem.store
    | Alloc -> F.wrap mem.alloc

  (* ------------------------------------------------------------------------ *)
  (* ---  Functional Closure                                              --- *)
  (* ------------------------------------------------------------------------ *)

  type formal = unit
  let pp_formal (_:Format.formatter) _ = () 
  let userdef_is_ref_param (_:logic_var) : bool = false
  let userdef_ref_signature (_:mem) :( F.var * logic_var * formal ) list = []
  let userdef_ref_apply (_:mem) (_:formal) (_:loc) : value = 
    Wp_parameters.fatal "[userdef_ref_apply] of model Store"
  let userdef_ref_has_cvar (_ : logic_var) : bool = false


end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
