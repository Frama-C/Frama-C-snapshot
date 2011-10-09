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
(** Memory Model for Hoare.                                                   *)
(* -------------------------------------------------------------------------- *)

(* TODO, ZD : 
   A lot of the code of hoare model is identical to the code of store. 
   Mayby, we can try to share more (even some) *)

open Cil_types
open Cil_datatype

module Create
  (F:Formula.S)
  (A:Mint.S   with module F = F)
  (R:Mfloat.S with module F = F)
  =
struct
  type m_pointer
  type pointer = m_pointer F.term

  type decl = F.decl

  let unsupported = Wp_error.unsupported

  type m_alloc = Formula.m_array
  let t_alloc : Formula.tau = Formula.ADT("farray",[Formula.Integer])
  type alloc = m_alloc F.term

  let model_ptr    = F.e_app2 "ptr"
  let model_base   = F.e_app1 "base"
  let model_offset = F.e_app1 "offset"
  let model_shift  = F.e_app2 "shift"

  let model_range   = F.e_app3 "range_ptr"
  let model_rbase   = F.e_app1 "rbase"
  let model_roffset = F.e_app1 "roffset"
  let model_range_of_ptr = F.e_app2 "range_of_ptr"
  let model_range_of_ptr_range = F.e_app3 "range_of_ptr_range"
  let model_separated = F.p_app2 "separated"
  let model_valid = F.p_app2 "valid" 



  let sizeof te = F.e_int64 (Ctypes.sizeof_object te)
  let n_size n te = F.i_mult n (sizeof te)
  let add_offset d te k = F.i_add d (n_size k te)
  let cardinal a b = F.i_add F.i_one (F.i_sub b a)

  let offset_of_field f =
    let rec acc sz l f =
      match l with
        | [] -> Wp_parameters.fatal "[offset_of_field] not found %s" f.fname
        | fi::m ->
            if Cil_datatype.Fieldinfo.equal f fi
            then sz
            else  acc (F.i_add sz  (sizeof (Ctypes.object_of fi.ftype))) m f
    in
    acc F.i_zero f.fcomp.cfields f

  module Model = struct
    module F = F
    module A = A
    module R = R



    let tau_of_loc = Formula.ADT("pointer",[])

    type loc =  | Addr of F.integer * F.integer | Ptr of pointer

    let ptr = function
      | Addr(b,d) -> model_ptr b d
      | Ptr p -> p

    let base = function
      | Addr(b,_) -> b
      | Ptr p -> model_base p

    let offset = function
      | Addr(_,d) -> d
      | Ptr p -> model_offset p

    let loc_of_term _ p = Ptr (F.unwrap p)
    let term_of_loc loc = F.wrap (ptr loc)


    let rec pp_loc fmt l = match l with
      | Addr (x,i) -> Format.fprintf fmt "@@ptr(%a,%a)"
          F.pp_term x F.pp_term i
      | Ptr p -> Format.fprintf fmt "%a" F.pp_term p


    let equal_loc_bool l1 l2 =
        F.e_app2 "eq_ptr_bool" (ptr l1) (ptr l2)
    let lt_loc_bool l1 l2 =
        F.e_app2 "lt_ptr_bool" (ptr l1) (ptr l2)
    let le_loc_bool l1 l2 =
        F.e_app2 "le_ptr_bool" (ptr l1) (ptr l2)
    let equal_loc l1 l2 =
        F.p_app2 "eq_ptr" (ptr l1) (ptr l2)
    let lt_loc l1 l2 =
        F.p_app2 "lt_ptr" (ptr l1) (ptr l2)
    let le_loc l1 l2 =
        F.p_app2 "le_ptr" (ptr l1) (ptr l2)
    let minus_loc l1 l2 =
        F.e_app2 "minus_ptr" (ptr l1) (ptr l2)

    let null = Addr(F.i_zero,F.i_zero)
    let is_null l =
      F.e_app2 "addr_eq" (ptr l) (model_ptr F.i_zero F.i_zero)

    let cast_loc_to_int _tp _p _ti = unsupported "cast from pointer to int"
    let cast_int_to_loc _ti _i _tp = unsupported "cast from int to pointer"
  end

  let cast_loc_to_loc ty1 ty2 l =
    if Ctypes.equal (Ctypes.object_of ty1)(Ctypes.object_of ty2) then l
    else
      unsupported "cast from pointer (%a) to pointer (%a)"
        !Ast_printer.d_type ty1 !Ast_printer.d_type ty2

  module V =  Datalib.Cvalues(Model)
  module L = Datalib.Create(V)
  include V
  open Model

  module Globals = F.DRegister
    (struct
       include F.Varinfo
	 
       let declare x _ =
         let pool = F.pool () in
         let xa = F.p_fresh pool "ta" (Formula.Model t_alloc) in
         let sx = sizeof (Ctypes.object_of x.vtype) in
         let xk = F.Xindex.get_ind x in
         let sa = F.e_access (F.var xa) xk in
         let gta = F.p_app1 "global" (F.var xa) in
         Formula.Axiom (F.p_forall [xa]
			  (F.p_implies gta (F.p_eq (F.unwrap sa) sx)))
	   
       let section = Formula.S_Model_Prop
       let prefix = "Alloc"
       let basename x = x.vname
       let clear () = ()
       let pp_descr fmt _x =
         Format.fprintf fmt "Global allocation table"
     end)
    
    
    
  let global v = Globals.define v
    
  let cvar _m vi =
    if vi.vglob then global vi ;
    Addr (F.Xindex.get_ind vi,F.i_zero )
      
	
 let inner_loc _ = Wp_parameters.fatal "[inner_loc] reserved to funvar"

  let lvar _m lv x =
    let ty = 
      match lv.lv_type with 
	| Ctype ty -> ty 
	| ty -> Wp_parameters.fatal 
	    "[lvar] c type of pure logic type %a"
	      !Ast_printer.d_logic_type ty
    in
    loc_of_term (Ctypes.object_of ty)(F.var x) 
      
  let offset loc te n =
    match loc with
      | Model.Addr(b,d) ->  Model.Addr(b,add_offset d te n)
      | Model.Ptr p -> Model.Ptr (model_shift p (n_size n te ))
	  
  let field l f =
    if f.fcomp.cstruct then
      let pos = offset_of_field f in
      match l with
        | Model.Addr(b,d) -> Model.Addr(b,(F.i_add d pos))
        | Model.Ptr p -> Model.Ptr (model_shift p pos)
    else l
      
      
  let shift = offset
  let index = offset
  let startof l _cv = l
    


  type mem = 
      { vars : F.var Varinfo.Hashtbl.t ; 
        x_alloc : F.var ;
        alloc : alloc ; 
      }
       
  let mem () = 
    let x_t =  L.fresh "ta" (Formula.Model t_alloc) in
    {
      vars = Varinfo.Hashtbl.create 10;
      x_alloc = x_t ; 
      alloc = F.var x_t;
    }
      
  (************************************************************************************)
      
  (** Get the wp variable of the C variable. *)
  let get_var m lv =
    try Varinfo.Hashtbl.find m lv
    with Not_found ->
      let ty = lv.vtype in
      let t = tau_of_object (Ctypes.object_of ty) in
      let v = L.fresh lv.vname (Formula.Acsl(t,Ctype ty)) in
      Varinfo.Hashtbl.add m lv v; v

  let load _m _cobj _loc = unsupported "undirect access"

  let base_address _  = function
    | Model.Addr(b,_) -> Model.Addr(b,F.i_zero)
    | Model.Ptr p -> Model.Ptr (model_ptr (model_base p) F.i_zero)

  let block_length mem l = F.unwrap (F.e_access mem.alloc (base l))
   

  (* --- WP Calculus *)

  let update ~(at:mem) ~(here:mem) p =
    let dov vi v_l p =
      let v_here = get_var here.vars vi in
      L.subst v_l (F.var v_here) p
    in 
    L.subst at.x_alloc here.alloc
      (Varinfo.Hashtbl.fold dov at.vars p)

  let quantify m p =
    let xs = Varinfo.Hashtbl.fold (fun _ v xs -> v::xs) m.vars [] in
    L.forall (m.x_alloc::xs) p

  let subst_lval _env _t _loc _v_exp _p = unsupported "undirect access"

  let alloc ta v sz = F.e_update ta v sz
  let free ta v = F.e_update ta v (F.wrap F.i_zero) 

  let alloc_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         let sz_x = F.wrap (sizeof (Ctypes.object_of x.vtype)) in
         L.subst m.x_alloc (alloc m.alloc v_x sz_x) p
      ) p xs
      
  let free_vars m xs p =
    List.fold_left
      (fun p x ->
         let v_x = F.Xindex.get_ind x in
         L.subst m.x_alloc (free m.alloc v_x) p
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


  let local_scope m vars scope p =
    match scope with
      | Mcfg.SC_Function_frame -> p
      | Mcfg.SC_Block_in | Mcfg.SC_Function_in ->
          notexists_vars m vars (alloc_vars m vars p)
      | Mcfg.SC_Block_out | Mcfg.SC_Function_out  ->
          free_vars m vars p
      | Mcfg.SC_Global ->
          if L.has_context_vars [m.x_alloc] p
          then F.p_implies (F.p_app1 "global" m.alloc) p
          else p

  (* ----------------------------------------------------------------------- *)
  (* --- Setof Locations                                                 --- *)
  (* ----------------------------------------------------------------------- *)

  let range_of_assignable = function
    | F.Aloc(te,loc) ->
        let size = sizeof te in
        begin
          match loc with
            | Model.Addr(b,d) -> model_range b d size
            | Model.Ptr p -> model_range_of_ptr p size
        end
    | F.Arange(te,loc,rg) ->
        begin
        match rg with
          |{F.inf=None;F.sup=Some h} ->
             let size = n_size (F.i_add h F.i_one) te in
           (match loc with
              | Model.Addr(b,d) -> model_range b d size
              | Model.Ptr p -> model_range_of_ptr p size
           )
          | {F.inf=Some l;F.sup=Some h} ->
              let delta = n_size l te in
              let size = n_size (cardinal l h) te in
            (match loc with
              | Model.Addr(b,d) ->
                  model_range b (F.i_add d delta) size
              | Model.Ptr p ->
                  model_range_of_ptr (model_shift p delta) size
           )
          | _ -> unsupported "infinite range for array"

        end

  let valid mem z =
  let r = range_of_assignable z in model_valid mem.alloc r

  let separated _ z1 z2 =
    let r1 = range_of_assignable z1 in
    let r2 = range_of_assignable z2 in
    model_separated r1 r2


  (* ----------------------------------------------------------------------- *)
  (* --- Zone Assigns                                                    --- *)
  (* ----------------------------------------------------------------------- *)

  type m_dzone
  type dzone = m_dzone F.term
  let tau_of_dzone = Formula.ADT("zone",[])

  let dzone_assigned _ _ =
    Wp_parameters.not_yet_implemented "zone representation in Hoare"

  let dzone_subset _z1 _z2 =
    Wp_parameters.not_yet_implemented "zone subset in Hoare"

  let dzone_union _z1 _z2 =
    Wp_parameters.not_yet_implemented "zone union in Hoare"

  let dzone_empty () =
    Wp_parameters.not_yet_implemented "empty zone in Hoare"

  let effect_supported = false


  (* ----------------------------------------------------------------------- *)
  (* --- Normal Assigns                                                  --- *)
  (* ----------------------------------------------------------------------- *)

  let assigns_goal _env _sloc _lto = unsupported "proof of assigns clauses"

  let assigns_supported = false

  (* ----------------------------------------------------------------------- *)
  (* --- Havoc                                                           --- *)
  (* ----------------------------------------------------------------------- *)

  let subst_havoc _ _ =
    Wp_parameters.not_yet_implemented "subst_havoc of pointer"

  (* ----------------------------------------------------------------------- *)
  (* --- User Predicate environment                                      --- *)
  (* ----------------------------------------------------------------------- *)

  type closure = Var of Cil_types.varinfo | Alloc

  let pp_closure fmt = function 
    | Var vinfo -> Format.fprintf fmt "value of %a" !Ast_printer.d_var vinfo
    | Alloc ->  Format.fprintf fmt "allocation table"

  let userdef_mem_signature m =
    Varinfo.Hashtbl.fold
      (fun varinfo fvar signature -> (fvar,Var varinfo)::signature)
      m.vars [m.x_alloc,Alloc]
    
  let userdef_mem_apply mem = function 
    | Var varinfo -> F.var (get_var mem.vars varinfo)
    | Alloc -> F.wrap mem.alloc

  (* ------------------------------------------------------------------------ *)
  (* ---  Functional Closure                                              --- *)
  (* ------------------------------------------------------------------------ *)

  type formal = unit
  let pp_formal (_:Format.formatter) _ = () 
  let userdef_is_ref_param (_:logic_var) : bool = false
  let userdef_ref_signature (_:mem) : ( F.var * logic_var * formal ) list = []
  let userdef_ref_apply (_:mem) (_:formal) (_:loc) : value = 
    Wp_parameters.fatal "[userdef_ref_apply] of model Hoare"
  let userdef_ref_has_cvar (_ : logic_var) : bool = false 

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
