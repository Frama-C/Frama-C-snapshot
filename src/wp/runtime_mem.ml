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
(** Memory Model for Runtime:
* more information about it in {{:../../wp/Notes/m3.html}this document}
* *)
(* -------------------------------------------------------------------------- *)

let dkey = "runtime" (* debugging key *)

open Cil_types
open Cil_datatype

let unsupported = Wp_error.unsupported

type compute_int_mode = CIMterms | CIMvalues | CIMcompute

module Create
  (F:Formula.S)
  (A:Mint.S   with module F = F)
  (R:Mfloat.S with module F = F)
  =
struct

  type decl = F.decl   

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** The memory is composed of 2 parts: one that deals with the values,
  * and the other one that handle allocation information. More over, there
  * is a table in the [Pre] memory to handle logic variables that are used for
  * the initial value of the parameters.
  * *)

  type m_mbits
  let t_mbits : Formula.tau = Formula.ADT ("memory", [])
  type mem_bits = m_mbits F.term

  type m_alloc
  let t_alloc : Formula.tau = Formula.ADT ("memalloc", [])
  type mem_alloc = m_alloc F.term

  type m_mem
  let t_mem : Formula.tau = Formula.ADT ("memory", [])
  type mem = { vbits : F.var ; valloc : F.var ;}



  type m_format 
  type format = m_format F.term
 (** ----- Formats : *)

  let mk_iformat i = Pretty_utils.sfprintf "%a_format" Ctypes.pp_int i
  let mk_fformat f = Pretty_utils.sfprintf "%a_format" Ctypes.pp_float f
  let int_format = F.e_app0 "int_format"
  let real_format = F.e_app0 "real_format"
  let i_format i = F.e_app0 (mk_iformat i)
  let f_format f = F.e_app0 (mk_fformat f)
  let format_of_addr _ty =
    (* format_of_c_int_type (Ctypes.c_ptr()) *)
      F.e_app0 "rt_addr_format"


  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let tcomp_of_comp comp =  (* TODO: we shouldn't loose the TComp ! *)
    TComp (comp, {scache = Not_Computed}, [])

  let cil_field_info f =
    let t = tcomp_of_comp (f.fcomp) in
      try
        let offset = Field (f, NoOffset) in
          Cil.bitsOffset t offset
      with Cil.SizeOfError (msg,t) ->
        unsupported "sizeof %a : %s for field '%s'"
          !Ast_printer.d_type t msg f.fname
  let cil_field_offset f = fst (cil_field_info f)
  let cil_field_size f = snd (cil_field_info f)

  let name_of_var vi : F.name = F.Xindex.get_ind vi
  let name_of_field f = F.Findex.get_ind f

 


  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** Size and offset are sometimes known constants. We can choose to
 * represent them are simple [int] and compute values as far as possible,
 * or to keep them as named terms.
 * The first solution gives smaller - yet easier provable - goals,
 * but if if fails, the user has few information the understand the problem.
 * We will try to have both solutions in order to be able to test them.
 *)

  let compute_int_mode = CIMterms (* TODO: add an option ? *)

  (** integer terms with a phantom type to mark what kind of object it is. *)
  type 'a tint = F.integer

  module Aint : sig
    type 'a t

    val of_int : int -> 'a t
    val of_int64 : Int64.t -> 'a t
    val of_term : 'a tint -> 'a t

    val add : 'a t -> 'b t -> 'a t
    val sub : 'a t -> 'b t -> 'a t
    val cnst_mult :  Int64.t -> 'a t -> 'a t
    val mult : 'b t -> 'a t -> 'a t

    val to_term : 'a t -> 'a tint

    val eq_pred : 'a t -> 'a t -> F.pred

  end = struct

    let compute = (compute_int_mode = CIMcompute)

    type aint =
      | AIcnst of Int64.t
      | AIterm of F.integer
      | AImult of Int64.t * F.integer
      | AIadd of aint * Int64.t

    type 'a t = aint

    let term_of_cnst i =  F.e_icst (Int64.to_string i)

    let of_int64 i = if compute then AIcnst i else AIterm (term_of_cnst i)
    let of_int (i:int) = of_int64 (Int64.of_int i)
    let of_term t = AIterm t

    let term_of_add t1 t2 = F.e_iop Formula.Iadd t1 t2
    let term_of_sub t1 t2 = F.e_iop Formula.Isub t1 t2
    let term_of_mult t1 t2 = F.e_iop Formula.Imul t1 t2

    let rec to_term (ai: 'a t) : 'a tint = match ai with
      | AIcnst i -> term_of_cnst i
      | AImult (i, t) -> term_of_mult (term_of_cnst i) t
      | AIadd (t, i) -> term_of_add (to_term t) (term_of_cnst i)
      | AIterm t -> t

    let cnst_is_zero i = 0 = Int64.compare i (Int64.zero)

    let is_zero ai = match ai with
      | AIcnst i -> cnst_is_zero i
      | _ -> false

    let add_cnst ai i = match ai with
      | AIcnst i' when compute -> AIcnst (Int64.add i i')
      | AIadd (t, i')  when compute ->
          AIadd (t, Int64.add i i')
      | _ when cnst_is_zero i && compute -> ai
      | _ -> AIadd (ai, i)

    let rec cnst_mult i ai =
      if compute then match ai with
        | AIcnst i' -> AIcnst (Int64.mul i i')
        | AImult (i', t) -> AImult (Int64.mul i i', t)
        | _ when cnst_is_zero i -> AIcnst Int64.zero
        | AIadd (t, i') ->
            add_cnst (cnst_mult i t) (Int64.mul i i')
        | AIterm t -> AImult (i, t)
      else of_term (term_of_mult (term_of_cnst i) (to_term ai))

    let mult ai1 ai2 = match ai1, ai2 with
      | AIcnst i1, _ -> cnst_mult i1 ai2
      | _, AIcnst i2 -> cnst_mult i2 ai1
      | AImult (i1, t1), AImult (i2, t2) when compute ->
          cnst_mult (Int64.mul i1 i2) (AIterm (term_of_mult t1 t2))
      | _, _ -> (* TODO: develop other cases ? *)
          let t1 = to_term ai1 in
          let t2 = to_term ai2 in
            AIterm (term_of_mult t1 t2)

    let rec add ai1 ai2 = match ai1, ai2 with
      | AIcnst i1, _ -> add_cnst ai2 i1
      | _, AIcnst i2 -> add_cnst ai1 i2
      | AIadd (t1, i1), AIadd (t2, i2) when compute ->
          add_cnst (add t1 t2) (Int64.add i1 i2)
      | _, _ -> (* TODO: develop other cases ? *)
          let t1 = to_term ai1 in
          let t2 = to_term ai2 in
            AIterm (term_of_add t1 t2)

    let sub ai1 ai2 = match ai1, ai2 with (* TODO: compute... *)
      | _, _ ->
          let t1 = to_term ai1 in
          let t2 = to_term ai2 in
            AIterm (term_of_sub t1 t2)

    let eq_cnst i1 i2 = 0 = Int64.compare i1 i2

    let eq_pred ai1 ai2 = match ai1, ai2 with
      | AIcnst i1, AIcnst i2 when compute ->
          if eq_cnst i1 i2 then F.p_true else F.p_false
      | _, _ ->  (* TODO: develop other cases ? *)
          F.p_eq (to_term ai1) (to_term ai2)
  end

  (** Phantom types to tag the terms *)
  type m_addr
  type m_offset
  type m_size
  type m_zone

  (** Specialized type of F.integer terms *)
  type t_addr = m_addr tint
  type t_offset = m_offset tint
  type t_size = m_size tint
  type t_zone = m_zone F.term

  module Tint : sig

    type x_addr
    val xaddr_of_var : mem_alloc -> varinfo -> x_addr
    val xaddr_of_integer : F.integer -> x_addr
    val integer_of_xaddr : x_addr -> F.integer
    val varinfo_of_xaddr : x_addr -> varinfo option
    val pp_addr : Format.formatter -> x_addr -> unit
    val base : mem_alloc -> x_addr -> x_addr
    val term_of_xaddr : x_addr -> t_addr

    type x_size
    val size_of_int : int -> x_size
    val size_of_int64 : Int64.t -> x_size
    val cnst_mult_size : Int64.t -> x_size -> x_size
    val xsize_of_range : F.integer -> F.integer -> x_size -> x_size
    val term_of_xsize : x_size -> t_size

    val shift_n_elem : x_addr -> F.integer -> x_size -> x_addr
    val shift_field : x_addr -> fieldinfo -> x_addr

    val toffset_of_field : ?mode:compute_int_mode -> fieldinfo -> t_offset
    val tsize_of_field : ?mode:compute_int_mode -> fieldinfo -> t_size

    type x_zone
    val mk_xzone : x_addr -> x_size -> x_zone
    val xzone_of_var : mem_alloc -> varinfo -> x_size -> x_zone
    val pp_xzone : Format.formatter -> x_zone -> unit
    val term_of_xzone : x_zone -> m_zone F.term
    val eq_zone : x_zone -> x_zone -> F.pred
    val xzone_disj : x_zone -> x_zone -> F.pred

  end = struct

    (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
    (**  {2 Extracted specification from runtime.why}
    * see definitions and axioms in
    * {{:../../../share/why/runtime.why}runtime.why}
    *)
    module L = struct
      let vaddr : mem_alloc -> F.name -> t_addr = F.e_app2 "rt_vaddr"

      let zone : t_addr -> t_size -> t_zone =  F.e_app2 "rt_zone"
      let vzone : mem_alloc -> F.name-> t_zone =  F.e_app2 "rt_vzone"

      let foffset : F.name -> t_offset = F.e_app1 "rt_foffset"
      let fsize : F.name -> t_size = F.e_app1 "rt_fsize"

      let shift : t_addr -> t_offset -> t_addr = F.e_app2 "rt_shift"

      let disj : t_zone -> t_zone -> F.pred = F.p_app2 "rt_disj"

      let base : mem_alloc -> t_addr -> t_addr = F.e_app2 "rt_abase"
    end

    type x_addr =
      | Lvaddr of (mem_alloc * varinfo)
      | Laddr of m_addr Aint.t

    type x_offset = m_offset Aint.t
    type x_size = m_size Aint.t
    type x_zone =
      | Zterm of t_zone
      | Zpair of x_addr * x_size

    let xaddr_of_var ma v = Lvaddr (ma, v)

    let term_of_xaddr ai : t_addr = match ai with
      | Lvaddr (ma, v) -> L.vaddr ma (name_of_var v)
      | Laddr ai -> Aint.to_term ai

    let aint_of_xaddr a : m_addr Aint.t = match a with
      | Laddr a -> a
      | _ -> Aint.of_term (term_of_xaddr a)

    let varinfo_of_xaddr a = match a with
      | Lvaddr (_, vi) -> Some vi
      | _ -> None

    let pp_addr fmt a = F.pp_term fmt (term_of_xaddr a)

    let xaddr_of_integer ti = Laddr (Aint.of_term ti)
    let integer_of_xaddr xa = term_of_xaddr xa

    let base ma a =  Laddr (Aint.of_term (L.base ma (term_of_xaddr a)))

    let offset_of_int i : x_offset = Aint.of_int i
    let size_of_int sz : x_size = Aint.of_int sz
    let size_of_int64 sz : x_size = Aint.of_int64 sz
    let cnst_mult_size n (sz:x_size) : x_size = Aint.cnst_mult n sz

    let xsize_of_range min max sz =
      let nb = Aint.sub (Aint.of_term max) (Aint.of_term min) in
      let nb = Aint.add nb (Aint.of_int 1) in
         Aint.mult nb sz

    let term_of_xsize ai : t_size = Aint.to_term ai

    let xoffset_of_field ?(mode=compute_int_mode) f =
      if mode = CIMterms then
        Aint.of_term (L.foffset (name_of_field f))
      else offset_of_int (cil_field_offset f)

    let toffset_of_field ?(mode=compute_int_mode) f =
      Aint.to_term (xoffset_of_field ~mode f)

    let xsize_of_field ?(mode=compute_int_mode) f =
      if mode = CIMterms then
        Aint.of_term (L.fsize (name_of_field f))
      else size_of_int (cil_field_size f)

    let tsize_of_field ?(mode=compute_int_mode) f =
      Aint.to_term (xsize_of_field ~mode f)

    let shift (a:x_addr) (offset:x_offset) : x_addr =
      if compute_int_mode = CIMcompute then
         Laddr (Aint.add (aint_of_xaddr a) offset)
      else
        Laddr (Aint.of_term
                 (L.shift (term_of_xaddr a) (Aint.to_term offset)))

    let shift_n_elem a n sz =
      let o : x_offset = Aint.of_term n in
      let o = Aint.mult sz o in
      shift a o

    let shift_field a f = shift a (xoffset_of_field f)

    let xzone_of_var ma v sz =
      if compute_int_mode = CIMterms then
        Zterm (L.vzone ma (name_of_var v))
      else
        Zpair (xaddr_of_var ma v, sz)

    let mk_xzone a sz : x_zone = match a with
      | Lvaddr (ma, v) -> xzone_of_var ma v sz
      | _ -> Zpair (a, sz)

    let term_of_xzone xz = match xz with
      | Zterm t -> t
      | Zpair (a, sz) -> L.zone (term_of_xaddr a) (term_of_xsize sz)

    let pp_xzone fmt xz = F.pp_term fmt (term_of_xzone xz)

    let eq_zone z1 z2 = (* TODO: SMP *)
      F.p_eq (term_of_xzone z1) (term_of_xzone z2)

    let xzone_disj z1 z2 = (* TODO: SMP *)
      L.disj (term_of_xzone z1) (term_of_xzone z2)
  end

  (* open Tint *)
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** {3 about size and offset} *)

  let rec sizeof_c_object t : Tint.x_size = match t with
    | Ctypes.C_comp comp ->
        begin
          try
            let t = tcomp_of_comp comp in
            let sz = Cil.bitsSizeOf t in
              Tint.size_of_int sz
          with Cil.SizeOfError (msg, t) ->
            unsupported "sizeof %a : %s" !Ast_printer.d_type t msg
        end
    | Ctypes.C_array {Ctypes.arr_flat = Some flat} ->
        let nb = flat.Ctypes.arr_cell_nbr in
        let sz = sizeof_c_object (Ctypes.object_of flat.Ctypes.arr_cell) in
          Tint.cnst_mult_size nb sz
    | _ -> Tint.size_of_int64 (Ctypes.sizeof_object t)

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
    (**  {2 Extracted specification from runtime.why}
    * see definitions and axioms in
    * {{:../../../share/why/runtime.why}runtime.why}
    *)

  type m_bits
  type t_bits = m_bits F.term

  type m_dzone
  type dzone = m_dzone F.term

  module RtLib = struct
    let rt_global : F.name -> F.pred = F.p_app1 "rt_global"

    let rt_vsize : F.name -> t_size = F.e_app1 "rt_vsize"

    (* let rt_vformat : F.name -> F.format = F.e_app1 "rt_vformat"
    let rt_fformat : F.name -> F.format = F.e_app1 "rt_fformat" *)

    let load : mem_bits -> t_zone -> t_bits = F.e_app2 "rt_load"
    let store : mem_bits -> t_addr -> t_bits -> mem_bits = F.e_app3 "rt_store"
    let havoc : mem_bits -> t_zone -> mem_bits = F.e_app2 "rt_havoc"

    let to_bits : format -> F.abstract -> t_bits = F.e_app2 "rt_to_bits"
    let from_bits : t_bits -> format -> F.abstract = F.e_app2 "rt_from_bits"

    let alloc : mem_alloc -> F.name -> mem_alloc = F.e_app2 "rt_valloc"

    let block_length : mem_alloc -> t_addr -> t_size
                                                = F.e_app2 "rt_block_length"
    let valid : mem_alloc -> t_zone -> F.pred =  F.p_app2 "rt_valid"

    let is_havoc : mem_alloc -> mem_bits -> dzone -> mem_bits -> F.pred =
      F.p_app4 "rt_is_havoc"

    let free : mem_alloc -> F.name -> mem_alloc = F.e_app2 "rt_vfree"

    let zs_empty : dzone = F.e_app0 "zs_empty"
    let zs_singleton : t_zone -> dzone = F.e_app1 "zs_singleton"
    let zs_union : dzone -> dzone -> dzone = F.e_app2 "zs_union"
    let zs_incl : dzone -> dzone -> F.pred = F.p_app2 "zs_incl"
  end

  let z_from_bits b fmt : F.integer = F.unwrap (RtLib.from_bits b fmt)
  let real_from_bits b fmt : F.real = F.unwrap (RtLib.from_bits b fmt)

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

  module Model =
  struct

    module F = F
    module A = A
    module R = R

    type loc = Tint.x_addr

    let pp_loc fmt l = Tint.pp_addr fmt l

    let cast_loc_to_int _tp loc ti : F.integer =
      let int_term = Tint.term_of_xaddr loc in
      F.i_convert (Ctypes.c_ptr()) ti int_term


    let cast_int_to_loc ti (i:F.integer) _tp : loc =
      let i = F.i_convert ti (Ctypes.c_ptr()) i in
      (Tint.xaddr_of_integer i)


    let null = Tint.xaddr_of_integer F.i_zero

    let is_null l =
      F.e_icmp Formula.Ceq (Tint.integer_of_xaddr l) F.i_zero

    let minus_loc l1 l2 =
      F.e_app2 "rt_addr_minus" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let le_loc_bool l1 l2 =
      F.e_app2 "rt_addr_le_bool" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let lt_loc_bool l1 l2 =
      F.e_app2 "rt_addr_lt_bool" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let equal_loc_bool l1 l2 =
      F.e_app2 "rt_addr_eq_bool" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let le_loc l1 l2 = F.p_app2 "rt_addr_le" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let lt_loc l1 l2 = F.p_app2 "rt_addr_lt" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)
    let equal_loc l1 l2 = F.p_app2 "rt_addr_eq" (Tint.term_of_xaddr l1)
        (Tint.term_of_xaddr l2)


    let term_of_loc a  = F.wrap (Tint.term_of_xaddr a)
    let loc_of_term _ t = Tint.xaddr_of_integer (F.unwrap t)

    let tau_of_loc = Formula.Integer

  end

  let startof loc _ = loc

  let cast_loc_to_loc _t1 _t2 p = p

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** {3 about zone} *)
  (**
  * Mwp requires a type [dzone] defined as [m_zone F.term]
  * that represent why formula of the model why zone type
  * (which name is given by [tau_of_zone] below).
  *
  * Because we need to provide function such as [dzone_union],
  * this [dzone] is defined as a set of the [rt_zone] defined in WHY.
   *)

  let tau_of_dzone = Formula.ADT("zones",[])


  let xzone_assigned  = function
    | F.Aloc( te , loc ) ->
        Tint.mk_xzone loc (sizeof_c_object te)
    | F.Arange( te , loc , rg ) ->
        match rg with
          | {F.inf = Some min; F.sup = Some max} ->
              let sz = sizeof_c_object te in
              let addr = Tint.shift_n_elem loc min sz in
              let size = Tint.xsize_of_range min max sz in
              Tint.mk_xzone addr size
          | _ -> unsupported "unbounded range"

  let dzone_assigned _m a = RtLib.zs_singleton (Tint.term_of_xzone (xzone_assigned a))
  let dzone_empty () = RtLib.zs_empty

  let dzone_subset dz1 dz2 = RtLib.zs_incl dz1 dz2

  let dzone_union dz1 dz2 = RtLib.zs_union dz1 dz2

  let effect_supported = true


  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

  let int_format_for_hyp t = match t with
    | Ctypes.C_int cint -> Some (i_format cint)
    | Ctypes.C_pointer ty -> Some (format_of_addr ty)
    | _ -> None

  let add_int_format_hyp vformat t h = match int_format_for_hyp t with
    | None -> h
    | Some fmt ->
        let h_format =  F.p_eq vformat fmt in
          F.p_and h_format h

  module VarDecl = F.DRegister
    (struct
       include F.Varinfo

     (** Global variable has a fixed zone in any allocation memory.
     * [forall ma, rt_vsize (ma, v) = sz /\ rt_vaddr (ma, v) = rt_gaddr v] *)
       let declare v _ =
         let t = Ctypes.object_of v.vtype in
         let h =
           if v.vglob then RtLib.rt_global (name_of_var v)
           else F.p_true
         in
         let sz = sizeof_c_object t in
         let size = RtLib.rt_vsize (name_of_var v) in
         let h_size = F.p_eq size (Tint.term_of_xsize sz) in
         let h = F.p_and h h_size in
         (* let vformat = RtLib.rt_vformat (name_of_var v) in
         let h = add_int_format_hyp vformat t h in *)
           (* TODO: size info might be redondant with format... *)
           (* TODO: format for other types... *)
           Formula.Axiom h

       let section = Formula.S_Model_Prop
       let prefix = "Decl"
       let basename x = x.vname
       let clear () = ()
       let pp_descr fmt _x =
         Format.fprintf fmt "Global declaration"
     end)
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  module Fields = F.DRegister
    (struct
       include F.Fieldinfo

       let declare f _ =
         let foff = Tint.toffset_of_field ~mode:CIMterms f in
         let off = Tint.toffset_of_field ~mode:CIMvalues f in
         let hoff = F.p_eq foff off in

         let fsz = Tint.tsize_of_field ~mode:CIMterms f in
         let sz = Tint.tsize_of_field ~mode:CIMvalues f in
         let hsz = F.p_eq fsz sz in
         let h = F.p_and hoff hsz in

           (*
         let t = Ctypes.object_of  f.ftype in
         let fformat = RtLib.rt_fformat (name_of_field f) in
         let h = add_int_format_hyp fformat t h in
         *)

           Formula.Axiom h

       let section = Formula.S_Model_Prop
       let prefix = "Finfo"
       let basename x =
         let name = F.Compinfo.basename  x.fcomp in
           (name^"_"^x.fname)
       let clear () = ()
       let pp_descr fmt _x =
         Format.fprintf fmt "Field info"
     end)
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  module V =  Datalib.Cvalues(Model)
  module L = Datalib.Create(V)

  module Data = struct
    include V

    type m_of_mem = m_mbits

    let tau_of_mem = t_mbits

    let forall_loc pool =
      let p = F.p_fresh pool "p" (Formula.Model Formula.Integer) in
      [p] , (Tint.xaddr_of_integer (F.var p))


    let global vi = VarDecl.define vi

    let cvar m vi =
      VarDecl.define vi ;
      Tint.xaddr_of_var (F.var m.valloc) vi

    let inner_loc _ = Wp_parameters.fatal "[inner_loc] reserved to funvar"
		     
    let lvar _m lv x = 
      let ty = 
	match lv.lv_type with | Ctype ty -> ty | _ -> assert false 
      in
      loc_of_term (Ctypes.object_of ty)(F.var x) 

    let shift (l:loc) t i : loc = Tint.shift_n_elem l i (sizeof_c_object t)

    let index = shift

    (** Even if union field has 0 offset, we have to use on operation because
    * the size of the location to be consider might change from one field to
    * another. *)
    let field (l:loc) f =
      Fields.define f;
      Tint.shift_field l f

    let value_of_bits = ref (fun _ _ -> assert false)
    let bits_of_value = ref (fun _ _ -> assert false)

    (** Read a data of type [te] at [loc] and returns it as a logic value. *)
    let load_mem mb te loc =
      let xzone = Tint.mk_xzone loc (sizeof_c_object te) in
      let tzone = Tint.term_of_xzone xzone in
      let bits = RtLib.load mb tzone in
      (!value_of_bits te bits)

    let store_mem mb te loc v =
      let tzone = Tint.term_of_xaddr loc in
      RtLib.store mb tzone (!bits_of_value te v)

  end

  module DF = Data_mem.Create(Data)

    



  let load m te loc =
    DF.loaded te;
    Data.load_mem (F.var m.vbits) te loc
    (*
    match Tint.varinfo_of_xaddr loc (*, logic_of_mem m *) with
      | Some vi, Some tbl when vi.vformal ->
          (* Pre state : vformal value is a logic variable
          * which reprent the initial value of the parameter. *)
          let v =
            try Varinfo.Hashtbl.find tbl vi
            with Not_found ->
              let ct = Ctype vi.vtype in
              let t = Data.tau_of_object (Ctypes.object_of vi.vtype) in
              let v = L.fresh vi.vname (Formula.Acsl (t,ct)) in
              (Varinfo.Hashtbl.add tbl vi v; v)
          in (Data.value_of_logic te (F.var v))
      | _, _ ->

    *)

  let store m loc t exp_val =
    DF.stored t;
    Data.store_mem (F.var m.vbits) t loc exp_val

  include Data

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** {3 about bits}
  * [bits] type is used to represent the values that are stored in the memory.
  * We have functions that encode/decode bits to/from ACSL logic values
  * depending on the intermediate C type [t].
  * *)

  let format_of_compinfo comp = DF.record_format comp
    (* if comp.cstruct then DF.record_format comp
    else unsupported "format of union" *)

  let format_of_array arr = DF.array_format arr

  let rt_format_of_ctype t = match t with
    | Ctypes.C_int c_int -> i_format c_int
    | Ctypes.C_float c_float ->  f_format c_float
    | Ctypes.C_comp comp -> format_of_compinfo comp
    | Ctypes.C_array arr -> format_of_array arr
    | Ctypes.C_pointer ty -> format_of_addr ty

  (** Compute the logic value from bits interpreted with type [t]. *)
  let rec value_of_bits t bits : Data.value = match t with
    | Ctypes.C_int c_int ->
        let c_val = z_from_bits bits (i_format c_int) in
        V_int (c_int, c_val)
    | Ctypes.C_float c_float ->
        let ft = f_format c_float in
        let c_val = real_from_bits bits ft in
        V_float (c_float, c_val)
    | Ctypes.C_comp comp ->
        let ft = format_of_compinfo comp in
        let c_val =  RtLib.from_bits bits ft in
        V_record (comp, F.unwrap c_val)
    | Ctypes.C_array arr ->
        let ft = format_of_array arr in
        let c_val =  RtLib.from_bits bits ft in
        let logic_val = (* D.encode ft*) (F.unwrap c_val) in
          V_array (arr, F.unwrap logic_val)
    | Ctypes.C_pointer ty ->
	let cv = Ctypes.object_of ty in
        let c_val = z_from_bits bits (format_of_addr cv) in
        let addr = Tint.xaddr_of_integer c_val in
        V_pointer (cv, addr)

  (* TODO: is it normal that we don't use t?
  * Maybe we should check that it is the same than in the value ??? *)
  let rec bits_of_value _t value : t_bits = match value with
    | V_int (c_int, i) ->
        let ft = i_format c_int in
          RtLib.to_bits ft (F.wrap i)
    | V_float (c_float, f) ->
        let ft = f_format c_float in
          RtLib.to_bits ft (F.wrap f)
    | V_pointer (ty, loc) ->
        let ft = format_of_addr ty in
          RtLib.to_bits ft (F.wrap (Tint.integer_of_xaddr loc))
    | V_union _ -> unsupported "bits_of_value of union"
    | V_record (comp, r) ->
        let ft = format_of_compinfo comp in
        let e = (*D.decode ft*) (F.wrap r) in
          RtLib.to_bits ft e
    | V_array (arr, t) ->
        let ft = format_of_array arr in
        let e = (* D.decode ft*) (F.wrap t) in
          RtLib.to_bits ft e

  (** Horrible thing but no (known) way to avoid... *)
  let () =
    begin
      Data.value_of_bits := value_of_bits ;
      Data.bits_of_value := bits_of_value ;
    end
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

  (** {3 Frame Environment} *)

  let mem ()= 
    let va = L.fresh "ma" (Formula.Model t_alloc) in
    let vb = L.fresh "mb" (Formula.Model t_mbits) in
    { vbits = vb ; valloc = va ;}

  (** {3 Validity }*)

  let valid m a =
    let xzone = xzone_assigned a in
    RtLib.valid (F.var m.valloc) (Tint.term_of_xzone xzone)

  let separated _m z1 z2 =
    Tint.xzone_disj (xzone_assigned z1) (xzone_assigned z2)

  let tbits_of_var a : t_bits = F.var a

  let subst_havoc m a =
    let xzone = xzone_assigned a in
    (* let v = L.fresh "v" (Mdata.Vmodel(Formula.ADT("bits",[]))) in
       let bits = tbits_of_var v in *)
    let new_vmh sigma =
      let mb = L.apply sigma (F.var m.vbits) in
      F.wrap (RtLib.havoc mb (Tint.term_of_xzone xzone))
    in
    [F.Update( m.vbits, new_vmh)]

  let assigns_goal m1 region m2 =
    let zones =
      match region with
        | [] -> dzone_empty ()
        | [a] -> dzone_assigned m1 a
        | a::others ->
            List.fold_left
              (fun dz a ->
                 dzone_union dz (dzone_assigned m1 a)
              ) (dzone_assigned m1 a) others
    in
    RtLib.is_havoc (F.var m1.valloc) (F.var m1.vbits) zones (F.var m2.vbits)

  let assigns_supported = true

  (** {3  Special locations} *)

  let base_address m loc = Tint.base (F.var m.valloc) loc

  let block_length m p =
    RtLib.block_length (F.var m.valloc) (Tint.term_of_xaddr p)

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** {3 User-defined Predicates} *)

  type closure =
    | Mem
    | Alloc

  let pp_closure fmt  = function
    | Mem ->Format.fprintf fmt "memory store"
    | Alloc ->Format.fprintf fmt "allocation table"

  let userdef_mem_signature m = [m.vbits,Mem ; m.valloc,Alloc]

  let userdef_mem_apply m cl = match cl with
    | Mem -> F.wrap (F.var m.vbits)
    | Alloc -> F.wrap (F.var m.valloc)

  (* ------------------------------------------------------------------------ *)
  (* ---  Functional Closure                                              --- *)
  (* ------------------------------------------------------------------------ *)

  type formal = unit
  let pp_formal _ _ = () 
  let userdef_is_ref_param (_:logic_var): bool = false
  let userdef_ref_signature (_:mem) : ( F.var * logic_var * formal ) list = []
  let userdef_ref_apply (_:mem) (_:formal) (_:loc) : value = assert false
  let userdef_ref_has_cvar (_ : logic_var) : bool = false


  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  (** {2 Mwp.S requirements} *)

  let update ~(at:mem) ~(here:mem) p =
    let p = L.subst at.vbits (F.var here.vbits) p in
    let p = L.subst at.valloc (F.var here.valloc) p in
    p

  let quantify m p =
    L.forall [m.vbits;m.valloc] p

  let subst_lval m t ptr exp p =
    let s = F.wrap (store m ptr t exp) in
    L.subst m.vbits s p

  (* ------------------------------------------------------------------------ *)
  (* ---  Local Scope                                                     --- *)
  (* ------------------------------------------------------------------------ *)

  let local_scope m vars scope_kind p =
    let vmh = m.vbits in
    let mh = F.var vmh in
    let do_var p v =
      let alloc v p =
        Wp_parameters.debug ~dkey "[local_scope] alloc %s@." v.vname;
        let ma = RtLib.alloc mh (name_of_var v) in
        let p = L.subst vmh ma p in
        p
      in
(*
      let init v p = (* initialize parameter : v@Here = v@Pre *)
        let m_pre = mem_at env Clabels.Pre in
        let v_loc = cvar m_here v in
        let v_pre = load m_pre t v_loc in
        let p = subst_lval env t v_loc v_pre p in
        p
      in
*)
        let p = match scope_kind with
        | Mcfg.SC_Function_in ->
            let p = alloc v p in p
            (*
              let p = match logic_of_mem (mem_at env Clabels.Pre) with
              | None -> p
              | Some tbl ->
                try let lv = Varinfo.Hashtbl.find tbl v in
                  L.forall [lv] p
                with Not_found -> p
            in p *)
        | Mcfg.SC_Function_frame ->
            (* let p = init v p in  *)
            let p = alloc v p in
            p
        | Mcfg.SC_Block_in ->
            let p = alloc v p in
            (*let h = add_int_format_hyp (name_of_var v) t F.p_true in
             F.p_implies h p
             *) p
        | Mcfg.SC_Block_out | Mcfg.SC_Function_out ->
            Wp_parameters.debug ~dkey "[local_scope] free %s@." v.vname;
            L.subst vmh (RtLib.free mh (name_of_var v)) p
        | Mcfg.SC_Global -> (* nothing to do *) p
      in p
    in List.fold_left do_var p vars

  let global_scope _ p = p 

end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
