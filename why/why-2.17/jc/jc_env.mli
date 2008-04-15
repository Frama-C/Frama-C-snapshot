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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_env.mli,v 1.67 2008/11/05 14:03:15 filliatr Exp $ *)

type native_type = 
    Tunit | Tboolean | Tinteger | Treal | Tdouble | Tfloat | Tstring

type inv_sem = InvNone | InvOwnership | InvArguments

type separation_sem = SepNone | SepRegions

type annotation_sem = 
    AnnotNone | AnnotInvariants | AnnotElimPre | AnnotStrongPre | AnnotWeakPre

type abstract_domain = AbsNone | AbsBox | AbsOct | AbsPol

type int_model = IMbounded | IMmodulo

type root_kind = Rvariant | RplainUnion | RdiscrUnion

type jc_type =
  | JCTnative of native_type
  | JCTlogic of string
  | JCTenum of enum_info
  | JCTpointer of pointer_class * Num.num option * Num.num option
  | JCTnull
  | JCTany (* used when typing (if ... then raise E else ...): raise E is any *)
  | JCTtype_var of Jc_type_var.t

and pointer_class =
  | JCtag of struct_info * jc_type list (* struct_info, type parameters *)
  | JCroot of root_info

and enum_info =
    { 
      jc_enum_info_name : string;
      jc_enum_info_min : Num.num;
      jc_enum_info_max : Num.num;
    }

and struct_info =
    { 
              jc_struct_info_params : Jc_type_var.t list;
              jc_struct_info_name : string;
      mutable jc_struct_info_parent : (struct_info * jc_type list) option;
      mutable jc_struct_info_hroot : struct_info;
      mutable jc_struct_info_fields : field_info list;
      mutable jc_struct_info_root : root_info option;
        (* only valid for root structures *)
    }

and root_info =
    {
      jc_root_info_name : string;
(*      mutable jc_root_info_tags : struct_info list;*)
      mutable jc_root_info_hroots : struct_info list;
(*      jc_root_info_open : bool;*)
      jc_root_info_kind : root_kind;
      mutable jc_root_info_union_size_in_bytes: int;
    }

and field_info =
    {
      jc_field_info_tag : int;
      jc_field_info_name : string;
      jc_field_info_final_name : string;
      jc_field_info_type : jc_type;
      jc_field_info_struct: struct_info;
        (* The structure in which the field is defined *)
      jc_field_info_hroot : struct_info;
        (* The root of the structure in which the field is defined *)
      jc_field_info_rep : bool; (* "rep" flag *)
      jc_field_info_bitsize : int option;
        (* Size of the field in bits, optional *)
    }

type region = 
    {
      mutable jc_reg_variable : bool;
      mutable jc_reg_bitwise : bool;
      jc_reg_id : int;
      jc_reg_name : string;
      jc_reg_final_name : string;
      jc_reg_type : jc_type;
    }

type var_info = {
    jc_var_info_tag : int;
    jc_var_info_name : string;
    mutable jc_var_info_final_name : string;
    mutable jc_var_info_type : jc_type;
    mutable jc_var_info_region : region;
    mutable jc_var_info_formal : bool;
    mutable jc_var_info_assigned : bool;
    jc_var_info_static : bool;
  }

type exception_info =
    {
      jc_exception_info_tag : int;
      jc_exception_info_name : string;
      jc_exception_info_type : jc_type option;
    }

type label_info =
    { 
      label_info_name : string;
      label_info_final_name : string;
      mutable times_used : int;
    }

type label = 
  | LabelName of label_info
  | LabelHere
  | LabelPost
  | LabelPre
  | LabelOld

type alloc_class =
  | JCalloc_root of root_info
  | JCalloc_bitvector

type mem_class =
  | JCmem_field of field_info 
  | JCmem_plain_union of root_info
  | JCmem_bitvector

(*
Local Variables: 
compile-command: "unset LANG ; make -C .. byte"
End: 
*)
