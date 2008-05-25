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

(* $Id: jc_env.mli,v 1.53 2008/04/10 16:05:55 moy Exp $ *)

type native_type = Tunit | Tboolean | Tinteger | Treal | Tstring

type inv_sem = InvNone | InvOwnership | InvArguments

type separation_sem = SepNone | SepRegions

type annotation_sem = 
    AnnotNone | AnnotInvariants | AnnotWeakPre | AnnotStrongPre

type abstract_domain = AbsNone | AbsBox | AbsOct | AbsPol

type int_model = IMbounded | IMmodulo

type jc_type =
  | JCTnative of native_type
  | JCTlogic of string
  | JCTenum of enum_info
  | JCTpointer of tag_or_variant * Num.num option * Num.num option
  | JCTnull
  | JCTany

and tag_or_variant =
  | JCtag of struct_info
  | JCvariant of variant_info
  | JCunion of variant_info

and enum_info =
    { 
      jc_enum_info_name : string;
      jc_enum_info_min : Num.num;
      jc_enum_info_max : Num.num;
    }

and struct_info =
    { 
              jc_struct_info_name : string;
      mutable jc_struct_info_parent : struct_info option;
      mutable jc_struct_info_root : struct_info;
      mutable jc_struct_info_fields : field_info list;
      mutable jc_struct_info_variant : variant_info option;
        (* only valid for root structures *)
    }

and variant_info =
    {
      jc_variant_info_name : string;
(*      mutable jc_variant_info_tags : struct_info list;*)
      mutable jc_variant_info_roots : struct_info list;
(*      jc_variant_info_open : bool;*)
      jc_variant_info_is_union : bool;
    }

and field_info =
    {
      jc_field_info_tag : int;
      jc_field_info_name : string;
      jc_field_info_final_name : string;
      jc_field_info_type : jc_type;
      jc_field_info_struct: struct_info;
        (* The structure in which the field is defined *)
      jc_field_info_root : struct_info;
        (* The root of the structure in which the field is defined *)
      jc_field_info_rep : bool; (* "rep" flag *)
    }

type field_or_variant_info = 
    FVfield of field_info | FVvariant of variant_info

type region = 
    {
      jc_reg_variable : bool;
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

type logic_label = 
  | LabelName of label_info
  | LabelHere
  | LabelPost
  | LabelPre
  | LabelOld

(*
Local Variables: 
compile-command: "unset LANG ; make -C .. byte"
End: 
*)
