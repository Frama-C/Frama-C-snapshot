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

(* $Id: java_pervasives.ml,v 1.16 2008/05/23 13:51:39 marche Exp $ *)

(*** Utility functions ***)


open Java_env

(* for method_info types *)

let get_method_info_class_or_interface_name mi =
  match mi.method_info_class_or_interface with
    | TypeClass ci -> ci.class_info_name
    | TypeInterface ii -> ii.interface_info_name
(*
    | TypeLogic _ -> assert false
*)

open Java_ast

(* for parsed exprs *)

let expr_no_loc e = 
  { java_pexpr_loc = Loc.dummy_position ; java_pexpr_node = e }

let expr_node_true = JPElit(Bool true)

let expr_true = expr_no_loc expr_node_true

let expr_zero = expr_no_loc (JPElit(Integer "0"))


open Java_tast

(* for typed statements *)

let make_statement_no_loc node = 
  { java_statement_loc = Loc.dummy_position ; java_statement_node = node }
    
(* for typed exprs *)

let make_expr_no_loc t node = 
  { java_expr_loc = Loc.dummy_position ;
    java_expr_type = t ;
    java_expr_node = node }
    
    
(*
let default_loop_annotation =
  { kml_loop_invariant = expr_true;
    kml_loop_assigns = None;
    kml_loop_variant = expr_zero;
  }

let default_method_specification =
  { kml_requires = expr_true;
  }
*)

open Java_env

let null_type = JTYnull
let unit_type = JTYbase Tunit
let boolean_type = JTYbase Tboolean
let integer_type = JTYbase Tinteger
let int_type = JTYbase Tint
let real_type = JTYbase Treal
let double_type = JTYbase Tdouble
let logic_string_type = JTYbase Tstring

let min_byte = Num.num_of_string "-128"
let max_byte = Num.num_of_string "127"
let min_short = Num.num_of_string "-32768"
let max_short = Num.num_of_string "32767"
let min_int = Num.num_of_string "-2147483648"
let max_int = Num.num_of_string "2147483647"
let min_long = Num.num_of_string "-9223372036854775808"
let max_long = Num.num_of_string "9223372036854775807"
let min_char = Num.num_of_string "0"
let max_char = Num.num_of_string "65535"

let in_byte_range n = Num.le_num min_byte n && Num.le_num n max_byte
let in_short_range n = Num.le_num min_short n && Num.le_num n max_short
let in_char_range n = Num.le_num min_char n && Num.le_num n max_char


(*
Local Variables: 
compile-command: "make -C .. bin/krakatoa.byte"
End: 
*)
