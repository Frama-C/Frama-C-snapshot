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
(* --- Linked identifiers                                                 --- *)
(* -------------------------------------------------------------------------- *)

let mk_empty      = "empty"
let mk_singleton  = "singleton"
let mk_union      = "union"
let mk_inter      = "inter"
let mk_remove     = "remove"

let mk_range      = "range"
let mk_range_inf  = "range_inf"
let mk_range_sup  = "range_sup"
let mk_integers   = "integers_set"
let mk_radd       = "plus_int"
let mk_rmult      = "mult_int"
let mk_rneg       = "neg_int"

let set_range_index = "set_range_index"


let mk_imodulo i = Pretty_utils.sfprintf  "as_%a"  Ctypes.pp_int i
let mk_iguard i =  Pretty_utils.sfprintf "is_%a" Ctypes.pp_int i


let mk_fguard f =  Pretty_utils.sfprintf "is_%a" Ctypes.pp_float f 

(* -------------------------------------------------------------------------- *)
(* --- PRELUDE.why                                                        --- *)
(* -------------------------------------------------------------------------- *)

let neg_int = "neg_int"
let add_int = "add_int"
let sub_int = "sub_int"
let mul_int = "mul_int"
let div_int = "computer_div"
let mod_int = "computer_mod"
let eq_int = "eq"
let ne_int = "neq"
let lt_int = "lt_int"
let le_int = "le_int"

(* -------------------------------------------------------------------------- *)
(* --- BOOL.why                                                           --- *)
(* -------------------------------------------------------------------------- *)

let bool_not = "bool_not"
let bool_and = "bool_and"
let bool_or  = "bool_or"

(* -------------------------------------------------------------------------- *)
(* --- INTEGERS.why                                                       --- *)
(* -------------------------------------------------------------------------- *)

let eq_int_bool = "eq_int_bool"
let ne_int_bool = "neq_int_bool"
let lt_int_bool = "lt_int_bool"
let le_int_bool = "le_int_bool"

(* -------------------------------------------------------------------------- *)
(* --- REAL.why                                                           --- *)
(* -------------------------------------------------------------------------- *)

let neg_real = "neg_real"
let add_real = "add_real"
let sub_real = "sub_real"
let mul_real = "mul_real"
let fract_real = "div_real"

let eq_real_bool  = "eq_real_bool"
let ne_real_bool  = "neq_real_bool"
let lt_real_bool  = "lt_real_bool"
let le_real_bool  = "le_real_bool"

let eq_real = "eq_real"
let ne_real  = "neq_real"
let lt_real  = "lt_real"
let le_real  = "le_real"

let integer_of_real = "truncate_real_to_int"
let real_of_integer = "real_of_int"

(* -------------------------------------------------------------------------- *)
(* --- Lexically correct identifiers and reserved Prefixes                --- *)
(* -------------------------------------------------------------------------- *)

let identifier x =
  let range a c b = a <= c && c <= b in
  let buffer = Buffer.create 80 in
  for i=0 to String.length x - 1 do
    let c = x.[i] in
    if range 'a' c 'z'
      || range 'A' c 'Z'
      || (i > 0 && range '0' c '9')
      || c = '_'
    then
      Buffer.add_char buffer c ;
  done ;
  Buffer.contents buffer

(* Does not need to be projectified but could be. *)

let reserved_prefix_tbl = Hashtbl.create 17
let register_prefix s =
  Hashtbl.replace reserved_prefix_tbl s ()
let has_reserved_prefix name =
  try
    let index = String.index name '_' in
    let prefix = String.sub name 0 index in
    Hashtbl.mem reserved_prefix_tbl prefix
  with Not_found -> false

(* --------------------------------------------------------------------- *)
(* --- Pure Type Conversions                                         --- *)
(* --------------------------------------------------------------------- *)

open Cil_types
open Format
open Ctypes

module Tau =
struct
(* These pretty print functions are just for pretty print and debugs - file fol_pretty and
   F.pp_tau for debug ;) *)

  let tau_of_object = function
    | C_int _ -> Formula.Integer
    | C_float _ -> Formula.Real
    | C_pointer _ -> Formula.Pointer Formula.Integer (*useless and non sense value *)
    | C_comp c -> Formula.Record c
    | C_array a -> Formula.Array a

  let tau_of_ctype_logic t = tau_of_object (object_of t)

  let rec pp_tau fmt = function
    | Formula.Integer -> pp_print_string fmt "int"
    | Formula.Real -> pp_print_string fmt "real"
    | Formula.Boolean -> pp_print_string fmt "bool"
    | Formula.Pointer _ -> pp_print_string fmt "pointer"
    | Formula.Record c -> Format.fprintf fmt "%s" c.Cil_types.cname
    | Formula.Array a ->
        Format.fprintf fmt "%a farray" pp_tau
          (tau_of_object (object_of a.arr_element))
    | Formula.Set te ->
        if Wp_parameters.verbose_atleast 2
        then Format.fprintf fmt "%a set" pp_tau te
        else pp_print_string fmt "set"
    | Formula.ADT(s,[]) -> pp_print_string fmt s
    | Formula.ADT(s,[t]) -> Format.fprintf fmt "%a %s" pp_tau t s
    | Formula.ADT(s,t::ts) ->
        Format.fprintf fmt "@[(%a" pp_tau t ;
        List.iter (fun t -> Format.fprintf fmt ",@,%a" pp_tau t) ts ;
        Format.fprintf fmt ") %s@]" s


  let tau_of_ctype t = tau_of_object (Ctypes.object_of t)

  let name_of_adt = ref (fun lt -> Printf.sprintf "<%s>" lt.lt_name)

  let rec tau_of_logic_type = function
    | Ctype c -> tau_of_object (object_of c)
    | Linteger ->  Formula.Integer
    | Lreal ->  Formula.Real
    | Ltype( d , [] ) when d.lt_name = Utf8_logic.boolean ->  Formula.Boolean
    | Ltype( {lt_name="set"} , [t] ) -> Formula.Set (tau_of_logic_type t)
    | Ltype( lt , ts) ->
        let d = !name_of_adt lt in
        Formula.ADT (d,List.map tau_of_logic_type ts)
    | Lvar _ -> Wp_parameters.not_yet_implemented "logic type variables"
    | Larrow _ -> Wp_parameters.not_yet_implemented "type of logic function"

end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
