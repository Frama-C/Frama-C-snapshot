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
(* --- WHY Export                                                         --- *)
(* -------------------------------------------------------------------------- *)


module EWhy (L:sig val tau_of_ctype_logic : Cil_types.typ -> Formula.tau end)=
struct

type pred = Fol.pred
type decl = Fol.decl

let rec export_tau fmt = function
  | Formula.Integer -> Format.pp_print_string fmt "int"
  | Formula.Real -> Format.pp_print_string fmt "real"
  | Formula.Boolean -> Format.pp_print_string fmt "bool"
  | Formula.Pointer t -> export_tau fmt t
  | Formula.Record c -> Format.fprintf fmt "%s" c.Cil_types.cname
  | Formula.Array arr ->
      let t = L.tau_of_ctype_logic arr.Ctypes.arr_element in
      Format.fprintf fmt "%a farray" export_tau t
  | Formula.Set te ->
      Format.fprintf fmt "%a set" export_tau te
  | Formula.ADT(s,[]) -> Format.pp_print_string fmt s
  | Formula.ADT(s,[t]) -> Format.fprintf fmt "%a %s" export_tau t s
  | Formula.ADT(s,t::ts) ->
      Format.fprintf fmt "@[(%a" export_tau t ;
      List.iter (fun t -> Format.fprintf fmt ",@,%a" export_tau t) ts ;
      Format.fprintf fmt ") %s@]" s

let rec export_term fmt t =  Fol_pretty.fpp_term export_term fmt t


let rec pp_pred_atom fmt p =
  Fol_pretty.epp_pred_atom {
    Fol_pretty.pp_type = export_tau ;
    Fol_pretty.pp_term = export_term ;
    Fol_pretty.pp_pred = pp_pred_atom;
  } fmt p

let export_pred  fmt p =
  Fol_pretty.epp_pred_vbox {
    Fol_pretty.pp_type = export_tau ;
    Fol_pretty.pp_term = export_term ;
    Fol_pretty.pp_pred = pp_pred_atom ;
  } fmt p


let export_section fmt title =
  begin
    Format.fprintf fmt "(*----------------------------------------*)@\n" ;
    Format.fprintf fmt "(*--- %-32s ---*)@\n" title ;
    Format.fprintf fmt "(*----------------------------------------*)@\n" ;
  end

let export_item fmt name item =
  Fol_pretty.fpp_item export_term export_pred export_tau fmt name item

let export_decl fmt d =
  Pretty_utils.pp_trail Fol_pretty.fpp_header fmt d ;
  Format.pp_print_newline fmt () ;
  export_item fmt d.Formula.d_name d.Formula.d_item

let export_goal fmt x g =
  Fol_pretty.fpp_goal export_pred fmt x g;

end
