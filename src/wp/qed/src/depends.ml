(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Logic

module Make(T : Term) =
struct
  open T

  (* -------------------------------------------------------------------------- *)
  (* --- Dependencies                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  module Types  = Set.Make(ADT)
  module Fields = Set.Make(Field)
  module Funs   = Set.Make(Fun)

  type depends = {
    mutable d_types  : Types.t ;
    mutable d_fields : Fields.t ;
    mutable d_funs   : Funs.t ;
    mutable d_preds  : Funs.t ;
  }

  let depends () = {
    d_types = Types.empty ;
    d_fields = Fields.empty ;
    d_funs = Funs.empty ;
    d_preds = Funs.empty ;
  }

  let subset d1 d2 =
    Types.subset d1.d_types d2.d_types &&
      Fields.subset d1.d_fields d2.d_fields &&
      Funs.subset d1.d_funs d2.d_funs &&
      Funs.subset d1.d_preds d2.d_preds

  let union d1 d2 = {
    d_types = Types.union d1.d_types d2.d_types ;
    d_fields = Fields.union d1.d_fields d2.d_fields ;
    d_funs = Funs.union d1.d_funs d2.d_funs ;
    d_preds = Funs.union d1.d_preds d2.d_preds ;
  }

  let iter_types f d = Types.iter f d.d_types
  let iter_fields f d = Fields.iter f d.d_fields
  let iter_functions f d = Funs.iter f d.d_funs
  let iter_predicates f d = Funs.iter f d.d_preds

  let mem_type d a = Types.mem a d.d_types
  let mem_field d f = Fields.mem f d.d_fields
  let mem_function d f = Funs.mem f d.d_funs
  let mem_predicate d p = Funs.mem p d.d_preds

  let add_type d a = d.d_types <- Types.add a d.d_types
  let add_field d f = d.d_fields <- Fields.add f d.d_fields
  let add_function d f = d.d_funs <- Funs.add f d.d_funs
  let add_predicate d p = d.d_preds <- Funs.add p d.d_preds
  let add_depend ~target:d ~source:d0 =
    begin
      d.d_types <- Types.union d.d_types d0.d_types ;
      d.d_fields <- Fields.union d.d_fields d0.d_fields ;
      d.d_funs <- Funs.union d.d_funs d0.d_funs ;
      d.d_preds <- Funs.union d.d_preds d0.d_preds ;
    end

  let rec add_tau d = function
    | Prop | Bool | Int | Real | Tvar _ -> ()
    | Array(a,b) -> add_tau d a ; add_tau d b
    | Record fts -> List.iter (fun (f,t) -> add_field d f ; add_tau d t) fts
    | Data(a,ts) -> add_type d a ; List.iter (add_tau d) ts

  type mode = Sterm | Sprop

  let rec add_expr d m e =
    match T.repr e with

      (* term sub-terms *)
      | True | False | Kint _ | Kreal _
      | Times _ | Add _ | Mul _ | Div _ | Mod _ 
      | Eq _ | Neq _ | Leq _ | Lt _ | Aget _ | Aset _ 
	  -> T.e_iter (add_expr d Sterm) e

      (* prop/term level *)
      | And _ | Or _ | Not _ | Imply _ | If _
	  -> T.e_iter (add_expr d m) e

      (* special *)
      | Fun(f,es) -> 
	  begin 
	    List.iter (add_expr d Sterm) es ;
	    match m with 
	      | Sterm -> add_function d f 
	      | Sprop -> add_predicate d f
	  end
      | Rdef fts -> 
	  List.iter (fun (f,e) -> add_field d f ; add_expr d Sterm e) fts
      | Rget(e,f) -> add_field d f ; add_expr d Sterm e
      | Apply(e,es) -> add_expr d m e ; List.iter (add_expr d Sterm) es
      | Var x -> add_tau d (tau_of_var x)
      | Bind(_,x,p) -> add_tau d (tau_of_var x) ; add_expr d Sprop p

  let add_term d e = add_expr d Sterm e
  let add_prop d p = add_expr d Sprop p

end

