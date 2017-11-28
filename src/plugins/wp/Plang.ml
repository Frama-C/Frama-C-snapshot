(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Format
open Qed.Logic
open Qed.Engine
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Variables Marker                                                   --- *)
(* -------------------------------------------------------------------------- *)

type pool = {
  mutable vars : Vars.t ;
  mutable mark : Tset.t ;
}

let pool () = { vars = Vars.empty ; mark = Tset.empty }
let alloc_domain p = p.vars

let rec walk p f e =
  if not (Tset.mem e p.mark) &&
     not (Vars.subset (F.vars e) p.vars)
  then
    begin
      p.mark <- Tset.add e p.mark ;
      match F.repr e with
      | Fvar x -> p.vars <- Vars.add x p.vars ; f x
      | _ -> F.lc_iter (walk p f) e
    end

let alloc_e = walk
let alloc_p pool f p = walk pool f (F.e_prop p)
let alloc_xs pool f xs =
  let ys = Vars.diff xs pool.vars in
  if not (Vars.is_empty ys) then
    begin
      Vars.iter f ys ;
      pool.vars <- Vars.union xs pool.vars ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Lang Pretty Printer                                                --- *)
(* -------------------------------------------------------------------------- *)

module E = Qed.Export.Make(Lang.F.QED)
module Env = E.Env

type scope = Qed.Engine.scope

class engine =
  object(self)
    inherit E.engine as super
    inherit Lang.idprinting
    method infoprover w = w.altergo

    (* --- Types --- *)

    method t_int = "Z"
    method t_real = "R"
    method t_bool = "bool"
    method t_prop = "Prop"
    method t_atomic _ = true
    method pp_tvar fmt k =
      if 0 <= k && k < 26 then
        fprintf fmt "'%c" (char_of_int (int_of_char 'a' + k))
      else
        fprintf fmt "'%d" (k-26)
    method pp_array fmt t = fprintf fmt "%a[]" self#pp_subtau t
    method pp_farray fmt t k =
      fprintf fmt "@[<hov 2>%a[%a]@]" self#pp_subtau t self#pp_tau k
    method pp_datatype a fmt ts =
      Qed.Plib.pp_call_var ~f:(self#datatype a) self#pp_tau fmt ts

    (* --- Primitives --- *)

    method e_true _ = "true"
    method e_false _ = "false"
    method pp_int _ = Integer.pretty ~hexa:false
    method pp_real fmt q = Format.fprintf fmt "(%s)" (Q.to_string q)

    (* --- Atomicity --- *)

    method callstyle = CallVar
    method is_atomic e =
      match F.repr e with
      | Kint z -> Z.leq Z.zero z
      | Kreal _ -> true
      | Apply _ -> true
      | Aset _ | Aget _ | Fun _ -> true
      | _ -> F.is_simple e

    (* --- Operators --- *)

    method op_spaced = Qed.Export.is_ident
    method op_scope _ = None
    method op_real_of_int = Op "(R)"
    method op_add _ = Assoc "+"
    method op_sub _ = Assoc "-"
    method op_mul _ = Assoc "*"
    method op_div _ = Op "/"
    method op_mod _ = Op "%"
    method op_minus _ = Op "-"
    method op_equal _ = Op "="
    method op_noteq _ = Op "!="
    method op_eq _ _ = Op "="
    method op_neq _ _ = Op "!="
    method op_lt _ _ = Op "<"
    method op_leq _ _ = Op "<="
    method op_not _ = Op "!"
    method op_and = function Cprop -> Assoc "/\\" | Cterm -> Assoc "&"
    method op_or = function Cprop -> Assoc "\\/" | Cterm -> Assoc "|"
    method op_equiv = function Cprop -> Op "<->" | Cterm -> Op "="
    method op_imply _ = Op "->"

    (* --- Ternary --- *)

    method pp_conditional fmt cond pthen pelse =
      begin
        fprintf fmt "@[<hov 0>if %a" self#pp_atom cond ;
        fprintf fmt "@ then %a" self#pp_atom pthen ;
        fprintf fmt "@ else %a" self#pp_atom pelse ;
        fprintf fmt "@]" ;
      end

    (* --- Arrays --- *)

    method pp_array_get fmt a k =
      Format.fprintf fmt "@[<hov 2>%a@,[%a]@]" self#pp_atom a self#pp_flow k

    method pp_array_set fmt a k v =
      Format.fprintf fmt "@[<hov 2>%a@,[%a@ <- %a]@]"
        self#pp_atom a self#pp_atom k self#pp_flow v

    (* --- Records --- *)

    method pp_get_field fmt a fd =
      Format.fprintf fmt "%a.%s" self#pp_atom a (self#field fd)

    method pp_def_fields fmt fvs =
      let base,fvs = match F.record_with fvs with
        | None -> None,fvs | Some(r,fvs) -> Some r,fvs in
      begin
        fprintf fmt "@[<hov 2>{" ;
        let open Qed.Plib in
        iteri
          (fun i (f,v) ->
             ( match i , base with
               | (Isingle | Ifirst) , Some r ->
                   fprintf fmt "@ %a with" self#pp_flow r
               | _ -> () ) ;
             ( match i with
               | Ifirst | Imiddle ->
                   fprintf fmt "@ @[<hov 2>%s = %a ;@]"
                     (self#field f) self#pp_flow v
               | Isingle | Ilast ->
                   fprintf fmt "@ @[<hov 2>%s = %a@]"
                     (self#field f) self#pp_flow v )
          ) fvs ;
        fprintf fmt "@ }@]" ;
      end

    (* --- Lists --- *)

    method! pp_fun cmode fct ts =
      if fct == Vlist.f_concat then Vlist.pretty self ts else
      if fct == Vlist.f_elt then Vlist.elements self ts else
      if fct == Vlist.f_repeat then Vlist.pprepeat self ts else
        super#pp_fun cmode fct ts

    (* --- Higher Order --- *)

    method pp_apply (_:cmode) (_:term) (_:formatter) (_:term list) =
      failwith "Qed: higher-order application"

    method pp_lambda (_:formatter) (_: (string * tau) list) =
      failwith "Qed: lambda abstraction"

    (* --- Binders --- *)

    method! shareable e = super#shareable e && Vlist.shareable e

    method pp_forall tau fmt = function
      | [] -> ()
      | x::xs ->
          fprintf fmt "@[<hov 2>forall %a" self#pp_var x ;
          List.iter (fun x -> fprintf fmt ",@,%a" self#pp_var x) xs ;
          fprintf fmt "@ : %a.@]" self#pp_tau tau ;

    method pp_exists tau fmt = function
      | [] -> ()
      | x::xs ->
          fprintf fmt "@[<hov 2>exists %a" self#pp_var x ;
          List.iter (fun x -> fprintf fmt ",@,%a" self#pp_var x) xs ;
          fprintf fmt "@ : %a.@]" self#pp_tau tau ;

    method pp_let fmt _ x e =
      fprintf fmt "@[<hov 4>let %s = %a in@]@ " x self#pp_flow e

    (* --- Predicates --- *)

    method pp_pred fmt p = self#pp_prop fmt (F.e_prop p)

  end
