(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Exportation Engine for Why-3                                       --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Export
open Engine

module Make(T : Term) =
struct

  module T = T
  module E = Export_whycore.Make(T)
  module Env = E.Env

  open T

  type tau = (Field.t,ADT.t) datatype
  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  class virtual engine =
    object(self)

      inherit E.engine as super
      method! sanitize = Export.sanitize ~to_lowercase:true
      
      (* -------------------------------------------------------------------------- *)
      (* --- Types                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method! pp_tau fmt = function
        | Prop -> assert false (* prop should never be printed *)
        | x -> super#pp_tau fmt x

      method t_atomic = function
        | Int | Real | Bool | Prop | Tvar _ -> true
        | Array _ -> false
        | Data(_,[]) -> true
        | Data _ -> false
        | Record _ -> true

      method pp_farray fmt a b =
        fprintf fmt "map %a %a" self#pp_subtau a self#pp_subtau b

      method pp_array fmt b =
        fprintf fmt "map int %a" self#pp_subtau b

      method pp_datatype adt fmt = function
        | [] -> pp_print_string fmt (self#datatype adt)
        | ts -> Plib.pp_call_apply ~f:(self#datatype adt) self#pp_subtau fmt ts

      (* -------------------------------------------------------------------------- *)
      (* --- Primitives                                                         --- *)
      (* -------------------------------------------------------------------------- *)

      method callstyle = CallApply
      method op_spaced (_:string) = true

      method pp_array_cst fmt (key : tau) v =
        try
          let elt = T.typeof v in
          let tau = Array(key,elt) in
          fprintf fmt "@[<hov 2>(const@ %a@ : %a)@]"
            self#pp_atom v self#pp_tau tau
        with Not_found ->
          fprintf fmt "@[<hov 2>(const@ %a)@]" self#pp_atom v

      (* -------------------------------------------------------------------------- *)
      (* --- Arithmetics                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_int amode fmt k = match amode with
        | Aint -> pp_print_string fmt (Z.to_string k)
        | Areal ->
            if Z.lt k Z.zero then
              (* unary minus is -. instead of - in Why3... *)
              fprintf fmt "-.%s.0" (Z.to_string (Z.neg k))
            else
              fprintf fmt "%s.0" (Z.to_string k)

      method pp_real fmt r =
        if Z.equal r.Q.den Z.one then
          self#pp_int Areal fmt r.Q.num
        else
          fprintf fmt "(%a@ /. %a)"
            (self#pp_int Areal) r.Q.num
            (self#pp_int Areal) r.Q.den

      method op_real_of_int = Call "real_of_int"

      method op_add = function Aint -> Assoc "+"  | Areal -> Assoc "+."
      method op_sub = function Aint -> Assoc "-"  | Areal -> Assoc "-."
      method op_mul = function Aint -> Assoc "*"  | Areal -> Assoc "*."
      method op_div = function Aint -> Call "div" | Areal -> Op "/."
      method op_mod = function Aint -> Call "mod" | Areal -> Call "rmod"
      method op_minus = function Aint -> Op "-" | Areal -> Op "-."

      method op_eq cmode (_:amode) = match cmode with
        | Cprop -> Op "="
        | Cterm -> Call "eqb"

      method op_neq cmode (_:amode) = match cmode with
        | Cprop -> Op "<>"
        | Cterm -> Call "neqb"

      method op_lt cmode amode = match cmode , amode with
        | Cprop , Aint  -> Op "<"
        | Cprop , Areal -> Op "<."
        | Cterm , Aint  -> Call "zlt"
        | Cterm , Areal -> Call "rlt"

      method op_leq cmode amode = match cmode , amode with
        | Cprop , Aint  -> Op "<="
        | Cprop , Areal -> Op "<=."
        | Cterm , Aint  -> Call "zleq"
        | Cterm , Areal -> Call "rleq"

      (* -------------------------------------------------------------------------- *)
      (* --- Logical Connectives                                                --- *)
      (* -------------------------------------------------------------------------- *)

      method e_true   = function Cterm -> "True"  | Cprop -> "true"
      method e_false  = function Cterm -> "False" | Cprop -> "false"

      method op_equal = function Cterm -> Call "eqb" | Cprop -> Op "="
      method op_noteq = function Cterm -> Call "neqb" | Cprop -> Op "<>"

      method op_not   = function Cprop -> Op "not"    | Cterm -> Call "notb"
      method op_and   = function Cprop -> Assoc "/\\" | Cterm -> Call "andb"
      method op_or    = function Cprop -> Assoc "\\/" | Cterm -> Call "orb"
      method op_imply = function Cprop -> Assoc "->"  | Cterm -> Call "implb"
      method op_equiv = function Cprop -> Op "<->"    | Cterm -> Op "="

      (* -------------------------------------------------------------------------- *)
      (* --- Conditional                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_conditional fmt a b c =
        begin
          fprintf fmt "@[<hov 0>if " ;
          self#with_mode Mpositive (fun _ -> self#pp_atom fmt a) ;
          fprintf fmt "@ then %a" self#pp_atom b ;
          fprintf fmt "@ else %a" self#pp_atom c ;
          fprintf fmt "@]" ;
        end

      (* -------------------------------------------------------------------------- *)
      (* --- Atomicity                                                          --- *)
      (* -------------------------------------------------------------------------- *)

      method is_atomic e =
        match T.repr e with
        | Kint z -> Z.leq Z.zero z
        | Apply(_,[]) -> false
        | Apply _ -> true
        | Acst _ | Aset _ | Aget _ -> true
        | _ -> T.is_simple e

      (* -------------------------------------------------------------------------- *)
      (* --- Records                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method op_record = "{" , "}"

      (* -------------------------------------------------------------------------- *)
      (* --- Binders                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_let fmt (_:pmode) x e =
        fprintf fmt "@[<hov 4>let %s = %a in@]@ " x self#pp_flow e

      method pp_forall tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>forall %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt "@ %a" self#pp_var x) xs ;
            fprintf fmt "@ : %a.@]" self#pp_tau tau ;

      method pp_intros tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>forall %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt "@ %a" self#pp_var x) xs ;
            fprintf fmt "@ : %a@]" self#pp_tau tau ;

      method pp_exists tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>exists %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt "@ %a" self#pp_var x) xs ;
            fprintf fmt "@ : %a.@]" self#pp_tau tau ;

      method pp_trigger fmt t =
        let rec pretty fmt = function
          | TgAny -> assert false
          | TgVar x -> self#pp_var fmt (self#find x)
          | TgGet(t,k) -> fprintf fmt "@[<hov 2>%a[%a]@]" pretty t pretty k
          | TgSet(t,k,v) -> fprintf fmt "@[<hov 2>%a[%a@ <- %a]@]" pretty t pretty k pretty v
          | TgFun(f,ts) -> call Cterm f fmt ts
          | TgProp(f,ts) -> call Cprop f fmt ts
        and call mode f fmt ts =
          match self#link f, mode with
          | F_call f, _
          | F_bool_prop (f,_), Cterm
          | F_bool_prop (_,f), Cprop ->
              Plib.pp_call_apply ~f pretty fmt ts
          | F_left f, _ -> Plib.pp_fold_apply ~f pretty fmt ts
          | F_right f, _ -> Plib.pp_fold_apply_rev ~f pretty fmt (List.rev ts)
          | F_assoc op, _ -> Plib.pp_assoc ~op pretty fmt ts
          | F_subst s, _ -> Plib.substitute_list pretty s fmt ts
          | F_list(fc,fn) , _ ->
              let rec plist fc fn fmt = function
                | [] -> pp_print_string fmt fn
                | x::xs ->
                    fprintf fmt "[<hov 2>(%s@ %a@ %a)@]" fc
                      pretty x (plist fc fn) xs
              in plist fc fn fmt ts
        in fprintf fmt "@[<hov 2>%a@]" pretty t

      (* -------------------------------------------------------------------------- *)
      (* --- Declarations                                                       --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_declare_adt fmt adt n =
        begin
          fprintf fmt "type %s" (self#datatype adt) ;
          for i=1 to n do self#pp_tvar fmt i done ;
        end

      method pp_declare_def fmt adt n def =
        begin
          fprintf fmt "@[<hov 4>" ;
          self#pp_declare_adt fmt adt n ;
          fprintf fmt "@ = %a@]" self#pp_tau def ;
        end

      method pp_declare_sum fmt adt n cases =
        begin
          fprintf fmt "@[<hv 1>" ;
          self#pp_declare_adt fmt adt n ;
          List.iter
            (fun (c,ts) ->
               fprintf fmt "@ @[<hov 4>| %s@]" (link_name (self#link c)) ;
               List.iter (fun t -> fprintf fmt "@ %a" self#pp_tau t) ts ;
            ) cases ;
          fprintf fmt "@]"
        end

      method declare_signature fmt f ts t =
        begin
          let cmode = Export.ctau t in
          fprintf fmt "@[<hov 4>%a" (self#pp_declare_symbol cmode) f ;
          List.iter (fun t -> fprintf fmt "@ %a" self#pp_subtau t) ts ;
          match t with
          | Prop -> fprintf fmt "@]@\n"
          | _ -> fprintf fmt "@ : %a@]@\n" self#pp_tau t ;
        end

      method declare_definition fmt f xs t e =
        self#global
          begin fun () ->
            let cmode = Export.ctau t in
            fprintf fmt "@[<hov 4>%a" (self#pp_declare_symbol cmode) f ;
            List.iter
              (fun x ->
                 let a = self#bind x in
                 let t = T.tau_of_var x in
                 fprintf fmt "@ (%a : %a)" self#pp_var a self#pp_tau t
              ) xs ;
            match cmode with
            | Cprop ->
                fprintf fmt " =@ @[<hov 0>%a@]@]@\n"
                  self#pp_prop e
            | Cterm ->
                fprintf fmt " : %a =@ @[<hov 0>%a@]@]@\n"
                  self#pp_tau t (self#pp_expr t) e
          end

      method declare_fixpoint ~prefix fmt f xs t e =
        begin
          self#declare_signature fmt f (List.map tau_of_var xs) t ;
          let fix = prefix ^ (link_name (self#link f)) in
          self#declare_axiom fmt fix xs [] (e_eq (e_fun f (List.map e_var xs)) e) ;
        end

    end

end
