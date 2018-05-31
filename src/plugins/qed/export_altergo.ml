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
(* --- Exportation Engine for Alt-Ergo                                    --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Plib
open Engine
open Export

module Make(T : Term) =
struct

  open T
  module E = Export_whycore.Make(T)
  module Env = E.Env
  module ADT = T.ADT
  module Field = T.Field
  module Fun = T.Fun

  type trigger = (T.var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) Engine.ftypedef

  class virtual engine =
    object(self)

      inherit E.engine

      (* -------------------------------------------------------------------------- *)
      (* --- Types                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method t_atomic (_:tau) = true

      method pp_array fmt data =
        fprintf fmt "%a farray" self#pp_tau data

      method pp_farray fmt key data =
        fprintf fmt "(%a,%a) farray" self#pp_tau key self#pp_tau data

      method virtual get_typedef : ADT.t -> tau option
      method virtual set_typedef : ADT.t -> tau -> unit

      method pp_datatype adt fmt ts =
        match self#get_typedef adt with
        | Some def ->
            let t = Kind.tmap (Array.of_list ts) def in
            self#pp_tau fmt t
        | None ->
            match ts with
            | [] -> pp_print_string fmt (self#datatype adt)
            | [t] -> fprintf fmt "%a@ %s" self#pp_tau t (self#datatype adt)
            | t::ts ->
                fprintf fmt "@[<hov 2>(%a" self#pp_tau t ;
                List.iter (fun t -> fprintf fmt ",@,%a" self#pp_tau t) ts ;
                fprintf fmt ")@ %s@]" (self#datatype adt)

      (* -------------------------------------------------------------------------- *)
      (* --- Primitives                                                         --- *)
      (* -------------------------------------------------------------------------- *)

      method callstyle = CallVar

      method pp_array_cst fmt (key : tau) v =
        try
          let elt = T.typeof v in
          let tau = Array(key,elt) in
          fprintf fmt "@[<hov 2>(const(%a)@ : %a)@]"
            self#pp_flow v self#pp_tau tau
        with Not_found ->
          fprintf fmt "const(%a)" self#pp_flow v

      (* -------------------------------------------------------------------------- *)
      (* --- Arithmetics                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_int amode fmt z = match amode with
        | Aint -> pp_print_string fmt (Z.to_string z)
        | Areal -> fprintf fmt "%s.0" (Z.to_string z)
      
      method pp_real fmt r =
        if Z.equal r.Q.den Z.one then
          self#pp_int Areal fmt r.Q.num
        else
          fprintf fmt "(%a@ / %a)"
            (self#pp_int Areal) r.Q.num
            (self#pp_int Areal) r.Q.den

      method op_real_of_int = Call "real_of_int"

      method op_minus (_:amode) = Op "-"
      method op_add (_:amode) = Assoc "+"
      method op_sub (_:amode) = Assoc "-"
      method op_mul (_:amode) = Assoc "*"
      method op_div = function Aint -> Call "comp_div" | Areal -> Op "/"
      method op_mod = function Aint -> Call "comp_mod" | Areal -> Call "rmod"

      method op_eq cmode _amode =
        match cmode with
        | Cprop -> Op "="
        | Cterm -> Call "eqb"

      method op_neq cmode _amode =
        match cmode with
        | Cprop -> Op "<>"
        | Cterm -> Call "neqb"

      method op_lt cmode amode =
        match cmode , amode with
        | Cprop , _ -> Op "<"
        | Cterm , Aint -> Call "zlt"
        | Cterm , Areal -> Call "rlt"

      method op_leq cmode amode =
        match cmode , amode with
        | Cprop , _ -> Op "<="
        | Cterm , Aint -> Call "zleq"
        | Cterm , Areal -> Call "rleq"

      (* -------------------------------------------------------------------------- *)
      (* --- Logical Connectives                                                --- *)
      (* -------------------------------------------------------------------------- *)

      method e_true  _ = "true"
      method e_false _ = "false"

      method op_not   = function Cprop -> Op "not"    | Cterm -> Call "notb"
      method op_and   = function Cprop -> Assoc "and" | Cterm -> Call "andb"
      method op_or    = function Cprop -> Assoc "or"  | Cterm -> Call "orb"
      method op_imply = function Cprop -> Assoc "->"  | Cterm -> Call "implb"
      method op_equiv = function Cprop -> Op "<->"    | Cterm -> Call "eqb"

      method op_equal = function Cprop -> Op "=" | Cterm -> Call "eqb"
      method op_noteq = function Cprop -> Op "<>" | Cterm -> Call "neqb"

      (* -------------------------------------------------------------------------- *)
      (* --- Conditional                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_conditional fmt a b c =
        match Export.pmode self#mode with
        | Negative ->
            let cond = T.e_and [T.e_imply [a] b ; T.e_imply [T.e_not a] c] in
            self#pp_flow fmt cond
        | Positive ->
            let cond = T.e_or [T.e_and [a;b] ; T.e_and [T.e_not a;c]] in
            self#pp_flow fmt cond
        | Boolean ->
            begin
              fprintf fmt "@[<hov 2>match_bool(" ;
              self#with_mode Mterm (fun _ -> self#pp_atom fmt a) ;
              fprintf fmt ",@ %a" self#pp_atom b ;
              fprintf fmt ",@ %a" self#pp_atom c ;
              fprintf fmt ")@]" ;
            end

      (* -------------------------------------------------------------------------- *)
      (* --- Records                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method op_record = "{" , "}"

      (* -------------------------------------------------------------------------- *)
      (* --- Atomicity                                                          --- *)
      (* -------------------------------------------------------------------------- *)

      method op_spaced = is_identifier
      method is_atomic e =
        match T.repr e with
        | Kint z -> Z.leq Z.zero z
        | Kreal _ -> true
        | Apply _ -> true
        | Acst _ | Aset _ | Aget _ | Fun _ -> true
        | _ -> T.is_simple e

      (* -------------------------------------------------------------------------- *)
      (* --- Lets                                                               --- *)
      (* -------------------------------------------------------------------------- *)

      method typeof e = T.typeof e

      val mutable quantify_let = false
      method set_quantify_let e = quantify_let <- e

      method pp_let fmt pmode x e =
        try
          let tau = self#typeof e in
          match pmode with
          | Positive when quantify_let ->
              fprintf fmt "@[<hov 4>forall %s : %a. %s = %a ->@]@ "
                x self#pp_tau tau x self#pp_flow e
          | Negative when quantify_let ->
              fprintf fmt "@[<hov 4>exists %s : %a. %s = %a and@]@ "
                x self#pp_tau tau x self#pp_flow e
          | _ ->
              fprintf fmt "@[<hov 4>let %s = %a : %a in@]@ "
                x self#pp_atom e self#pp_tau tau
        with Not_found ->
          fprintf fmt "@[<hov 4>let %s = %a in@]@ "
            x self#pp_flow e

      (* -------------------------------------------------------------------------- *)
      (* --- Binders                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_forall tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>forall %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt ",@,%a" self#pp_var x) xs ;
            fprintf fmt "@ : %a.@]" self#pp_tau tau ;

      method pp_intros tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>forall %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt ",@,%a" self#pp_var x) xs ;
            fprintf fmt "@ : %a@]" self#pp_tau tau ;

      method pp_exists tau fmt = function
        | [] -> ()
        | x::xs ->
            fprintf fmt "@[<hov 2>exists %a" self#pp_var x ;
            List.iter (fun x -> fprintf fmt ",@,%a" self#pp_var x) xs ;
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
              Plib.pp_call_var ~f pretty fmt ts
          | F_left f, _ -> Plib.pp_fold_call ~f pretty fmt ts
          | F_right f, _ -> Plib.pp_fold_call_rev ~f pretty fmt (List.rev ts)
          | F_assoc op, _ -> Plib.pp_assoc ~e:"?" ~op pretty fmt ts
          | F_subst s, _ -> Plib.substitute_list pretty s fmt ts
          | F_list(fc,fn) , _ ->
              let rec plist fc fn fmt = function
                | [] -> pp_print_string fmt fn
                | x::xs ->
                    fprintf fmt "[<hov 2>%s(@,%a,@,%a)@]" fc
                      pretty x (plist fc fn) xs
              in plist fc fn fmt ts
        in fprintf fmt "@[<hov 2>%a@]" pretty t

      method pp_goal fmt p = self#pp_prop fmt p

      (* -------------------------------------------------------------------------- *)
      (* --- Declarations                                                       --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_declare_adt fmt adt = function
        | 0 -> fprintf fmt "type %s" (self#datatype adt)
        | 1 -> fprintf fmt "type %a %s" self#pp_tvar 1 (self#datatype adt)
        | n ->
            begin
              fprintf fmt "type (%a" self#pp_tvar 1 ;
              for i=2 to n do fprintf fmt ",%a" self#pp_tvar i done ;
              fprintf fmt ") %s" (self#datatype adt) ;
            end

      method pp_declare_def fmt adt n def =
        begin
          fprintf fmt "(* @[<hov 4>inlined type " ;
          self#pp_declare_adt fmt adt n ;
          fprintf fmt "@ = %a@] *)" self#pp_tau def ;
          self#set_typedef adt def ;
        end

      method pp_declare_sum fmt adt n cases =
        let is_enum = function (_,[]) -> true | _ -> false in
        if List.for_all is_enum cases then
          begin
            fprintf fmt "@[<hov 4>" ;
            self#pp_declare_adt fmt adt n ;
            Plib.iteri
              (fun index (c,_) -> match index with
                 | Ifirst | Isingle ->
                     fprintf fmt " = %s" (link_name (self#link c))
                 | Imiddle | Ilast ->
                     fprintf fmt "@ | %s" (link_name (self#link c))
              ) cases ;
            fprintf fmt "@]"
          end
        else
          begin
            self#pp_declare_adt fmt adt n ;
            pp_print_newline fmt () ;
            let result = Data(adt,Kind.type_params n) in
            List.iter
              (fun (c,ts) ->
                 self#declare_signature fmt c ts result
              ) cases ;
            let rank = "rank_" ^ self#datatype adt in
            fprintf fmt "logic %s : %a -> int@\n" rank self#pp_tau result ;
            Plib.iterk
              (fun k (c,ts) ->
                 fprintf fmt "@[<hov 2>axiom %s_%d:@ " rank k ;
                 let xs = Plib.mapk
                     (fun k t ->
                        fprintf fmt "forall x%d:%a.@ " k self#pp_tau t ;
                        Printf.sprintf "x%d" k) ts
                 in
                 let f = link_name (self#link c) in
                 fprintf fmt "%s(%a)=%d@]@\n" rank
                   (Plib.pp_call_var ~f pp_print_string)
                   xs k
              ) cases ;
          end

      method declare_signature fmt f ts t =
        begin
          fprintf fmt "@[<hv 4>logic %s :@ " (link_name (self#link f)) ;
          if ts <> [] then
            begin
              Plib.pp_listcompact ~sep:"," self#pp_tau fmt ts ;
              fprintf fmt "@ -> " ;
            end ;
          fprintf fmt "%a@]@\n" self#pp_tau t
        end

      method declare_definition fmt f xs t e =
        self#global
          begin fun () ->
            let cmode = Export.ctau t in
            fprintf fmt "@[<hv 4>%a@,(" (self#pp_declare_symbol cmode) f ;
            Plib.pp_listsep ~sep:","
              (fun fmt x ->
                 let a = self#bind x in
                 let t = T.tau_of_var x in
                 fprintf fmt "%a:%a" self#pp_var a self#pp_tau t
              ) fmt xs ;
            match cmode with
            | Cprop ->
                fprintf fmt ") =@ @[<hov 0>%a@]@]@\n"
                  self#pp_prop e
            | Cterm ->
                fprintf fmt ") :@ %a =@ @[<hov 0>%a@]@]@\n"
                  self#pp_tau t (self#pp_expr t) e
          end

    end

end
