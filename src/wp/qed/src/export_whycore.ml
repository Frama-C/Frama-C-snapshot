(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Common Exportation Engine for Alt-Ergo and Why3                    --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format
open Plib
open Linker
open Engine
open Export

module Make(T : Term) =
struct

  open T
  module T = T
  module E = Export.Make(T)

  type trigger = (T.var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  let rec full_trigger = function
    | TgAny -> false
    | TgVar _ -> true
    | TgGet(a,k) -> full_trigger a && full_trigger k
    | TgSet(a,k,v) -> full_trigger a && full_trigger k && full_trigger v
    | TgFun(_,xs) | TgProp(_,xs) -> List.for_all full_trigger xs

  let rec full_triggers = function
    | [] -> []
    | ts :: tgs ->
        match List.filter full_trigger ts with
        | [] -> full_triggers tgs
        | ts -> ts :: full_triggers tgs

  module TauMap = Map.Make
      (struct
        type t = T.tau
        let compare = Kind.compare_tau T.Field.compare T.ADT.compare
      end)

  class virtual engine =
    object(self)

      inherit E.engine

      initializer
        begin
          self#declare_all [ "int" ; "real" ; "bool" ; "prop" ] ;
        end

      (* -------------------------------------------------------------------------- *)
      (* --- Types                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method t_int = "int"
      method t_real = "real"
      method t_bool = "bool"
      method t_prop = "prop"

      method pp_tvar fmt k = 
        if 1 <= k && k <= 26 
        then fprintf fmt "'%c" (char_of_int (int_of_char 'a' + k - 1))
        else fprintf fmt "'_%d" k

      (* -------------------------------------------------------------------------- *)
      (* --- Scope                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method op_scope _ = None

      (* -------------------------------------------------------------------------- *)
      (* --- Arrays                                                             --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_array_get fmt a k = 
        fprintf fmt "@[<hov 2>%a[%a]@]" self#pp_atom a self#pp_flow k

      method pp_array_set fmt a k v = 
        fprintf fmt "@[<hov 2>%a[%a@ <- %a]@]"
          self#pp_atom a self#pp_atom k self#pp_flow v

      (* -------------------------------------------------------------------------- *)
      (* --- Records                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual op_record : string * string

      method pp_get_field fmt r f = 
        fprintf fmt "%a.%s" self#pp_atom r (self#field f)

      method pp_def_fields fmt fvs =
        let base,fvs = match T.record_with fvs with 
          | None -> None,fvs | Some(r,fvs) -> Some r,fvs in
        begin
          let (left,right) = self#op_record in
          fprintf fmt "@[<hov 2>%s" left ;
          Plib.iteri
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
          fprintf fmt "@ %s@]" right ; 
        end

      (* -------------------------------------------------------------------------- *)
      (* --- Higher Order                                                       --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_apply (_:cmode) (_:term) (_:formatter) (_:term list) =
        failwith "Qed.Export.Why: higher-order application"

      (* -------------------------------------------------------------------------- *)
      (* --- Higher Order                                                       --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_param fmt x =
        fprintf fmt "%a:%a" self#pp_var x self#pp_tau (T.tau_of_var x)

      method pp_lambda (_:formatter) (_:var list) =
        failwith "Qed.Export.Why : lambda abstraction"

      (* -------------------------------------------------------------------------- *)
      (* --- Declarations                                                       --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual pp_declare_adt : formatter -> ADT.t -> int -> unit
      method virtual pp_declare_def : formatter -> ADT.t -> int -> tau -> unit
      method virtual pp_declare_sum : formatter -> ADT.t -> int -> (Fun.t * tau list) list -> unit

      method declare_type fmt adt n = function
        | Tabs -> 
            self#pp_declare_adt fmt adt n ;
            pp_print_newline fmt ()
        | Tdef def -> 
            self#pp_declare_def fmt adt n def ;
            pp_print_newline fmt ()
        | Tsum cases -> 
            self#pp_declare_sum fmt adt n cases ;
            pp_print_newline fmt ()
        | Trec fts ->
            begin
              Format.fprintf fmt "@[<hv 0>@[<hv 2>" ;
              self#pp_declare_adt fmt adt n ;
              let left,right = self#op_record in
              fprintf fmt " = %s" left ;
              Plib.iteri
                (fun index (f,t) ->
                   match index with
                   | Isingle | Ilast -> 
                       fprintf fmt "@ @[<hov 2>%s : %a@]" (self#field f) self#pp_tau t
                   | Imiddle | Ifirst -> 
                       fprintf fmt "@ @[<hov 2>%s : %a@] ;" (self#field f) self#pp_tau t
                ) fts ;
              fprintf fmt "@] %s@]@\n" right ;
            end

      method pp_declare_symbol t fmt f =
        let name = declare_name (self#link f) in
        match t with
        | Cprop -> fprintf fmt "predicate %s" name
        | Cterm -> fprintf fmt "function %s" name

      method virtual pp_trigger : trigger printer
      method virtual pp_intros : tau -> var list printer (* forall with no separatyor *)

      method declare_prop ~kind fmt lemma xs tgs (p : term) =
        self#global
          begin fun () ->
            fprintf fmt "@[<hv 2>%s %s:" kind lemma ;
            let groups = List.fold_left
                (fun groups x ->
                   self#bind x ;
                   let t = T.tau_of_var x in
                   let xs = try TauMap.find t groups with Not_found -> [] in
                   TauMap.add t (x::xs) groups
                ) TauMap.empty xs in
            let order = TauMap.fold
                (fun t xs order -> (t,List.sort Var.compare xs)::order)
                groups [] in
            let tgs = full_triggers tgs in
            Plib.iteri
              (fun index (t,xs) ->
                 let do_triggers = match index with
                   | Ifirst | Imiddle -> false
                   | Isingle | Ilast -> tgs<>[] in
                 if do_triggers then
                   begin
                     let pp_or = Plib.pp_listcompact ~sep:"|" in
                     let pp_and = Plib.pp_listcompact ~sep:"," in
                     let pp_triggers = pp_or (pp_and self#pp_trigger) in
                     fprintf fmt "@ @[<hov 2>%a@]" (self#pp_intros t) xs ;
                     fprintf fmt "@ @[<hov 2>[%a].@]" pp_triggers tgs ;
                   end
                 else
                   fprintf fmt "@ @[<hov 2>%a.@]" (self#pp_intros t) xs
              ) order ;
            fprintf fmt "@ @[<hov 2>%a@]@]@\n" self#pp_prop p
          end

      method declare_axiom = self#declare_prop ~kind:"axiom"

    end

end
