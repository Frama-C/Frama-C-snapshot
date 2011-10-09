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

open Fol
open Fol_norm
open Formula
open Format

(* -------------------------------------------------------------------------- *)
(* --- Coq Export                                                         --- *)
(* -------------------------------------------------------------------------- *)

let get_ufield f = "get_"^(f.Cil_types.fname)^"_"^(f.Cil_types.fcomp.Cil_types.cname)
let set_ufield f = "set_"^(f.Cil_types.fname)^"_"^(f.Cil_types.fcomp.Cil_types.cname)

let constant fmt = function
  | ConstInt n ->
     let k = Big_int.big_int_of_string n in
     if Big_int.lt_big_int k Big_int.zero_big_int then
       pp_print_string fmt ("( "^n^" )")
     else pp_print_string fmt n
  | ConstBool b -> pp_print_string fmt (if b then "true" else "false")
  | ConstUnit -> pp_print_string fmt "void"
  | ConstFloat f -> fprintf fmt "%s%%R" (Kreal.convert f)

let pp_list pp fmt = function
  | [] -> ()
  | x::xs ->
      pp fmt x ;
      List.iter (fun x -> fprintf fmt " @,%a" pp x) xs

let pp_typelist pp fmt = function
  | [] -> ()
  | x::xs ->
      pp fmt x ;
      List.iter (fun x -> fprintf fmt " @,->%a" pp x) xs

let pp_flow fmt nil op pp = function
    | [] ->
        pp_print_string fmt nil
    | x::xs ->
        fprintf fmt "@[<hov 1>(%a" pp x ;
        List.iter (fun x -> fprintf fmt "@, %s%a" op pp x) xs ;
        fprintf fmt ")@]"

let pp_block fmt tab op pp ps =
  match ps with
    | [] -> ()
    | p::ps ->
        fprintf fmt "%s %a" tab pp p ;
        List.iter (fun p -> fprintf fmt "@\n%s %a" op pp p) ps

let pp_var fmt v =
  let name = Fol_decl.identifier (Var.var_name v) in
  fprintf fmt "%s" name

let rec collect_assoc op xs = function
  | [] -> List.rev xs
  | Tapp(id,ts) :: others when id = op ->
      collect_assoc op (collect_assoc op xs ts) others
  | t::others ->
      collect_assoc op (t::xs) others

let field f = "field_"^f.Cil_types.fname^"_"^f.Cil_types.fcomp.Cil_types.cname
let ufield f = "field_"^f.Cil_types.fname^"_"^f.Cil_types.fcomp.Cil_types.cname

let fpp_term term fmt t =
  match t with
    | Tconst c -> constant fmt c
    | Tvar v -> pp_var fmt v
    | Tapp (id, []) -> pp_print_string fmt id
    | Tapp ("ite",[c;a;b]) | Tif (c,a,b) ->
        fprintf fmt "(@[<v 0>if %a@ then %a@ else %a@])"
          term c term a term b
    | Tapp ("neg_int",[a]) ->
        fprintf fmt "@[<hov 1> (-%a)@]" term a
    | Tapp ("add_int", ts) ->
        let xs = collect_assoc "add_int" [] ts in
        pp_flow fmt "0" "+" term xs
    | Tapp ("sub_int", [a;b]) ->
        fprintf fmt "@[<hov 1>(%a@ -%a)@]" term a term b
    | Tapp ("mul_int", ts) ->
        let xs = collect_assoc "mul_int" [] ts in
        pp_flow fmt "1" "*" term xs
    | Tapp (id, t::ts) ->
        fprintf fmt "@[<hov 2>(%s@, %a" id term t ;
        List.iter (fun t -> fprintf fmt " @, %a" term t) ts ;
        fprintf fmt ")@]"
    | Tlet (x,v,t) ->
        fprintf fmt "(@[<v 0>let %a@ := %a@ in %a@])"
          pp_var x term v term t
    | Taccess(t,i) ->
        fprintf fmt "@[<hov 2>(access@, %a@, %a)@]" term t term i
    | Tupdate(t,i,v)  ->
        fprintf fmt "@[<hov 2>(update@, %a@, %a@, %a)@]" term t term i term v
    | Tgetfield(f,r) ->
        if f.Cil_types.fcomp.Cil_types.cstruct then
        fprintf fmt "@[<hov 2>(%a.(%s))@]" term r (field f)
        else (fprintf fmt "@[<hov 2>(%s @, %a)@]"(get_ufield f) term r)
    | Tsetfield(f,r,v) ->
        let cp = f.Cil_types.fcomp in
        if cp.Cil_types.cstruct then
          begin
            let built_rec = "mk"^(String.capitalize cp.Cil_types.cname) in
            fprintf fmt "@[<hov 2> ( %s " built_rec;
            List.iter (fun g ->
                         (if Cil_datatype.Fieldinfo.equal g f then
                            Format.fprintf fmt"(%a)@]" term v
                          else
                            Format.fprintf fmt "%a.(%s)" term r (field g))
                      ) cp.Cil_types.cfields   ;
            fprintf fmt ")@]"
          end
        else fprintf fmt "@[<hov 2>(%s @, %a @, %a)@]"(set_ufield f) term r term v

let rec collect_or ps = function
  | Por(a,b) -> collect_or (collect_or ps b) a
  | p -> p :: ps

let rec collect_and ps = function
  | Pand(a,b) -> collect_and (collect_and ps b) a
  | p -> p :: ps

let rec collect_imply ps = function
  | Pimplies(a,b) -> collect_and (collect_imply ps b) a
  | p -> p :: ps

let rec collect_iff ps = function
  | Piff(a,b) -> collect_iff (collect_iff ps b) a
  | p -> p :: ps

type 'a pp = Format.formatter -> 'a -> unit
type pp_env = {
  pp_type : Formula.tau pp ;
  pp_term : term pp ;
  pp_pred : pred pp ;
}

let pp_args pp_tau fmt = function
  | [] -> ()
  | [x] -> Format.fprintf fmt "(%a@,:@,%a)@,:%a@,=@,"
      pp_var x pp_tau (Var.var_type x) pp_tau (Var.var_type x)
  | x::m ->
      Format.fprintf fmt "(%a@,:@,%a)@,"
       pp_var x pp_tau (Var.var_type x);
      List.iter (fun x -> Format.fprintf fmt "@,(%a@,:@,%a)@,"
                   pp_var x pp_tau (Var.var_type x) ) m;
      Format.fprintf fmt "@,:%a@,=@\n" pp_tau (Var.var_type x)

open Fol_cc

let fpp_f_let pp_tau pp_term fmt fl =
  Format.fprintf fmt "@[<hov 2>Definition %s@,%a%a@]"
    fl.name (pp_args pp_tau) fl.param
    pp_term fl.body

let fpp_lf_let pp_tau pp_term fmt = function
  | [] -> ()
  | x::xs ->
      let pp = fpp_f_let pp_tau pp_term in
      pp fmt x ;
      List.iter (fun x -> fprintf fmt "%a@\n" pp x ) xs

let rec epp_pred_vbox env fmt p =
  match p with
    | Pand _ -> pp_block fmt "   " " /\\ " env.pp_pred (collect_and [] p)
    | Por _ -> pp_block fmt "  " " \\/ " env.pp_pred (collect_or [] p)
    | Pimplies _ -> pp_block fmt "  " "->" env.pp_pred (collect_imply [] p)
    | Piff _ -> pp_block fmt "   " "<->" env.pp_pred (collect_iff [] p)
    | Pforall(x,p) ->
        fprintf fmt "forall (%a:%a),@\n" pp_var x env.pp_type (Var.var_type x) ;
        epp_pred_vbox env fmt p
    | Pexists(x,p) ->
        fprintf fmt "exists %a:%a,@\n" pp_var x env.pp_type (Var.var_type x) ;
        epp_pred_vbox env fmt p
    | Plet(x,t,p) ->
        fprintf fmt "@[<hov 2>let %a:=@ %a@ in @]@\n" pp_var x env.pp_term t ;
        epp_pred_vbox env fmt p
    | Pif(t,p,q) ->
        fprintf fmt "@[<hov 0>if @[<hov 2>%a@]@ then@]@\n  %a@\nelse@\n  %a"
          env.pp_term t env.pp_pred p env.pp_pred q
    | (Ptrue | Pfalse | Papp _ | Pnot _ | Pnamed _) ->
        env.pp_pred fmt p

let rec epp_pred_atom env fmt p =
  match p with
    | Pand _ | Por _ | Pimplies _ | Piff _ | Pif _
    | Pforall _ | Pexists _ | Plet _ ->
        fprintf fmt "@[<v 1>(%a)@]" (epp_pred_vbox env) p
    | Pnot p ->
        fprintf fmt "@[<hov 2>(~@ %a)@]" (epp_pred_atom env) p
    | Pnamed(tag,p) ->
        fprintf fmt "@[<hov 0>(*%s:*)@,%a@]" tag (epp_pred_atom env) p
    | Ptrue -> pp_print_string fmt "True"
    | Pfalse -> pp_print_string fmt "False"
    | Papp(id,[]) -> pp_print_string fmt id
    | Papp (("eq" | "eq_int" | "eq_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ =@ %a)@]" env.pp_term t1 env.pp_term t2
    | Papp (("neq" | "neq_int" | "neq_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <>@ %a)@]" env.pp_term t1 env.pp_term t2
    | Papp (("lt_int"| "lt_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <@ %a)@]" env.pp_term t1 env.pp_term t2
    | Papp (("le_int"| "le_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <=@ %a)@]" env.pp_term t1 env.pp_term t2
    | Papp(id,t::ts) ->
        fprintf fmt "@[<hov 2>(%s @,%a" id env.pp_term t ;
        List.iter (fun t -> fprintf fmt "@ %a" env.pp_term t) ts ;
        fprintf fmt ")@]"

let fpp_pred predicate pp_term pp_type fmt p =
  match p with
    | Ptrue -> fprintf fmt "True"
    | Pfalse -> fprintf fmt "False"
    | Papp (id, [])-> fprintf fmt "%s" id
    | Papp ("eq", [t1; t2]) -> fprintf fmt "(%a =@ %a)" pp_term t1 pp_term t2
    | Papp ("neq", [t1; t2]) -> fprintf fmt "(%a <>@ %a)" pp_term t1 pp_term t2
    | Papp (id, l) -> fprintf fmt "@[(%s @, %a)@]" id (pp_list pp_term) l
    | Pimplies (a, b) -> fprintf fmt "(@[%a ->@ %a@])" predicate a predicate b
    | Piff (a, b) -> fprintf fmt "(@[%a <->@ %a@])" predicate a predicate b
    | Pand (a, b) -> fprintf fmt "(@[%a /\\ @ %a@])" predicate a predicate b
    | Por (a, b) -> fprintf fmt "(@[%a \\/ @ %a@])" predicate a predicate b
    | Pnot a -> fprintf fmt "(~ %a)" predicate a
    | Pif (a, b, c) ->
        fprintf fmt "(@[if %a then@ %a else@ %a@])"
          pp_term a predicate b predicate c
    | Pforall (v,p) ->
        fprintf fmt "@[<hov 0>(forall (%a:%a),@ %a@])"
          pp_var v pp_type (Var.var_type v) predicate p
    | Pexists (v,p) ->
        fprintf fmt "@[<hov 0>(exists %a:%a,@ %a@])"
          pp_var v pp_type (Var.var_type v) predicate p
    | Plet (x,v,p) ->
        fprintf fmt "@[<hov 0>(let %a :=@[<hov 2>@ %a@ in@]@ %a@])"
          pp_var x pp_term v predicate p
    | Pnamed (n, p) ->
        fprintf fmt "@[(*%s:*) %a@]" n predicate p


let rec fpp_fields pp_tau tau_of_ctype_logic fmt = function
  | [] -> ()
  | [f] -> Format.fprintf fmt "%s@,:@,%a@\n" (field f) pp_tau
      (tau_of_ctype_logic f.Cil_types.ftype)
  | f::m ->
      Format.fprintf fmt "%s@,:@,%a;@\n" (field f) pp_tau
        (tau_of_ctype_logic f.Cil_types.ftype);
      fpp_fields pp_tau tau_of_ctype_logic fmt m

let pp_param pp_tau fmt x =
  Format.fprintf fmt "%a:%a" pp_var x pp_tau (Fol.Var.var_type x)

let fpp_item predicate pp_tau tau_of_ctype_logic pp_term fmt x =
  function
    | Formula.Cons k ->
        fprintf fmt "Definition %s:Z:= %d.@\n" x k
    | Formula.Function ([], t) ->
        fprintf fmt "Parameter %s: %a.@\n" x pp_tau t
    | Formula.Function (tl, t) ->
        fprintf fmt "Parameter %s: @[<hov 0>%a -> %a@].@\n" x (pp_typelist pp_tau) tl pp_tau t
    | Formula.Predicate [] ->
        fprintf fmt "Parameter %s: Prop.@\n" x
    | Formula.Predicate tl ->
        fprintf fmt "Parameter %s: @[<hov 0>%a -> Prop.@]@\n" x (pp_typelist pp_tau) tl
    | Formula.FunctionDef (xs,tr,exp) ->
	Format.fprintf fmt "@[<hv 2>Definition %s (%a) : %a :=@ @[<hov 0>%a.@]@]@\n"
	  x (pp_list (pp_param pp_tau)) xs pp_tau tr pp_term exp
    | Formula.PredicateDef (xs,prop) ->
	Format.fprintf fmt "@[<hv 2>Definition %s (%a): Prop :=@ @[<hov 0>%a.@]@]@\n"
	  x (pp_list (pp_param pp_tau)) xs predicate prop
    | Formula.Axiom p ->
        begin
          match Fol_norm.compile p with
            | Pred p' -> fprintf fmt "@[<hv 2>Axiom %s:@ %a.@\n@]@\n" x predicate p'
            | Conv (defs,p') ->
                fpp_lf_let pp_tau pp_term fmt defs  ;
                fprintf fmt "@[<hv 2>Axiom %s:@ %a.@\n@]@\n" x predicate p'
        end
    | Formula.Type 0 ->
        fprintf fmt "Definition %s:=Set.@\n" x
    | Formula.Type n ->
        fprintf fmt "@[<hov 2>Definition %s:=Set" x;
        for k=1 to n do fprintf fmt " -> Set" done ;
        fprintf fmt ".@]@\n"
    | Formula.Trecord c ->
        let rname = String.capitalize c.Cil_types.cname in
        if c.Cil_types.cstruct then
          begin
            fprintf fmt "@[<hov 2> Record %s : Set := mk%s@\n" rname rname ;
            fprintf fmt "{ @\n" ;
            fpp_fields pp_tau tau_of_ctype_logic fmt c.Cil_types.cfields ;
            fprintf fmt "}.@]@\n"
          end
        else
          begin
            fprintf fmt "@[<hov 2> Definition %s:=Set.@\n" rname;
            let l =  c.Cil_types.cfields in
            List.iter (fun f ->
                         let fd = field f in
                         let t = tau_of_ctype_logic f.Cil_types.ftype in
                         Format.pp_print_newline fmt () ;
                         fprintf fmt "Parameter %s: %s -> %a.@\n "
                           (get_ufield f) rname pp_tau t;
                         Format.pp_print_newline fmt () ;
                         fprintf fmt "Parameter %s: %s -> %a -> %s.@\n "
                           (set_ufield f) rname pp_tau t rname;
                         Format.pp_print_newline fmt () ;
                         fprintf fmt "Axiom get_set_same_%s:@\n" fd;
                         fprintf fmt " forall r v, %s (%s r v) = v.@\n"
                           (get_ufield f) (set_ufield f)
                      ) l

          end



let fpp_header fmt d =
  begin
    d.d_title fmt ;
    Format.pp_print_newline fmt () ;
    ( match d.d_source with
        | Some { Lexing.pos_fname=f ; Lexing.pos_lnum=k } ->
            Format.fprintf fmt "%s:%d: " f k
        | None -> () ) ;
    d.d_descr fmt ;
  end

let fpp_decl predicate pp_tau tau_of_ctype_logic pp_term fmt d =
  begin
    fpp_header fmt d ;
    Format.pp_print_newline fmt () ;
    fpp_item predicate pp_tau tau_of_ctype_logic pp_term fmt d.d_name d.d_item ;
    Format.pp_print_newline fmt () ;
  end

let fpp_goal predicate fmt x p =
  fprintf fmt "@[<hv 2>Lemma %s:@ %a.@]@." x predicate p


module ECoq(L:sig val tau_of_ctype_logic : Cil_types.typ -> Formula.tau end) =
struct

type pred = Fol.pred
type decl = Fol.decl
	 
let rec export_tau fmt = function
  | Integer -> pp_print_string fmt "Z"
  | Real -> pp_print_string fmt "R"
  | Boolean -> pp_print_string fmt "bool"
  | Pointer t -> export_tau fmt t
  | Record c ->
      let rname = String.capitalize c.Cil_types.cname in
      Format.fprintf fmt "%s" rname
  | Array arr ->
      Format.fprintf fmt "(array %a)"
        export_tau (L.tau_of_ctype_logic arr.Ctypes.arr_element)
  | Set te ->
      Format.fprintf fmt "(set %a)" export_tau te
  | ADT(s,[]) -> pp_print_string fmt s
  | ADT("farray",[t]) -> Format.fprintf fmt "(array %a)" export_tau t
  | ADT(s,[t]) -> Format.fprintf fmt "(%s %a)" s export_tau t
  | ADT(s,t::ts) ->
      Format.fprintf fmt "@ (%s " s;
      Format.fprintf fmt "@ %a " export_tau t ;
      List.iter (fun t -> Format.fprintf fmt "@ %a" export_tau t) ts;
      Format.fprintf fmt ")"


let rec export_term fmt t = fpp_term export_term fmt t

let rec pp_pred_atom fmt p =
  epp_pred_atom {
    pp_type = export_tau;
    pp_term = export_term ;
    pp_pred = pp_pred_atom;
  } fmt p

let export_pred fmt p =
  epp_pred_vbox {
    pp_type = export_tau;
    pp_term = export_term ;
    pp_pred = pp_pred_atom;
  } fmt p

let export_item fmt name item = fpp_item export_pred export_tau fmt name item

let export_section fmt title =
  begin
    Format.fprintf fmt "(*----------------------------------------*)@\n" ;
    Format.fprintf fmt "(*--- %-32s --- *)@\n" title ;
    Format.fprintf fmt "(*----------------------------------------*)@\n" ;
  end

let export_decl fmt d =
  Pretty_utils.pp_trail fpp_header fmt d ;
  Format.pp_print_newline fmt () ;
    fpp_item export_pred export_tau L.tau_of_ctype_logic export_term fmt d.d_name d.d_item

let export_goal fmt x g =
  match Fol_norm.compile g with
    | Pred p' -> fpp_goal export_pred fmt x p'
    | Conv (defs,p') ->
        fpp_lf_let export_tau export_term fmt defs  ;
        fpp_goal export_pred fmt x p'
end
