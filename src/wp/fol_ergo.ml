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


open Format

(* -------------------------------------------------------------------------- *)
(* --- Alt-ergo Export                                                    --- *)
(* -------------------------------------------------------------------------- *)

let get_field f = "get_"^(f.Cil_types.fname)^"_"^(f.Cil_types.fcomp.Cil_types.cname)
let set_field f = "set_"^(f.Cil_types.fname)^"_"^(f.Cil_types.fcomp.Cil_types.cname)


let constant fmt = function
  | Fol.ConstInt n -> 
      if n.[0] = '-'
      then Format.fprintf fmt "(%s)" n
      else pp_print_string fmt n
  | Fol.ConstBool b -> pp_print_string fmt (if b then "true" else "false")
  | Fol.ConstUnit -> pp_print_string fmt "void"
  | Fol.ConstFloat f -> 
      if f.[0] = '-' 
      then Format.fprintf fmt "(%s)" f
      else pp_print_string fmt f

let pp_list pp fmt = function
  | [] -> ()
  | x::xs ->
      pp fmt x ;
      List.iter (fun x -> fprintf fmt ",@,%a" pp x) xs

let pp_flow fmt nil op pp = function
    | [] ->
        pp_print_string fmt nil
    | x::xs ->
        fprintf fmt "@[<hov 1>(%a" pp x ;
        List.iter (fun x -> fprintf fmt "@,%s%a" op pp x) xs ;
        fprintf fmt ")@]"

let pp_block fmt tab op pp ps =
  match ps with
    | [] -> ()
    | p::ps ->
        fprintf fmt "%s %a" tab pp p ;
        List.iter (fun p -> fprintf fmt "@\n%s %a" op pp p) ps

let pp_var fmt v =
  let name = Fol_decl.identifier (Fol.Var.var_name v) in
  fprintf fmt "%s" name

let rec collect_assoc op xs = function
  | [] -> List.rev xs
  | Fol.Tapp(id,ts) :: others when id = op ->
      collect_assoc op (collect_assoc op xs ts) others
  | t::others ->
      collect_assoc op (t::xs) others

let rec fpp_term fmt t =
  match t with
    | Fol.Tconst c -> constant fmt c
    | Fol.Tvar v -> pp_var fmt v
    | Fol.Tapp (id, []) -> pp_print_string fmt id
    | Fol.Tapp ("ite",[c;a;b]) | Fol.Tif (c,a,b) ->
        fprintf fmt "(@[<v 0>if %a@ then %a@ else %a@])"
          fpp_term c fpp_term a fpp_term b
	  
    (* INT *)
    | Fol.Tapp ("neg_int", [t]) ->
        fprintf  fmt "@[<hov 1>(-%a)@]" fpp_term t
    | Fol.Tapp ("add_int", ts) ->
        let xs = collect_assoc "add_int" [] ts in
        pp_flow fmt "0" "+" fpp_term xs
    | Fol.Tapp ("sub_int", [a;b]) ->
        fprintf fmt "@[<hov 1>(%a@,-%a)@]" fpp_term a fpp_term b
    | Fol.Tapp ("mul_int", ts) ->
        let xs = collect_assoc "mul_int" [] ts in
        pp_flow fmt "1" "*" fpp_term xs

    (* REAL *)
    | Fol.Tapp ("neg_real", [t]) ->
	fprintf  fmt "@[<hov 1>(-%a)@]" fpp_term t
    | Fol.Tapp ("add_real", ts) ->
        let xs = collect_assoc "add_real" [] ts in
        pp_flow fmt "0.0" "+" fpp_term xs
    | Fol.Tapp ("sub_real", [a;b]) ->
        fprintf fmt "@[<hov 1>(%a@,-%a)@]" fpp_term a fpp_term b
    | Fol.Tapp ("mul_real", ts) ->
        let xs = collect_assoc "mul_real" [] ts in
        pp_flow fmt "1.0" "*" fpp_term xs
    | Fol.Tapp ("div_real", [a;b]) ->
        fprintf fmt "@[<hov 1>(%a@,/%a)@]" fpp_term a fpp_term b

    (* OTHER *)
    | Fol.Taccess(a,k) ->
        Format.fprintf fmt "@[<hv 2>%a[%a]@]" fpp_term a fpp_term k
    | Fol.Tupdate(a,k,b) ->
        Format.fprintf fmt "@[<hv 2>%a[%a@,<-%a]@]"
          fpp_term a fpp_term k fpp_term b
    | Fol.Tgetfield(f,r) ->
        Format.fprintf fmt "@[<hv 2>%s(%a)@]" (get_field f) fpp_term r
    | Fol.Tsetfield(f,r,v) ->
        Format.fprintf fmt "@[<hv 2>%s(%a,%a)@]" (set_field f) fpp_term r fpp_term v
    | Fol.Tapp (id, t::ts) ->
        fprintf fmt "@[<hov 2>%s(@,%a" id fpp_term t ;
        List.iter (fun t -> fprintf fmt ",@,%a" fpp_term t) ts ;
        fprintf fmt ")@]"
    | Fol.Tlet (x,v,t) ->
        fprintf fmt "(@[<hov 0>let %a=%a in@ %a@])"
          pp_var x fpp_term v fpp_term t

let rec collect_or ps = function
  | Fol.Por(a,b) -> collect_or (collect_or ps b) a
  | p -> p :: ps

let rec collect_and ps = function
  | Fol.Pand(a,b) -> collect_and (collect_and ps b) a
  | p -> p :: ps

let rec collect_imply ps = function
  | Fol.Pimplies(a,b) -> collect_and (collect_imply ps b) a
  | p -> p :: ps

let rec collect_iff ps = function
  | Fol.Piff(a,b) -> collect_iff (collect_iff ps b) a
  | p -> p :: ps


type 'a pp = Format.formatter -> 'a -> unit
type pp_env = {
  pp_type : Formula.tau pp ;
  pp_term : Fol.term pp ;
  pp_pred : Fol.pred pp ;
}

open Fol_cc

let rec export_tau tau_of_ctype_logic fmt = function
  | Formula.Integer -> pp_print_string fmt "int"
  |  Formula.Real -> pp_print_string fmt "real"
  |  Formula.Boolean -> pp_print_string fmt "bool"
  |  Formula.Pointer t -> (export_tau tau_of_ctype_logic) fmt t
  |  Formula.Record c -> Format.fprintf  fmt "%s" c.Cil_types.cname
  |  Formula.Array arr ->
      let t = tau_of_ctype_logic arr.Ctypes.arr_element in
      Format.fprintf fmt "%a farray" (export_tau tau_of_ctype_logic) t
  | Formula.Set te ->
      Format.fprintf fmt "%a set" (export_tau tau_of_ctype_logic) te
  | Formula.ADT("farray",[t]) ->
      Format.fprintf fmt "%a farray" (export_tau tau_of_ctype_logic) t
  | Formula.ADT(s,[]) -> pp_print_string fmt s
  | Formula.ADT(s,[t]) -> Format.fprintf fmt "%a %s" (export_tau tau_of_ctype_logic) t s
  | Formula.ADT(s,t::ts) ->
      Format.fprintf fmt "@[(%a"(export_tau tau_of_ctype_logic)  t ;
      List.iter (fun t -> Format.fprintf fmt ",@,%a" ( export_tau tau_of_ctype_logic) t) ts ;
      Format.fprintf fmt ") %s@]" s


let pp_args tau_of_ctype_logic fmt = function
  | [] -> ()
  | [x] -> Format.fprintf fmt "(%a@,:@,%a)@,:%a@,=@,"
      pp_var x (export_tau tau_of_ctype_logic) (Fol.Var.var_type x)
               (export_tau tau_of_ctype_logic) (Fol.Var.var_type x)
  | x::m ->
      Format.fprintf fmt "(%a@,:@,%a@,"
       pp_var x (export_tau tau_of_ctype_logic) (Fol.Var.var_type x);
      List.iter (fun x -> Format.fprintf fmt ",@,%a@,:@,%a@,"
                   pp_var x (export_tau tau_of_ctype_logic) (Fol.Var.var_type x) ) m;
      Format.fprintf fmt ")@,:%a@,=@\n" (export_tau tau_of_ctype_logic) (Fol.Var.var_type x)

let fpp_f_let tau_of_ctype_logic fmt fl =
  Format.fprintf fmt "@[<hov 2>function %s@,%a%a@]"
    fl.name (pp_args tau_of_ctype_logic) fl.param
    fpp_term fl.body

let fpp_lf_let  tau_of_ctype_logic fmt = function
  | [] -> ()
  | x::xs ->
      let pp = fpp_f_let tau_of_ctype_logic in
      pp fmt x ;
      List.iter (fun x -> fprintf fmt "%a@\n" pp x ) xs


let rec epp_pred_vbox env fmt p =
  match p with
    | Fol.Pand _ -> pp_block fmt "   " "and" env.pp_pred (collect_and [] p)
    | Fol.Por _ -> pp_block fmt "  " "or" env.pp_pred (collect_or [] p)
    | Fol.Pimplies _ -> pp_block fmt "  " "->" env.pp_pred (collect_imply [] p)
    | Fol.Piff _ -> pp_block fmt "   " "<->" env.pp_pred (collect_iff [] p)
    | Fol.Pforall(x,p) ->
        fprintf fmt "forall %a:%a.@\n" pp_var x env.pp_type (Fol.Var.var_type x) ;
        epp_pred_vbox env fmt p
    | Fol.Pexists(x,p) ->
        fprintf fmt "exists %a:%a.@\n" pp_var x env.pp_type (Fol.Var.var_type x) ;
        epp_pred_vbox env fmt p
    |Fol.Pif(t,p,q) ->
        fprintf fmt "@[<hov 0>if @[<hov 2>%a@]@ then@]@\n  %a@\nelse@\n  %a"
          env.pp_term t env.pp_pred p env.pp_pred q
    | (Fol.Ptrue | Fol.Pfalse | Fol.Papp _ | Fol.Pnot _ | Fol.Pnamed _) ->
        env.pp_pred fmt p
    | Fol.Plet(x,t,p) ->
        fprintf fmt "let %a = @[%a@] in@\n" pp_var x env.pp_term t ;
        epp_pred_vbox env fmt p

(*TODO : ensures that label are different from keywords of the host language*)
let tag_named tag = "tag_"^tag


let rec epp_pred_atom env fmt p =
  match p with
    | Fol.Pand _ | Fol.Por _ | Fol.Pimplies _ | Fol.Piff _ | Fol.Pif _
    | Fol.Pforall _ | Fol.Pexists _ | Fol.Plet _ ->
        fprintf fmt "@[<v 1>(%a)@]" (epp_pred_vbox env) p
    | Fol.Pnot p ->
        fprintf fmt "@[<hov 2>(not@ %a)@]" (epp_pred_atom env) p
    | Fol.Pnamed(tag,p) ->
        fprintf fmt "@[<hov 0>%s:@,%a@]" (tag_named tag) (epp_pred_atom env) p
    | Fol.Ptrue -> pp_print_string fmt "true"
    | Fol.Pfalse -> pp_print_string fmt "false"
    | Fol.Papp(id,[]) -> pp_print_string fmt id
    | Fol.Papp (("eq" | "eq_int" | "eq_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ =@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("neq" | "neq_int" |"neq_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <>@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("lt_int"| "lt_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("le_int"|"le_real"), [t1; t2]) ->
        fprintf fmt "@[<hov 1>(%a@ <=@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp(id,t::ts) ->
        fprintf fmt "@[<hov 2>%s(@,%a" id env.pp_term t ;
        List.iter (fun t -> fprintf fmt ",@ %a" env.pp_term t) ts ;
        fprintf fmt ")@]"

let fpp_pred predicate tau_of_ctype_logic fmt p =
  match p with
    | Fol.Ptrue -> fprintf fmt "true"
    | Fol.Pfalse -> fprintf fmt "false"
    | Fol.Papp (id, [])-> fprintf fmt "%s" id
    | Fol.Papp ("eq", [t1; t2]) -> fprintf fmt "(%a =@ %a)" fpp_term t1 fpp_term t2
    | Fol.Papp ("neq", [t1; t2]) -> fprintf fmt "(%a <>@ %a)" fpp_term t1 fpp_term t2
    | Fol.Papp (id, l) -> fprintf fmt "@[%s(%a)@]" id (pp_list fpp_term) l
    | Fol.Pimplies (a, b) -> fprintf fmt "(@[%a ->@ %a@])" predicate a predicate b
    | Fol.Piff (a, b) -> fprintf fmt "(@[%a <->@ %a@])" predicate a predicate b
    | Fol.Pand (a, b) -> fprintf fmt "(@[%a and@ %a@])" predicate a predicate b
    | Fol.Por (a, b) -> fprintf fmt "(@[%a or@ %a@])" predicate a predicate b
    | Fol.Pnot a -> fprintf fmt "(not %a)" predicate a
    | Fol.Pif (a, b, c) ->
        fprintf fmt "(@[if %a then@ %a else@ %a@])"
          fpp_term a predicate b predicate c
    | Fol.Pforall (v,p) ->
        fprintf fmt "@[<hov 0>(forall %a:%a.@ %a@])"
          pp_var v (export_tau tau_of_ctype_logic) (Fol.Var.var_type v) predicate p
    | Fol.Pexists (v,p) ->
        fprintf fmt "@[<hov 0>(exists %a:%a.@ %a@])"
          pp_var v(export_tau tau_of_ctype_logic)  (Fol.Var.var_type v) predicate p
    | Fol.Plet (x,v,p) ->
        fprintf fmt "@[<hov 0>(let %a=%a in@ %a@])"
          pp_var x fpp_term v predicate p
    | Fol.Pnamed (n, p) ->
        fprintf fmt "@[%s: %a@]" (tag_named n) predicate p

let export_get_set_field tau_of_ctype_logic fmt f =
  let cn = f.Cil_types.fcomp.Cil_types.cname in
  let fn = tau_of_ctype_logic f.Cil_types.ftype in
  let get_f = get_field f in
  let set_f = set_field f in
  Format.fprintf fmt "logic %s: %s -> %a @\n"
 get_f cn (export_tau tau_of_ctype_logic) fn;
  Format.fprintf fmt "logic %s: %s , %a -> %s @\n" set_f cn
  (export_tau tau_of_ctype_logic) fn cn


let export_get_set_other tau_of_ctype_logic fmt f get_f g =
  let set_g = set_field g in
  Format.pp_print_newline fmt () ;
  Format.fprintf fmt
    "(* Definition of the commutativity of the get field %s over the set field %s*)@\n"
    f.Cil_types.fname g.Cil_types.fname;
  Format.pp_print_newline fmt () ;
  Format.fprintf fmt "axiom GetSetOther_%s_%s@,:@\n" f.Cil_types.fname g.Cil_types.fname;
  Format.fprintf fmt "forall r:%s.@,forall v:%a.@\n"
    f.Cil_types.fcomp.Cil_types.cname
    (export_tau tau_of_ctype_logic) (tau_of_ctype_logic g.Cil_types.ftype);
  Format.fprintf fmt "%s(%s(r,v))@,=@, %s(r)@\n" get_f set_g get_f;
  Format.pp_print_newline fmt ()

let export_generated_axiomatics tau_of_ctype_logic fmt f =
    let get_f = get_field f in
    let set_f = set_field f in
    Format.fprintf fmt
            "(* Definition of the good properties of the field %s*)@\n" f.Cil_types.fname;
    Format.pp_print_newline fmt () ;
    Format.fprintf fmt "axiom GetSetSame_%s@,:@\n" f.Cil_types.fname;
    Format.fprintf fmt "forall r:%s.@,forall v:%a.@\n"
      f.Cil_types.fcomp.Cil_types.cname
      (export_tau tau_of_ctype_logic) (tau_of_ctype_logic f.Cil_types.ftype);
    Format.fprintf fmt "%s(%s(r,v))@,=@, v@\n" get_f set_f ;
    Format.pp_print_newline fmt () ;
    if f.Cil_types.fcomp.Cil_types.cstruct then
    (List.iter (fun g ->
                  if Cil_datatype.Fieldinfo.equal f g then () else
                    export_get_set_other tau_of_ctype_logic fmt f get_f g )
       f.Cil_types.fcomp.Cil_types.cfields ;)
    else ();
    Format.pp_print_newline fmt () ;
    Format.pp_print_newline fmt ()

let pp_param tau_of_ctype_logic fmt x =
  Format.fprintf fmt "%a:%a" pp_var x (export_tau tau_of_ctype_logic) (Fol.Var.var_type x)
    
let fpp_item term predicate tau_of_ctype_logic fmt x =
  function	
    | Formula.Cons k ->
        fprintf fmt "function %s (): int = %d@\n" x k
    | Formula.Function ([], t) ->
        fprintf fmt "logic %s: %a@\n" x (export_tau tau_of_ctype_logic) t
    | Formula.Function (tl, t) ->
        fprintf fmt "logic %s: @[<hov 0>%a -> %a@]@\n" x
          (pp_list (export_tau tau_of_ctype_logic) ) tl (export_tau tau_of_ctype_logic)  t
    | Formula.Predicate [] ->
        fprintf fmt "logic %s: prop@\n" x
    | Formula.Predicate tl ->
        fprintf fmt "logic %s: @[<hov 0>%a -> prop@]@\n" x
          (pp_list (export_tau  tau_of_ctype_logic)) tl
    | Formula.FunctionDef (xs,tr,exp) ->
	Format.fprintf fmt "@[<hv 2>function %s (%a) : %a =@ @[<hov 0>%a@]@]@\n"
	  x (pp_list (pp_param tau_of_ctype_logic)) xs (export_tau tau_of_ctype_logic) tr term exp
    | Formula.PredicateDef (xs,prop) ->
	Format.fprintf fmt "@[<hv 2>predicate %s (%a) =@ @[<hov 0>%a@]@]@\n"
	  x (pp_list (pp_param tau_of_ctype_logic)) xs predicate prop
    | Formula.Axiom p ->
       begin
         match Fol_norm.compile p with
           | Fol_norm.Pred p' -> fprintf fmt "@[<hv 2>axiom %s:@ %a@]@\n" x predicate p'
           | Fol_norm.Conv (defs,p') ->
               fpp_lf_let tau_of_ctype_logic fmt defs  ;
               fprintf fmt "@[<hv 2>axiom %s:@ %a@]@\n" x predicate p'
        end
    | Formula.Type 0 ->
        fprintf fmt "type %s@\n" x
    | Formula.Type 1 ->
        fprintf fmt "type 'a %s@\n" x
    | Formula.Type n ->
        fprintf fmt "@[<hov 2>type ('a" ;
        for k=2 to n do
          fprintf fmt ",%c" (char_of_int (int_of_char 'a'+k-1))
        done ;
        Format.fprintf fmt ") %s@]@\n" x
    | Formula.Trecord c ->
        begin
          Format.fprintf fmt "type %s@\n" c.Cil_types.cname ;
          let l = c.Cil_types.cfields in
          List.iter (fun f -> export_get_set_field  tau_of_ctype_logic fmt f) l ;
          List.iter (fun f -> export_generated_axiomatics  tau_of_ctype_logic fmt f) l
        end


let fpp_header fmt d =
  begin
    d.Formula.d_title fmt ;
    Format.pp_print_newline fmt () ;
    ( match d.Formula.d_source with
        | Some { Lexing.pos_fname=f ; pos_lnum=k } ->
            Format.fprintf fmt "%s:%d: " f k
        | None -> () ) ;
    d.Formula.d_descr fmt ;
  end

let fpp_decl term predicate  tau_of_ctype_logic fmt d =
  begin
    fpp_header fmt d ;
    Format.pp_print_newline fmt () ;
    fpp_item term predicate tau_of_ctype_logic fmt d.Formula.d_name d.Formula.d_item ;
    Format.pp_print_newline fmt () ;
  end

let fpp_goal predicate fmt x p =
  fprintf fmt "@[<hv 2>goal %s:@ %a@]@." x predicate p

let export_term fmt t = fpp_term fmt t

let rec pp_pred_atom  tau_of_ctype_logic fmt p =
  epp_pred_atom {
    pp_type = export_tau  tau_of_ctype_logic;
    pp_term = export_term ;
    pp_pred = pp_pred_atom tau_of_ctype_logic ;
  } fmt p

let export_pred  tau_of_ctype_logic fmt p =
  epp_pred_vbox {
    pp_type = export_tau  tau_of_ctype_logic;
    pp_term = export_term ;
    pp_pred = pp_pred_atom  tau_of_ctype_logic;
  } fmt p

let export_item  tau_of_ctype_logic fmt name item =
fpp_item (export_pred tau_of_ctype_logic) fmt name item

let export_decl tau_of_ctype_logic fmt d =
  Pretty_utils.pp_trail fpp_header fmt d ;
  Format.pp_print_newline fmt () ;
  fpp_item export_term (export_pred tau_of_ctype_logic) tau_of_ctype_logic fmt d.Formula.d_name d.Formula.d_item

let export_goal tau_of_ctype_logic fmt x g=
  match Fol_norm.compile g with
    | Fol_norm.Pred p' ->  fpp_goal (export_pred tau_of_ctype_logic) fmt x p'
    | Fol_norm.Conv (defs,p') ->
        fpp_lf_let tau_of_ctype_logic fmt defs  ;
        fpp_goal (export_pred tau_of_ctype_logic) fmt x p'


module Make(L: sig val tau_of_ctype_logic : Cil_types.typ -> Formula.tau end) =
struct
  type pred = Fol.pred
  type decl = Fol.decl

  let export_section fmt title =
    begin
      Format.fprintf fmt "(*----------------------------------------*)@\n" ;
      Format.fprintf fmt "(*--- %-32s ---*)@\n" title ;
      Format.fprintf fmt "(*----------------------------------------*)@\n" ;
    end

  let export_tau fmt t =
    export_tau L.tau_of_ctype_logic fmt t

  let export_decl fmt d =
    export_decl L.tau_of_ctype_logic fmt d

  let export_goal fmt g p =
    export_goal L.tau_of_ctype_logic fmt g p

end
