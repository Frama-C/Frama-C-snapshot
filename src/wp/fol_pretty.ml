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

open Cilutil




let f_suffixe f = (f.Cil_types.fname)^"_"^(f.Cil_types.fcomp.Cil_types.cname)
let get_field f = "get_"^(f_suffixe f)
let set_field f = "set_"^(f_suffixe f)



(* ---------------------------------------------------------------------- *)
(* --- Output Utilities                                               --- *)
(* ---------------------------------------------------------------------- *)



let constant fmt = function
  | Fol.ConstInt n -> Format.pp_print_string fmt n
  | Fol.ConstBool b ->  Format.pp_print_string fmt (if b then "true" else "false")
  | Fol.ConstUnit ->  Format.pp_print_string fmt "void"
  | Fol.ConstFloat f ->  Format.pp_print_string fmt f

let pp_list pp fmt = function
  | [] -> ()
  | x::xs ->
      pp fmt x ;
      List.iter (fun x ->  Format.fprintf fmt ",@,%a" pp x) xs

let pp_flow fmt nil op pp = function
    | [] -> 
	 Format.pp_print_string fmt nil
    | x::xs ->
	 Format.fprintf fmt "@[<hov 1>(%a" pp x ;
	List.iter (fun x ->  Format.fprintf fmt "@,%s%a" op pp x) xs ;
	 Format.fprintf fmt ")@]"


let pp_block fmt op pp ps =
  Pretty_utils.pp_list
    ~pre:"@[<hv>" ~sep:("@ " ^^ op ^^ "@ ") ~suf:"@]"
    pp fmt ps

let pp_block_implies fmt op pp ps =
  Pretty_utils.pp_list
    ~pre:"@[<v>" ~sep:(" " ^^ op ^^ "@ ") ~suf:"@]"
    pp fmt ps

let pp_var fmt v =
  let name = Fol_decl.identifier (Fol.Var.var_name v) in
   Format.fprintf fmt "%s" name

let rec collect_assoc op xs = function
  | [] -> List.rev xs
  | Fol.Tapp(id,ts) :: others when id = op -> 
      collect_assoc op (collect_assoc op xs ts) others
  | t::others ->
      collect_assoc op (t::xs) others

let fpp_term term fmt t =
  match t with
    | Fol.Tconst c -> constant fmt c
    | Fol.Tvar v -> pp_var fmt v
       | Fol.Tapp (id, []) -> Format.pp_print_string fmt id
    | Fol.Tapp ("ite",[c;a;b]) | Fol.Tif (c,a,b) ->
	Format.fprintf fmt "(@[<hv>if %a@ then %a@ else %a@])"
	  term c term a term b

    (* INT *)
    | Fol.Tapp ("neg_int", [t]) ->
        Format.fprintf fmt "@[<hov 1>(-%a)@]" term t
    | Fol.Tapp ("add_int", ts) ->
	let xs = collect_assoc "add_int" [] ts in
	pp_flow fmt "0" "+" term xs
    | Fol.Tapp ("sub_int", [a;b]) ->
	Format.fprintf fmt "@[<hov 1>(%a@,-%a)@]" term a term b
    | Fol.Tapp ("mul_int", ts) ->
	let xs = collect_assoc "mul_int" [] ts in
	pp_flow fmt "1" "*" term xs
	  
    (* REAL *)
    | Fol.Tapp ("neg_real", [t]) ->
	Format.fprintf fmt "@[<hov 1>(-%a)@]" term t
    | Fol.Tapp ("add_real", ts) ->
        let xs = collect_assoc "add_real" [] ts in
        pp_flow fmt "0.0" "+" term xs
    | Fol.Tapp ("sub_real", [a;b]) ->
        Format.fprintf fmt "@[<hov 1>(%a@,-%a)@]" term a term b
    | Fol.Tapp ("mul_real", ts) ->
        let xs = collect_assoc "mul_real" [] ts in
        pp_flow fmt "1.0" "*" term xs
    | Fol.Tapp ("div_real", [a;b]) ->
        Format.fprintf fmt "@[<hov 1>(%a@,/%a)@]" term a term b

    | Fol.Tapp (id, t::ts) ->
	Format.fprintf fmt "@[<hov 2>%s(@,%a" id term t ;
	List.iter (fun t -> Format.fprintf fmt ",@,%a" term t) ts ;
	Format.fprintf fmt ")@]"
    | Fol.Tlet (x,v,t) ->
	Format.fprintf fmt "(@[<hv 0>let %a@ = %a@ in %a@])"
	  pp_var x term v term t
    | Fol.Taccess(a,k) -> Format.fprintf fmt "access(%a,@,%a)" term a term k
    | Fol.Tupdate(a,k,b) -> 
	Format.fprintf fmt "@[<hv 2>update(%a,@,%a,@,%a)@]" 
	  term a term k term b

    | Fol.Tgetfield (f,r) -> 
	Format.fprintf fmt "@[<hv 2>%s(%a)@]" (get_field f) term r
    | Fol.Tsetfield (f,r,v) ->
	Format.fprintf fmt "@[<hv 2>%s(%a,@,%a)@]" (set_field f) 
	  term r term v
   

let fpretty_term term fmt t = 
  if Wp_parameters.verbose_atleast 2 
  then fpp_term term fmt t
  else match t with
    | Fol.Tapp(("encode"|"decode"),[_;v]) ->
	Format.fprintf fmt "{%a}" term v
    | Fol.Taccess(a,k) -> Format.fprintf fmt "%a[%a]" term a term k
    | Fol.Tupdate(a,k,b) -> 
	Format.fprintf fmt "@[<hv 2>%a[%a@,->%a]@]" 
	  term a term k term b

    | t -> 
	fpp_term term fmt t

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

(*TODO : ensures that label are different from keywords of the host language*)
let tag_named tag = "tag_"^tag


let pp_tau =  Fol_decl.Tau.pp_tau

let rec epp_pred_vbox env fmt p =
  match p with
    | Fol.Pand _ -> pp_block fmt "and" env.pp_pred (collect_and [] p)
    | Fol.Por _ -> pp_block fmt "or" env.pp_pred (collect_or [] p)
    | Fol.Pimplies _ ->
        pp_block_implies fmt "->" env.pp_pred (collect_imply [] p)
    | Fol.Piff _ -> pp_block fmt "<->" env.pp_pred (collect_iff [] p)
    | Fol.Pforall(x,p) ->
	Format.fprintf fmt "forall %a:%a.@\n" pp_var x env.pp_type (Fol.Var.var_type x) ;
	epp_pred_vbox env fmt p
    | Fol.Pexists(x,p) ->
	Format.fprintf fmt "exists %a:%a.@\n" pp_var x env.pp_type (Fol.Var.var_type x) ;
	epp_pred_vbox env fmt p
    | Fol.Plet(x,t,p) ->
	Format.fprintf fmt "@[<hov 2>let %a =@ %a@ in@]@\n" pp_var x env.pp_term t ;
	epp_pred_vbox env fmt p
    | Fol.Pif(t,p,q) ->
	Format.fprintf fmt "@[<hov 0>if @[<hov 2>%a@]@ then@]@\n  %a@\nelse@\n  %a"
	  env.pp_term t env.pp_pred p env.pp_pred q
    | (Fol.Ptrue | Fol.Pfalse | Fol.Papp _ | Fol.Pnot _ | Fol.Pnamed _) ->
	env.pp_pred fmt p

let rec epp_pred_atom env fmt p =
  match p with
    | Fol.Pand _ | Fol.Por _ | Fol.Pimplies _ | Fol.Piff _ | Fol.Pif _
    | Fol.Pforall _ | Fol.Pexists _ | Fol.Plet _ ->
	Format.fprintf fmt "@[<v 1>(%a)@]" (epp_pred_vbox env) p
    | Fol.Pnot p ->
	Format.fprintf fmt "@[<hov 2>(not@ %a)@]" (epp_pred_atom env) p
    | Fol.Ptrue -> Format.pp_print_string fmt "true"
    | Fol.Pfalse -> Format.pp_print_string fmt "false"
    | Fol.Papp(id,[]) -> Format.pp_print_string fmt id
    | Fol.Papp (("eq" | "eq_int" | "eq_real"), [t1; t2]) ->
	Format.fprintf fmt "@[<hov 1>(%a@ =@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("neq" | "neq_int"| "neq_real"), [t1; t2]) ->
	Format.fprintf fmt "@[<hov 1>(%a@ <>@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("lt_int"| "lt_real"), [t1; t2]) ->
	Format.fprintf fmt "@[<hov 1>(%a@ <@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp (("le_int"| "le_real"), [t1; t2]) ->
	Format.fprintf fmt "@[<hov 1>(%a@ <=@ %a)@]" env.pp_term t1 env.pp_term t2
    | Fol.Papp(id,t::ts) ->
	Format.fprintf fmt "@[<hov 2>%s(@,%a" id env.pp_term t ;
	List.iter (fun t -> Format.fprintf fmt ",@ %a" env.pp_term t) ts ;
	Format.fprintf fmt ")@]"
    | Fol.Pnamed(tag,p) ->
	Format.fprintf fmt "@[<hov 0>@[<hov 0>%s:%a@]" 
	  (tag_named tag) (epp_pred_named env) p

and epp_pred_named env fmt = function
  | Fol.Pnamed(tag,p) ->
      Format.fprintf fmt "@,%s:" (tag_named tag) ;
      epp_pred_named env fmt p
  | p ->
      Format.fprintf fmt "@]@,%a" (epp_pred_atom env) p

let fpp_pred predicate pp_tau pp_term fmt p =
  match p with
    | Fol.Ptrue -> Format.fprintf fmt "true"
    | Fol.Pfalse -> Format.fprintf fmt "false"
    | Fol.Papp (id, [])-> Format.fprintf fmt "%s" id
    | Fol.Papp ("eq", [t1; t2]) -> Format.fprintf fmt "(%a =@ %a)" pp_term t1 pp_term t2
    | Fol.Papp ("neq", [t1; t2]) -> Format.fprintf fmt "(%a <>@ %a)" pp_term t1 pp_term t2
    | Fol.Papp (id, l) -> Format.fprintf fmt "@[%s(%a)@]" id (pp_list pp_term) l
    | Fol.Pimplies (a, b) -> Format.fprintf fmt "(@[%a ->@ %a@])" predicate a predicate b
    | Fol.Piff (a, b) -> Format.fprintf fmt "(@[%a <->@ %a@])" predicate a predicate b
    | Fol.Pand (a, b) -> Format.fprintf fmt "(@[%a and@ %a@])" predicate a predicate b
    | Fol.Por (a, b) -> Format.fprintf fmt "(@[%a or@ %a@])" predicate a predicate b
    | Fol.Pnot a -> Format.fprintf fmt "(not %a)" predicate a
    | Fol.Pif (a, b, c) ->
	Format.fprintf fmt "(@[if %a then@ %a else@ %a@])"
	  pp_term a predicate b predicate c
    | Fol.Pforall (v,p) ->
	Format.fprintf fmt "@[<hov 0>(forall %a:%a.@ %a@])"
	  pp_var v pp_tau (Fol.Var.var_type v) predicate p
    | Fol.Pexists (v,p) ->
	Format.fprintf fmt "@[<hov 0>(exists %a:%a.@ %a@])"
	  pp_var v  pp_tau (Fol.Var.var_type v) predicate p
    | Fol.Plet (x,v,p) ->
	Format.fprintf fmt "@[<hov 0>(let %a =@[<hov 2>@ %a@ in@]@ %a@])"
	  pp_var x pp_term v predicate p
    | Fol.Pnamed (n, p) ->
	Format.fprintf fmt "@[%s: %a@]" (tag_named n) predicate p


let export_get_set_field fmt pp_tau f =
  let cn = f.Cil_types.fcomp.Cil_types.cname in 
  let fn = Fol_decl.Tau.tau_of_ctype_logic f.Cil_types.ftype in
  let get_f = get_field f in 
  let set_f = set_field f in
  Format.fprintf fmt "logic %s: %s -> %a @\n" get_f cn pp_tau fn; 
  Format.fprintf fmt "logic %s: %s , %a -> %s @\n" set_f cn pp_tau fn cn 
    

let export_get_set_other fmt pp_tau f get_f g =  
  let set_g = set_field g in
  Format.pp_print_newline fmt () ; 
  Format.fprintf fmt 
    "(* Definition of the commutativity of the get field %s over the set field %s*)@\n" 
    f.Cil_types.fname g.Cil_types.fname;
  Format.pp_print_newline fmt () ; 
  Format.fprintf fmt "axiom GetSetOther_%s_%s@,:@\n" f.Cil_types.fname g.Cil_types.fname; 
  Format.fprintf fmt "forall r:%s.@,forall v:%a.@\n" 
    f.Cil_types.fcomp.Cil_types.cname  
    pp_tau (Fol_decl.Tau.tau_of_ctype_logic g.Cil_types.ftype); 
  Format.fprintf fmt "%s(%s(r,v))@,=@, %s(r)@\n" get_f set_g get_f; 
  Format.pp_print_newline fmt () 

let export_generated_axiomatics fmt pp_tau f = 
    let get_f = get_field f in 
    let set_f = set_field f in
    Format.fprintf fmt 
	    "(* Definition of the good properties of the field %s*)@\n" f.Cil_types.fname;
    Format.pp_print_newline fmt () ; 
    Format.fprintf fmt "axiom GetSetSame_%s@,:@\n" f.Cil_types.fname; 
    Format.fprintf fmt "forall r:%s.@,forall v:%a.@\n" 
      f.Cil_types.fcomp.Cil_types.cname 
      pp_tau (Fol_decl.Tau.tau_of_ctype_logic f.Cil_types.ftype); 
    Format.fprintf fmt "%s(%s(r,v))@,=@, v@\n" get_f set_f ; 
    Format.pp_print_newline fmt () ; 
    if f.Cil_types.fcomp.Cil_types.cstruct then
    (List.iter (fun g -> 
		  if Cil_datatype.Fieldinfo.equal f g then () else 
		    export_get_set_other fmt pp_tau f get_f g) 
      f.Cil_types.fcomp.Cil_types.cfields ;)
    else ();
    Format.pp_print_newline fmt () ;
    Format.pp_print_newline fmt ()  

let pp_section fmt title =
  begin
    Format.fprintf fmt "----------------------------------------@\n" ;
    Format.fprintf fmt "--- %s@\n" title ;
    Format.fprintf fmt "----------------------------------------@\n@\n" ;
  end

let pp_param pp_tau fmt x =
  Format.fprintf fmt "%a:%a" pp_var x pp_tau (Fol.Var.var_type x)
  
let fpp_item term predicate pp_tau fmt x =
  function
    | Formula.Cons k ->
	Format.fprintf fmt "function %s () : int = %d@\n" x k
    | Formula.Function ([], t) ->
	Format.fprintf fmt "logic %s: %a@\n" x pp_tau t
    | Formula.Function (tl, t) ->
	Format.fprintf fmt "logic %s: @[<hov 0>%a -> %a@]@\n" x (pp_list pp_tau) tl pp_tau t
    | Formula.Predicate([]) ->
	Format.fprintf fmt "logic %s: prop@\n" x
    | Formula.Predicate(tl) ->
	Format.fprintf fmt "logic %s: @[<hov 0>%a -> prop@]@\n" x (pp_list pp_tau) tl
    | Formula.FunctionDef (xs,tr,exp) ->
	Format.fprintf fmt "@[<hv 2>function %s (%a) : %a =@ @[<hov 0>%a@]@]@\n"
	  x (pp_list (pp_param pp_tau)) xs pp_tau tr term exp
    | Formula.PredicateDef(xs,p) ->
	Format.fprintf fmt "@[<hv 2>predicate %s (%a) =@ @[<hov 0>%a@]@]@\n"
	  x (pp_list (pp_param pp_tau)) xs predicate p
    | Formula.Axiom p ->
	Format.fprintf fmt "@[<hv 2>axiom %s:@ %a@]@\n" x predicate p
    | Formula.Type 0 ->
	Format.fprintf fmt "type %s@\n" x
    | Formula.Type 1 ->
	Format.fprintf fmt "type 'a %s@\n" x
    | Formula.Type n ->
	Format.fprintf fmt "@[<hov 2>type ('a" ;
	for k=2 to n do Format.fprintf fmt ",%c" (char_of_int (int_of_char 'a'+k-1)) done ;
	Format.fprintf fmt ") %s@]@\n" x
    | Formula.Trecord c -> 
	begin
	  Format.fprintf fmt "type %s@\n" c.Cil_types.cname ;
	  let l = c.Cil_types.cfields in 
	  List.iter (fun f -> export_get_set_field fmt pp_tau f) l ; 
	  List.iter (fun f -> export_generated_axiomatics fmt pp_tau f) l 
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

let fpp_decl term predicate fmt d =
  begin
    fpp_header fmt d ;
    Format.pp_print_newline fmt () ;
    fpp_item term predicate pp_tau fmt d.Formula.d_name d.Formula.d_item ;
    Format.pp_print_newline fmt () ;
  end

let fpp_goal predicate fmt x p =
  Format.fprintf fmt "@[<hv 2>goal %s:@ %a@]@." x predicate p;
 



