(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Why code obfuscator *)

open Format
open Pp
open Ident
open Logic
open Ptree

let out = ref ""
let prefix = ref "x"

let spec = ["-o", Arg.Set_string out, "set output file";
	    "-prefix", Arg.Set_string prefix, "set names prefix"]
let usage = "why-obfuscator [options] files...\nOptions are:"

let files = Queue.create ()

let rec explain_exception fmt = function
  | Lexer.Lexical_error s -> 
      fprintf fmt "Lexical error: %s" s
  | Parsing.Parse_error -> 
      fprintf fmt "Syntax error"
  | Stream.Error s -> 
      fprintf fmt "Syntax error: %s" s
  | Loc.Located (loc, e) ->
      fprintf fmt "%a%a" Loc.report_position loc explain_exception e
  | e ->
      fprintf fmt "Anomaly: %s" (Printexc.to_string e); raise e

let add_file f =
  try
    let c = open_in f in
    let lb = Lexing.from_channel c in
    lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
    let p = Lexer.parse_file lb in
    Queue.add p files;
    close_in c
  with e -> 
    explain_exception err_formatter e;
    pp_print_newline err_formatter ();
    exit 1

let () = Arg.parse spec add_file usage

let fmt = 
  if !out = "" then 
    std_formatter
  else
    let c = open_out !out in
    at_exit (fun () -> close_out c);
    formatter_of_out_channel c

let renamings = Hashtbl.create 97

let fresh_id =
  let c = ref 0 in
  fun id -> incr c; !prefix ^ string_of_int !c

let rename_global id =
  Hashtbl.add renamings (Ident.string id) (fresh_id ())

let gident fmt id =
  let s = Ident.string id in
  fprintf fmt "%s" (try Hashtbl.find renamings s with Not_found -> s)

module M = Map.Make(String)

let ident m fmt id =
  let s = Ident.string id in
  fprintf fmt "%s" (try M.find s m with Not_found ->
		      try Hashtbl.find renamings s with Not_found -> s)

let rename m id =
  let id' = fresh_id () in
  M.add (Ident.string id) id' m

let rec pure_type fmt = function
  | PTint -> fprintf fmt "int"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "real"
  | PTexternal([],id) -> fprintf fmt "%a" gident id
  | PTvar {tag=t; type_val=None} -> fprintf fmt "'a%d" t
  | PTvar {tag=t; type_val=Some pt} -> pure_type fmt pt
  | PTexternal([t],id) -> 
      fprintf fmt "%a %a" pure_type t gident id
  | PTexternal(l,id) -> fprintf fmt "(%a) %a" 
      (print_list space pure_type) l
      gident id

let rec ppure_type fmt = function
  | PPTint -> fprintf fmt "int"
  | PPTbool -> fprintf fmt "bool"
  | PPTunit -> fprintf fmt "unit"
  | PPTreal -> fprintf fmt "real"
  | PPTexternal ([],id,_) -> fprintf fmt "%a" gident id
  | PPTvarid (id,_) -> fprintf fmt "'%a" Ident.print id
  | PPTexternal ([t],id,_) -> 
      fprintf fmt "%a %a" ppure_type t gident id
  | PPTexternal (l,id,_) -> fprintf fmt "(%a) %a" 
      (print_list comma ppure_type) l gident id

let constant fmt = function
  | ConstInt n -> 
      fprintf fmt "%s" n
  | ConstBool b -> 
      fprintf fmt "%b" b
  | ConstUnit -> 
      fprintf fmt "void" 
  | ConstFloat (i,f,"") -> 
      fprintf fmt "%s.%s" i f
  | ConstFloat (i,f,e) -> 
      fprintf fmt "%s.%se%s" i f e

let rec lexpr m fmt p = 
  let lexprm = lexpr m in
  match p.pp_desc with
  | PPconst c ->
      constant fmt c
  | PPvar id -> 
      ident m fmt id
  | PPapp (id, l) ->
      fprintf fmt "%a(%a)" gident id (print_list comma lexprm) l
  | PPtrue ->
      fprintf fmt "true"
  | PPfalse ->
      fprintf fmt "false"
  | PPinfix (a, PPand, b) ->
      fprintf fmt "(@[%a and@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPor, b) ->
      fprintf fmt "(@[%a or@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPimplies, b) -> 
      fprintf fmt "(@[%a ->@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPiff, b) -> 
      fprintf fmt "(@[%a <->@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPmod, b) -> 
      fprintf fmt "(@[%a %%@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPdiv, b) -> 
      fprintf fmt "(@[%a /@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPmul, b) -> 
      fprintf fmt "(@[%a *@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPsub, b) -> 
      fprintf fmt "(@[%a -@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPadd, b) -> 
      fprintf fmt "(@[%a +@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPneq, b) -> 
      fprintf fmt "(@[%a <>@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPeq, b) -> 
      fprintf fmt "(@[%a =@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPge, b) -> 
      fprintf fmt "(@[%a >=@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPgt, b) -> 
      fprintf fmt "(@[%a >@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPle, b) -> 
      fprintf fmt "(@[%a <=@ %a@])" lexprm a lexprm b
  | PPinfix (a, PPlt, b) -> 
      fprintf fmt "(@[%a <@ %a@])" lexprm a lexprm b
  | PPif (a, b, c) -> 
      fprintf fmt "(@[if %a then@ %a else@ %a@])" 
	lexprm a lexprm b lexprm c
  | PPprefix (PPnot, a) ->
      fprintf fmt "(not %a)" lexprm a
  | PPprefix (PPneg, a) ->
      fprintf fmt "(-(%a))" lexprm a
  | PPforall (id,v,tl,p) ->
      let m = rename m id in
      fprintf fmt "@[<hov 2>(forall %a:%a%a.@ %a)@]" 
	(ident m) id ppure_type v (triggers m) tl (lexpr m) p
  | PPexists (id,v,p) ->
      let m = rename m id in
      fprintf fmt "@[<hov 2>(exists %a:%a.@ %a)@]" 
	(ident m) id ppure_type v (lexpr m) p
  | PPfpi (t, (i1,f1,e1), (i2,f2,e2)) ->
      fprintf fmt "@[<hov 2>fpi(%a,@ %s.%se%s,@ %s.%se%s)@]" 
	lexprm t i1 f1 e1 i2 f2 e2
  | PPnamed (_, p) ->
      lexprm fmt p

and triggers m fmt = function
  | [] -> ()
  | tl -> fprintf fmt " [%a]" (print_list alt (trigger m)) tl

and trigger m = print_list space (lexpr m)

let assertion m fmt a = lexpr m fmt a.pa_value

let pre m fmt l = 
  if l <> [] then begin
    fprintf fmt "@[ ";
    print_list semi (assertion m) fmt l;
    fprintf fmt " @]"
  end

let exn m fmt (x,c) = 
  fprintf fmt "| %a => @[%a@]@," Ident.print x (assertion m) c

let post m fmt = function
  | None -> 
      ()
  | Some (c,[]) -> 
      fprintf fmt "@[ %a @]" (assertion m) c
  | Some (c, l) -> 
      fprintf fmt "@[ %a@ %a @]" (assertion m) c (print_list space (exn m)) l

let effect m fmt { pe_reads = r; pe_writes = w; pe_raises = e } =
  fprintf fmt "@[";
  if r <> [] then begin
    fprintf fmt "reads ";
    print_list (fun fmt () -> fprintf fmt ",@ ") (ident m) fmt r;
  end;
  if r <> [] && w <> [] then fprintf fmt "@ ";
  if w <> [] then begin
    fprintf fmt "writes ";
    print_list (fun fmt () -> fprintf fmt ",@ ") (ident m) fmt w;
  end;
  if (r <> [] || w <> []) && e <> [] then fprintf fmt "@ ";
  if e <> [] then begin
    fprintf fmt "raises ";
    print_list (fun fmt () -> fprintf fmt ",@ ") (ident m) fmt e;
  end;
  if r <> [] || w <> [] || e <> [] then fprintf fmt "@ ";
  fprintf fmt "@]"

let rec type_v m fmt = function
  | PVpure pt -> ppure_type fmt pt
  | PVref pt -> fprintf fmt "(%a ref)" ppure_type pt
  | PVarrow (bl,c) ->
      let m = List.fold_left rename m (List.map fst bl) in
      fprintf fmt "@[<hov 2>%a ->@ %a@]" 
	(print_list arrow (type_binder m)) bl (type_c m) c

and type_c m fmt c =
  let id = c.pc_result_name in
  let v = c.pc_result_type in
  let p = c.pc_pre in
  let q = c.pc_post in
  let e = c.pc_effect in
  if e.pe_reads = [] && e.pe_writes = [] && e.pe_raises = [] 
  && p = [] && q = None then
    type_v m fmt v
  else
    fprintf fmt "@[{%a}@ returns %a: %a@ %a@,{%a}@]" 
      (pre m) p Ident.print id (type_v m) v (effect m) e (post m) q

and type_binder m fmt = function
  | id, v when id == Ident.anonymous -> 
      type_v m fmt v
  | id, v ->
      fprintf fmt "@[%a:%a@]" (ident m) id (type_v m) v

let bracket_assertion m fmt a =
  fprintf fmt "{ %a }" (assertion m) a

let binder m fmt (id,v) = 
  fprintf fmt "(%a: %a)" (ident m) id (type_v m) v

let invariant m fmt p =
  fprintf fmt "invariant %a" (assertion m) p

let variant m fmt (t, id) =
  if id == Ident.t_zwf_zero then 
    lexpr m fmt t
  else
    fprintf fmt "%a for %a" (lexpr m) t Ident.print id

let invariant_variant m fmt = function
  | None, None -> 
      ()
  | Some _ as inv, None ->
      fprintf fmt "{ %a }" (print_option (invariant m)) inv
  | inv, Some var -> 
      fprintf fmt "{ %a variant %a }" 
	(print_option (invariant m)) inv (variant m) var 

let opt_variant m fmt = function
  | None -> ()
  | Some var -> fprintf fmt "{ variant %a }" (variant m) var

let is_binop id = 
  id == t_add || id == t_sub || id == t_mul || id == t_div
  || id == t_mod_int || id == t_eq || id == t_neq
  || id == t_lt || id == t_le || id == t_gt || id == t_ge

let binop id = 
  if id == t_add then "+"
  else if id == t_sub then "-"
  else if id == t_mul then "*"
  else if id == t_div then "/"
  else if id == t_mod_int then "%"
  else if id == t_eq then "="
  else if id == t_neq then "<>"
  else if id == t_lt then "<"
  else if id == t_le then "<="
  else if id == t_gt then ">"
  else if id == t_ge then ">="
  else assert false

let rec program m fmt p = 
  let progm = program m in
  match p.pdesc with
  | Svar id -> 
      ident m fmt id
  | Sderef id -> 
      fprintf fmt "!%a" (ident m) id
  | Stry ({ pdesc = Sloop (inv, var, { pdesc = Sif (e1, e2, _) })}, _) ->
      fprintf fmt "while %a do@ %a@ %a done"
	progm e1 (invariant_variant m) (inv,var) progm e2
  | Stry (e1, hl) ->
      fprintf fmt "try %a@ with %a end" 
	progm e1 (print_list alt (handler m)) hl
  | Sloop _ -> 
      assert false
  | Sif (e1, e2, e3) ->
      fprintf fmt "(if %a then@ %a else@ %a)" progm e1 progm e2 progm e3
  | Slazy_and (e1, e2) ->
      fprintf fmt "(%a &&@ %a)" progm e1 progm e2
  | Slazy_or (e1, e2) ->
      fprintf fmt "(%a ||@ %a)" progm e1 progm e2
  | Snot e1 ->
      fprintf fmt "(not %a)" progm e1
  | Sapp ({pdesc = Sapp ({ pdesc = Svar id }, e1)}, e2) when is_binop id ->
      fprintf fmt "(%a %s %a)" progm e1 (binop id) progm e2
  | Sapp ({ pdesc = Svar id }, e1) when id == t_neg ->
      fprintf fmt "(-(%a))" progm e1 
  | Sapp (e1, e2) ->
      fprintf fmt "(%a@ %a)" progm e1 progm e2
  | Sletref (id, e1, e2) ->
      let m = rename m id in
      fprintf fmt "(let %a = ref %a in@ %a)" 
	(ident m) id progm e1 (program m) e2
  | Sletin (id, e1, e2) ->
      let m = rename m id in
      fprintf fmt "(let %a = %a in@ %a)" (ident m) id progm e1 (program m) e2
  | Sseq (e1, e2) ->
      fprintf fmt "(%a;@ %a)" progm e1 progm e2
  | Slam (bl, al, e) ->
      let m = List.fold_left rename m (List.map fst bl) in
      fprintf fmt "(fun %a -> %a %a)" (print_list space (binder m)) bl
	(print_list space (bracket_assertion m)) al (program m) e
  | Srec (f, bl, v, var, al, e) ->
      let m = List.fold_left rename m (List.map fst bl) in
      fprintf fmt "(let rec %a %a : %a %a ->@ %a %a)" 
	(ident m) f (print_list space (binder m)) bl
	(type_v m) v (opt_variant m) var
	(print_list space (bracket_assertion m)) al (program m) e
  | Sraise (id, None, None) -> 
      fprintf fmt "(raise %a)" Ident.print id
  | Sraise (id, Some e, None) -> 
      fprintf fmt "(raise (%a %a))" Ident.print id (program m) e
  | Sraise (id, None, Some v) -> 
      fprintf fmt "(raise %a : %a)" Ident.print id (type_v m) v
  | Sraise (id, Some e, Some v) -> 
      fprintf fmt "(raise (%a %a) : %a)" 
	Ident.print id (program m) e (type_v m) v
  | Sconst c ->
      constant fmt c
  | Sabsurd None ->
      fprintf fmt "absurd"
  | Sabsurd (Some v) ->
      fprintf fmt "(absurd: %a)" (type_v m) v
  | Sany c ->
      fprintf fmt "[ %a ]" (type_c m) c
  | Slabel (l, e) ->
      fprintf fmt "(%s: %a)" l progm e
  | Sassert (al, e) ->
      fprintf fmt "(assert %a; %a)"
	(print_list space (bracket_assertion m)) al (program m) e
  | Spost (e, p, Types.Transparent) ->
      fprintf fmt "(%a { %a })" progm e (post m) (Some p)
  | Spost (e, p, Types.Opaque) ->
      fprintf fmt "(%a {{ %a }})" progm e (post m) (Some p)

and handler m fmt = function
  | (id, None), e ->
      fprintf fmt "%a -> %a" Ident.print id (program m) e
  | (id, Some x), e ->
      let m = rename m x in
      fprintf fmt "%a %a -> %a" Ident.print id (ident m) x (program m) e

let print_external fmt b = if b then fprintf fmt "external "

let type_var fmt id = fprintf fmt "'%a" Ident.print id

let type_parameters fmt = function
  | [] -> ()
  | [id] -> fprintf fmt "%a "type_var id
  | l -> fprintf fmt "(%a) " (print_list comma type_var) l

let logic_binder m fmt (_, id, pt) =
  fprintf fmt "%a: %a" (ident m) id ppure_type pt

let logic_type fmt = function
  | PPredicate ptl -> 
      fprintf fmt "%a -> prop" (print_list comma ppure_type) ptl
  | PFunction (ptl, pt) -> 
      fprintf fmt "%a -> %a" (print_list comma ppure_type) ptl ppure_type pt

let decl fmt = function
  | Program (_,id, p) -> 
      rename_global id;
      fprintf fmt "@[<hov 2>let %a =@ %a@]" gident id (program M.empty) p
  | Parameter (_, e, ids, v) -> 
      List.iter rename_global ids;
      fprintf fmt "@[<hov 2>%aparameter %a:@ %a@]" print_external e
	(print_list comma gident) ids (type_v M.empty) v
  | Exception (_, id, None) -> 
      fprintf fmt "exception %a" Ident.print id
  | Exception (_, id, Some pt) -> 
      fprintf fmt "exception %a of %a" Ident.print id ppure_type pt
  | Logic (_, e, ids, lt) -> 
      List.iter rename_global ids;
      fprintf fmt "%alogic %a : %a" print_external e 
	(print_list comma gident) ids logic_type lt
  | Axiom (_, id, p) ->
      rename_global id;
      fprintf fmt "axiom %a : %a" gident id (lexpr M.empty) p
  | Goal (_, id, p) ->
      rename_global id;
      fprintf fmt "goal %a : %a" gident id (lexpr M.empty) p
  | Predicate_def (_, id, bl, p) ->
      rename_global id;
      let m = List.fold_left rename M.empty (List.map (fun (_,x,_) -> x) bl) in
      fprintf fmt "@[<hov 2>predicate %a(%a) =@ %a@]" gident id 
	(print_list comma (logic_binder m)) bl (lexpr m) p
  | Inductive_def(_,id, bl, l) ->
      rename_global id;
      fprintf fmt "@[<hov 2>predicate %a: @[%a@] {@\n  @[<v 0>%a@]@\n}@\n@]" 
	gident id 
	logic_type bl 
	(print_list newline 
	   (fun fmt (_,id,p) -> 
	      fprintf fmt "%a: %a;" gident id (lexpr M.empty) p)) l
  | Function_def (_, id, bl, pt, e) ->
      rename_global id;
      let m = List.fold_left rename M.empty (List.map (fun (_,x,_) -> x) bl) in
      fprintf fmt "@[<hov 2>function %a(%a) : %a =@ %a@]" gident id
	(print_list comma (logic_binder m)) bl ppure_type pt (lexpr m) e
  | TypeDecl (_, e, pl, id) ->
      rename_global id;
      fprintf fmt "%atype %a%a" print_external e type_parameters pl gident id

let file = print_list newline decl

let () = 
  fprintf fmt "@[";
  Queue.iter (file fmt) files;
  fprintf fmt "@]@."

