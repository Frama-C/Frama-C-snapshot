(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
open Cil_types
open Pretty_utils
open Logic_ptree

let print_constant fmt = function
  | IntConstant s -> pp_print_string fmt s
  | FloatConstant s -> pp_print_string fmt s
  | StringConstant s -> fprintf fmt "\"%s\"" s
  | WStringConstant s -> fprintf fmt "\"%s\"" s

let rec print_logic_type name fmt typ =
  let pname = match name with
    | Some d -> (fun fmt -> fprintf fmt "@ %t" d)
    | None -> (fun _ -> ())
  in
  match typ with
      LTvoid -> fprintf fmt "void%t" pname
    | LTinteger ->
        fprintf fmt "%s%t"
	  (if Kernel.Unicode.get () then Utf8_logic.integer else "integer")
	  pname
    | LTreal ->
        fprintf fmt "%s%t"
	  (if Kernel.Unicode.get () then Utf8_logic.real else "real")
	  pname
    | LTint i -> fprintf fmt "%a%t" Cil_printer.pp_ikind i pname
    | LTfloat f -> fprintf fmt "%a%t" Cil_printer.pp_fkind f pname
    | LTarray (t,c) ->
        let pname fmt =
          fprintf fmt "%t[@[%a@]]" pname (pp_opt print_constant) c
        in
        print_logic_type (Some pname) fmt t
    | LTpointer t ->
        let needs_paren = match t with LTarray _ -> true | _ -> false in
        let pname fmt =
          Format.fprintf fmt "%a*%t%a"
            (pp_cond needs_paren) "(" pname (pp_cond needs_paren) ")"
        in
        print_logic_type (Some pname) fmt t
    | LTunion s -> fprintf fmt "union@ %s%t" s pname
    | LTenum s -> fprintf fmt "enum@ %s%t" s pname
    | LTstruct s -> fprintf fmt "struct@ %s%t" s pname
    | LTnamed (s,l) ->
        fprintf fmt "%s%a%t"
          s
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@]>"
             (print_logic_type None)) l
          pname
    | LTarrow(args,ret) ->
        let pname fmt =
          fprintf fmt "%t(@[%a@])" pname
            (pp_list ~sep:",@ " (print_logic_type None)) args
        in
        print_logic_type (Some pname) fmt ret

let print_typed_ident fmt (t,s) =
  print_logic_type (Some (fun fmt -> pp_print_string fmt s)) fmt t

let print_quantifiers fmt l = pp_list ~sep:",@ " print_typed_ident fmt l

let get_relation_string = function
    Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">=" | Eq -> "==" | Neq -> "!="

let get_binop_string = function
    Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Bbw_and -> "&"
  | Bbw_or -> "|"
  | Bbw_xor -> "^"
  | Blshift -> "<<"
  | Brshift -> ">>"

let get_unop_string = function
    Uminus -> "-" | Ustar -> "*" | Uamp -> "&" | Ubw_not -> "~"

let getParenthLevel e =
  match e.lexpr_node with
    | PLnamed _ -> 95
    | PLlambda _ | PLlet _ | PLrange _ -> 90
    | PLforall _ | PLexists _ -> 87
    | PLimplies _ | PLiff _ -> 85
    | PLand _ | PLor _ | PLxor _ -> 80
    | PLif _ -> 77
    | PLbinop (_,(Bbw_and | Bbw_or | Bbw_xor),_) -> 75
    | PLrel _ -> 70
    | PLbinop (_,(Badd|Bsub|Blshift|Brshift),_) -> 60
    | PLbinop (_,(Bmul|Bdiv|Bmod),_) -> 40
    | PLunop ((Uamp|Uminus|Ubw_not),_) | PLcast _ | PLnot _ -> 30
    | PLcoercion _ | PLcoercionE _ -> 25
    | PLunop (Ustar,_) | PLdot _ | PLarrow _ | PLarrget _
    | PLsizeof _ | PLsizeofE _ -> 20
    | PLapp _ | PLold _ | PLat _ 
    | PLoffset _ | PLbase_addr _ | PLblock_length _
    | PLupdate _  | PLinitField _ | PLinitIndex _
    | PLvalid _ | PLvalid_read _ | PLinitialized _ 
    | PLallocable _ | PLfreeable _ | PLfresh _ 
    | PLseparated _ | PLsubtype _ | PLunion _ | PLinter _ -> 10
    | PLvar _ | PLconstant _ | PLresult | PLnull | PLtypeof _ | PLtype _
    | PLfalse | PLtrue | PLcomprehension _ | PLempty | PLsingleton _ -> 0

let rec print_path_elt fmt = function
    | PLpathField s -> fprintf fmt ".%s" s
    | PLpathIndex i -> fprintf fmt "[@[%a@]]" print_lexpr i

and print_path_val fmt (path, v) =
  match v with
    | PLupdateTerm e ->
	fprintf fmt "@[%a@ =@ %a@]"
	  (pp_list ~sep:"@;" print_path_elt) path print_lexpr e
    | PLupdateCont path_val_list ->
	fprintf fmt "{ \\with %a@ }"
	  (pp_list ~sep:",@ " print_path_val) path_val_list

and print_init_index fmt (i,v) =
  print_path_val fmt ([PLpathIndex i], PLupdateTerm v)

and print_init_field fmt (s,v) =
  print_path_val fmt ([PLpathField s], PLupdateTerm v)

and print_lexpr fmt e = print_lexpr_level 100 fmt e

and print_label_1 fmt l =
  match l with 
    | None -> ()
    | Some s -> fprintf fmt "{%s}" s

and print_label_2 fmt l =
  match l with 
    | None -> ()
    | Some (s1,s2) -> fprintf fmt "{%s,%s}" s1 s2

and print_lexpr_level n fmt e =
  let n' = getParenthLevel e in
  let print_lexpr fmt e = print_lexpr_level n' fmt e in
  let print_lexpr_plain fmt e = print_lexpr_level 100 fmt e in
  let aux fmt e =
    match e.lexpr_node with
        PLvar s -> pp_print_string fmt s
      | PLapp(s,tv,args) ->
          fprintf fmt "%s@;%a@;(@[%a@])"
            s
            (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@]>" pp_print_string) tv
            (pp_list ~sep:",@ " print_lexpr_plain) args
      | PLlambda (quant,e) ->
          fprintf fmt "@[<2>\\lambda@ @[%a@];@ %a@]"
            print_quantifiers quant print_lexpr e
      | PLlet (n,def,body) ->
          fprintf fmt "@[@[<2>\\let@ %s@ =@ %a;@]@\n%a@]"
            n print_lexpr def  print_lexpr body
      | PLconstant c -> print_constant fmt c
      | PLunop(op,e) -> fprintf fmt "%s%a" (get_unop_string op) print_lexpr e
      | PLbinop(e1,op,e2) ->
          fprintf fmt "%a@ %s@ %a"
            print_lexpr e1 (get_binop_string op) print_lexpr e2
      | PLdot(e,f) -> fprintf fmt "%a.%s" print_lexpr e f
      | PLarrow(e,f) -> fprintf fmt "%a->%s" print_lexpr e f
      | PLarrget(b,i) ->
          fprintf fmt "%a[@;@[%a@]@;]" print_lexpr b print_lexpr i
      | PLold(e) -> fprintf fmt "\\old(@;@[%a@]@;)" print_lexpr_plain e
      | PLat(e,s) -> fprintf fmt "\\at(@;@[%a,@ %s@]@;)" print_lexpr_plain e s
      | PLbase_addr (l,e) -> fprintf fmt "\\base_addr%a(@;@[%a@])" print_label_1 l print_lexpr_plain e
      | PLblock_length (l,e) ->
          fprintf fmt "\\block_length%a(@;@[%a@])" print_label_1 l print_lexpr_plain e
      | PLoffset (l,e) ->
          fprintf fmt "\\offset%a(@;@[%a@])" print_label_1 l print_lexpr_plain e
      | PLresult -> pp_print_string fmt "\\result"
      | PLnull -> pp_print_string fmt "\\null"
      | PLcast (t,e) -> fprintf fmt "(@[%a@])@;%a"
          (print_logic_type None) t print_lexpr e
      | PLrange(e1,e2) ->
          fprintf fmt "%a@;..@;%a"
            (pp_opt print_lexpr) e1 (pp_opt print_lexpr) e2
      | PLsizeof t -> fprintf fmt "sizeof(@;@[%a@]@;)" (print_logic_type None) t
      | PLsizeofE e -> fprintf fmt "sizeof(@;@[%a@]@;)" print_lexpr_plain e
      | PLcoercion(e,t) ->
          fprintf fmt "%a@ :>@ %a" print_lexpr e (print_logic_type None) t
      | PLcoercionE(e1,e2) ->
          fprintf fmt "%a@ :>@ %a" print_lexpr e1 print_lexpr e2
      | PLupdate(e1,path,e2) ->
          fprintf fmt "{@ @[%a@ \\with@ %a@]}"
            print_lexpr_plain e1 print_path_val (path, e2)
      | PLinitField(init_field_list) ->
          fprintf fmt "{@ %a@}"
	    (pp_list ~sep:",@ " print_init_field) init_field_list
      | PLinitIndex(init_index_list) ->
          fprintf fmt "{@ %a@}"
	    (pp_list ~sep:",@ " print_init_index) init_index_list
      | PLtypeof e -> fprintf fmt "typeof(@;@[%a@]@;)" print_lexpr_plain e
      | PLtype t -> fprintf fmt "\\type(@;@[%a@]@;" (print_logic_type None) t
      | PLfalse -> pp_print_string fmt "\\false"
      | PLtrue -> pp_print_string fmt "\\true"
      | PLrel (e1,rel,e2) ->
          fprintf fmt "%a@ %s@ %a"
            print_lexpr e1 (get_relation_string rel) print_lexpr e2
      | PLand(e1,e2) -> fprintf fmt "%a@ &&@ %a" print_lexpr e1 print_lexpr e2
      | PLor(e1,e2) -> fprintf fmt "%a@ ||@ %a" print_lexpr e1 print_lexpr e2
      | PLxor(e1,e2) -> fprintf fmt "%a@ ^^@ %a" print_lexpr e1 print_lexpr e2
      | PLimplies(e1,e2) ->
          fprintf fmt "%a@ ==>@ %a" print_lexpr e1 print_lexpr e2
      | PLiff(e1,e2) ->
          fprintf fmt "%a@ <==>@ %a" print_lexpr e1 print_lexpr e2
      | PLnot e -> fprintf fmt "!@;%a" print_lexpr e
      | PLif (e1,e2,e3) ->
          fprintf fmt "%a@ ?@ %a@ :@ %a"
            print_lexpr e1 print_lexpr e2 print_lexpr e3
      | PLforall(q,e) ->
          fprintf fmt "@[\\forall@ @[%a@];@ %a@]"
            print_quantifiers q print_lexpr e
      | PLexists(q,e) ->
          fprintf fmt "@[\\exists@ @[%a@];@ %a@]"
            print_quantifiers q print_lexpr e
      | PLvalid (l,e) -> fprintf fmt "\\valid%a(@;@[%a@]@;)" print_label_1 l print_lexpr_plain e
      | PLvalid_read (l,e) -> fprintf fmt "\\valid_read%a(@;@[%a@]@;)" print_label_1 l print_lexpr_plain e
      | PLinitialized (l,e) ->
          fprintf fmt "\\initialized%a(@;@[%a@]@;)" print_label_1 l print_lexpr_plain e
      | PLseparated l ->
          fprintf fmt "\\separated(@;@[%a@]@;)"
            (pp_list ~sep:",@ " print_lexpr_plain) l
      | PLfreeable (l,e) -> 
	  fprintf fmt "\\freeable%a(@;@[%a@]@;)" print_label_1 l print_lexpr_plain e
      | PLallocable (l,e) -> 
	  fprintf fmt "\\allocable%a(@;@[%a@]@;)" print_label_1 l print_lexpr_plain e
      | PLfresh (l2,e1,e2) -> 
	  fprintf fmt "\\fresh%a(@;@[%a@],@[%a@]@;)" print_label_2 l2 print_lexpr_plain e1 print_lexpr_plain e2
      | PLnamed(s,e) -> fprintf fmt "%s:@ %a" s print_lexpr e
      | PLsubtype (e1,e2) ->
          fprintf fmt "%a@ <:@ %a" print_lexpr e1 print_lexpr e2
      | PLcomprehension(e,q,p) ->
          fprintf fmt "{@ @[%a;@ %a%a@]@ }"
            print_lexpr e print_quantifiers q
            (pp_opt ~pre:"@ |@ " print_lexpr) p
      | PLsingleton e -> fprintf fmt "{@ @[%a@]@ }" print_lexpr e
      | PLempty -> pp_print_string fmt "\\empty"
      | PLunion l->
          fprintf fmt "\\union(%a)"
            (pp_list ~pre:"@;@[" ~sep:",@ " ~suf:"@]@;" print_lexpr_plain) l
      | PLinter l->
          fprintf fmt "\\inter(%a)"
            (pp_list ~pre:"@;@[" ~sep:",@ " ~suf:"@]@;" print_lexpr_plain) l
  in
  if n <= n' then fprintf fmt "(@[%a@])" aux e else aux fmt e

let print_typedef fmt = function
    | TDsum l ->
        let print_const fmt (s,args) =
          fprintf fmt "%s%a" s
            (pp_list ~pre:"@ (@[" ~sep:",@ " ~suf:"@])"
               (print_logic_type None))
            args
        in
        pp_list ~sep:"@ |@ " print_const fmt l
    | TDsyn t -> print_logic_type None fmt t

let print_type_annot fmt ty =
  fprintf fmt "@[type@ invariant@ %s(@;@[%a@ %s]@;)@ =@ %a;@]"
    ty.inv_name (print_logic_type None) ty.this_type ty.this_name
    print_lexpr ty.inv

let print_model_annot fmt ty =
  fprintf fmt "@[model@ %a {@;@[%a@ %s]@;}@ @]"
    (print_logic_type None) ty.model_for_type 
    (print_logic_type None) ty.model_type 
    ty.model_name

let rec print_decl fmt d =
  match d.decl_node with
    | LDlogic_def(name,labels,tvar,rt,prms,body) ->
        fprintf fmt "@[<2>logic@ %a@ %s%a%a%a@ =@ %a;@]"
          (print_logic_type None) rt name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_list ~pre:"(@[" ~sep:",@ " ~suf:"@])" print_typed_ident) prms
          print_lexpr body
    | LDlogic_reads(name,labels,tvar,rt,prms,reads) ->
        fprintf fmt "@[<2>logic@ %a@ %s%a%a%a@ =@ %a;@]"
          (print_logic_type None) rt name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_list ~pre:"(@[" ~sep:",@ " ~suf:"@])" print_typed_ident) prms
          (pp_opt ~pre:"@[<2>reads@ " (pp_list ~sep:",@ " print_lexpr)) reads
    | LDtype(name,tvar,def) ->
        fprintf fmt "@[<2>type@ %s%a%a;@]" name
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_opt ~pre:"@ =@ " print_typedef) def
    | LDpredicate_reads(name,labels,tvar,prms,reads) ->
        fprintf fmt "@[<2>predicate@ %s%a%a%a@ =@ %a;@]" name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_list ~pre:"(@[" ~sep:",@ " ~suf:"@])" print_typed_ident) prms
          (pp_opt ~pre:"@[<2>reads@ " (pp_list ~sep:",@ " print_lexpr)) reads
    | LDpredicate_def(name,labels,tvar,prms,body) ->
        fprintf fmt "@[<2>predicate@ %s%a%a%a@ =@ %a;@]" name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_list ~pre:"(@[" ~sep:",@ " ~suf:"@])" print_typed_ident) prms
          print_lexpr body
    | LDinductive_def(name,labels,tvar,prms,cases) ->
        let print_case fmt (name,labels,tvar,body) =
          fprintf fmt "@[<2>case@ %s%a%a:@ %a;@]" name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
            print_lexpr body
        in
        fprintf fmt "@[<2>inductive@ %s%a%a@;(%a)@ {@\n%a@]@\n}" name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          (pp_list ~sep:",@ " print_typed_ident) prms
          (pp_list ~sep:"@\n" print_case) cases
    | LDlemma(name,is_axiom,labels,tvar,body) ->
        fprintf fmt "@[<2>%a@ %s%a%a:@ %a;@]"
          (pp_cond ~pr_false:"lemma" is_axiom) "axiom" name
          (pp_list ~pre:"{@[" ~sep:",@ " ~suf:"@]}" pp_print_string) labels
          (pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@>}" pp_print_string) tvar
          print_lexpr body
    | LDaxiomatic (s,d) ->
        fprintf fmt "@[<2>axiomatic@ %s@ {@\n%a@]@\n}" s
          (pp_list ~sep:"@\n" print_decl) d
    | LDinvariant (s,e) ->
        fprintf fmt "@[<2>invariant@ %s:@ %a;@]" s print_lexpr e
    | LDtype_annot ty -> print_type_annot fmt ty
    | LDmodel_annot ty -> print_model_annot fmt ty
    | LDvolatile(tsets,(read,write)) ->
        fprintf fmt "@[<2>volatile@ %a%a%a;@]"
	  (pp_list ~pre:"@[" ~sep:",@ " ~suf:"@]" print_lexpr) tsets
          (pp_opt ~pre:"@ reads@ " pp_print_string) read
          (pp_opt ~pre:"@ writes@ " pp_print_string) write

let print_deps fmt deps =
  match deps with
      FromAny -> ()
    | From l ->
      pp_list ~pre:"@ @[<2>\\from@ " ~sep:",@ " ~suf:"@]" print_lexpr fmt l

let print_assigns fmt a =
  match a with
      WritesAny -> ()
    | Writes l ->
      pp_list ~sep:"@\n"
        (fun fmt (loc,deps) ->
          fprintf fmt "@\nassigns@ %a%a;"
            print_lexpr loc
            print_deps deps)
        fmt l

let print_allocation ~isloop fmt fa = 
  match fa with
    | FreeAllocAny -> ()
    | FreeAlloc([],[]) -> 
	let prefix = if isloop then "loop " else "" in
	  fprintf fmt "@\n%sallocates@ \\nothing;" prefix 
    | FreeAlloc(f,a) ->
	let prefix = if isloop then "loop " else "" in
	let pFreeAlloc kw fmt af =
	  match af with
	    | [] -> ()
	    | _ -> fprintf fmt "@\n%s%s@ %a;" prefix kw (pp_list ~sep:",@ " print_lexpr) a
        in fprintf fmt "%a%a" (pFreeAlloc "frees") f (pFreeAlloc "allocates") a

let print_clause name fmt e = fprintf fmt "@\n%s@ %a;" name print_lexpr e

let print_post fmt (k,e) = 
  print_clause (Cil_printer.get_termination_kind_name k) fmt e

let print_behavior fmt bhv =
  fprintf fmt "@[<2>behavior@ %s:%a%a%a%a%a@]"
    bhv.b_name
    (pp_list ~pre:"" ~suf:"" (print_clause "assumes")) bhv.b_assumes
    (pp_list ~pre:"" ~suf:"" (print_clause "requires")) bhv.b_requires
    (pp_list ~pre:"" ~suf:"" print_post) bhv.b_post_cond
    (print_allocation ~isloop:false) bhv.b_allocation
    print_assigns bhv.b_assigns
    (* TODO: prints extensions *)

let print_variant fmt (v,cmp) =
  fprintf fmt "%a%a;" print_lexpr v
    (pp_opt ~pre:"@ for@ " pp_print_string) cmp

let print_spec fmt spec =
  fprintf fmt "@[%a%a%a%a%a@]"
    (pp_list ~sep:"@\n" ~suf:"@\n" print_behavior) spec.spec_behavior
    (pp_opt ~pre:"decreases@ " ~suf:"@\n" print_variant) spec.spec_variant
    (pp_opt ~pre:"terminates@ " ~suf:"@\n" print_lexpr) spec.spec_terminates
    (pp_list ~pre:"complete@ behaviors@ "
       ~sep:"@\n" ~suf:"@\n" (pp_list ~sep:",@ " pp_print_string))
    spec.spec_complete_behaviors
    (pp_list ~pre:"disjoint@ behaviors@ "
       ~sep:"@\n" ~suf:"@\n" (pp_list ~sep:",@ " pp_print_string))
    spec.spec_disjoint_behaviors

let print_loop_pragma fmt p =
  match p with
      Unroll_specs l -> fprintf fmt "UNROLL@ %a" (pp_list ~sep:",@ " print_lexpr) l
    | Widen_hints l ->
        fprintf fmt "WIDEN_HINTS@ %a" (pp_list ~sep:",@ " print_lexpr) l
    | Widen_variables l ->
        fprintf fmt "WIDEN_VARIABLES@ %a" (pp_list ~sep:",@ " print_lexpr) l

let print_slice_pragma fmt p =
  match p with
    | SPexpr e -> fprintf fmt "expr@ %a" print_lexpr e
    | SPctrl -> pp_print_string fmt "ctrl"
    | SPstmt -> pp_print_string fmt "stmt"

let print_impact_pragma fmt p =
  match p with
    | IPexpr e -> fprintf fmt "expr@ %a" print_lexpr e
    | IPstmt -> pp_print_string fmt "stmt"

let print_pragma fmt p =
  match p with
      Loop_pragma p -> fprintf fmt "loop@ pragma@ %a;" print_loop_pragma p
    | Slice_pragma p -> fprintf fmt "slice@ pragma@ %a;" print_slice_pragma p
    | Impact_pragma p -> fprintf fmt "impact@ pragma@ %a;" print_impact_pragma p

let print_code_annot fmt ca =
  let print_behaviors fmt bhvs =
    (pp_list ~pre:"for@ " ~sep:",@ " ~suf:":@ " pp_print_string) fmt bhvs
  in
  match ca with
      AAssert(bhvs,e) ->
        fprintf fmt "%aassert@ %a;" print_behaviors bhvs print_lexpr e
    | AStmtSpec (bhvs,s) -> 
	fprintf fmt "%a%a" 
	  print_behaviors bhvs 
	  print_spec s
    | AInvariant (bhvs,loop,e) ->
        fprintf fmt "%a%ainvariant@ %a;"
          print_behaviors bhvs (pp_cond loop) "loop@ " print_lexpr e
    | AVariant e -> fprintf fmt "loop@ variant@ %a;" print_variant e
    | AAssigns (bhvs,a) ->
        fprintf fmt "%aloop@ %a" print_behaviors bhvs print_assigns a
    | AAllocation (bhvs,fa) ->
        fprintf fmt "%a%a" print_behaviors bhvs (print_allocation ~isloop:true) fa
    | APragma p -> print_pragma fmt p

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
