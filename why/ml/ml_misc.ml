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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

open Lexing
open Ml_ocaml.Location

module StringMap = Map.Make(String)

(******************************************************************************)

let error x =
  Printf.ksprintf
    (fun s -> Printf.fprintf stderr "%s\n%!" s; exit 1) x

let locate_error loc =
  Printf.ksprintf
    (fun s -> error "File \"%s\", line %d, characters %d-%d:\n%s"
       loc.loc_start.pos_fname
       loc.loc_start.pos_lnum
       (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
       (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)
       s)

let caml_error loc report error =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  report fmt error;
  Format.pp_print_flush fmt ();
  locate_error loc "%s" (Buffer.contents buf)

let log x =
  Printf.ksprintf
    (fun s -> Printf.printf "%s\n%!" s) x

let not_implemented loc =
  Printf.ksprintf (locate_error loc "Not implemented (%s)")

let fresh_int = let c = ref(-1) in fun () -> incr c; !c

let rec list_filter_option acc = function
  | [] -> List.rev acc
  | None::rem -> list_filter_option acc rem
  | (Some x)::rem -> list_filter_option (x::acc) rem
let list_filter_option l = list_filter_option [] l

let rec list_fold_map acc f useracc = function
  | [] -> useracc, List.rev acc
  | x::rem ->
      let useracc', y = f useracc x in
      list_fold_map (y::acc) f useracc' rem
let list_fold_map x = list_fold_map [] x

let rec list_fold_map2 acc f useracc l1 l2 = match l1, l2 with
  | [], [] -> useracc, List.rev acc
  | x1::rem1, x2::rem2 ->
      let useracc', y = f useracc x1 x2 in
      list_fold_map2 (y::acc) f useracc' rem1 rem2
  | _::_, [] | [], _::_ -> raise (Invalid_argument "list_fold_map2")
let list_fold_map2 x = list_fold_map2 [] x

let rec list_fold_mapi i acc f useracc = function
  | [] -> useracc, List.rev acc
  | x::rem ->
      let useracc', y = f useracc i x in
      list_fold_mapi (i+1) (y::acc) f useracc' rem
let list_fold_mapi x = list_fold_mapi 0 [] x

let rec list_mapi f index acc = function
  | [] -> List.rev acc
  | x::rem -> list_mapi f (index + 1) ((f index x)::acc) rem
let list_mapi f = list_mapi f 0 []

let rec list_iteri f index = function
  | [] -> ()
  | x::rem ->
      f index x;
      list_iteri f (index + 1) rem
let list_iteri f = list_iteri f 0

let rec list_fold_lefti f i acc = function
  | [] -> acc
  | x::rem ->
      list_fold_lefti f (i+1) (f i acc x) rem
let list_fold_lefti f = list_fold_lefti f 0

let triple_of_list ?(loc = Ml_ocaml.Location.none) = function
  | [x; y; z] -> x, y, z
  | args -> locate_error loc "3 arguments needed, %d found" (List.length args)

let couple_of_list ?(loc = Ml_ocaml.Location.none) = function
  | [x; y] -> x, y
  | args -> locate_error loc "2 arguments needed, %d found" (List.length args)

let singleton_of_list ?(loc = Ml_ocaml.Location.none) = function
  | [x] -> x
  | args -> locate_error loc "1 argument needed, %d found" (List.length args)

(******************************************************************************)

let identifier_of_symbol_char = function
  | '!' -> "bang"
  | ':' -> "colon"
  | '=' -> "equal"
  | '[' -> "lsquare"
  | ']' -> "rsquare"
  | '\'' -> "prime"
  | c -> String.make 1 c

let identifier_of_symbol = function
  | "ref" -> "jessica_ref"
  | s ->
      let buf = Buffer.create 10 in
      for i = 0 to String.length s - 1 do
	Buffer.add_string buf (identifier_of_symbol_char s.[i])
      done;
      Buffer.contents buf

let idents = Hashtbl.create 111 (* 111 = 42 + 69 *)

let fresh_ident base =
  if Hashtbl.mem idents base then begin
    let i = Hashtbl.find idents base + 1 in
    Hashtbl.replace idents base i;
    identifier_of_symbol base ^ string_of_int i
  end else begin
    Hashtbl.add idents base 0;
    identifier_of_symbol base
  end

(******************************************************************************)

open Jc_ast
open Jc_env
open Jc_fenv
open Jc_output

let default_region = Jc_region.dummy_region

let is_unit t = t = JCTnative Tunit

let is_void_statement_node = function
  | JCTSblock []
  | JCTSexpr({ jc_texpr_node = JCTEconst JCCvoid }) -> true
  | _ -> false

let is_void_statement s = is_void_statement_node s.jc_tstatement_node

let make_expr ?(loc=Loc.dummy_position) ?(label="") ~node ~ty = {
  jc_texpr_node = node;
  jc_texpr_loc = loc;
  jc_texpr_type = ty;
  jc_texpr_label = label; (* ? *)
  jc_texpr_region = default_region;
}

let make_assertion ?(loc=Loc.dummy_position) ?(label="") ~node = {
  jc_assertion_node = node;
  jc_assertion_loc = loc;
  jc_assertion_label = label; (* ? *)
}

let make_term ?(loc=Loc.dummy_position) ?(label="") ~node ~ty = {
  jc_term_node = node;
  jc_term_type = ty;
  jc_term_loc = loc;
  jc_term_label = label;
  jc_term_region = default_region;
}

let make_bool_expr ?(loc=Loc.dummy_position) ?(label="") ~node =
  make_expr ~loc:loc ~label:label ~node:node ~ty:(JCTnative Tboolean)

let make_int_expr ?(loc=Loc.dummy_position) ?(label="") ~node =
  make_expr ~loc:loc ~label:label ~node:node ~ty:(JCTnative Tinteger)

let make_bool_term ?(loc=Loc.dummy_position) ?(label="") ~node =
  make_term ~loc:loc ~label:label ~node:node ~ty:(JCTnative Tboolean)

let make_int_term ?(loc=Loc.dummy_position) ?(label="") ~node =
  make_term ~loc:loc ~label:label ~node:node ~ty:(JCTnative Tinteger)

let make_eq_term a b =
  (* it shouldn't always be "int"... but for the output it works *)
  make_bool_term (JCTbinary(a, Beq_int, b))

let make_eq_expr a b =
  make_expr (JCTEbinary(a, Beq_int, b)) (JCTnative Tboolean)

let make_eq_assertion a b =
  make_assertion (JCAbool_term(make_eq_term a b))

let make_var_term vi =
  make_term (JCTvar vi) vi.jc_var_info_type

let make_var_expr vi =
  make_expr (JCTEvar vi) vi.jc_var_info_type

let make_and a b = match a.jc_assertion_node, b.jc_assertion_node with
  | JCAand al, JCAand bl -> make_assertion (JCAand(al@bl))
  | JCAtrue, _ -> b
  | _, JCAtrue -> a
  | _ -> make_assertion (JCAand [ a; b ])

let make_implies a b = match a.jc_assertion_node with
  | JCAtrue -> b
  | _ -> make_assertion (JCAimplies(a, b))

let make_and_expr a b = match a.jc_texpr_node, b.jc_texpr_node with
  | JCTEconst JCCboolean true, _ -> b
  | _, JCTEconst JCCboolean true -> a
  | _ -> make_bool_expr (JCTEbinary(a, Bland, b))

let make_and_term a b = match a.jc_term_node, b.jc_term_node with
  | JCTconst JCCboolean true, _ -> b
  | _, JCTconst JCCboolean true -> a
  | _ -> make_bool_term (JCTbinary(a, Bland, b))

let make_or a b = match a.jc_assertion_node, b.jc_assertion_node with
  | JCAor al, JCAor bl -> make_assertion (JCAor(al@bl))
  | JCAfalse, _ -> b
  | _, JCAfalse -> a
  | _ -> make_assertion (JCAor [ a; b ])

let make_or_expr a b = match a.jc_texpr_node, b.jc_texpr_node with
  | JCTEconst JCCboolean false, _ -> b
  | _, JCTEconst JCCboolean false -> a
  | _ -> make_bool_expr (JCTEbinary(a, Blor, b))

let make_or_term a b = match a.jc_term_node, b.jc_term_node with
  | JCTconst JCCboolean false, _ -> b
  | _, JCTconst JCCboolean false -> a
  | _ -> make_bool_term (JCTbinary(a, Blor, b))

let make_and_list = List.fold_left make_and (make_assertion JCAtrue)

let make_and_list_expr = List.fold_left make_and_expr
  (make_bool_expr (JCTEconst(JCCboolean true)))

let make_and_list_term = List.fold_left make_and_term
  (make_bool_term (JCTconst(JCCboolean true)))

let make_or_list = List.fold_left make_or (make_assertion JCAfalse)

let make_or_list_expr = List.fold_left make_or_expr
  (make_bool_expr (JCTEconst(JCCboolean false)))

let make_or_list_term = List.fold_left make_or_term
  (make_bool_term (JCTconst(JCCboolean false)))

let expr_of_int i = make_int_expr(JCTEconst(JCCinteger(string_of_int i)))

let term_of_int i = make_int_term(JCTconst(JCCinteger(string_of_int i)))

let make_var_info ~name ~ty = {
    jc_var_info_tag = fresh_int ();
    jc_var_info_name = name;
    jc_var_info_final_name = name;
    jc_var_info_type = ty;
    jc_var_info_formal = false;
    jc_var_info_assigned = false;
    jc_var_info_static = false;
    jc_var_info_region = default_region;
  }

(* Jc_pervasives produces names that will be used by Jessie too, which is bad *)
let new_var = let var_cnt = ref 0 in fun ?add ty ->
  let add = match add with
    | None -> "_"
    | Some s -> "_"^s^"_"
  in
  incr var_cnt;
  let id = "jessica"^add^(string_of_int !var_cnt) in
  make_var_info ~name:id ~ty:ty

let ignored_var ty = make_var_info ~name:"jessica_ignored" ~ty:ty

let expr_seq_to_let =
  List.fold_right
    (fun e acc ->
       make_expr
	 (JCTElet(ignored_var e.jc_texpr_type, e, acc))
	 acc.jc_texpr_type)

let make_statement ?(loc=Loc.dummy_position) s = {
  jc_tstatement_node = s;
  jc_tstatement_loc = loc;
}

let make_statement_block ?(loc=Loc.dummy_position) sl =
  (* remove voids and flatten blocks *)
  let sl = List.map
    (fun s ->
       if is_void_statement s then [] else
	 match s with
	   | { jc_tstatement_node = JCTSblock l } -> l
	   | _ -> [ s ])
    sl
  in
  match List.flatten sl with
    | ([] as l)
    | ([ { jc_tstatement_node = JCTSdecl _ } ] as l)
    | (_::_::_ as l) -> make_statement ~loc:loc (JCTSblock l)
    | [ x ] -> x

let make_affect vi e =
  if is_unit e.jc_texpr_type then
    make_statement (JCTSexpr e)
  else
    make_statement (JCTSexpr({ e with jc_texpr_node = JCTEassign_var(vi, e) }))

let make_affect_field x fi e =
  if is_unit e.jc_texpr_type then
    make_statement (JCTSexpr e)
  else
    make_statement (JCTSexpr({ e with jc_texpr_node =
				 JCTEassign_heap(x, fi, e) }))

let make_affect_field_expr x fi e =
  make_expr (JCTEassign_heap(x, fi, e)) (JCTnative Tunit)

let make_discard e =
  make_statement (JCTSexpr e)

let make_return e =
  make_statement (JCTSreturn(e.jc_texpr_type, e))

let make_var_decl vi init s =
  make_statement (JCTSdecl(vi, init, s))

let make_var_decls =
  List.fold_left (fun acc vi -> make_var_decl vi None acc)

let make_var_tmp ty init cont =
  let vi = new_var ty in
  make_var_decl vi init (cont vi (make_expr (JCTEvar vi) ty))

let make_let_tmp ty f =
  let vi = new_var ty in
  let ve = make_expr (JCTEvar vi) ty in
  let a, b = f vi ve in
  JCTElet(vi, a, b)

let make_pointer ?min ?max tov =
  JCTpointer(tov,
	     (match min with None -> None | Some i -> Some(Num.num_of_int i)),
	     (match max with None -> None | Some i -> Some(Num.num_of_int i)))

let make_valid_pointer tov =
  make_pointer ~min:0 ~max:0 tov

let make_let_alloc_tmp ?(count=expr_of_int 1) si f =
  let ty = make_valid_pointer (JCtag si) in
  let init = make_expr (JCTEalloc(count, si)) ty in
  make_let_tmp ty (fun vi ve -> init, f vi ve)

let make_seq_expr el acc =
  let ty = acc.jc_texpr_type in
  List.fold_left
    (fun acc e -> make_expr (JCTElet(ignored_var e.jc_texpr_type, e, acc)) ty)
    acc
    (List.rev el)

let make_alloc_tmp si =
  let ty = make_valid_pointer (JCtag si) in
  let init = make_expr (JCTEalloc(expr_of_int 1, si)) ty in
  make_var_tmp ty (Some init)

(*let make_alloc_tmp si cont =
  let tmp_ty = make_pointer_type si in
  let tmp_var = new_var tmp_ty in
  let tmp_expr = make_expr (JCTEvar tmp_var) tmp_ty in
  let tmp_init = make_expr (JCTEalloc(expr_of_int 1, si)) tmp_ty in
  make_var_decl tmp_var (Some tmp_init) (cont tmp_var tmp_expr)*)

let void = make_expr (JCTEconst JCCvoid) (JCTnative Tunit)

let make_variant name =
  let name = identifier_of_symbol name in {
    jc_variant_info_name = name;
    jc_variant_info_roots = [];
  }

let make_root_struct vi name =
  let name = identifier_of_symbol name in
  let rec si = {
    jc_struct_info_name = name;
    jc_struct_info_parent = None;
    jc_struct_info_root = si;
    jc_struct_info_fields = [];
    jc_struct_info_variant = Some vi;
  } in
  vi.jc_variant_info_roots <- si::vi.jc_variant_info_roots;
  si

let make_field si name jcty =
  let name = identifier_of_symbol name in
  let fi = {
    jc_field_info_tag = fresh_int ();
    jc_field_info_name = name;
    jc_field_info_final_name = name;
    jc_field_info_type = jcty;
    jc_field_info_struct = si;
    jc_field_info_root = si.jc_struct_info_root;
    jc_field_info_rep = true;
  } in
  si.jc_struct_info_fields <- fi::si.jc_struct_info_fields;
  fi

let make_var name ty =
  let name = identifier_of_symbol name in
  {
    jc_var_info_tag = fresh_int ();
    jc_var_info_name = name;
    jc_var_info_final_name = name;
    jc_var_info_type = ty;
    jc_var_info_region = default_region;
    jc_var_info_formal = false;
    jc_var_info_assigned = false;
    jc_var_info_static = false;
  }

let dummy_variant = make_variant "dummy_variant"
let dummy_struct = make_root_struct dummy_variant "dummy_struct"

let make_struct_def si invs =
  JCstruct_def(
    si.jc_struct_info_name,
    (match si.jc_struct_info_parent with
       | None -> None
       | Some si -> Some si.jc_struct_info_name),
    si.jc_struct_info_fields,
    invs
  )

let make_variant_def vi =
  JCvariant_type_def(
    vi.jc_variant_info_name,
    List.map (fun root -> root.jc_struct_info_name) vi.jc_variant_info_roots
  )

let make_app li args = {
  jc_app_fun = li;
  jc_app_args = args;
  jc_app_region_assoc = [];
  jc_app_label_assoc = [];
}

let make_app_term_node li args = JCTapp (make_app li args)

let quantify q vi body =
  make_assertion (JCAquantifier(q, vi, body))

let quantify_list q =
  List.fold_right (quantify q)

let make_fun_info ~name ~return_type ~params () = {
  jc_fun_info_tag = fresh_int ();
  jc_fun_info_name = name;
  jc_fun_info_final_name = name;
  jc_fun_info_result = make_var ("jessica_"^name^"_result") return_type;
  jc_fun_info_parameters = params;
  jc_fun_info_calls = [];
  jc_fun_info_logic_apps = [];
  jc_fun_info_effects = Jc_pervasives.empty_fun_effect;
  jc_fun_info_return_region = default_region;
  jc_fun_info_param_regions = [];
  jc_fun_info_is_recursive = false;
}

let make_fun_def ~name ~return_type ~params ?body ~spec () =
  JCfun_def(
    return_type,
    name,
    params,
    spec,
    body
  )

let make_behavior ?throws ?assumes ?assigns
    ?(ensures=make_assertion JCAtrue) () =
  {
    jc_behavior_throws = throws;
    jc_behavior_assumes = assumes;
    jc_behavior_assigns = begin match assigns with
      | None -> None
      | Some l -> Some(Loc.dummy_position, l)
    end;
    jc_behavior_ensures = ensures;
  }

let make_fun_spec ?(requires=make_assertion JCAtrue)
    ?(free_requires=make_assertion JCAtrue)
    ?ensures ?assigns ?(behaviors=[]) () =
  {
    jc_fun_requires = requires;
    jc_fun_free_requires = free_requires;
    jc_fun_behavior =
      let b = match ensures, assigns with
	| None, None -> None
	| Some e, None -> Some (make_behavior ~ensures:e ())
	| None, Some a -> Some (make_behavior ~assigns:a ())
	| Some e, Some a -> Some (make_behavior ~ensures:e ~assigns:a ())
      in
      match b with
	| None -> behaviors
	| Some b -> (Loc.dummy_position, "default", b)::behaviors
  }

let make_offset_min term si =
  make_int_term (JCToffset(Offset_min, term, si))

let make_offset_max term si =
  make_int_term (JCToffset(Offset_max, term, si))

let result_var ty =
  make_var_info "\\result" ty

let result_term ty =
  make_var_term (result_var ty)

let make_deref_term a b =
  make_term (JCTderef(a, LabelHere, b)) b.jc_field_info_type

let make_shift_term a b =
  make_term (JCTshift(a, b)) a.jc_term_type

let make_deref_location a b =
  JCLderef(a, LabelHere, b, default_region)

(*
Local Variables: 
compile-command: "unset LANG; make -C .. -f build.makefile jessica.all"
End: 
*)
