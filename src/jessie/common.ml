(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
(**************************************************************************)

(* $Id: common.ml,v 1.31 2008/05/23 16:55:39 uid570 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info

(* Utility functions *)
open Format


(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)

let flatten_multi_dim_array = ref false


(*****************************************************************************)
(* Source locations                                                         *)
(*****************************************************************************)

let is_unknown_location loc =
  (fst loc).Lexing.pos_lnum = 0


(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)

(* Query functions on types *)

let app_term_type f default = function
  | Ctype typ -> f typ
  | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> default

let force_app_term_type f = function
  | Ctype typ -> f typ
  | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> assert false

let get_unique_field ty = match unrollType ty with
  | TComp(compinfo,_) ->
      begin match compinfo.cfields with
	| [content_fi] -> content_fi
	| _ -> 
	    Format.printf "type %a@." !Ast_printer.d_type ty;
	    assert false
      end
  | _ -> assert false

(* Integral types *)

let size_in_bytes ik =
  let size = function
    | IBool -> assert false
    | IChar | ISChar | IUChar -> 1 (* Cil assumes char is one byte *)
    | IInt | IUInt -> !theMachine.sizeof_int
    | IShort | IUShort -> !theMachine.sizeof_short
    | ILong | IULong -> !theMachine.sizeof_long
    | ILongLong | IULongLong -> !theMachine.sizeof_longlong
  in
  size ik

let integral_type_size_in_bytes ty =
  match unrollType ty with
    | TInt(IBool,_attr) -> assert false (* TODO *)
    | TInt(ik,_attr) -> size_in_bytes ik
    | TEnum _ -> !theMachine.sizeof_enum
    | _ -> assert false

let integral_type_size_in_bits ty =
  integral_type_size_in_bytes ty * 8

let min_value_of_integral_type ty =
  let min_of signed size_in_bytes =
    if signed then
      Big_int.minus_big_int
	(Big_int.power_int_positive_int 2
	  (size_in_bytes * 8 - 1))
    else Big_int.zero_big_int
  in
  match unrollType ty with
    | TInt(IBool,_attr) -> Big_int.zero_big_int
    | TInt(ik,_attr) ->
	min_of (isSigned ik) (size_in_bytes ik)
    | TEnum _ ->
	min_of !theMachine.enum_are_signed !theMachine.sizeof_enum
    | _ -> assert false

let max_value_of_integral_type ty =
  let max_of signed size_in_bytes =
    if signed then
      Big_int.pred_big_int
	(Big_int.power_int_positive_int 2
	  (size_in_bytes * 8 - 1))
    else
      Big_int.pred_big_int
	(Big_int.power_int_positive_int 2
	  (size_in_bytes * 8))
  in
  match unrollType ty with
    | TInt(IBool,_attr) -> Big_int.unit_big_int
    | TInt(ik,_attr) ->
	max_of (isSigned ik) (size_in_bytes ik)
    | TEnum _ ->
	max_of !theMachine.enum_are_signed !theMachine.sizeof_enum
    | _ -> assert false

let name_of_integral_type ty =
  let name_it signed size_in_bytes =
    (if signed then "" else "u") ^ "int"
    ^ (string_of_int (size_in_bytes * 8))
  in
  match unrollType ty with
    | TInt(IBool,_attr) -> "_bool"
    | TInt(ik,_attr) ->
	name_it (isSigned ik) (size_in_bytes ik)
    | TEnum _ ->
	name_it !theMachine.enum_are_signed !theMachine.sizeof_enum
    | _ -> assert false

(* Reference type *)

(* We introduce a reference type, that is different from the C pointer or
 * array type. It is a direct translation in C of the Jessie bounded pointer
 * type, where the lower/upper bounds that can be safely accessed are
 * statically known. To avoid introducing a new type, we reuse the existing
 * C pointer type, with an attribute "arrlen" to give the size.
 * Then, we use it as a regular pointer type. E.g., we allow dynamic allocation
 * of such references:
 *     r = (TRef(T)) (malloc (sizeof(T)));
 * and usual dereference:
 *     T t = *r;
 * Another advantage is it should be working fine with [typeOf], [typeOfLval],
 * [pointed_type] and similar functions.
 *
 * As a result of this transformation, all allocation/releases of memory
 * on a reference type do implicitly allocate/release the fields of reference
 * type. It will be translated in Jessie in various alloc/free statements.
 *)

let arraylen_attr_name = "arraylen"

let mkTRef elemty =
  (* Define the same arguments as for [mkTRefArray] *)
  let size = constant_expr 1L and attr = [] in
  (* Do the same as in [mkTRefArray] *)
  let siz = expToAttrParam size in
  let attr = addAttribute (Attr(arraylen_attr_name,[siz])) attr in
  (* Avoid creating an array for single pointed elements that do not
   * originate in a C array, to avoid having to access to the first
   * element everywhere.
   *)
  TPtr(elemty,attr)

let mkTRefArray (elemty,size,attr) =
  (* Check the array size is of a correct form *)
  ignore (lenOfArray64 (Some size));
  let siz = expToAttrParam size in
  let attr = addAttribute (Attr(arraylen_attr_name,[siz])) attr in
  (* Make the underlying type an array so that indexing it is still valid C. *)
  TPtr(TArray(elemty,Some size,[]),attr)

let reference_size ty =
  match findAttribute arraylen_attr_name (typeAttrs ty) with
    | [AInt i] -> Int64.of_int i
    | _ -> assert false

let is_reference_type ty =
  isPointerType ty && hasAttribute arraylen_attr_name (typeAttrs ty)

let is_array_reference_type ty =
  is_reference_type ty && isArrayType (direct_pointed_type ty)

let reference_of_array ty =
  let rec reftype ty =
    if isArrayType ty then 
      let elty = reftype (direct_element_type ty) in
(*       if array_size ty > 0L then *)
	let size = constant_expr (direct_array_size ty) in
	mkTRefArray(elty,size,[])
(*       else *)
(* 	(\* Array of zero size, e.g. in struct array hack. *\) *)
(* 	TPtr(elty,[]) *)
    else ty
  in
  assert (isArrayType ty);
  reftype ty

(* Locally use 64 bits integers *)
open Integer

let bits_sizeof ty =
  let rec rec_size ?(top_size=false) ty =
    match unrollType ty with
      | TPtr _ ->
	  if is_reference_type ty && not top_size then
	    rec_size (pointed_type ty) * (reference_size ty)
	  else
	    Int64.of_int (bitsSizeOf ty)
      | TArray _ -> assert false (* Removed by translation *)
      | TFun _ ->
	  Errormsg.s
	    (bug "Function pointer type %a not allowed" !Ast_printer.d_type ty)
      | TNamed _ -> assert false (* Removed by call to [unrollType] *)
      | TComp(compinfo,_attr) ->
	  if compinfo.cstruct then
	    List.fold_left
	      (fun acc fi -> acc + (rec_size fi.ftype)) 0L compinfo.cfields
	  else
	    List.fold_left max 0L
	      (List.map (fun fi -> rec_size fi.ftype) compinfo.cfields)
      | TEnum _ | TVoid _ | TInt _ | TFloat _ | TBuiltin_va_list _ ->
	  Int64.of_int (bitsSizeOf ty)
  in
  rec_size ~top_size:true ty

(* Come back to normal 31 bits integers *)
open Pervasives


(*****************************************************************************)
(* Names                                                                     *)
(*****************************************************************************)

(* Name management *)

let unique_name =
  let unique_names = Hashtbl.create 127 in
  function s ->
    try
      let s = if s = "" then "unnamed" else s in
      let count = Hashtbl.find unique_names s in
      incr count;
      s ^ "_" ^ (string_of_int !count)
    with Not_found ->
      Hashtbl.add unique_names s (ref 0); s

let unique_name_if_empty s =
  if s = "" then unique_name "unnamed" else s

(* Type name *)

let string_explode s =
  let rec next acc i =
    if i >= 0 then next (s.[i] :: acc) (i-1) else acc
  in
  next [] (String.length s - 1)

let string_implode ls =
  let s = String.create (List.length ls) in
  ignore (List.fold_left (fun i c -> s.[i] <- c; i + 1) 0 ls);
  s

let filter_alphanumeric s assoc default =
  let is_alphanum c =
    String.contains "abcdefghijklmnopqrstuvwxyz" c
    || String.contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ" c
    || String.contains "123456789" c
    || c = '_'
  in
  let alphanum_or_default c =
    if is_alphanum c then c
    else try List.assoc c assoc with Not_found -> default
  in
  string_implode (List.map alphanum_or_default (string_explode s))

let type_name ty =
  ignore (flush_str_formatter ());
  fprintf str_formatter "%a" !Ast_printer.d_type ty;
  let name = flush_str_formatter () in
  filter_alphanumeric name [('*','x')] '_'


(*****************************************************************************)
(* Visitors                                                                  *)
(*****************************************************************************)

(* Visitor for adding globals and global initializations (for global
 * variables). It delays updates to the file until after the visitor has
 * completed its work, to avoid visiting a newly created global or
 * initialization.
 *)

let attach_detach_mode = ref false
let globinits : stmt list ref = ref []
let globals : global list ref = ref []
let globactions : (unit -> unit) list ref = ref []

let attach_globinit init =
  assert (!attach_detach_mode);
  globinits := init :: !globinits

let attach_global g =
  assert (!attach_detach_mode);
  globals := g :: !globals

let attach_globaction action =
  assert (!attach_detach_mode);
  globactions := action :: !globactions

let detach_globinits file =
  let gif =
    Kernel_function.get_definition (Globals.Functions.get_glob_init file)
  in
  gif.sbody.bstmts <- List.rev_append !globinits gif.sbody.bstmts;
  globinits := []

let detach_globals file =
  file.globals <- !globals @ file.globals;
  List.iter
    (function GVar(v,init,_) -> Globals.Vars.add v init | _ -> ()) !globals;
  globals := []

let detach_globactions () =
  List.iter (fun f -> f ()) !globactions;
  globactions := []

let do_and_update_globals action file =
  attach_detach_mode := true;
  assert (!globinits = [] && !globals = [] && !globactions = []);
  action file;
  detach_globinits file;
  detach_globals file;
  detach_globactions ();
  attach_detach_mode := false

let visit_and_update_globals visitor file =
  do_and_update_globals (visitCilFile visitor) file

(* Visitor for adding statements in front of the body *)

let adding_statement_mode = ref false
let pending_statements_at_beginning : stmt list ref = ref []
let pending_statements_at_end : stmt list ref = ref []

let add_pending_statement ~beginning s =
  assert (!adding_statement_mode);
  if beginning then
    pending_statements_at_beginning := s :: !pending_statements_at_beginning
  else
    pending_statements_at_end := s :: !pending_statements_at_end

let insert_pending_statements f =
  f.sbody.bstmts <-
    List.rev_append !pending_statements_at_beginning f.sbody.bstmts;
  pending_statements_at_beginning := [];
  match !pending_statements_at_end with [] -> () | slist ->
    (* Insert pending statements before return statement *)
    let return_stat =
      Kernel_function.find_return (Globals.Functions.get f.svar)
    in
    (* Remove labels from the single return statement. Leave them on the most
     * external block with cleanup code instead.
     *)
    let s = { return_stat with labels = []; } in
    let block = mkBlock (List.rev_append (s :: slist) []) in
    return_stat.skind <- Block block;
    pending_statements_at_end := []

class proxy_frama_c_visitor (visitor : Visitor.frama_c_visitor) =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  (* Modify visitor on functions so that it prepends/postpends statements *)
  method vfunc f =
    adding_statement_mode := true;
    assert (!pending_statements_at_beginning = []);
    assert (!pending_statements_at_end = []);

    match visitor#vfunc f with
      | SkipChildren ->
	  adding_statement_mode := false;
	  SkipChildren
      | ChangeTo f' ->
	  adding_statement_mode := false;
	  ChangeTo f'
      | DoChildren ->
	  let postaction_func f =
	    insert_pending_statements f;
	    adding_statement_mode := false;
	    f
	  in
	  ChangeDoChildrenPost (f, postaction_func)
      | ChangeDoChildrenPost (f', action) ->
	  let postaction_func f =
	    let f = action f in
	    insert_pending_statements f;
	    adding_statement_mode := false;
	    f
	  in
	  ChangeDoChildrenPost (f', postaction_func)

  (* Inherit all other visitors *)

  (* Methods introduced by the Frama-C visitor *)
  method vfile = visitor#vfile
  method vrooted_code_annotation = visitor#vrooted_code_annotation
  method vglob_aux = visitor#vglob_aux
  method vstmt_aux = visitor#vstmt_aux

  (* Methods from Cil visitor for coding constructs *)
  method vblock = visitor#vblock
  method vvrbl = visitor#vvrbl
  method vvdec = visitor#vvdec
  method vexpr = visitor#vexpr
  method vlval = visitor#vlval
  method voffs = visitor#voffs
  method vinitoffs = visitor#vinitoffs
  method vinst = visitor#vinst
  method vinit = visitor#vinit
  method vtype = visitor#vtype
  method vattr = visitor#vattr
  method vattrparam = visitor#vattrparam

  (* Methods from Cil visitor for logic constructs *)
  method vlogic_type = visitor#vlogic_type
  method vtsets_lhost = visitor#vtsets_lhost
  method vtsets_elem = visitor#vtsets_elem
  method vtsets_lval = visitor#vtsets_lval
  method vtsets_offset = visitor#vtsets_offset
  method vtsets = visitor#vtsets
  method vterm = visitor#vterm
  method vterm_node = visitor#vterm_node
  method vterm_lval = visitor#vterm_lval
  method vterm_lhost = visitor#vterm_lhost
  method vterm_offset = visitor#vterm_offset
  method vlogic_info_decl = visitor#vlogic_info_decl
  method vlogic_info_use = visitor#vlogic_info_use
  method vlogic_var = visitor#vlogic_var
  method vquantifiers = visitor#vquantifiers
  method vpredicate = visitor#vpredicate
  method vpredicate_named = visitor#vpredicate_named
  method vpredicate_info_decl = visitor#vpredicate_info_decl
  method vpredicate_info_use = visitor#vpredicate_info_use
  method vbehavior = visitor#vbehavior
  method vspec = visitor#vspec
  method vassigns = visitor#vassigns
  method vloop_pragma = visitor#vloop_pragma
  method vslice_pragma = visitor#vslice_pragma
  method vzone = visitor#vzone
  method vcode_annot = visitor#vcode_annot
  method vtype_annot = visitor#vtype_annot
  method vannotation = visitor#vannotation

end

let visit_and_push_statements visit visitor file =
  let visitor = new proxy_frama_c_visitor visitor in
  visit (visitor :> cilVisitor) file

(* Visitor for tracing computation *)

class trace_frama_c_visitor (visitor : Visitor.frama_c_visitor) =
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  (* Inherit all visitors, printing visited item on the way *)

  (* Methods introduced by the Frama-C visitor *)
  method vfile = visitor#vfile
  method vrooted_code_annotation = visitor#vrooted_code_annotation
  method vglob_aux g =
    Format.printf "%a@." !Ast_printer.d_global g;
    visitor#vglob_aux g
  method vstmt_aux s =
    Format.printf "%a@." !Ast_printer.d_stmt s;
    visitor#vstmt_aux s

  (* Methods from Cil visitor for coding constructs *)
  method vfunc = visitor#vfunc
  method vblock b =
    Format.printf "%a@." !Ast_printer.d_block b;
    visitor#vblock b
  method vvrbl v =
    Format.printf "%s@." v.vname;
    visitor#vvrbl v
  method vvdec v =
    Format.printf "%s@." v.vname;
    visitor#vvdec v
  method vexpr e =
    Format.printf "%a@." !Ast_printer.d_exp e;
    visitor#vexpr e
  method vlval lv =
    Format.printf "%a@." !Ast_printer.d_lval lv;
    visitor#vlval lv
  method voffs off =
    Format.printf "%a@." !Ast_printer.d_offset off;
    visitor#voffs off
  method vinitoffs off =
    Format.printf "%a@." !Ast_printer.d_offset off;
    visitor#vinitoffs off
  method vinst i =
    Format.printf "%a@." !Ast_printer.d_instr i;
    visitor#vinst i
  method vinit = visitor#vinit
  method vtype ty =
    Format.printf "%a@." !Ast_printer.d_type ty;
    visitor#vtype ty
  method vattr attr =
    Format.printf "%a@." !Ast_printer.d_attr attr;
    visitor#vattr attr
  method vattrparam pattr =
    Format.printf "%a@." !Ast_printer.d_attrparam pattr;
    visitor#vattrparam pattr

  (* Methods from Cil visitor for logic constructs *)
  method vlogic_type lty =
    Format.printf "%a@." !Ast_printer.d_logic_type lty;
    visitor#vlogic_type lty
  method vtsets_lhost tslhost =
    Format.printf "%a@." !Ast_printer.d_tsets_lhost tslhost;
    visitor#vtsets_lhost tslhost
  method vtsets_elem ts =
    Format.printf "%a@." !Ast_printer.d_tsets_elem ts;
    visitor#vtsets_elem ts
  method vtsets_lval tslv =
    Format.printf "%a@." !Ast_printer.d_tsets_lval tslv;
    visitor#vtsets_lval tslv
  method vtsets_offset tsoff =
    Format.printf "%a@." !Ast_printer.d_tsets_offset tsoff;
    visitor#vtsets_offset tsoff
  method vtsets ts =
    Format.printf "%a@." !Ast_printer.d_tsets ts;
    visitor#vtsets ts
  method vterm t =
    Format.printf "%a@." !Ast_printer.d_term t;
    visitor#vterm t
  method vterm_node = visitor#vterm_node
  method vterm_lval tlv =
    Format.printf "%a@." !Ast_printer.d_term_lval tlv;
    visitor#vterm_lval tlv
  method vterm_lhost = visitor#vterm_lhost
  method vterm_offset = visitor#vterm_offset
  method vlogic_info_decl = visitor#vlogic_info_decl
  method vlogic_info_use = visitor#vlogic_info_use
  method vlogic_var lv =
    Format.printf "%a@." !Ast_printer.d_logic_var lv;
    visitor#vlogic_var lv
  method vquantifiers = visitor#vquantifiers
  method vpredicate = visitor#vpredicate
  method vpredicate_named p =
    Format.printf "%a@." !Ast_printer.d_predicate_named p;
    visitor#vpredicate_named p
  method vpredicate_info_decl = visitor#vpredicate_info_decl
  method vpredicate_info_use = visitor#vpredicate_info_use
  method vbehavior = visitor#vbehavior
  method vspec funspec =
    Format.printf "%a@." !Ast_printer.d_funspec funspec;
    visitor#vspec funspec
  method vassigns = visitor#vassigns
  method vloop_pragma = visitor#vloop_pragma
  method vslice_pragma = visitor#vslice_pragma
  method vzone = visitor#vzone
  method vcode_annot annot =
    Format.printf "%a@." !Ast_printer.d_code_annotation annot;
    visitor#vcode_annot annot
  method vtype_annot annot =
    Format.printf "%a@." !Ast_printer.d_type_annotation annot;
    visitor#vtype_annot annot
  method vannotation annot =
    Format.printf "%a@." !Ast_printer.d_annotation annot;
    visitor#vannotation annot

end

let visit_and_trace_framac visit visitor file =
  let visitor = new trace_frama_c_visitor visitor in
  visit visitor file

let visit_and_trace_cil visit visitor file =
  let visitor = new trace_frama_c_visitor visitor in
  visit (visitor :> cilVisitor) file

(* Visitor for fixpoint computation *)

let change = ref false

let signal_change () = change := true

let visit_until_convergence visitor file =
  change := true;
  while !change do
    change := false;
    visitCilFile visitor file;
  done

(* Visitor methods for sharing preaction/postaction between exp/term/tsets *)

let do_on_term_lval (preaction_lval,postaction_lval) tlv =
  let preaction_tlval tlv =
    match preaction_lval with None -> tlv | Some preaction_lval ->
      let lv,env = !Db.Properties.Interp.force_term_lval_to_lval tlv in
      let lv = preaction_lval lv in
      !Db.Properties.Interp.force_back_lval_to_term_lval env lv
  in
  let postaction_tlval tlv =
    match postaction_lval with None -> tlv | Some postaction_lval ->
      let lv,env = !Db.Properties.Interp.force_term_lval_to_lval tlv in
      let lv = postaction_lval lv in
      !Db.Properties.Interp.force_back_lval_to_term_lval env lv
  in
  ChangeDoChildrenPost (preaction_tlval tlv, postaction_tlval)

let do_on_tsets_lval (preaction_lval,postaction_lval) tslv =
  let preaction_tslval tslv =
    match preaction_lval with None -> tslv | Some preaction_lval ->
      let lv,env = !Db.Properties.Interp.force_tsets_lval_to_lval tslv in
      let lv = preaction_lval lv in
      !Db.Properties.Interp.force_back_lval_to_tsets_lval env lv
  in
  let postaction_tslval tslv =
    match postaction_lval with None -> tslv | Some postaction_lval ->
      let lv,env = !Db.Properties.Interp.force_tsets_lval_to_lval tslv in
      let lv = postaction_lval lv in
      !Db.Properties.Interp.force_back_lval_to_tsets_lval env lv
  in
  ChangeDoChildrenPost (preaction_tslval tslv, postaction_tslval)

let do_on_term (preaction_expr,postaction_expr) t =
  let preaction_term t =
    match preaction_expr with None -> t | Some preaction_expr ->
      let e,env = !Db.Properties.Interp.force_term_to_exp t in
      let e = map_under_info preaction_expr e in
      !Db.Properties.Interp.force_back_exp_to_term env e
  in
  let postaction_term t =
    match postaction_expr with None -> t | Some postaction_expr ->
      let e,env = !Db.Properties.Interp.force_term_to_exp t in
      let e = map_under_info postaction_expr e in
      !Db.Properties.Interp.force_back_exp_to_term env e
  in
  ChangeDoChildrenPost (preaction_term t, postaction_term)

let do_on_tsets_elem (preaction_expr,postaction_expr) ts =
  let preaction_tselem ts =
    match preaction_expr with None -> ts | Some preaction_expr ->
      let e,env = !Db.Properties.Interp.force_tsets_elem_to_exp ts in
      let e = preaction_expr e in
      !Db.Properties.Interp.force_back_exp_to_tsets_elem env e
  in
  let postaction_tselem ts =
    match postaction_expr with None -> ts | Some postaction_expr ->
      let e,env = !Db.Properties.Interp.force_tsets_elem_to_exp ts in
      let e = postaction_expr e in
      !Db.Properties.Interp.force_back_exp_to_tsets_elem env e
  in
  ChangeDoChildrenPost (preaction_tselem ts, postaction_tselem)


(*****************************************************************************)
(* Debugging                                                                 *)
(*****************************************************************************)

let checking = true

let print_to_stdout file =
  (* Printer takes into account annotations *)
  dumpFile (new Printer.print ()) stdout "stdout" file

class checkTypes =
  let preaction_expr e = ignore (typeOf e); e in
object

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vexpr e =
    ChangeDoChildrenPost (preaction_expr e, fun x -> x)

(* I comment on purpose additional verifications, because they cause some tests 
   to fail, see e.g. [copy_struct], because term-lhost TResult does not have 
   a type for a proper conversion to expression. *)

(*   method vterm = *)
(*     do_on_term (Some preaction_expr,None) *)

(*   method vtsets_elem = *)
(*     do_on_tsets_elem (Some preaction_expr,None) *)

end

let check_types file =
  let visitor = new checkTypes in
  visitCilFile (visitor :> cilVisitor) file

class check_file: Visitor.frama_c_visitor  =
object(self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  val known_fields = FieldinfoHashtbl.create 7

  method vterm t =
    match t.term_node with
      | TLval _ ->
	  begin match t.term_type with
	    | Ctype ty ->
		if isVoidType ty then
		  Format.printf "%a@." !Ast_printer.d_term t;
		assert (not (isVoidType ty)); DoChildren
	    | _ -> DoChildren
	  end
      | _ -> DoChildren

  method voffs = function
      NoOffset -> SkipChildren
    | Index _ -> DoChildren
    | Field(fi,_) ->
        begin
          try
            if not (fi == FieldinfoHashtbl.find known_fields fi)
            then
              Errormsg.s
                (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                   fi.fname fi.fcomp.cname)
          with Not_found ->
            Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                          fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method vterm_offset = function
      TNoOffset -> SkipChildren
    | TIndex _ -> DoChildren
    | TField(fi,_) ->
        begin
          try
            if not (fi == FieldinfoHashtbl.find known_fields fi)
            then
              Errormsg.s
                (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                   fi.fname fi.fcomp.cname)
          with Not_found ->
            Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                          fi.fname fi.fcomp.cname)
        end;
        DoChildren

  method private check_tsets_offset tsoff =
    begin match tsoff with
	TSNo_offset | TSIndex _ | TSRange _ -> ()
      | TSField(fi,_) ->
          begin
            try
              if not (fi == FieldinfoHashtbl.find known_fields fi)
              then
		Errormsg.s
                  (Cil.error "[AST Integrity Check] field %s of type %s is not \
                            shared between declaration and use"
                    fi.fname fi.fcomp.cname)
            with Not_found ->
              Errormsg.s (Cil.error "[AST Integrity Check] field %s of \
                                   type %s is unbound in the AST"
                fi.fname fi.fcomp.cname)
          end
    end;
    tsoff

  method vtsets_offset tsoff =
    ChangeDoChildrenPost(self#check_tsets_offset tsoff,self#check_tsets_offset)

  method vinitoffs = self#voffs

  method vglob_aux = function
      GCompTag(c,_) ->
        List.iter
          (fun x -> FieldinfoHashtbl.add known_fields x x) c.cfields;
        DoChildren
    | _ -> DoChildren

end


(*****************************************************************************)
(* Miscellaneous                                                             *)
(*****************************************************************************)

(* Smart constructors *)

let mkterm tnode ty loc =
  {
    term_node = tnode;
    term_loc = loc;
    term_type = ty;
    term_name = []
  }

let mkInfo e =
  match e with Info _ -> e | _ ->
    let einfo = {
      exp_loc = locUnknown;
      (* In many cases, the correct type may not be available, as
       * the expression may result from a conversion from a term or a tset.
       * Calling [typeOf] on such an expression may raise an error.
       * Therefore, put here a dummy type until tsets correctly typed.
       *)
      exp_type = Ctype voidType; (* Ctype(typeOf e); *)
      exp_name = [];
    } in
    Info(e,einfo)

(* Manipulation of offsets *)

let rec offset_list = function
  | NoOffset -> []
  | Field (fi,off) -> (Field (fi, NoOffset)) :: offset_list off
  | Index (e,off) -> (Index (e, NoOffset)) :: offset_list off

let is_last_offset = function
  | NoOffset -> true
  | Field (_fi,NoOffset) -> true
  | Field (_fi,_) -> false
  | Index (_e,NoOffset) -> true
  | Index (_e,_) -> false

(* Transform an index on a multi-dimensional array into an index on the
 * same array that would be flattened to a single dimensional array.
 *)
let rec lift_offset ty = function
  | Index(idx1,(Index(_idx2, _off) as suboff)) ->
      let subty = direct_element_type ty in
      let siz = array_size subty in
      begin match lift_offset subty suboff with
	| Index(idx, off) ->
	    let mulidx = BinOp(Mult,idx1,constant_expr siz,intType) in
	    (* Keep info at top-level for visitors on terms that where
	     * translated to expressions. Those expect these info when
	     * translating back to term.
	     *)
	    let addidx =
	      map_under_info (fun _e -> BinOp(PlusA,mulidx,idx,intType)) idx1
	    in
	    Index(addidx,off)
	| _ -> assert false
      end
  | Index(idx1,NoOffset) as off ->
      let subty = direct_element_type ty in
      if isArrayType subty then
	let siz = array_size subty in
	(* Keep info at top-level *)
	let mulidx =
	  map_under_info
	    (fun _e -> BinOp(Mult,idx1,constant_expr siz,intType)) idx1
	in
	Index(mulidx,NoOffset)
      else off
  | off -> off

(* Predefined entities *)

let name_of_offset_min = "offset_min"
let name_of_offset_max = "offset_max"

let name_of_assert = "assert"
let name_of_free = "free"
let name_of_malloc = "malloc"
let name_of_calloc = "calloc"

let jessie_memory_model =
  [ (* logic functions *)
    name_of_offset_min; name_of_offset_max;
    (* coding functions *)
    name_of_assert; name_of_malloc; name_of_calloc; name_of_free;
  ]

let in_jessie_memory_model s = List.mem s jessie_memory_model

let is_assert_function v = isFunctionType v.vtype && v.vname = name_of_assert
let is_free_function v = isFunctionType v.vtype && v.vname = name_of_free
let is_malloc_function v = isFunctionType v.vtype && v.vname = name_of_malloc
let is_calloc_function v = isFunctionType v.vtype && v.vname = name_of_calloc

(* Allocation/deallocation *)

let malloc_function () =
  try
    Kernel_function.get_vi (Globals.Functions.find_by_name "malloc")
  with Not_found ->
    let params = Some ["size",uintType,[]] in
    let f =
      findOrCreateFunc
	(Cil_state.file ()) "malloc" (TFun(voidPtrType,params,false,[]))
    in
    let behav = {
      b_name = "malloc_default";
      b_assumes = [];
      b_ensures = [];
      b_assigns = [];
    } in
    let spec = { (empty_funspec ()) with spec_behavior = [behav]; } in
    (* No easy way to create a [fundec] from a [varinfo] in order to create
     * an adequate list of parameter [varinfo]. Pass [None] for now.
     *)
    Globals.Functions.add
      (Db_types.Declaration(spec,f,None,locUnknown));
    f

let free_function () =
  try
    Kernel_function.get_vi (Globals.Functions.find_by_name "free")
  with Not_found ->
    let params = Some ["ptr",voidPtrType,[]] in
    let f =
      findOrCreateFunc
	(Cil_state.file ()) "free" (TFun(voidType,params,false,[]))
    in
    (* No easy way to create a [fundec] from a [varinfo] in order to create
     * an adequate list of parameter [varinfo]. Pass [None] for now.
     *)
    Globals.Functions.add
      (Db_types.Declaration(empty_funspec (),f,None,locUnknown));
    f

let mkalloc v ty loc =
  let callee = Lval(Var(malloc_function ()),NoOffset) in
  let arg = sizeOf ty in
  Call(Some(Var v,NoOffset),callee,[arg],loc)

let mkalloc_statement v ty loc = mkStmt (Instr(mkalloc v ty loc))

let mkalloc_array v ty num loc =
  let callee = Lval(Var(malloc_function ()),NoOffset) in
  let arg = constant_expr (Int64.mul num (Int64.of_int (sizeOf_int ty))) in
  Call(Some(Var v,NoOffset),callee,[arg],loc)

let mkalloc_array_statement v ty num loc =
  mkStmt (Instr(mkalloc_array v ty num loc))

let mkfree v loc =
  let callee = Lval(Var(free_function ()),NoOffset) in
  let arg = Lval(Var v,NoOffset) in
  Call(None,callee,[arg],loc)

let mkfree_statement v loc = mkStmt (Instr(mkfree v loc))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
