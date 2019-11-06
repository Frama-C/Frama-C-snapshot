(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types
open Cil
open Logic_ptree
open Logic_const
open Logic_utils
open Format

exception Backtrack

let ($) = Extlib.($)

let add_offset_lval =
  Kernel.deprecated
    "Logic_typing.add_offset_lval"
    ~now:"Logic_const.addTermOffsetLval"
    Logic_const.addTermOffsetLval

let loc_join (b,_) (_,e) = (b,e)
let unescape s = Logic_lexer.chr (Lexing.from_string s)

let wcharlist_of_string s =
  let res = ref [] in
  let i = ref 0 in
  let rec treat_escape_octal n nb_pass =
    if nb_pass > 2 then res:= n::!res
    else if !i >= String.length s then res:= n::!res
    else match String.get s !i with
        x when '0' <= x && x <= '9' ->
        incr i;
        treat_escape_octal
          (Int64.add (Int64.mul (Int64.of_int 8) n)
             (Int64.of_int (Char.code x - Char.code '0'))) (nb_pass + 1)
      | _ -> res:= n::!res
  in
  let rec treat_escape_hexa n =
    if !i >= String.length s then res:= n::!res
    else match String.get s !i with
        x when '0' <= x && x <= '9' ->
        incr i;
        treat_escape_hexa
          (Int64.add (Int64.mul (Int64.of_int 16) n)
             (Int64.of_int (Char.code x - Char.code '0')))
      | x when 'A' <= x && x <= 'F' ->
        incr i;
        treat_escape_hexa
          (Int64.add (Int64.mul (Int64.of_int 16) n)
             (Int64.of_int (Char.code x - Char.code 'A' + 10)))
      | x when 'a' <= x && x <= 'f' ->
        incr i;
        treat_escape_hexa
          (Int64.add (Int64.mul (Int64.of_int 16) n)
             (Int64.of_int (Char.code x - Char.code 'a' + 10)))
      | _ -> res:= n::!res
  in
  let treat_escape_sequence () =
    if !i >= String.length s then
      Kernel.warning ~current:true "Ill-formed escape sequence in wide string"
    else begin
      match String.get s !i with
        x when '0' <= x && x <= '9' ->
        treat_escape_octal Int64.zero 0
      | 'x' -> incr i; treat_escape_hexa Int64.zero
      | 'a' -> incr i; res:= Int64.of_int 7::!res
      | 'b' -> incr i; res:= Int64.of_int 8::!res
      | 'f' -> incr i; res:= Int64.of_int 12::!res
      | 'n' -> incr i; res:= Int64.of_int (Char.code '\n') :: !res
      | 'r' -> incr i; res:=Int64.of_int (Char.code '\r')::!res
      | 't' -> incr i; res:= Int64.of_int (Char.code '\t') ::!res
      | '\'' -> incr i; res:=Int64.of_int (Char.code '\'')::!res
      | '"' -> incr i; res:= Int64.of_int (Char.code '"') ::!res
      | '?' -> incr i; res:= Int64.of_int (Char.code '?') ::!res
      | '\\' -> incr i; res:= Int64.of_int (Char.code '\\')::!res
      | c ->
        incr i;
        Kernel.warning ~current:true
          "Ill-formed escape sequence in wide string";
        res:= Int64.of_int (Char.code c) :: !res
    end
  in
  while (!i < String.length s) do
    match String.get s !i with
    | '\\' -> incr i; treat_escape_sequence ()
    | c -> res := Int64.of_int (Char.code c)::!res; incr i
  done;
  List.rev (!res)

let type_of_list_elem t = Logic_const.type_of_list_elem (unroll_type t)

let is_list_type t = Logic_const.is_list_type (unroll_type t)

let type_of_set_elem t = Logic_const.type_of_element (unroll_type t)

let is_set_type t = Logic_const.is_set_type (unroll_type t)

let plain_mk_mem ?loc t ofs = match t.term_node with
  | TAddrOf lv -> Logic_const.addTermOffsetLval ofs lv
  | TStartOf lv ->
    Logic_const.addTermOffsetLval (TIndex (Cil.lzero ?loc (), ofs)) lv
  | _ -> TMem t, ofs

let optimize_comprehension term =
  (* [term] is equal to {t<x> | \subset(x, set)}. We are trying to get rid
     of the comprehension by lifting the operations done in [t<x>] over it. *)
  let lift_operation_above_subset set x t =
    let loc = set.term_loc in
    (* Auxiliary function that maps [f] over [set], providing [set] is an
       lvalue. The other cases are too complex. *)
    let lval_term f =
      match set.term_node with
      | TLval lv -> f lv
      | _ -> term
    in
    let lval f typ = lval_term (fun lv -> Logic_const.term ~loc (f lv) typ) in
    let is_x y = Cil_datatype.Logic_var.equal x y in
    let set_type = make_set_type t.term_type in
    match t.term_node with
    | TLval (TVar y, TNoOffset) when is_x y ->
      set (* { x | \subset(x, set) } -> set *)
    | TLval (TVar y, o) when is_x y ->
      (* { x.o | \subset(x, set) } -> set.o *)
      lval (fun lv -> TLval (Logic_const.addTermOffsetLval o lv)) set_type
    | TLval (TMem { term_node = TLval (TVar y, TNoOffset)},o2)
      when is_x y -> (* { *(x+o2) | \subset(x, set) } -> *(set+o2) *)
      Logic_const.term ~loc (TLval (plain_mk_mem ~loc set o2)) set_type
    | TLval (TMem { term_node = TLval (TVar y, o1); term_type = ty},o2)
      when is_x y -> (* { (x+o1)->o2 | subset(x, set) } -> (set+o1)->o2*)
      lval
        (fun lv ->
           TLval
             (plain_mk_mem ~loc
                (Logic_const.term ~loc
                   (TLval (Logic_const.addTermOffsetLval o1 lv))
                   (make_set_type ty)) o2))
        set_type
    | TLval
        (TMem
           { term_node =
               TBinOp(op, { term_node = TLval (TVar y, o1);
                            term_type = ty }, shift)},o2)
      when is_x y -> (* {(op(x+o1, shift))->o2} -> (op(set+o1, shift))->o2 *)
      let inner_set_type = make_set_type ty in
      lval
        (fun lv ->
           TLval
             (TMem(
                 Logic_const.term ~loc
                   (TBinOp(
                       op,
                       Logic_const.term ~loc
                         (TLval (Logic_const.addTermOffsetLval o1 lv))
                         inner_set_type,
                       shift)) inner_set_type),o2))
        set_type
    | TUnOp (op, { term_node = TLval(TVar y,TNoOffset)}) when is_x y ->
      (* { op(x) | \subset(x, set) } -> op(set) *)
      Logic_const.term ~loc (TUnOp(op,set)) set_type
    | TBinOp(op,{term_node = TLval(TVar y, TNoOffset)},t2) when is_x y ->
      (* { op(x, t2) | \subset(x, set) } -> op(set, t2) *)
      Logic_const.term ~loc (TBinOp(op,set,t2)) set_type
    | TBinOp(op,t1,{term_node = TLval(TVar y, TNoOffset)}) when is_x y ->
      (* { op(t1, x) | \subset(x, set) } -> op(t1, x) *)
      Logic_const.term ~loc (TBinOp(op,t1,set)) set_type
    | TAddrOf (TVar y, o) when is_x y ->
      (* { &x->o | \subset(x, set) } -> &set->o *)
      lval_term
        (fun lv ->
           Logic_utils.mk_logic_AddrOf
             ~loc (Logic_const.addTermOffsetLval o lv)
             (Cil.typeTermOffset set.term_type o))
    | TStartOf (TVar y,o) when is_x y ->
      (* { &x[0]->o | \subset(x, set) } -> &set[0]->o *)
      lval_term
        (fun lv ->
           let lv = Logic_const.addTermOffsetLval o lv in
           let ty = Cil.typeOfTermLval lv in
           Logic_utils.mk_logic_StartOf (Logic_const.term ~loc (TLval lv) ty))
    | TLogic_coerce(lt,{ term_node = TLval(TVar y,TNoOffset)}) when is_x y ->
      (* { (lt)x | \subset(x, set) } -> (lt set)set *)
      { t with
        term_node = TLogic_coerce(Logic_const.make_set_type lt,set);
        term_type = Logic_const.make_set_type lt }
    | _ -> term
  in
  match term.term_node with
  | Tcomprehension
      (t, [x],
       Some
         { pred_content =
             Papp({l_var_info = {lv_name="\\subset"}},[],[elt;set]) }) ->
    (match elt.term_node with
     | TLogic_coerce
         (_,
          { term_node =
              TLval(TVar y, TNoOffset) })
       when Cil_datatype.Logic_var.equal x y ->
       lift_operation_above_subset set x t
     | _ -> term)
  | _ -> term

(* apply a function meant to operate on plain types to a possible set. *)
let lift_set f loc =
  let rec aux loc =
    match loc.term_node with
    | Tcomprehension(t,q,p) ->
      let t' = aux t in
      let typ = Logic_const.make_set_type t'.term_type in
      { loc with term_node = Tcomprehension(t',q,p); term_type = typ}
    | Tunion l -> {loc with term_node = Tunion(List.map aux l)}
    | Tinter l -> {loc with term_node = Tinter(List.map aux l)}
    | Tempty_set -> loc
    (* coercion from a set to another set: keep the current coercion
       over the result of the transformation. *)
    | TLogic_coerce(set,t1)
      when is_set_type set && is_set_type t1.term_type ->
      let res = aux t1 in
      { loc with term_node = TLogic_coerce(set, res) }
    (* coercion from a singleton to a set: performs the transformation.
    *)
    | TLogic_coerce(oset, t1) when is_set_type oset ->
      let t = f t1 in
      let nset = make_set_type t.term_type in
      (* performs the coercion into a set. *)
      let singleton_coerce =
        { t with term_node = TLogic_coerce(nset, t); term_type = nset }
      in
      (* see whether we have to coerce the set type itself. *)
      if is_same_type oset nset then singleton_coerce
      else { loc with term_node = TLogic_coerce(oset, singleton_coerce) }
    (* if we a term of type set, try to apply f to each
       element of x by using a comprehension, and see whether we can get
       rid of said comprehension afterwards. *)
    | _  when is_set_type loc.term_type ->
      let elt_type = type_of_set_elem loc.term_type in
      let x = Cil_const.make_logic_var_quant "_x" elt_type in
      let t = Logic_const.tvar ~loc:loc.term_loc x in
      let sub = Logic_env.find_all_logic_functions "\\subset" in
      (* only one \subset function *)
      let sub = List.hd sub in
      let t2 = Logic_const.tvar ~loc:loc.term_loc x in
      let t2 =
        Logic_const.term
          ~loc:loc.term_loc (TLogic_coerce (loc.term_type,t2)) loc.term_type
      in
      let p = Logic_const.papp ~loc:loc.term_loc (sub, [], [t2;loc]) in
      let c = { loc with term_node = Tcomprehension(t,[x],Some p) } in
      let res = aux c in
      optimize_comprehension res
    (* plain term: apply the function directly. *)
    | _ -> f loc
  in aux loc

let is_same_type t1 t2 =
  Cil_datatype.Logic_type.equal
    (Logic_utils.unroll_type t1) (Logic_utils.unroll_type t2)

let type_rel = function
  | Eq -> Cil_types.Req
  | Neq -> Cil_types.Rneq
  | Lt -> Cil_types.Rlt
  | Le -> Cil_types.Rle
  | Gt -> Cil_types.Rgt
  | Ge -> Cil_types.Rge

let type_binop = function
  | Badd -> PlusA
  | Bsub -> MinusA
  | Bmul -> Mult
  | Bdiv -> Div
  | Bmod -> Mod
  | Bbw_and -> BAnd
  | Bbw_or -> BOr
  | Bbw_xor -> BXor
  | Blshift -> Shiftlt
  | Brshift -> Shiftrt

let binop_of_rel = function
  | Eq -> Cil_types.Eq
  | Neq -> Cil_types.Ne
  | Ge -> Cil_types.Ge
  | Gt -> Cil_types.Gt
  | Le -> Cil_types.Le
  | Lt -> Cil_types.Lt

(* Logical environments *)

module Lenv = struct
  (* locals: logic variables (e.g. quantified variables in \forall, \exists) *)

  module Smap = FCMap.Make(String)

  type t = {
    local_vars: Cil_types.logic_var Smap.t;
    local_logic_info: Cil_types.logic_info Smap.t;
    type_vars: Cil_types.logic_type Smap.t;
    logic_labels: Cil_types.logic_label Smap.t;
    current_logic_label: Cil_types.logic_label option;
    is_post_state: Cil_types.termination_kind option;
    is_funspec: bool;
    enclosing_post_state: Cil_types.termination_kind option;
    (* to determine in which post-state we should go in case of nested
       \at(\at(...,Post),Pre)
    *)
  }

  let string_of_current_label env =
    Extlib.opt_bind (
      function
      | FormalLabel _ -> None
      | BuiltinLabel Init -> Some "Init"
      | BuiltinLabel Pre -> Some "Pre"
      | BuiltinLabel Old -> Some "Old"
      | BuiltinLabel Post -> Some "Post"
      | BuiltinLabel Here -> Some "Here"
      | BuiltinLabel LoopCurrent -> Some "LoopCurrent"
      | BuiltinLabel LoopEntry -> Some "LoopEntry"
      | StmtLabel s ->
        (match
           List.find_opt
             (function Label (_,_,b) -> b | _ -> false) !s.labels
         with
         | None -> None
         | Some (Label (lab,_,_)) -> Some lab
         | Some _ -> None))
      env.current_logic_label

  let fresh_var env name kind typ =
    let name =
      let exists name =
        Smap.mem name env.local_vars ||
        Smap.mem name env.local_logic_info ||
        (Logic_env.find_all_logic_functions name <> [])
      in
      let rec aux i =
        if i < 0 then
          Kernel.fatal ~current:true "Out of indexes for temp logic var";
        let name' = name ^ "_" ^ (string_of_int i) in
        if exists name' then aux (i+1) else name'
      in if exists name then aux 0 else name
    in Cil_const.make_logic_var_kind name kind typ

  let no_label env = Smap.is_empty env.logic_labels

  let enter_post_state env kind =
    let real_kind =
      match kind, env.enclosing_post_state with
      | _, None -> kind
      | Normal, Some kind -> kind
      | _, Some _ ->
        Kernel.fatal ~current:true "Inconsistent logic labels env stack"
    in
    { env with
      is_post_state = Some real_kind; enclosing_post_state = Some real_kind
    }

  let exit_post_state env = { env with is_post_state = None }

  let current_post_state env = env.is_post_state

  let add_var v var env =
    { env with local_vars = Smap.add v var env.local_vars }
  let find_var v env = Smap.find v env.local_vars
  let add_type_var v typ env =
    { env with type_vars = Smap.add v typ env.type_vars }
  let find_type_var v env = Smap.find v env.type_vars

  let add_logic_info v li env =
    let env =
      { env with local_logic_info = Smap.add v li env.local_logic_info }
    in add_var v li.l_var_info env
  let find_logic_info v env = Smap.find v env.local_logic_info

  (* logic labels *)
  let add_logic_label l lab env =
    { env with logic_labels = Smap.add l lab env.logic_labels }
  let find_logic_label l env = Smap.find l env.logic_labels

  let set_current_logic_label lab env =
    let env =
      { env with current_logic_label = Some lab }
    in match lab with
      BuiltinLabel Post -> enter_post_state env Normal
    | BuiltinLabel Here -> env
    | BuiltinLabel _ | FormalLabel _ | StmtLabel _ -> exit_post_state env

  let default_label = ref None

  let empty () =
    default_label := None;
    {
      local_vars = Smap.empty;
      local_logic_info = Smap.empty;
      type_vars = Smap.empty;
      logic_labels = Smap.empty;
      current_logic_label = None;
      is_post_state = None;
      enclosing_post_state=None;
      is_funspec=false
    }

  let funspec () =
    let empty = empty () in { empty with is_funspec = true }

end

let append_init_label env =
  Lenv.add_logic_label "Init" Logic_const.init_label env

let append_here_label env =
  let env = Lenv.add_logic_label "Here" Logic_const.here_label env in
  Lenv.set_current_logic_label Logic_const.here_label env

let append_pre_label env =
  Lenv.add_logic_label "Pre" Logic_const.pre_label env

let append_old_and_post_labels env =
  Lenv.add_logic_label "Post" Logic_const.post_label
    (Lenv.add_logic_label "Old" Logic_const.old_label env)

let append_loop_labels env =
  Lenv.add_logic_label "LoopEntry" Logic_const.loop_entry_label
    (Lenv.add_logic_label "LoopCurrent" Logic_const.loop_current_label env)

let add_var var info env = Lenv.add_var var info env

let add_result env typ =
  if Logic_utils.isLogicVoidType typ then env
  else
    let v = Cil_const.make_logic_var_kind "\\result" LVC typ in
    Lenv.add_var "\\result" v env

let add_exit_status env =
  let v = Cil_const.make_logic_var_global "\\exit_status" Linteger in
  Lenv.add_var "\\exit_status" v env

let enter_post_state env kind = Lenv.enter_post_state env kind

let post_state_env kind typ =
  let env = Lenv.funspec () in
  let env = append_init_label env in
  let env = append_here_label env in
  let env = append_old_and_post_labels env in
  (* NB: this allows to have \result and Exits as termination kind *)
  let env = add_result env typ in
  let env = add_exit_status env in
  let env = enter_post_state env kind in
  env

type type_namespace = Typedef | Struct | Union | Enum

module Type_namespace =
  Datatype.Make(struct
    include Datatype.Serializable_undefined

    let reprs = [Typedef]
    let name = "Logic_typing.type_namespace"
    type t = type_namespace
    let compare : t -> t -> int = Transitioning.Stdlib.compare
    let equal : t -> t -> bool = (=)
    let hash : t -> int = Hashtbl.hash
  end)


type typing_context = {
  is_loop: unit -> bool;
  anonCompFieldName : string;
  conditionalConversion : typ -> typ -> typ;
  find_macro : string -> lexpr;
  find_var : ?label:string -> var:string -> logic_var;
  find_enum_tag : string -> exp * typ;
  find_comp_field: compinfo -> string -> offset;
  find_type : type_namespace -> string -> typ;
  find_label : string -> stmt ref;
  remove_logic_function : string -> unit;
  remove_logic_info: logic_info -> unit;
  remove_logic_type: string -> unit;
  remove_logic_ctor: string -> unit;
  add_logic_function: logic_info -> unit;
  add_logic_type: string -> logic_type_info -> unit;
  add_logic_ctor: string -> logic_ctor_info -> unit;
  find_all_logic_functions: string -> logic_info list;
  find_logic_type: string -> logic_type_info;
  find_logic_ctor: string -> logic_ctor_info;
  pre_state:Lenv.t;
  post_state:Cil_types.termination_kind list -> Lenv.t;
  assigns_env:Lenv.t;
  silent: bool;
  logic_type:
    typing_context -> location -> Lenv.t ->
    Logic_ptree.logic_type -> Cil_types.logic_type ;
  type_predicate:
    typing_context -> Lenv.t -> Logic_ptree.lexpr -> predicate;
  type_term:
    typing_context -> Lenv.t -> Logic_ptree.lexpr -> term;
  type_assigns:
    typing_context ->
    accept_formal:bool ->
    Lenv.t ->
    Logic_ptree.assigns -> Cil_types.assigns;
  error: 'a 'b. location -> ('a,formatter,unit,'b) format4 -> 'a;
  on_error: 'a 'b. ('a -> 'b) -> (unit -> unit) -> 'a -> 'b
}

module Extensions = struct
  let typer_tbl = Hashtbl.create 5
  let find_typer name = Hashtbl.find typer_tbl name
  let is_extension name = Hashtbl.mem typer_tbl name
  let register name category status typer =
    if is_extension name then
      Kernel.warning ~wkey:Kernel.wkey_acsl_extension
        "Trying to register ACSL extension %s twice. Ignoring second extension"
        name
    else begin
      Logic_env.register_extension name category;
      Hashtbl.add typer_tbl name (status,typer)
    end

  let typer name ~typing_context:typing_context ~loc p =
    try
      let status,typer = find_typer name in
      status, typer ~typing_context ~loc p
    with Not_found ->
      Kernel.fatal ~source:(fst loc) "unsupported clause of name '%s'" name
end

let register_behavior_extension name f =
  Extensions.register name Ext_contract f

let register_global_extension name f =
  Extensions.register name Ext_global f

let register_code_annot_extension name f =
  Extensions.register name (Ext_code_annot Ext_here) f

let register_code_annot_next_stmt_extension name f =
  Extensions.register name (Ext_code_annot Ext_next_stmt) f

let register_code_annot_next_loop_extension name f =
  Extensions.register name (Ext_code_annot Ext_next_loop) f

let register_code_annot_next_both_extension name f =
  Extensions.register name (Ext_code_annot Ext_next_both) f

let rec arithmetic_conversion ty1 ty2 =
  match unroll_type ty1, unroll_type ty2 with
  | Ctype ty1, Ctype ty2 ->
    if isIntegralType ty1 && isIntegralType ty2
    then Linteger
    else Lreal
  | (Linteger, Ctype t | Ctype t, Linteger) when isIntegralType t ->
    Linteger
  | (Linteger, Ctype t | Ctype t , Linteger) when isArithmeticType t-> Lreal
  | (Lreal, Ctype ty | Ctype ty, Lreal) when isArithmeticType ty -> Lreal
  | Linteger, Linteger -> Linteger
  | (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
  | Ltype ({lt_name="set"} as lt,[t1]),
    Ltype ({lt_name="set"},[t2]) ->
    Ltype(lt,[arithmetic_conversion t1 t2])
  | _ ->
    Kernel.fatal
      ~current:true
      "arithmetic conversion between non arithmetic types %a and %a"
      Cil_printer.pp_logic_type ty1 Cil_printer.pp_logic_type ty2

let plain_arithmetic_type t = Cil.isLogicArithmeticType t
let plain_integral_type t = Cil.isLogicIntegralType t
let plain_boolean_type t = Cil.isLogicBooleanType t
let plain_fun_ptr t = Cil.isLogicFunPtrType t

let is_arithmetic_type = plain_or_set plain_arithmetic_type
let is_integral_type = plain_or_set plain_integral_type
let is_fun_ptr = plain_or_set plain_fun_ptr

let rec type_of_pointed t =
  match unroll_type t with
    Ctype ty when isPointerType ty -> Ctype (Cil.typeOf_pointed ty)
  | Ltype ({lt_name = "set"} as lt,[t]) ->
    Ltype(lt,[type_of_pointed t])
  | _ ->
    Kernel.fatal ~current:true "type %a is not a pointer type"
      Cil_printer.pp_logic_type t

let rec ctype_of_pointed t =
  match unroll_type t with
    Ctype ty when isPointerType ty -> Cil.typeOf_pointed ty
  | Ltype ({lt_name = "set"},[t]) -> ctype_of_pointed t
  | _ ->
    Kernel.fatal ~current:true "type %a is not a pointer type"
      Cil_printer.pp_logic_type t

let type_of_array_elem =
  plain_or_set
    (fun t ->
       match unroll_type t with
         Ctype ty when isArrayType ty -> Ctype (Cil.typeOf_array_elem ty)
       | _ ->
         Kernel.fatal ~current:true "type %a is not an array type"
           Cil_printer.pp_logic_type t)

let rec ctype_of_array_elem t =
  match unroll_type t with
  |	Ctype ty when isArrayType ty -> Cil.typeOf_array_elem ty
  | Ltype ({lt_name = "set"},[t]) -> ctype_of_array_elem t
  | _ ->
    Kernel.fatal ~current:true "type %a is not a pointer type"
      Cil_printer.pp_logic_type t

let mk_mem ?loc t ofs =
  lift_set
    (fun t -> term ?loc (TLval (plain_mk_mem ?loc t ofs))
        (type_of_pointed t.term_type))
    t

let is_plain_array_type t =
  match unroll_type t with
  | Ctype ct -> Cil.isArrayType ct
  | _ -> false

let is_plain_pointer_type t =
  match unroll_type t with
  | Ctype ct -> Cil.isPointerType ct
  | _ -> false

let is_array_type = plain_or_set is_plain_array_type
let is_pointer_type = plain_or_set is_plain_pointer_type

module Make
    (C:
     sig
       val is_loop: unit -> bool
       val anonCompFieldName : string
       val conditionalConversion : typ -> typ -> typ
       val find_macro : string -> lexpr
       val find_var : ?label:string -> var:string -> logic_var
       val find_enum_tag : string -> exp * typ
       val find_comp_field: compinfo -> string -> offset
       val find_type : type_namespace -> string -> typ
       val find_label : string -> stmt ref
       val remove_logic_function : string -> unit
       val remove_logic_info: logic_info -> unit
       val remove_logic_type: string -> unit
       val remove_logic_ctor: string -> unit
       val add_logic_function: logic_info -> unit
       val add_logic_type: string -> logic_type_info -> unit
       val add_logic_ctor: string -> logic_ctor_info -> unit
       val find_all_logic_functions: string -> logic_info list
       val find_logic_type: string -> logic_type_info
       val find_logic_ctor: string -> logic_ctor_info
       val integral_cast: Cil_types.typ -> Cil_types.term -> Cil_types.term
       val error: location -> ('a,formatter,unit, 'b) format4 -> 'a
       val on_error: ('a -> 'b) -> (unit -> unit) -> 'a -> 'b
     end) =
struct

  let make_typing_context ~pre_state ~post_state ~assigns_env
      ~logic_type ~type_predicate ~type_term ~type_assigns = {
    silent = false;
    is_loop = C.is_loop;
    pre_state=pre_state;
    post_state=post_state;
    assigns_env=assigns_env;
    logic_type= logic_type;
    type_predicate= type_predicate;
    type_term= type_term;
    type_assigns = type_assigns;
    anonCompFieldName = C.anonCompFieldName;
    conditionalConversion = C.conditionalConversion;
    find_macro = C.find_macro;
    find_var = C.find_var;
    find_enum_tag = C.find_enum_tag;
    find_comp_field = C.find_comp_field;
    find_type = C.find_type ;
    find_label = C.find_label;
    remove_logic_function = C.remove_logic_function;
    remove_logic_info = C.remove_logic_info;
    remove_logic_type = C.remove_logic_type;
    remove_logic_ctor = C.remove_logic_ctor;
    add_logic_function = C.add_logic_function;
    add_logic_type = C.add_logic_type;
    add_logic_ctor = C.add_logic_ctor;
    find_all_logic_functions = C.find_all_logic_functions;
    find_logic_type = C.find_logic_type;
    find_logic_ctor = C.find_logic_ctor;
    error = C.error;
    on_error = C.on_error;
  }

  let rollback = Queue.create ()

  let start_transaction () = Queue.clear rollback

  let finish_transaction () = Queue.clear rollback

  let rollback_transaction () =
    Queue.iter (fun f -> f()) rollback; Queue.clear rollback

  let add_rollback_action f x = Queue.add (fun () -> f x) rollback

  let add_logic_function loc li =
    let l = Logic_env.find_all_logic_functions li.l_var_info.lv_name in
    if List.exists (Logic_utils.is_same_logic_profile li) l then begin
      C.error loc
        "%s %s is already declared with the same profile"
        (match li.l_type with None -> "predicate" | Some _ -> "logic function")
        li.l_var_info.lv_name
    end else begin
      C.add_logic_function li;
      add_rollback_action C.remove_logic_info li
    end

  let add_logic_type loc info =
    try
      ignore (C.find_logic_type info.lt_name);
      C.error loc "logic type %s is already defined" info.lt_name
    with Not_found ->
      C.add_logic_type info.lt_name info;
      add_rollback_action C.remove_logic_type info.lt_name

  let check_non_void_ptr loc ty =
    if Logic_utils.isLogicVoidPointerType ty then
      C.error loc "Cannot use a pointer to void here"

  let check_fun_ptr loc ty =
    let is_fun_ptr t =
      match Cil.unrollType t with
      | TPtr(t,_) when Cil.isFunctionType t -> true
      | _ -> false
    in
    if not (Logic_utils.isLogicType is_fun_ptr ty) then
      C.error loc "expecting a function pointer, found %a"
        Cil_datatype.Logic_type.pretty ty

  let check_object_ptr loc ty =
    let is_object_ptr t =
      match Cil.unrollType t with
      | TPtr(t,_) when not (Cil.isFunctionType t) -> true
      | _ -> false
    in
    if not (Logic_utils.isLogicType is_object_ptr ty) then
      C.error loc "expecting a pointer to an object, found %a"
        Cil_datatype.Logic_type.pretty ty

  let check_logic_pointer ~check_non_void loc t =
    if Logic_utils.is_C_array t then
      C.error loc
        "In ACSL, there is no implicit conversion between \
         a C array and a pointer. Either introduce an explicit \
         cast or take the address of the first element of %a"
        Cil_printer.pp_term t;
    if not (isLogicPointerType t.term_type) then
      C.error loc "%a is not a pointer." Cil_printer.pp_term t;
    if check_non_void then check_non_void_ptr t.term_loc t.term_type;
    check_object_ptr t.term_loc t.term_type

  let has_field f ty =
    try
      ignore (Logic_env.find_model_field f ty); true
    with Not_found ->
      (match Cil.unrollType ty with
       | TComp(comp,_,_) ->
         List.exists (fun x -> x.fname = f) comp.cfields
       | _ -> false)

  let plain_type_of_c_field loc f ty =
    match Cil.unrollType ty with
    | TComp (comp, _, attrs) ->
      (try
         let attrs = Cil.filter_qualifier_attributes attrs in
         let field = C.find_comp_field comp f in
         let typ = Cil.typeOffset ty field in
         Logic_utils.offset_to_term_offset ~cast:false field,
         Ctype (Cil.typeAddAttributes attrs typ)
       with Not_found -> C.error loc "cannot find field %s" f)
    | _ ->
      C.error loc "expected a struct with field %s" f

  let type_of_c_field loc f = function
    | Ctype ty ->
      (try
         let mf = Logic_env.find_model_field f ty in
         TModel(mf,TNoOffset), mf.mi_field_type
       with Not_found -> plain_type_of_c_field loc f ty)
    | _ -> C.error loc "expected a struct with field %s" f

  let plain_type_of_field loc f ty =
    type_of_c_field loc f (Logic_const.unroll_ltdef ty)

  let type_of_field loc f ty =
    match Logic_const.unroll_ltdef ty with
    | Ltype ({lt_name = "set"} as lt,[t]) ->
      let offs,typ = plain_type_of_field loc f t in offs, Ltype(lt,[typ])
    | t -> type_of_c_field loc f t

  let c_void_star = Ctype (TPtr (TVoid [], []))

  (* keep in sync with fresh_type below *)
  let generated_var s = String.contains s '#'

  (* keep in sync with generated_var above*)
  class fresh_type_var =
    object(self)
      inherit Cil.nopCilVisitor
      val alpha_rename = Hashtbl.create 7
      val mutable count = 0
      method private fresh_s s =
        count <- succ count; Printf.sprintf "%s#%d" s count
      method! vlogic_type = function
          Lvar s when Hashtbl.mem alpha_rename s ->
          Cil.ChangeTo (Lvar (Hashtbl.find alpha_rename s))
        | Lvar s ->
          let s' = self#fresh_s s in
          Hashtbl.add alpha_rename s s';
          Cil.ChangeTo (Lvar s')
        | _ -> Cil.DoChildren
      method reset_count () = count <- 0
      method reset () = Hashtbl.clear alpha_rename
    end

  let fresh_type = new fresh_type_var

  let fresh typ = visitCilLogicType (fresh_type :> cilVisitor) typ

  let fresh_type_var name = fresh (Lvar name)

  let instantiate env ty =
    let obj = object
      inherit Cil.nopCilVisitor
      method! vlogic_type t =
        match t with
          Lvar s when generated_var s ->
          (try
             Cil.ChangeDoChildrenPost
               (Lenv.find_type_var s env, fun x -> x)
           with Not_found ->
             Cil.DoChildren
             (* assert false *)
             (*FIXME: All type variables are supposed to be bound somewhere.
               However, there is currently no syntax to force an instantiation,
               e.g. for axiom foo<C>: length(Nil) == 0;
               (where length takes list<A> and Nil is list<B>): we don't equal
                A nor B to C, and can't write length<C> nor Nil<C>)
             *)
          )
        | _ -> Cil.DoChildren
    end
    in Cil.visitCilLogicType obj ty

  let rec c_type_of loc = function
    | Ctype t -> t
    | Ltype (tdef,_) as ty when is_unrollable_ltdef tdef ->
      c_type_of loc (unroll_ltdef ty)
    | Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ ->
      C.error loc "not a C type"

  let logic_type ctxt loc env t =
    let module C = struct end in (* force calls to go through ctxt *)
    let ltype t = ctxt.logic_type ctxt loc env t in
    let ctype t = ltype t |> c_type_of loc in
    match t with
    | LTvoid -> Ctype (TVoid [])
    | LTint ikind -> Ctype (TInt (ikind, []))
    | LTfloat fkind -> Ctype (TFloat (fkind, []))
    | LTarray (ty,length) ->
      let size = match length with
        | ASnone -> None
        | ASinteger s -> Some (parseIntExp ~loc s)
        | ASidentifier s ->
          let size = ctxt.type_term ctxt env
              {lexpr_node=PLvar(s);lexpr_loc=loc} in
          if size.term_type <> Linteger then
            ctxt.error loc "size of array must be an integral value";
          try
            let rec size_exp size =
              match size.term_node with
              | TConst lconst -> (* the identifier was a macro to an integer *)
                Some (new_exp ~loc (Const (lconstant_to_constant lconst)))
              | TLval (TVar( {lv_kind=LVGlobal} as lvar), TNoOffset) ->
                begin (* logic variable, so try to unfold its definition *)
                  match Logic_env.find_logic_cons lvar with
                  | {l_labels=[];l_body=(LBterm term);l_profile=[]} ->
                    size_exp term
                  | _ -> raise Not_found
                end
              | _ -> raise Not_found
            in size_exp size
          with Not_found ->
            ctxt.error loc "size of array must be an integral value";
      in
      Ctype (TArray (ctype ty, size,
                     Cil.empty_size_cache (),[]))
    | LTpointer ty -> Ctype (TPtr (ctype ty, []))
    | LTenum e ->
      (try Ctype (ctxt.find_type Enum e)
       with Not_found -> ctxt.error loc "no such enum %s" e)
    | LTstruct s ->
      (try Ctype (ctxt.find_type Struct s)
       with Not_found -> ctxt.error loc "no such struct %s" s)
    | LTunion u ->
      (try Ctype (ctxt.find_type Union u)
       with Not_found -> ctxt.error loc "no such union %s" u)
    | LTarrow (prms,rt) ->
      (* For now, our only function types are C function pointers. *)
      let prms = List.map (fun x -> "", ctype x, []) prms in
      let rt = ctype rt in
      (match prms with
         [] -> Ctype (TFun(rt,None,false,[]))
       | [(_,arg_typ,_)] when isVoidType arg_typ ->
         (* Same invariant as in C *)
         Ctype (TFun(rt,Some [],false,[]))
       | _ -> Ctype (TFun(rt,Some prms,false,[])))
    | LTnamed (id,[]) ->
      (try Lenv.find_type_var id env
       with Not_found ->
       try Ctype (ctxt.find_type Typedef id) with Not_found ->
       try
         let info = ctxt.find_logic_type id in
         if info.lt_params <> [] then
           ctxt.error loc "wrong number of parameter for type %s" id
         else Ltype (info,[])
       with Not_found ->
         ctxt.error loc "no such type %s" id)
    | LTnamed(id,l) ->
      (try
         let info = ctxt.find_logic_type id in
         if List.length info.lt_params <> List.length l then
           ctxt.error loc "wrong number of parameter for type %s" id
         else Ltype (info,List.map ltype l)
       with Not_found ->
         ctxt.error loc "no such type %s" id)
    | LTinteger -> Linteger
    | LTreal -> Lreal
    | LTattribute (ty,attr) ->
      (* attributes can only qualify C types *)
      Ctype (Cil.typeAddAttributes [attr] (ctype ty))

  let mk_logic_access env t =
    match t.term_node with
      TLval _ -> t
    | _ ->
      let var = Lenv.fresh_var env "tmp" LVLocal t.term_type in
      let info =
        { l_var_info = var;
          l_labels = [];
          l_tparams = [];
          l_type = Some t.term_type;
          l_profile = [];
          l_body = LBterm t }
      in { t with
           term_node =
             Tlet(info,{ t with
                         term_node = TLval(TVar var,TNoOffset)
                       })
         }

  let mk_dot env loc f_ofs f_type t =
    let rec t_dot_x t =
      match t.term_node with
      | TLval lv ->
        Logic_const.term
          ~loc (TLval (Logic_const.addTermOffsetLval f_ofs lv)) f_type
      | Tat (t1,l) ->
        Logic_const.term
          ~loc (Tat (t_dot_x t1,l)) f_type
      | _ ->
        let var = Lenv.fresh_var env "tmp" LVLocal t.term_type in
        let info =
          { l_var_info = var;
            l_labels = [];
            l_tparams = [];
            l_type = Some t.term_type;
            l_profile = [];
            l_body = LBterm t }
        in Logic_const.term
          ~loc (Tlet(info,{ t with
                            term_node = TLval(TVar var,f_ofs) ;
                            term_type = f_type
                          })) f_type
    in t_dot_x t

  let mk_at_here idx =
    let rec needs_at idx =
      match idx.term_node with
      | TConst _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
      | TAlignOf _ | TAlignOfE _ | Tat _ | Ttypeof _ | Ttype _
      | Tempty_set | Tbase_addr _ | Toffset _ | Tblock_length _ | Tnull
        -> false
      | TLval _ -> true
      | TUnOp(_,t) -> needs_at t
      | TBinOp(_,t1,t2) -> needs_at t1 || needs_at t2
      | TCastE(_,t) -> needs_at t
      | TAddrOf (_,o) -> needs_at_offset o
      | TStartOf (_,o) -> needs_at_offset o
      | Tapp(_,_,l) | TDataCons(_,l) -> List.exists needs_at l
      | Tlambda(_,t) -> needs_at t
      | TUpdate(t1,o,t2) -> needs_at t1 || needs_at_offset o || needs_at t2
      | Tunion l | Tinter l -> List.exists needs_at l
      | Tcomprehension(t,_,None) -> needs_at t
      | Tcomprehension(t,_,Some p) -> needs_at t || needs_at_pred p
      | Trange (None, None) -> false
      | Trange (None, Some t) | Trange(Some t, None) -> needs_at t
      | Trange (Some t1, Some t2) -> needs_at t1 || needs_at t2
      | Tlet(_,t) -> needs_at t
      | Tif(t1,t2,t3) -> needs_at t1 || needs_at t2 || needs_at t3
      | TLogic_coerce(_,t) -> needs_at t
    and needs_at_offset = function
      | TNoOffset -> false
      | TIndex (t,o) -> needs_at t || needs_at_offset o
      | TField(_,o) | TModel(_,o) -> needs_at_offset o
    and needs_at_pred p =
      match p.pred_content with
      | Pfalse | Ptrue | Pat _ -> false
      | Papp(_,_,t) | Pseparated t -> List.exists needs_at t
      | Prel(_,t1,t2) -> needs_at t1 || needs_at t2
      | Pand(p1,p2) | Por(p1,p2) | Pxor(p1,p2)
      | Pimplies(p1,p2) | Piff(p1,p2) -> needs_at_pred p1 || needs_at_pred p2
      | Pnot p | Plet (_,p) | Pforall(_,p) | Pexists(_,p) -> needs_at_pred p
      | Pif(t,p1,p2) -> needs_at t || needs_at_pred p1 || needs_at_pred p2
      | Pvalid (_,t) | Pvalid_read (_,t) | Pvalid_function t
      | Pinitialized (_,t) | Pdangling (_, t)
      | Pallocable(_,t) | Pfreeable(_,t)-> needs_at t
      | Pfresh (_,_,t,n) -> (needs_at t) && (needs_at n)
    in
    if needs_at idx then tat ~loc:idx.term_loc (idx,here_label) else idx

  let mkAddrOfAndMark loc (b,off as lval) t =
    (* Mark the vaddrof flag if b is a variable *)
    begin match lastTermOffset off with
      | TNoOffset ->
        (match b with
           TVar vi ->
           begin match vi.lv_origin with None -> ()
                                       | Some vi -> vi.vaddrof <- true
           end
         | _ -> ())
      | TIndex _ -> ()
      | TModel (mf,_) ->
        C.error loc "Cannot take the address of model field %s" mf.mi_name
      | TField(fi,_) -> fi.faddrof <- true
    end;
    Logic_utils.mk_logic_AddrOf ~loc lval t.term_type

  (* Compare the two types as logic types, ie by dismissing some irrelevant
     qualifiers and attributes *)
  let is_same_c_type ctyp1 ctyp2 =
    Cil_datatype.Logic_type.equal (Ctype ctyp1) (Ctype ctyp2)

  let rec c_mk_cast ?(force=false) e oldt newt =
    let loc = e.term_loc in
    if is_same_c_type oldt newt then begin
      if force then Logic_utils.mk_cast ~loc ~force newt e else e
    end else begin
      (* Watch out for constants *)
      if isPointerType newt && isLogicNull e && not (isLogicZero e) then
        (* \null can have any pointer type, see ACSL manual. *)
        (if force then
           Logic_const.term ~loc (TCastE (newt, e)) (Ctype newt)
         else
           { e with term_type = Ctype newt })
      else if isPointerType newt && isArrayType oldt then begin
        if not (is_C_array e) then
          C.error loc "cannot cast logic array to pointer type";
        let e = mk_logic_StartOf e in
        let oldt = Logic_utils.logicCType e.term_type in
        (* we have converted from array to ptr, but the pointed type might
           differ. Just do another round of conversion. *)
        c_mk_cast e oldt newt
      end else begin
        match Cil.unrollType newt, e.term_node with
        | TEnum (ei,[]), TConst (LEnum { eihost = ei'})
          when ei.ename = ei'.ename && not force -> e
        | _ ->
          { e with term_node =
                     (Logic_utils.mk_cast ~force newt e).term_node;
                   term_type = Ctype newt }
      end
    end

  let is_same_ptr_type ctyp1 ctyp2 =
    (isPointerType ctyp1) &&
    (isPointerType ctyp2) &&
    (is_same_c_type (typeOf_pointed ctyp1) (typeOf_pointed ctyp2))

  let is_same_array_type ctyp1 ctyp2 =
    (isArrayType ctyp1) && (isArrayType ctyp2) &&
    (is_same_c_type (typeOf_array_elem ctyp1) (typeOf_array_elem ctyp2))

  let is_same_logic_ptr_type ty1 ty2 =
    match (Logic_const.unroll_ltdef ty1, Logic_const.unroll_ltdef ty2) with
    | Ctype t1, Ctype t2 -> is_same_ptr_type t1 t2
    | _ -> false

  let is_same_logic_array_type ty1 ty2 =
    match (Logic_const.unroll_ltdef ty1, Logic_const.unroll_ltdef ty2) with
    | Ctype t1, Ctype t2 -> is_same_array_type t1 t2
    | _ -> false

  let is_function_pointer cty =
    try
      Cil.isFunctionType (Cil.typeOf_pointed  cty)
    with Assert_failure _ -> false

  let is_compatible_funtype ty1 ty2 =
    if is_same_c_type ty1 ty2 then true
    else begin
      let rt1, _, variadic1, _ = Cil.splitFunctionType ty1 in
      let rt2, args2, _, _ = Cil.splitFunctionType ty2 in
      if not (is_same_c_type rt1 rt2) then false
      else begin
        (* types are not identical: they can only be compatible
           if args2 are not specified and variadic1 is false. *)
        match args2 with
        | Some _ -> false
        | None -> not variadic1
      end
    end


  let is_implicit_pointer_conversion term ctyp1 ctyp2 =
    let same_pointed () =
      is_same_c_type (typeOf_pointed ctyp1) (typeOf_pointed ctyp2)
    in
    let same_array_elt () =
      is_same_c_type (typeOf_array_elem ctyp1) (typeOf_array_elem ctyp2)
    in
    let compatible_pointed () =
      same_pointed () ||
      (isVoidPtrType ctyp2 && not (is_function_pointer ctyp1))
      ||
      (is_function_pointer ctyp2 && is_function_pointer ctyp1 &&
       is_compatible_funtype (typeOf_pointed ctyp1) (typeOf_pointed ctyp2))
    in
    (isArrayType ctyp1 && isArrayType ctyp2 && same_array_elt ()) ||
    (isPointerType ctyp1 && isPointerType ctyp2 &&
     (compatible_pointed() || isLogicNull term))

  let is_enum_cst e t =
    match e.term_node with
    | TConst (LEnum ei) -> is_same_type (Ctype (TEnum (ei.eihost,[]))) t
    | _ -> false

  let logic_coerce t e =
    let real_type = set_conversion t e.term_type in
    let rec aux e =
      match e.term_node with
      | Tcomprehension(e,q,p) ->
        { e with term_type = real_type;
                 term_node = Tcomprehension (aux e,q,p) }
      | Tunion l ->
        { e with term_type = real_type; term_node = Tunion (List.map aux l) }
      | Tinter l ->
        { e with term_type = real_type; term_node = Tinter (List.map aux l) }
      | Tempty_set -> { e with term_type = real_type }
      | TLogic_coerce(t2,e) when Cil.no_op_coerce t2 e ->
        let e = aux e in
        { e with term_type = real_type; term_node = TLogic_coerce(real_type,e) }
      | _ when Cil.isLogicArithmeticType real_type ->
        Logic_utils.numeric_coerce real_type e
      | _ ->
        { e with term_type = real_type; term_node = TLogic_coerce(real_type,e) }
    in
    if is_same_type e.term_type t then e else aux e

  let location_to_char_ptr t =
    let convert_one_location t =
      let ptd_type = type_of_pointed t.term_type in
      if isLogicCharType ptd_type then
        logic_coerce (make_set_type t.term_type) t
      else if isLogicVoidType ptd_type then C.error t.term_loc
          "can not have a set of void pointers"
      else
        let loc = t.term_loc in
        let sizeof = term ~loc (TSizeOf (logicCType ptd_type)) Linteger in
        let range = trange ~loc (Some (lzero ~loc ()), Some sizeof) in
        let converted_type = set_conversion (Ctype Cil.charPtrType) t.term_type
        in
        let cast = term ~loc (TCastE(Cil.charPtrType, t)) converted_type in
        term ~loc (TBinOp(PlusPI,cast,range)) (make_set_type converted_type)
    in
    lift_set convert_one_location t

  let rec mk_cast ?(explicit=false) e newt =
    let force = explicit in
    let loc = e.term_loc in
    let truncate_info =
      List.hd @@ Logic_env.find_all_logic_functions "\\truncate"
    in
    if is_same_type e.term_type newt then begin
      if explicit then begin
        match Logic_const.unroll_ltdef newt with
        | Ctype cnewt ->
          { e with term_node = TCastE(cnewt,e); term_type = newt }
        | _ -> e
      end else e
    end else if is_enum_cst e newt then { e with term_type = newt }
    else begin
      match
        (unroll_type e.term_type),
        (* If any, use the typedef itself in the inserted cast *)
        (Logic_const.unroll_ltdef newt)
      with
      | Ctype oldt, Ctype newt ->
        c_mk_cast ~force e oldt newt
      | t1, Ltype ({lt_name = name},[])
        when name = Utf8_logic.boolean && is_integral_type t1 ->
        let t2 = Ltype (C.find_logic_type Utf8_logic.boolean,[]) in
        let e = mk_cast e Linteger in
        Logic_const.term ~loc (TBinOp(Ne,e,lzero ~loc())) t2
      | t1, Linteger when Logic_const.is_boolean_type t1 && explicit ->
        logic_coerce Linteger e
      | t1, Ctype t2 when Logic_const.is_boolean_type t1
                       && is_integral_type newt && explicit ->
        Logic_const.term ~loc (TCastE (t2,e)) newt
      | ty1, Ltype({lt_name="set"},[ty2])
        when is_pointer_type ty1 &&
             is_plain_pointer_type ty2 &&
             isLogicCharType (type_of_pointed ty2) ->
        location_to_char_ptr e
      | Ltype({lt_name = "set"},[_]), Ltype({lt_name="set"},[ty2]) ->
        let e = lift_set (fun e -> mk_cast e ty2) e in
        { e with term_type = make_set_type e.term_type}
      (* extremely dirty cast to allow Eva to understand some libc
         specifications *)
      | Ltype({lt_name = "set"},[_]), Ctype ty2 when explicit ->
        Logic_utils.mk_cast ~loc ty2 e
      | _ , Ltype({lt_name =  "set"},[ ty2 ]) ->
        let e = mk_cast e ty2 in
        logic_coerce (make_set_type e.term_type) e
      | Linteger, Linteger | Lreal, Lreal -> e
      | Linteger, Ctype t when isLogicPointerType newt && isLogicNull e ->
        c_mk_cast ~force e intType t
      | Linteger, (Ctype newt) | Lreal, (Ctype newt) when explicit ->
        Logic_utils.mk_cast ~loc newt e
      | Linteger, Ctype t when isIntegralType t ->
        (try C.integral_cast t e with Failure s -> C.error loc "%s" s)
      | Linteger, Ctype _ | Lreal, Ctype _ ->
        C.error loc "invalid implicit cast from %a to C type %a"
          Cil_printer.pp_logic_type e.term_type
          Cil_printer.pp_logic_type newt
      | Ctype t, Linteger when Cil.isIntegralType t -> logic_coerce Linteger e
      | Ctype t, Linteger when Cil.isArithmeticType t && explicit ->
        Logic_const.term
          ~loc (Tapp(truncate_info,[], [logic_coerce Lreal e])) Linteger
      | Ctype t, Lreal when isArithmeticType t -> logic_coerce Lreal e
      | Ctype _, (Lreal | Linteger) ->
        C.error loc "invalid implicit cast from %a to logic type %a"
          Cil_printer.pp_logic_type e.term_type
          Cil_printer.pp_logic_type newt
      | Linteger, Lreal -> logic_coerce Lreal e
      | Lreal, Linteger when explicit ->
        let term_node = Tapp(truncate_info,[],[e]) in
        Logic_const.term ~loc term_node Linteger
      | Lreal, Linteger ->
        C.error loc
          "invalid cast from real to integer. \
           Use conversion functions instead"
      | Larrow (args1,_), Larrow(args2,rt2) ->
        (match e.term_node with
         | Tlambda (prms,body) when
             Logic_utils.is_same_list is_same_type args1 args2 ->
           (* specialized coercion of the body of the lambda instead of
              the whole expression. *)
           (* Might also want to specialize when the prms type are not
              the same, but this implies pushing logic coercions in the
              body for the newly typed parameters... *)
           let body = mk_cast body rt2 in
           { e with
             term_node = Tlambda(prms,body);
             term_type = newt }
         | _ -> logic_coerce newt e)
      | Ltype _, _ | _, Ltype _
      | Lvar _,_ | _,Lvar _
      | Larrow _,_ | _,Larrow _ ->
        C.error loc "invalid cast from %a to %a"
          Cil_printer.pp_logic_type e.term_type
          Cil_printer.pp_logic_type newt

    end

  let rec c_cast_to ot nt e =
    if is_same_c_type ot nt then (ot, e)
    else begin
      let result = (nt, mk_cast e (Ctype nt)) in
      match ot, nt with
      | TNamed(r, _), _ -> c_cast_to r.ttype nt e
      | _, TNamed(r, _) -> c_cast_to ot r.ttype e
      | TInt(_ikindo,_), TInt(_ikindn,_) ->
        result
      | TInt _, TPtr _ -> result
      | TPtr _, TInt _ -> result
      | ((TArray (told,_,_,_) | TPtr (told,_)),
         (TPtr (tnew,_) | TArray(tnew,_,_,_)))
        when is_same_c_type told tnew -> result
      | (TPtr _ | TArray _), (TPtr _ | TArray _)
        when isLogicNull e -> result
      | TPtr _, TPtr _ when isVoidPtrType nt -> (nt, e)
      | TPtr (t1,_), TPtr (t2,_) when
          Cil.isFunctionType t1 &&
          Cil.isFunctionType t2 &&
          is_compatible_funtype t1 t2 -> result
      | TEnum _, TInt _ -> result
      | TFloat _, (TInt _|TEnum _) -> result
      | (TInt _|TEnum _), TFloat _ -> result
      | TFloat _, TFloat _ -> result
      | TInt _, TEnum _ -> result
      | TEnum _, TEnum _ -> result
      | TEnum _, TPtr _ -> result
      | TBuiltin_va_list _, (TInt _ | TPtr _) ->
        result
      | (TInt _ | TPtr _), TBuiltin_va_list _ ->
        Kernel.debug ~level:3 "Casting %a to __builtin_va_list"
          Cil_printer.pp_typ ot;
        result
      | TPtr _, TEnum _ ->
        Kernel.debug ~level:3 "Casting a pointer into an enumeration type";
        result
      | (TInt _ | TEnum _ | TPtr _ ), TVoid _ ->
        (ot, e)
      | TComp (comp1, _, _), TComp (comp2, _, _)
        when comp1.ckey = comp2.ckey ->
        nt, e
      | _ ->
        Kernel.fatal ~current:true
          "Logic_typing.c_cast_to: %a -> %a@."
          Cil_printer.pp_typ ot Cil_printer.pp_typ nt
    end

  (* for overloading: raised when an arguments list does not fit a
     formal parameter list *)
  exception Not_applicable

  (*
    convert term [oterm] of type [ot] to type [nt].
    when overloaded is true,
    raise exception Not_applicable if conversion not possible,
    otherwise print an error message with location [loc]
   *)
  let rec implicit_conversion ~overloaded loc oterm ot nt =
    match (unroll_type ot), (unroll_type nt) with
    | Ctype ty1, Ctype ty2 ->
      if is_same_c_type ty1 ty2
      then ot, oterm
      else if (isIntegralType ty1 && isIntegralType ty2) then begin
        let sz1 = bitsSizeOf ty1 in
        let sz2 = bitsSizeOf ty2 in
        if (sz1 < sz2
            || (sz1 = sz2 && (isSignedInteger ty1 = isSignedInteger ty2))
            || is_enum_cst oterm nt)
        then begin let t, e = c_cast_to ty1 ty2 oterm in Ctype t,e end
        else if overloaded then raise Not_applicable
        else
          C.error loc "invalid implicit conversion from '%a' to '%a'"
            Cil_printer.pp_typ ty1 Cil_printer.pp_typ ty2
      end else if is_implicit_pointer_conversion oterm ty1 ty2
               || (match unrollType ty1, unrollType ty2 with
                   | (TFloat (f1,_), TFloat (f2,_)) ->
                     f1 <= f2
                   (*[BM]
                     relies on internal representation of OCaml constant
                     constructors.*)
                   | _ -> false)
      then begin
        let t,e = c_cast_to ty1 ty2 oterm in Ctype t, e
      end else if overloaded then raise Not_applicable
      else if (* not overloaded: raise an error. *)
        isArrayType ty1 && isPointerType ty2
        && is_same_c_type (typeOf_array_elem ty1) (typeOf_pointed ty2)
      then
        if Logic_utils.is_C_array oterm then
          C.error loc
            "In ACSL, there is no implicit conversion between \
             a C array and a pointer. Either introduce an explicit \
             cast or take the address of the first element of %a"
            Cil_printer.pp_term oterm
        else
          C.error loc
            "%a is a logic array. Only C arrays can be \
             converted to pointers, and this conversion must be \
             explicit (cast or take the address of the first element)"
            Cil_printer.pp_term oterm
      else
        C.error loc "invalid implicit conversion from '%a' to '%a'"
          Cil_printer.pp_typ ty1 Cil_printer.pp_typ ty2
    | Ctype ty, Linteger when Cil.isIntegralType ty -> Linteger, oterm
    | Ctype ty, Lreal when Cil.isArithmeticType ty -> Lreal, oterm
    | Linteger, Lreal -> Lreal, oterm
    (* Integer 0 is also a valid pointer. *)
    | Linteger, Ctype ty when Cil.isPointerType ty && isLogicNull oterm ->
      nt, { oterm with
            term_node = TCastE(ty,oterm);
            term_type = nt }
    | Linteger, Ctype ty when Cil.isIntegralType ty ->
      (try
         nt, C.integral_cast ty oterm
       with Failure s ->
         if overloaded then raise Not_applicable
         else C.error loc "%s" s)
    | t1, Ltype ({lt_name = "set"},[t2]) when
        is_pointer_type t1 &&
        is_plain_pointer_type t2 &&
        isLogicCharType (type_of_pointed t2) ->
      nt, location_to_char_ptr oterm
    (* can convert implicitly a singleton into a set,
       but not the reverse. *)
    | Ltype (t1,l1), Ltype (t2,l2) when t1.lt_name = t2.lt_name ->
      (* not sure this is really what we want: can foo<int> be implicitly
         converted into foo<integer> ? *)
      let l =
        List.map2
          (fun x y ->
             fst (implicit_conversion ~overloaded loc oterm x y)) l1 l2
      in
      Ltype(t1,l),oterm
    | t1, Ltype ({lt_name = "set"},[t2]) ->
      let typ, term = implicit_conversion ~overloaded loc oterm t1 t2 in
      make_set_type typ, term
    | Linteger, Linteger | Lreal, Lreal -> ot, oterm
    | Lvar s1, Lvar s2 when s1 = s2 -> ot, oterm
    | Larrow(args1,rt1), Larrow(args2,rt2)
      when List.length args1 = List.length args2 ->
      (* contravariance. *)
      let args =
        List.map2
          (fun x y ->
             fst (implicit_conversion ~overloaded loc oterm x y))
          args2 args1
      in
      let rt,_ = implicit_conversion ~overloaded loc oterm rt1 rt2 in
      Larrow(args,rt), oterm
    | ((Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _),
       (Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _)) ->
      if overloaded then raise Not_applicable
      else
        C.error loc "invalid implicit conversion from %a to %a"
          Cil_printer.pp_logic_type ot Cil_printer.pp_logic_type nt

  let rec find_supertype ~overloaded loc t ot nt =
    match unroll_type ot, unroll_type nt with
    | Ctype ot, Ctype nt ->
      if is_same_c_type ot nt then Ctype ot
      else if Cil.isIntegralType ot && Cil.isIntegralType nt then Linteger
      else if Cil.isArithmeticType ot && Cil.isArithmeticType nt then Lreal
      else if is_implicit_pointer_conversion t ot nt then
        let res,_ = c_cast_to ot nt t in Ctype res
      else if overloaded then raise Not_applicable
      else
        C.error loc "incompatible types %a and %a@."
          Cil_printer.pp_typ ot Cil_printer.pp_typ nt
    | Ctype ot, (Ltype({lt_name = n},[]) as nt) when
        n = Utf8_logic.boolean && Cil.isIntegralType ot -> nt
    | Ltype({lt_name = n},[]) as ot, Ctype nt when
        n = Utf8_logic.boolean && Cil.isIntegralType nt -> ot
    | (Linteger, (Ltype({lt_name = n},[]) as t)
      | (Ltype({lt_name = n},[]) as t), Linteger)
      when n = Utf8_logic.boolean -> t
    | Ltype(ot,oprms), Ltype(nt,nprms) when ot == nt ->
      let res =
        List.map2 (find_supertype ~overloaded loc t) oprms nprms
      in
      Ltype(ot,res)
    | Ltype({lt_name = "\\list"} as lt, [t1]),
      Ltype({lt_name = "\\list"}, [t2]) ->
      let st = find_supertype ~overloaded loc t t1 t2 in
      Ltype(lt, [st])
    | Ltype({lt_name = "set"} as set, [t1]), t2
    | t1, Ltype({lt_name = "set"} as set, [t2]) ->
      let st = find_supertype ~overloaded loc t t1 t2 in
      Ltype(set, [st])
    | Lvar s1, Lvar s2 when s1 = s2 -> ot
    | Linteger, Ctype nt when Cil.isIntegralType nt -> Linteger
    | Linteger, Ctype nt when Cil.isPointerType nt && isLogicNull t ->
      Ctype nt
    | Ctype ot, Linteger when Cil.isIntegralType ot -> Linteger
    | Ctype ot, Linteger when Cil.isPointerType ot && isLogicNull t ->
      Ctype ot
    | Linteger, Linteger -> Linteger
    | Linteger, Lreal -> Lreal
    | Linteger, Ctype nt when Cil.isArithmeticType nt -> Lreal
    | Ctype ot, Linteger when Cil.isArithmeticType ot -> Lreal
    | Lreal, Linteger -> Lreal
    | Lreal, Lreal -> Lreal
    | Lreal, Ctype nt when Cil.isArithmeticType nt -> Lreal
    | Ctype nt, Lreal when Cil.isArithmeticType nt -> Lreal
    | Larrow(oargs,oret), Larrow(nargs,nret)
      when List.length oargs = List.length nargs ->
      let ret = find_supertype ~overloaded loc t oret nret in
      let args = List.map2 (find_supertype ~overloaded loc t) nargs oargs in
      Larrow(args,ret)
    | (Ctype _ | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _), _ ->
      if overloaded then raise Not_applicable
      else
        C.error loc "incompatible types %a and %a"
          Cil_printer.pp_logic_type ot Cil_printer.pp_logic_type nt

  let rec partial_unif ~overloaded loc term ot nt env =
    match (unroll_type ot),(unroll_type nt) with
    | Lvar s1, Lvar s2 ->
      if generated_var s1 then
        try
          let ot = Lenv.find_type_var s1 env in
          partial_unif ~overloaded loc term ot nt env
        with Not_found ->
          if generated_var s2 then
            try let nt = Lenv.find_type_var s2 env in
              partial_unif ~overloaded loc term ot nt env
            with Not_found ->
              if s1 < s2 then Lenv.add_type_var s2 ot env,ot,ot
              else if s2 < s1 then Lenv.add_type_var s1 nt env,nt,nt
              else env,ot,ot (* same type anyway *)
          else Lenv.add_type_var s1 nt env, nt, nt
      else
      if generated_var s2 then
        try
          let nt = Lenv.find_type_var s2 env in
          partial_unif ~overloaded loc term ot nt env
        with Not_found ->
          Lenv.add_type_var s2 ot env, ot, ot
      else if s1 = s2 then env, ot, ot (* same type *)
      else
        C.error loc
          "implicit unification of type variables %s and %s" s1 s2
    | Lvar s1, _ when generated_var s1 ->
      (try let ot = Lenv.find_type_var s1 env in
         let env,ot,nt =
           partial_unif ~overloaded loc term ot nt env in
         let st = find_supertype ~overloaded loc term ot nt in
         let env =
           if is_same_type ot st then env
           else Lenv.add_type_var s1 st env
         in
         env, ot, st
       with Not_found -> Lenv.add_type_var s1 nt env, nt, nt)
    | _, Lvar s2 when generated_var s2 ->
      (try
         let nt = Lenv.find_type_var s2 env in
         let env, ot, nt =
           partial_unif ~overloaded loc term ot nt env
         in
         let st = find_supertype ~overloaded loc term ot nt in
         let env =
           if is_same_type nt st then env
           else Lenv.add_type_var s2 st env
         in env, ot, st
       with Not_found ->
         Lenv.add_type_var s2 ot env, ot, ot)
    | Ltype(t1,l1), Ltype(t2,l2) when t1.lt_name = t2.lt_name ->
      let env =
        List.fold_right2
          (fun ot nt env ->
             let (env,_,_) = partial_unif ~overloaded loc term ot nt env in
             env)
          l1 l2 env
      in
      let l1 = List.map (instantiate env) l1 in
      let l2 = List.map (instantiate env) l2 in
      env,Ltype(t1,l1),Ltype(t2,l2)
    | Larrow(args1,rt1), Larrow(args2,rt2)
      when List.length args1 = List.length args2 ->
      let env =
        List.fold_right2
          (fun ot nt env ->
             let env,_,_ = partial_unif ~overloaded loc term ot nt env in
             env)
          args1 args2 env
      in
      let env, _, _ =
        partial_unif ~overloaded loc term rt1 rt2 env
      in
      let rt1 = instantiate env rt1 in
      let rt2 = instantiate env rt2 in
      let args1 = List.map (instantiate env) args1 in
      let args2 = List.map (instantiate env) args2 in
      env, Larrow(args1,rt1), Larrow(args2,rt2)
    | Ltype ({lt_name = "\\list"},[t1]), Ltype ({lt_name = "\\list"},[t2]) ->
      let (env, ot, nt) = partial_unif ~overloaded loc term t1 t2 env in
      env, ot, make_type_list_of nt
    | t1, Ltype ({lt_name = "set"},[t2]) ->
      let (env, ot, nt) = partial_unif ~overloaded loc term t1 t2 env in
      env, ot, make_set_type nt
    | Ltype({lt_name = "set"}, [t1]), t2 ->
      let (env, ot, nt) = partial_unif ~overloaded loc term t1 t2 env in
      env, make_set_type ot, make_set_type nt
    | t1,t2 when plain_boolean_type t1 && plain_boolean_type t2 ->
      env,ot,nt
    | ((Ctype _ | Linteger | Lreal | Ltype ({lt_name = "boolean"},[])),
       (Ctype _ | Linteger | Lreal | Ltype ({ lt_name = "boolean"},[]))) ->
      env,ot,nt
    | (Ltype _|Larrow _|Lvar _), _ | _, (Larrow _| Ltype _|Lvar _) ->
      if overloaded then raise Not_applicable
      else
        C.error loc "incompatible types %a and %a"
          Cil_printer.pp_logic_type ot
          Cil_printer.pp_logic_type nt

  let instantiate_app ~overloaded loc oterm nt env =
    let ot = oterm.term_type in
    let env, ot, nt = partial_unif ~overloaded loc oterm ot nt env in
    let t,e =
      implicit_conversion ~overloaded loc { oterm with term_type = ot} ot nt
    in
    env, t, e

  let convertible (t1,t) (t2,_) =
    let res =
      try
        let _ =
          implicit_conversion
            ~overloaded:true Cil_datatype.Location.unknown t t1 t2
        in true
      with Not_applicable -> false
    in
    Kernel.debug ~level:4 "Checking conversion between %a and %a: %B@."
      Cil_printer.pp_logic_type t1 Cil_printer.pp_logic_type t2 res;
    res

  let convertible_non_null (ty1,t as t1) (ty2,_ as t2) =
    match (unroll_type ty1, unroll_type ty2) with
    | Ctype ty1, Ctype ty2 when
        isPointerType ty1 && isPointerType ty2 && isLogicNull t ->
      isVoidPtrType ty2
    | _ -> convertible t1 t2

  (* TODO: filter on signatures, not on type-checked actual arguments !!!!!! *)
  let filter_non_minimal_arguments l ((_,_,tl,_) as p) =
    let rec aux acc l =
      match l with
      | [] -> p::acc
      | ((_,_,tl',_) as p')::r ->
        if List.for_all2 convertible tl tl' then
          if List.for_all2 convertible tl' tl then
            (* Both are equivalent. This might come from arbitrary
               conversions of null pointer. Let's see if one of the list
               subsumes the other without relying on null ptr.
            *)
            if not (List.for_all2 convertible_non_null tl tl') then
              if not (List.for_all2 convertible_non_null tl' tl) then
                (* Both have null pointers converted to other type.
                   Just don't choose a representative.
                *)
                aux (p'::acc) r
              else
                (* just use tl, it has less conversion than tl'. *)
                aux acc r
            else (* tl' has less conversion than tl, we can discard
                    the new entry *)
              List.rev_append acc l
          else
            (* tl subtype of tl' *)
            aux acc r
        else
        if List.for_all2 convertible tl' tl then
          (* tl' subtype of tl *)
          List.rev_append acc l
        else
          aux (p'::acc) r
    in
    let l = aux [] l in
    assert (l <> []); l

  let rec logic_arithmetic_promotion t =
    match unroll_type t with
    | Ctype ty when Cil.isIntegralType ty -> Linteger
    | Linteger -> Linteger
    | Lreal -> Lreal
    | Ctype ty ->
      (match Cil.unrollType ty with
         TFloat _ -> Lreal
       | _ ->
         Kernel.fatal ~current:true
           "logic arithmetic promotion on non-arithmetic type %a"
           Cil_printer.pp_logic_type t)
    | Ltype ({lt_name="set"} as lt,[t]) ->
      Ltype(lt,[logic_arithmetic_promotion t])
    | Ltype _ | Lvar _ | Larrow _ ->
      Kernel.fatal ~current:true
        "logic arithmetic promotion on non-arithmetic type %a"
        Cil_printer.pp_logic_type t

  let rec integral_promotion t =
    match unroll_type t with
    | Ctype ty when isIntegralType ty ->
      Linteger
    | Linteger -> Linteger
    | Ltype ({lt_name="set"} as lt,[t]) -> Ltype(lt,[integral_promotion t])
    | Ltype _ | Lreal | Lvar _ | Larrow _ | Ctype _ ->
      Kernel.fatal ~current:true
        "logic integral promotion on non-integral type %a"
        Cil_printer.pp_logic_type t

  let mk_shift loc env idx t_elt t =
    let idx = mk_cast idx (integral_promotion idx.term_type) in
    let add_offset array idx =
      Logic_const.term ~loc
        (TLval
           (Logic_const.addTermOffsetLval
              (TIndex (idx, TNoOffset)) array))
        t_elt
    in
    let here_idx = mk_at_here idx in
    match t.term_node with
    | TStartOf array -> add_offset array idx
    | TLval array when is_array_type t.term_type -> add_offset array idx
    | Tlet (def, ({ term_node = TLval array} as t))
      when is_array_type t.term_type ->
      Logic_const.term ~loc (Tlet (def, add_offset array idx)) t_elt
    | Tat({term_node = TStartOf (TVar { lv_origin = Some v},_ as lv)},lab)
      when v.vformal && lab = old_label && env.Lenv.is_funspec ->
      Logic_const.tat ~loc (add_offset lv here_idx,lab)
    | Tat({term_node = TLval (TVar { lv_origin = Some v},_ as lv)},lab)
      when v.vformal && lab = old_label && env.Lenv.is_funspec &&
           is_array_type t.term_type ->
      Logic_const.tat ~loc (add_offset lv here_idx,lab)
    | _ ->
      let b =
        { term_node = TBinOp (IndexPI, t, idx); term_name = [];
          term_loc = loc;
          term_type = set_conversion t.term_type idx.term_type }
      in
      mk_mem b TNoOffset



  let conditional_conversion loc env rel t1 t2 =
    let is_rel = Extlib.has_some rel in
    (* a comparison is mainly a function of type 'a -> 'a -> Bool/Prop.
       performs the needed unifications on both sides.*)
    let var = fresh_type_var "cmp" in
    let env,_,_ =
      partial_unif ~overloaded:false loc t1 t1.term_type var env in
    let env,ty2,_ =
      partial_unif ~overloaded:false loc t2 t2.term_type var env in
    (* in case first partial unification did not instantiate all variables
       we do another pass on t1 with information from t2.
    *)
    let env,ty1,_ =
      partial_unif ~overloaded:false loc t1 t1.term_type var env
    in
    let rec aux lty1 lty2 =
      match (unroll_type lty1), (unroll_type lty2) with
      | Ctype ty1, Ctype ty2 ->
        if isIntegralType ty1 && isIntegralType ty2 then
          if is_same_type lty1 lty2 then lty1
          else if (isSignedInteger ty1) <> (isSignedInteger ty2) then
            (* in ACSL, the comparison between 0xFFFFFFFF seen as int and
               unsigned int is not true: we really have to operate at
               the integer level.
            *)
            Linteger
            (* comparing an enumerated constant with a value of type enum
               is done on enum, not on the underlying type.
            *)
          else if is_enum_cst t1 lty2 then lty2
          else if is_enum_cst t2 lty1 then lty1
          else Ctype (C.conditionalConversion ty1 ty2)
        else if isArithmeticType ty1 && isArithmeticType ty2 then begin
          if is_same_type lty1 lty2 then begin
            if is_rel then begin
              let rel = Extlib.the rel in
              let kind =
                match Cil.unrollType ty1 with
                | TFloat (FFloat,_) -> "float"
                | TFloat (FDouble,_) -> "double"
                | TFloat (FLongDouble,_) -> "long double"
                | _ -> Kernel.fatal "floating point type expected"
              in
              let source = fst loc in
              Kernel.warning ~source ~wkey:Kernel.wkey_acsl_float_compare
                "comparing two %s values as real values. You might \
                 want to use \\%s_%s instead" kind rel kind;
              Lreal
            end else lty1
          end else Lreal
        end else if is_same_ptr_type ty1 ty2 || is_same_array_type ty1 ty2 then
          Ctype (C.conditionalConversion ty1 ty2)
        else if
          (isPointerType ty1 || isArrayType ty1) &&
          (isPointerType ty2 || isArrayType ty2)
        then C.error loc "types %a and %a are not convertible"
            Cil_printer.pp_typ ty1 Cil_printer.pp_typ ty2
        else (* pointer to integer conversion *)
          Ctype (C.conditionalConversion ty1 ty2)
      | (Linteger, Ctype t | Ctype t, Linteger)
        when Cil.isIntegralType t -> Linteger
      | (Linteger, Ctype t | Ctype t, Linteger)
        when Cil.isArithmeticType t -> Lreal
      (* In ACSL, you can convert implicitely from integral to boolean =>
         prefer boolean as common type when doing comparison. *)
      | Ltype({lt_name = name},[]), t
        when is_integral_type t && name = Utf8_logic.boolean ->
        Ltype(C.find_logic_type Utf8_logic.boolean,[])
      | t, Ltype({lt_name = name},[])
        when is_integral_type t && name = Utf8_logic.boolean ->
        Ltype(C.find_logic_type Utf8_logic.boolean,[])
      | Lreal, Ctype ty | Ctype ty, Lreal when isArithmeticType ty -> Lreal
      | Ltype (s1,l1), Ltype (s2,l2)  when s1.lt_name = s2.lt_name &&
                                           List.for_all2 is_same_type l1 l2 -> lty1
      | Lvar s1, Lvar s2 when s1 = s2 -> lty1
      | Linteger, Linteger -> Linteger
      | (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
      | Ltype ({lt_name = "\\list"} as lt,[t1]),
        Ltype({lt_name="\\list"},[t2]) ->
        Ltype(lt,[aux t1 t2])
      | Ltype ({lt_name = "set"} as lt,[t1]), Ltype({lt_name="set"},[t2]) ->
        Ltype(lt,[aux t1 t2])
      (* implicit conversion to set *)
      | Ltype ({lt_name = "set"} as lt,[t1]), t2
      | t1, Ltype({lt_name="set"} as lt,[t2]) -> Ltype(lt,[aux t1 t2])
      | t1, t2 when is_same_type t1 t2 -> t1
      | _ ->
        C.error loc "types %a and %a are not convertible"
          Cil_printer.pp_logic_type lty1 Cil_printer.pp_logic_type lty2
    in
    let rt = aux ty1 ty2 in
    env,rt,ty1,ty2

  type conversion = NoConv | ArithConv | IntegralConv | PointerConv

  let location_set_conversion loc transform_pointer_set t ot nt env =
    let ot = set_conversion ot nt in
    if is_same_type ot nt then transform_pointer_set, ot
    else if is_integral_type ot && is_integral_type nt then
      let typ = arithmetic_conversion ot nt in IntegralConv, typ
    else if is_arithmetic_type ot && is_arithmetic_type nt then
      let typ = arithmetic_conversion ot nt in ArithConv, typ
    else if is_pointer_type ot && is_pointer_type nt then
      let typ = Ctype Cil.charPtrType in
      PointerConv, (if is_set_type ot then make_set_type typ else typ)
    else
      let _,_,t = partial_unif ~overloaded:false loc t ot nt env in
      transform_pointer_set,t

  let make_set_conversion conv t =
    match conv with
    | NoConv -> t
    | ArithConv -> logic_coerce Lreal t
    | IntegralConv -> logic_coerce Linteger t
    | PointerConv -> location_to_char_ptr t

  let list_conversion loc t ot nt env =
    if is_same_type ot nt then ot
    else if plain_integral_type ot && plain_integral_type nt then ot
    else if plain_arithmetic_type ot && plain_arithmetic_type nt then ot
    else let _,_,t = partial_unif ~overloaded:false loc t ot nt env in
      t

  let list_promotion typ =
    if plain_integral_type typ then
      Linteger
    else if plain_arithmetic_type typ then
      Lreal
    else typ

  let list_coercion typ t =
    if is_same_type typ t.term_type then t
    else logic_coerce typ t

  (* Typing terms *)

  let parseInt loc s =
    let explode s =
      let l = ref [] in
      String.iter (fun c -> l:=Int64.of_int (Char.code c) :: !l) s;
      List.rev !l
    in
    match String.get s 0 with
    | 'L' -> (* L'wide_char' *)
      let content = String.sub s 2 (String.length s - 3) in
      let tokens = explode content in
      let value = Cil.reduce_multichar Cil.theMachine.Cil.wcharType tokens
      in
      tinteger_s64 ~loc value
    | '\'' -> (* 'char' *)
      let content = String.sub s 1 (String.length s - 2) in
      let tokens = explode content in
      let value,_= Cil.interpret_character_constant tokens in
      term ~loc (TConst (constant_to_lconstant value)) Linteger
    | _ -> Cil.parseIntLogic ~loc s

  let find_logic_label loc env l =
    try Lenv.find_logic_label l env
    with Not_found ->
    (* look for a C label *)
    try
      let lab = C.find_label l in
      StmtLabel lab
    with Not_found ->
      C.error loc "logic label `%s' not found" l

  let find_old_label loc env =
    try Lenv.find_logic_label "Old" env
    with Not_found ->
      C.error loc "\\old undefined in this context"

  let default_inferred_label = FormalLabel "L"

  let find_current_label loc env =
    match env.Lenv.current_logic_label with
    | Some lab -> lab
    | None ->
      if Lenv.no_label env then begin
        match !Lenv.default_label with
          None ->
          let lab = default_inferred_label in
          Lenv.default_label := Some lab; lab
        | Some lab -> lab
      end else
        C.error loc
          "no label in the context. (\\at or explicit label missing?)"

  let find_current_logic_label loc env = function
    | None -> find_current_label loc env
    | Some l -> find_logic_label loc env l

  let check_current_label loc env = ignore (find_current_label loc env)

  let labels_assoc loc id env fun_labels effective_labels =
    match fun_labels, effective_labels with
      [_], [] -> [find_current_label loc env]
    | _ ->
      try
        List.map2
          (fun _ l2 -> l2)
          fun_labels effective_labels
      with Invalid_argument _ ->
        C.error loc "wrong number of labels for %s" id

  let add_quantifiers ctxt loc ~kind q env =
    let (tq,env) =
      List.fold_left
        (fun (tq,env) (ty, id) ->
           let ty = unroll_type (logic_type ctxt loc env ty) in
           let v = Cil_const.make_logic_var_kind id kind ty in
           (v::tq, Lenv.add_var id v env))
        ([],env) q
    in
    (List.rev tq,env)

  class rename_variable v1 v2 =
    object
      inherit Cil.nopCilVisitor
      method! vlogic_var_use v =
        if v.lv_id = v1.lv_id then ChangeTo v2 else SkipChildren
    end

  (* rename v1 into v2 in t *)
  let rename_variable t v1 v2 =
    visitCilTerm (new rename_variable v1 v2) t

  let find_logic_info v env =
    try Lenv.find_logic_info v.lv_name env
    with Not_found ->
      let l = C.find_all_logic_functions v.lv_name in
      (* Data constructors can not be in eta-reduced form. v must be
         a logic function, so that List.find can not fail here.
      *)
      List.find (fun x -> x.l_var_info.lv_id = v.lv_id) l

  let eta_expand loc names env v =
    match (unroll_type v.lv_type) with
      Larrow(args,rt) ->
      let (_,vars) = List.fold_right
          (fun x (i,l) ->
             i+1,
             Cil_const.make_logic_var_quant ("x_" ^ (string_of_int i)) x ::l)
          args (0,[])
      in
      let args =
        List.map
          (fun x -> {term_name = [];
                     term_loc = loc;
                     term_node = TLval(TVar x,TNoOffset);
                     term_type = x.lv_type;
                    })
          vars
      in
      { term_loc = loc;
        term_name = names;
        term_node =
          Tlambda(vars,{term_name = [];
                        term_loc = loc;
                        term_node =
                          (* For now, it is not possible to have labels
                             appended to plain variable, so we have
                             to suppose that v has no label (this is checked
                             when type-checking v as a variable)
                          *)
                          Tapp(find_logic_info v env,[],args);
                        term_type = rt});
        term_type = v.lv_type}
    | _ -> { term_loc = loc; term_name = names;
             term_node = TLval(TVar v, TNoOffset);
             term_type = v.lv_type }

  let fresh_vars known_vars v =
    if List.mem v.lv_name known_vars then begin
      let i = ref 0 in
      while List.mem (v.lv_name ^ "_" ^ string_of_int !i) known_vars do
        incr i;
      done;
      v.lv_name <- v.lv_name ^ "_" ^ string_of_int !i
    end

  let normalize_lambda_term env term =
    let add_binders quants term =
      match term.term_node, (unroll_type term.term_type) with
      | Tlambda(quants',term), Larrow (args,rt_typ) ->
        let args = List.fold_right (fun x l -> x.lv_type :: l) quants args in
        { term with
          term_node = Tlambda (quants @ quants', term);
          term_type = Larrow (args,rt_typ) }
      | Tlambda _ , _ ->
        Kernel.fatal ~current:true "\\lambda with a non-arrow type"
      | _,typ ->
        { term with
          term_node = Tlambda(quants, term);
          term_type = Larrow(List.map (fun x -> x.lv_type) quants,typ) }
    in
    let rec aux known_vars kont term =
      match term.term_node with
      | TLval(TVar v, TNoOffset) ->
        known_vars, kont (eta_expand term.term_loc term.term_name env v)
      | TConst _ | TLval _ | TSizeOf _ | TSizeOfE _
      | TSizeOfStr _ | TAlignOf _ | TAlignOfE _
      | TUnOp _ | TBinOp _ | TCastE _ | TAddrOf _ | TStartOf _
      | Tapp _  | TDataCons _ | Tbase_addr _ | Toffset _
      | Tblock_length _ | Tnull
      | TUpdate _ | Ttypeof _ | Ttype _ | Tempty_set
      (* [VP] I suppose that an union of functions
         is theoretically possible but I'm not sure that we want to
         lift the lambda anyway, even though this contradicts the
         idea that you can always replace a term by a set of terms
      *)
      | Tunion _ | Tinter _ | Tcomprehension _
      | Trange _ | TLogic_coerce _

        -> known_vars, kont term
      | Tlambda (quants,term) ->
        List.iter (fresh_vars known_vars) quants;
        let known_vars =
          List.fold_left (fun l x -> x.lv_name :: l) known_vars quants
        in
        aux known_vars (kont $ (add_binders quants)) term
      | Tif (cond, ttrue, tfalse) ->
        let known_vars, ttrue = aux known_vars (fun x -> x) ttrue in
        let known_vars, tfalse = aux known_vars (fun x -> x) tfalse in
        let term =
          match ttrue.term_node, tfalse.term_node with
          | Tlambda(quants1,term1), Tlambda(quants2,term2) ->
            assert(
              Kernel.verify(List.length quants1 = List.length quants2)
                "Branches of conditional have different number \
                 of \\lambda");
            let term2 =
              List.fold_left2 rename_variable term2 quants2 quants1
            in
            { term with
              term_node =
                Tlambda(quants1,
                        {term with
                         term_node = Tif(cond,term1,term2);
                         term_type = term1.term_type});
              term_type = ttrue.term_type }
          | Tlambda _, _ | _, Tlambda _ ->
            Kernel.fatal ~current:true
              "Branches of conditional have different number of \\lambda"
          | _,_ -> term
        in known_vars, kont term
      | Tat (t,lab) ->
        let push_at t = match t.term_node with
            Tlambda(quants,t) ->
            { term with
              term_node =
                Tlambda(quants, {t with term_node = Tat (t,lab)})}
          | _ -> term
        in
        aux known_vars (kont $ push_at) t
      | Tlet(v,body) ->
        fresh_vars known_vars v.l_var_info;
        let known_vars = v.l_var_info.lv_name :: known_vars in
        let push_let t = match t.term_node with
            Tlambda(quants, t) ->
            { term with
              term_node =
                Tlambda(quants, { t with term_node = Tlet(v,t) } ); }
          | _ -> term
        in
        aux known_vars (kont $ push_let) body
    in snd (aux [] (fun x -> x) term)

  let has_extra_offset_to_TField loc t_type = function
    (* used for functional update of field under anonymous type *)
    | PLpathField f ->
      let f_ofs, _ = plain_type_of_c_field loc f t_type in
      let result = match f_ofs with
        | TField (_,TNoOffset) -> false
        | TField _ -> true ;
        | _ -> assert false
      in result
    | PLpathIndex _ -> false

  let updated_offset_term idx_typing check_type mk_field mk_idx loc t_type =
    function
    | PLpathField f ->
      let f_ofs, ofs_type = plain_type_of_c_field loc f t_type in
      let f_ofs, ofs_type = match f_ofs with
        | TField (f,TNoOffset) ->( mk_field f),ofs_type
        | TField (f,_) -> (mk_field f),
                          (* f is an anonymous field, find its type *)
                          Ctype (Cil.typeOffset t_type (Field (f,NoOffset)))
        | _ -> assert false
      in f_ofs,ofs_type
    | PLpathIndex idx ->
      let idx = idx_typing idx in
      let ofs_type =
        if Cil.isArrayType t_type && check_type idx.term_type
        then Ctype (Cil.typeOf_array_elem t_type)
        else C.error loc "subscripted value is not an array"
      in mk_idx idx, ofs_type

  let normalize_updated_offset_term idx_typing env loc t normalizing_cont toff =
    let t_type =
      try Logic_utils.logicCType (Logic_utils.unroll_type t.term_type)
      with Failure _ ->
        C.error loc "Trying to update field on a non struct type %a"
          Cil_printer.pp_logic_type t.term_type
    in
    let mk_let_info name t t_off2 type2 =
      match t with
      | { term_node = TConst _} -> (* just a copy *)
        assert (t_off2 = TNoOffset) ;
        (fun id -> id), t, { t with term_node = t.term_node }
      | { term_node = TLval((TVar _,_) as lv)} -> (* just a copy *)
        (fun id -> id), t,
        { t with term_node = TLval(Logic_const.addTermOffsetLval t_off2 lv);
                 term_type = type2}
      | _ -> (* to build a let *)
        let var = Lenv.fresh_var env name LVLocal t.term_type in
        let info =
          { l_var_info = var;
            l_labels = [];
            l_tparams = [];
            l_type = Some t.term_type;
            l_profile = [];
            l_body = LBterm t } in
        (fun body -> Tlet(info, { t with term_node = body})),
        { t with term_node = TLval(TVar var,TNoOffset)},
        { t with term_node = TLval(TVar var,t_off2);
                 term_type = type2}
    in
    let (toff, t_off2, opt_idx_let), ofs_type =
      let check_type typ = plain_integral_type typ
                           || C.error loc "range is only allowed for last offset"
      and mk_field f = TField(f,TNoOffset),TField(f,TNoOffset),(fun x -> x)
      and mk_idx idx =
        let mk_idx_let, idx, idx2 =
          mk_let_info "idx" idx TNoOffset idx.term_type
        in TIndex(idx,TNoOffset),TIndex(idx2,TNoOffset),mk_idx_let
      in updated_offset_term
        idx_typing check_type mk_field mk_idx
        loc t_type toff
    in
    let mk_let, t, t2 = mk_let_info "tmp" t t_off2 ofs_type in
    let v, v_type = normalizing_cont t2 in
    let v = Logic_const.term ~loc v v_type in
    let v = mk_cast v ofs_type in
    let updated = mk_let (opt_idx_let (TUpdate(t,toff,v)))
    in updated, t.term_type

  let update_term_wrt_default_label t =
    match !Lenv.default_label with
    | None -> t
    | Some lab ->
      match t.term_node with
      | TConst _
      | TLval (TVar _ ,_)
      | Tat _ -> t
      | _ -> { t with term_node = Tat(t,lab) }

  (* Updates recursive calls of a function that gets an implicit label during
     the type-checking of its body. We have to add said implicit label to the
     call itself to have a consistent AST, i.e. something like
     logic f{L}(...) = ... f{L}(...).

     In case of inductive predicates, the considered label is not the one given
     into the profile of the predicate, but the one given into the defined case,
     i.e. something like
     inductive P{...}(...) { case n{L}: .... P{L}(...) }.

     The visit must be done after the type-checking, as the implicit label may
     be added after the recursive call has been handled.
  *)
  let add_label info lab =
    let vis =
      object(self)
        val mutable curr_lab = lab
        inherit Cil.nopCilVisitor
        method! vterm_node t =
          match t with
          | Tapp(info',[],args) when Cil_datatype.Logic_info.equal info info' ->
            ChangeDoChildrenPost(Tapp(info,[curr_lab], args),Extlib.id)
          | _ -> DoChildren
        method! vpredicate_node p =
          match p with
          | Papp(info',[],args) when Cil_datatype.Logic_info.equal info info' ->
            ChangeDoChildrenPost (Papp(info, [curr_lab], args),Extlib.id)
          | _ -> DoChildren

        method private treat_ind_case (n,labs,t,p as ind) =
          match labs with
          | [ mylab ] ->
            let old = curr_lab in
            curr_lab <- mylab;
            let p' = Cil.visitCilPredicate self p in
            let res = if p' != p then (n, labs, t, p') else ind in
            curr_lab <- old;
            res
          | _ -> ind
        (* We do not have a context allowing to update the predicate.
           Implies that any recursive call is already explicitly guarded
        *)

        method! vlogic_info_decl info =
          match info.l_body with
          | LBinductive l ->
            let l' = Cil.mapNoCopy self#treat_ind_case l in
            if l != l' then info.l_body <- LBinductive l';
            SkipChildren
          | _ -> DoChildren
      end
    in
    (* body will be changed in place by visitor if needed. *)
    ignore (Cil.visitCilLogicInfo vis info)

  (* In case we have added an implicit label in the environment
     during type-checking of the body of the logic function/predicate,
     we update the label list, and visit the body to ensure potential
     recursive calls are coherent with the declaration.
  *)
  let update_info_wrt_default_label info =
    match info.l_labels with
    | [] ->
      (match !Lenv.default_label with
       | None -> ()
       | Some lab ->
         info.l_labels <- [ lab ];
         add_label info lab
      )
    | _ -> ()

  let update_predicate_wrt_default_label p =
    match !Lenv.default_label with
    | None -> p
    | Some lab ->
      { p with pred_content = Pat(p,lab) }

  let update_ind_case_wrt_default_label (name, labs, tvars, p as case) =
    match labs, !Lenv.default_label with
    | [], Some lab -> (name, [lab], tvars, update_predicate_wrt_default_label p)
    | _ -> case

  type lval_mode =
    { accept_empty: bool;
      accept_formal: bool;
      accept_array: bool;
      accept_models: bool;
      accept_func_ptr: bool;
    }

  let lval_addressable_mode =
    { accept_empty = false; accept_formal = true; accept_array = true;
      accept_models = false; accept_func_ptr = true; }
  let lval_assignable_mode =
    { accept_empty = true; accept_formal = true; accept_array = false;
      accept_models = true; accept_func_ptr = false; }

  let is_fct_ptr lv = Cil.isLogicFunctionType (Cil.typeOfTermLval lv)

  let check_lval_kind m t =
    let rec aux t = match t.term_node with
      | Tempty_set -> m.accept_empty
      | TLval (lhost,loff) ->
        (not (isLogicArrayType t.term_type) || m.accept_array) &&
        (match lhost with
         | TVar v -> begin
             match v.lv_origin with
             | None ->
               (* specific case: \exit_status is a model of the
                  exit status of the program. *)
               if Logic_const.is_exit_status t then m.accept_models
               else
                 false (* pure logic variable, at least as long as
                          model variables are not supported. *)
             | Some v -> not v.vformal || m.accept_formal
           end
         | TResult _ -> m.accept_models
         | _ -> true) &&
        (match snd (Logic_utils.remove_term_offset loff) with
         | TModel _ -> m.accept_models
         | _ -> true)
      | TAddrOf lv -> is_fct_ptr lv && m.accept_func_ptr
      | Tif (_,t1,t2) -> aux t1 && aux t2

      | Tunion l | Tinter l -> List.for_all aux l

      | TLogic_coerce (_,t) | Tat (t, _) | Tcomprehension (t,_,_) | Tlet (_, t)
        -> aux t

      | Trange _ | TConst _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _
      | TAlignOfE _ | TUnOp (_,_) | TBinOp (_,_,_) | TCastE (_,_)
      | TStartOf _ | Tlambda (_,_) | TDataCons (_,_) | Tbase_addr (_,_)
      | Toffset (_,_) | Tblock_length (_,_) | Tnull | Tapp _
      | TUpdate (_,_,_) | Ttypeof _ | Ttype _ ->
        false
    in
    aux t

  let boolean_term_to_predicate t =
    if Logic_utils.is_zero_comparable t then
      Logic_utils.scalar_term_to_predicate t
    else
      C.error t.term_loc "expecting a term that can be coerced to a boolean"

  let rec normalize_update_term ctxt env loc t v =
    let module C = struct end in
    function
    (* Transform terms like {x \with .c[idx] = v}
       into {x \with .c = {x.c \with [idx] = v}}.
       \let expressions can be introduced. *)
    | [] -> assert false (* parsing invariant *)
    | (toff::tail) as offs ->
      begin
        let t_type =
          try Logic_utils.logicCType (Logic_utils.unroll_type t.term_type)
          with Failure _ ->
            if ctxt.silent then raise Backtrack;
            ctxt.error loc "Update field on a non-struct type %a"
              Cil_printer.pp_logic_type t.term_type
        in
        let tail =
          if has_extra_offset_to_TField loc t_type toff then
            offs (* fields under an anonymous field are not removed *)
          else
            tail
        in
        match tail with
        | [] ->
          let toff, ofs_type =
            let mk_field f = TField (f, TNoOffset)
            and mk_idx idx = TIndex(idx,TNoOffset)
            and idx_typing idx = ctxt.type_term ctxt env idx
            in
            updated_offset_term
              idx_typing is_integral_type mk_field mk_idx loc t_type toff
          in
          let v = ctxt.type_term ctxt env v in
          let v = mk_cast v ofs_type in
          let updated = TUpdate(t,toff,v)
          in updated, t.term_type
        | toffs ->
          let idx_typing idx = ctxt.type_term ctxt env idx
          and normalizing_cont t2 =
            normalize_update_term ctxt env loc t2 v toffs
          in
          normalize_updated_offset_term
            idx_typing env loc t normalizing_cont toff
      end
  and normalize_update_cont ctxt env loc t =
    let module C = struct end in
    function
    | [],_ -> assert false (* parsing invariant *)
    | _,[] -> assert false (* parsing invariant *)
    | ((contoffs,PLupdateTerm v)::[]),toffs ->
      (* {x \with .c1 = {\with .c2 = v}} =
         {x \with .c1.c2 = v} *)
      normalize_update_term ctxt env loc t v (toffs@contoffs)
    | ((contoffs,PLupdateCont v)::[]),toffs ->
      (* {x \with .c1 = {\with .c2 = {\with...}}} =
         {x \with .c1.c2 = {\with...}} *)
      normalize_update_cont ctxt env loc t (v,(toffs@contoffs))
    | (cont::conts),toff::[] ->
      (* {x \with .c1 = {\with .c2 = v2, ..., c22 = v22}} =
         {x \with .c1 = {...{x.c1 \with .c2 = v2} .. \with c22 = v22} *)
      let idx_typing idx = ctxt.type_term ctxt env idx in
      let normalizing_cont t2 =
        let normalize t = function
          | contoffs,PLupdateTerm v ->
            normalize_update_term ctxt env loc t v contoffs
          | contoffs,PLupdateCont cont ->
            normalize_update_cont ctxt env loc t (cont, contoffs)
        in
        let normalize_folding (tn,typ) cont =
          normalize (Logic_const.term ~loc tn typ) cont
        in List.fold_left normalize_folding (normalize t2 cont) conts
      in normalize_updated_offset_term
        idx_typing env loc t normalizing_cont toff
    | cont,toff::toffs ->
      (* {x \with .c1.c2 = {\with...}} =
             {x \with .c1 = { x.c1 \with .c2 = {\with...}}} *)
      let idx_typing idx = ctxt.type_term ctxt env idx
      and normalizing_cont t2 =
        normalize_update_cont ctxt env loc t2 (cont,toffs)
      in
      normalize_updated_offset_term idx_typing env loc t normalizing_cont toff
  and locations_set ctxt ~lift_set env loc l init_type =
    let convert_ptr, locs, typ =
      List.fold_left
        (fun (convert_ptr,locs,typ) t ->
           let t' = ctxt.type_term ctxt env t in
           if (not lift_set) && is_set_type t'.term_type then begin
             if ctxt.silent then raise Backtrack;
             ctxt.error loc "set of sets is not yet implemented" ;
           end;
           let convert_ptr, typ =
             location_set_conversion
               t'.term_loc convert_ptr t' t'.term_type typ env
           in
           assert (locs <> [] || convert_ptr = NoConv);
           (convert_ptr,t'::locs, typ))
        (NoConv, [], init_type) l
    in
    let locs = List.rev_map (make_set_conversion convert_ptr) locs in
    locs,typ
  and lfun_app ctxt env loc f labels ttl =
    try
      let info = ctxt.find_logic_ctor f in
      if labels <> [] then begin
        if ctxt.silent then raise Backtrack;
        ctxt.error loc
          "symbol %s is a data constructor. \
           It cannot have logic labels" f;
      end;
      let params = List.map fresh info.ctor_params in
      let env, tl =
        type_arguments ~overloaded:false env loc params ttl
      in
      let t = Ltype(info.ctor_type,
                    List.map fresh_type_var
                      info.ctor_type.lt_params)
      in
      let t = instantiate env t in
      TDataCons(info,tl), t
    with Not_found ->
      let info, label_assoc, tl, t =
        type_logic_app env loc f labels ttl
      in
      match t with
      | None ->
        if ctxt.silent then raise Backtrack;
        ctxt.error loc "symbol %s is a predicate, not a function" f
      | Some t -> Tapp(info, label_assoc, tl), t
  and term_node ctxt env loc pl =
    let term = ctxt.type_term ctxt in
    let term_ptr pl =
      let t = term env pl in
      check_logic_pointer ~check_non_void:false loc t;
      t
    in
    let predicate = ctxt.type_predicate ctxt in
    match pl with
    | PLinitIndex _ ->
      ctxt.error loc "unsupported aggregated array construct"
    | PLinitField _ ->
      ctxt.error loc "unsupported aggregated field construct"
    | PLupdate (t, toff, PLupdateCont cont) ->
      let t = term env t in
      normalize_update_cont ctxt env loc t (cont, toff)
    | PLupdate (t, toff, PLupdateTerm v) ->
      let t = term env t in
      normalize_update_term ctxt env loc t v toff

    | PLsizeof typ ->
      (match Logic_const.unroll_ltdef (logic_type ctxt loc env typ)
       with
         Ctype t -> TSizeOf t,Linteger
       | _ -> if ctxt.silent then raise Backtrack;
         ctxt.error loc "sizeof can only handle C types")
    (* NB: don't forget to add the case of literal string
              when they are authorized in the logic *)
    | PLsizeofE
        { lexpr_node = PLconstant (StringConstant s | WStringConstant s) } ->
      TSizeOfStr s, Linteger
    | PLsizeofE lexpr ->
      let t = term env lexpr in
      (match Logic_const.unroll_ltdef t.term_type with
       | Ctype _ -> TSizeOfE t, Linteger
       | _ -> if ctxt.silent then raise Backtrack;
         ctxt.error loc "sizeof can only handle C types")
    | PLnamed _ -> assert false (* should be captured by term *)
    | PLconstant (IntConstant s) ->
      begin match (parseInt loc s).term_node with
        | TConst (Integer _ as c) -> TConst c, Linteger
        | TConst ((LChr _) as c) -> (* a char literal has type int *)
          TConst c, Linteger
        | _ -> assert false
      end
    | PLconstant (FloatConstant str) ->
      TConst (Logic_utils.string_to_float_lconstant str), Lreal
    | PLconstant (StringConstant s) ->
      TConst (LStr (unescape s)), Ctype Cil.charPtrType
    | PLconstant (WStringConstant s) ->
      TConst (LWStr (wcharlist_of_string s)),
      Ctype (TPtr(Cil.theMachine.wcharType,[]))
    | PLvar x ->
      let old_val info =
        let term =  TLval (TVar info, TNoOffset) in
        if env.Lenv.is_funspec then begin
          let term =
            match Lenv.current_post_state env with
              None -> term
            | Some _ ->
              (match info.lv_origin with
                 Some v when v.vformal ->
                 Tat(Logic_const.term ~loc term info.lv_type,
                     find_logic_label loc env "Old")
               | Some _ | None -> term)
          in term, info.lv_type
        end else term, info.lv_type
      in
      begin
        try
          let def = ctxt.find_macro x in
          let t = term env def in
          t.term_node, t.term_type
        with Not_found ->
        try
          (* NB: In the current implementation and ACSL format, \let
             can not take a label parameter. If this ever change,
             we need to check the labelling here as well (see below for
             globals)
          *)
          let lv = Lenv.find_var x env in
          (match lv.lv_type with
           | Ctype (TVoid _)->
             if ctxt.silent then raise Backtrack;
             ctxt.error (CurrentLoc.get())
               "Variable %s is bound to a predicate, not a term" x
           | _ -> old_val lv)
        with Not_found ->
        try
          let label = Lenv.string_of_current_label env in
          let info = ctxt.find_var ?label ~var:x in
          (match info.lv_origin with
           | Some lv ->
             check_current_label loc env;
             (* access to C variable need a current label *)
             lv.vreferenced <- true
           | None -> ());
          (match info.lv_type with
           | Ctype(TFun _ as t) ->
             (* function decays as a pointer *)
             TAddrOf (TVar info, TNoOffset), Ctype (TPtr (t,[]))
           | _ -> old_val info)
        with Not_found ->
        try
          let e,t = ctxt.find_enum_tag x in
          begin match e.enode with
            | Const c ->
              TConst (Logic_utils.constant_to_lconstant c), Ctype t
            | _ -> assert false
          end
        with Not_found ->
        try
          fresh_type#reset ();
          let info = ctxt.find_logic_ctor x in
          match info.ctor_params with
            [] ->
            TDataCons(info,[]),
            Ltype(info.ctor_type,
                  List.map fresh_type_var
                    info.ctor_type.lt_params)
          | _ ->
            ctxt.error loc "Data constructor %s needs arguments"
              info.ctor_name
        with Not_found ->
          (* We have a global logic variable. It may depend on
             a single state (multiple labels need to be explicitly
             instantiated and are treated as PLapp below).
             NB: for now, if we have a real function (with parameters
             other than labels) and a label,
             we end up with a Tapp with no argument, which is not
             exactly good. Either TVar should take an optional label
             for this particular case, or we should definitely move
             to partial app everywhere (since we have support for
             \lambda, this is not a very big step anyway)
          *)
          let make_expr f =
            let typ =
              match f.l_type, f.l_profile with
              | Some t, [] -> t
              | Some t, l -> Larrow (List.map (fun x -> x.lv_type) l, t)
              | None, _ ->
                if ctxt.silent then raise Backtrack;
                ctxt.error loc "%s is not a logic variable" x
            in
            let typ = fresh typ in
            match f.l_labels with
              [] ->
              TLval (TVar(f.l_var_info),TNoOffset), typ
            | [_] ->
              let curr = find_current_label loc env in
              Tapp(f,[curr],[]), typ
            | _ ->
              ctxt.error loc
                "%s labels must be explicitly instantiated" x
          in
          match ctxt.find_all_logic_functions x with

            [] -> ctxt.error loc "unbound logic variable %s" x
          | [f] -> make_expr f
          | l ->
            (try
               let f =
                 List.find (fun info -> info.l_profile = []) l
               in make_expr f
             with Not_found ->
               ctxt.error loc
                 "invalid use of overloaded function \
                  %s as constant" x)
      end
    | PLapp (f, labels, tl) ->
      let f = try (match (ctxt.find_macro f).lexpr_node with
          | PLvar (x) -> x
          | _ -> ctxt.error loc "invalid definition for macro %s" f)
        with Not_found -> f in
      let ttl = List.map (term env) tl in
      fresh_type#reset ();
      lfun_app ctxt env loc f labels ttl
    | PLunop (Ubw_not, t) ->
      let t = type_int_term ctxt env t in
      TUnOp (BNot, t), logic_arithmetic_promotion t.term_type
    | PLunop (Uminus, t) ->
      let t = type_num_term ctxt env t in
      TUnOp (Neg, t), logic_arithmetic_promotion t.term_type
    | PLunop (Ustar, t) ->
      check_current_label loc env;
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      if isLogicPointer t then begin
        let t = mk_logic_pointer_or_StartOf t in
        check_non_void_ptr loc t.term_type;
        let t = mk_mem t TNoOffset in
        t.term_node, t.term_type
      end else begin
        ctxt.error loc "invalid type %a for `unary *'"
          Cil_printer.pp_logic_type t.term_type
      end
    | PLunop (Uamp, t) ->
      check_current_label loc env;
      (* &x need a current label to have some semantics *)
      let t = term env t in
      if not (check_lval_kind lval_addressable_mode t) then
        ctxt.error t.term_loc "not an addressable left value: %a"
          Cil_printer.pp_term t;
      let t = term_lval (mkAddrOfAndMark loc) t in
      t.term_node, t.term_type
    | PLbinop (t1, (Badd | Bsub | Bmul | Bdiv | Bmod
                   | Bbw_and | Bbw_or | Bbw_xor | Blshift | Brshift as op), t2) ->
      let t1 = term env t1 in
      let ty1 = t1.term_type in
      let t2 = term env t2 in
      let ty2 = t2.term_type in
      let binop op tr =	TBinOp (op, mk_cast t1 tr, mk_cast t2 tr),
                        logic_arithmetic_promotion tr
      in
      begin match op with
        | Bmul | Bdiv
          when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
        | Bmod when is_integral_type ty1 && is_integral_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
        | Badd | Bsub
          when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
        | Bbw_and | Bbw_or | Bbw_xor
          when is_integral_type ty1 && is_integral_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
        | Bbw_xor
          when is_list_type ty1 && is_list_type ty2 ->
          fresh_type#reset ();
          lfun_app ctxt env loc "\\concat" [] [t1;t2]
        | Blshift | Brshift
          when is_integral_type ty1 && is_integral_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
        | Badd when isLogicPointer t1 && is_integral_type ty2 ->
          let t1 = mk_logic_pointer_or_StartOf t1 in
          let ty1 = t1.term_type in
          (match t1.term_node with
           | TStartOf lv ->
             TAddrOf
               (Logic_const.addTermOffsetLval
                  (TIndex (t2,TNoOffset)) lv)
           | _ ->
             TBinOp (PlusPI, t1, mk_cast t2 (integral_promotion ty2))),
          set_conversion ty1 ty2
        | Badd when is_integral_type ty1 && isLogicPointer t2 ->
          let t2 = mk_logic_pointer_or_StartOf t2 in
          let ty2 = t2.term_type in
          assert (isLogicPointerType t2.term_type);
          (match t2.term_node with
           | TStartOf lv ->
             TAddrOf (
               Logic_const.addTermOffsetLval (TIndex(t1,TNoOffset)) lv)
           | _ ->
             TBinOp (PlusPI, t2, mk_cast t1 (integral_promotion ty1))),
          set_conversion ty2 ty1
        | Bsub when isLogicPointer t1 && is_integral_type ty2 ->
          let t1 = mk_logic_pointer_or_StartOf t1 in
          TBinOp (MinusPI, t1, mk_cast t2 (integral_promotion ty2)),
          set_conversion ty1 ty2
        | Bsub when isLogicPointer t1 && isLogicPointer t2 ->
          let t1 = mk_logic_pointer_or_StartOf t1 in
          let t2 = mk_logic_pointer_or_StartOf t2 in
          TBinOp (MinusPP, t1, mk_cast t2 ty1), Linteger
        | _ ->
          ctxt.error loc
            "invalid operands to binary %a; unexpected %a and %a"
            Cil_printer.pp_binop (type_binop op)
            Cil_printer.pp_logic_type ty1
            Cil_printer.pp_logic_type ty2
      end
    | PLdot (t, f) ->
      let t = term env t in
      let f_ofs, f_type = type_of_field loc f t.term_type in
      let t = lift_set (mk_dot env loc f_ofs f_type) t in
      t.term_node, t.term_type

    | PLarrow (t, f) ->
      check_current_label loc env;
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      if not (isLogicPointer t) then
        ctxt.error loc "%a is not a pointer" Cil_printer.pp_term t;
      let t = mk_logic_pointer_or_StartOf t in
      let struct_type = type_of_pointed t.term_type in
      let f_ofs, f_type = type_of_field loc f struct_type in
      (mk_mem ~loc t f_ofs).term_node, f_type

    | PLarrget (t1, t2) ->
      let t1 = term env t1 in
      let t2 = term env t2 in
      (* access to a C value (either array or pointer) *)
      let t'1, t'2, tres =
        if isLogicPointer t1 && is_integral_type t2.term_type then
          begin
            check_current_label loc env;
            (* memory access need a current label to have some semantics *)
            let t1 = mk_logic_pointer_or_StartOf t1 in
            check_non_void_ptr t1.term_loc t1.term_type;
            (t1, t2,
             set_conversion (type_of_pointed t1.term_type) t2.term_type)
          end
        else if is_integral_type t1.term_type && isLogicPointer t2
        then begin
          check_current_label loc env;
          (* memory access need a current label to have some semantics *)
          let t2 = mk_logic_pointer_or_StartOf t2 in
          check_non_void_ptr t2.term_loc t2.term_type;
          (t2, t1,
           set_conversion (type_of_pointed t2.term_type) t1.term_type)
        end
        else if (* purely logical array access. *)
          isLogicArrayType t1.term_type && is_integral_type t2.term_type
        then
          mk_logic_access env t1, t2, type_of_array_elem t1.term_type
        else if
          isLogicArrayType t2.term_type && is_integral_type t1.term_type
        then
          mk_logic_access env t2, t1, type_of_array_elem t2.term_type
        else (* error *)
        if isLogicArrayType t1.term_type || isLogicArrayType t2.term_type
        then ctxt.error loc "subscript is not an integer range"
        else
          ctxt.error loc "subscripted value is neither array nor pointer"
      in
      let t = lift_set (mk_shift loc env t'2 tres) t'1 in
      t.term_node, t.term_type

    | PLif (t1, t2, t3) ->
      let t1 = type_bool_term ctxt env t1 in
      let t2 = term env t2 in
      let t3 = term env t3 in
      let env,ty,ty2,ty3 =
        conditional_conversion loc env None t2 t3 in
      let t2 = { t2 with term_type = instantiate env t2.term_type } in
      let _,t2 =
        implicit_conversion
          ~overloaded:false loc t2 t2.term_type ty2
      in
      let t3 = { t3 with term_type = instantiate env t3.term_type } in
      let _,t3 = implicit_conversion
          ~overloaded:false loc t3 t3.term_type ty3
      in
      Tif (t1, mk_cast t2 ty, mk_cast t3 ty), ty

    | PLold t ->
      let lab = find_old_label loc env in
      let env = Lenv.set_current_logic_label lab env in
      let t = term env t in
      (* could be Tat(t,lab) *)
      Tat (t, Logic_const.old_label), t.term_type
    | PLat (t, l) ->
      let lab = find_logic_label loc env l in
      let env = Lenv.set_current_logic_label lab env in
      let t = term env t in
      Tat (t, lab), t.term_type
    | PLbase_addr (l, t) ->
      (* base_addr need a current label to have some semantics *)
      let l = find_current_logic_label loc env l in
      let t = term_ptr t in
      let t =
        lift_set
          (fun t -> Logic_const.term (Tbase_addr (l,t))
              (Ctype Cil.charPtrType)) t
      in t.term_node, t.term_type
    | PLoffset (l, t) ->
      (* offset need a current label to have some semantics *)
      let l = find_current_logic_label loc env l in
      let t = term_ptr t in
      let t =
        lift_set (fun t -> Logic_const.term (Toffset (l,t)) Linteger) t
      in t.term_node, t.term_type
    | PLblock_length (l, t) ->
      (* block_length need a current label to have some semantics *)
      let l = find_current_logic_label loc env l in
      let t = term_ptr t in
      let t =
        lift_set
          (fun t -> Logic_const.term (Tblock_length (l,t)) Linteger)
          t
      in t.term_node, t.term_type
    | PLresult ->
      (try let t = Lenv.find_var "\\result" env in
         match t.lv_type with
           Ctype ty ->
           TLval(TResult ty,TNoOffset), t.lv_type
         | _ ->
           Kernel.fatal ~current:true "\\result associated to non-C type"
       (* \\result is the value returned by a C function.
          It has always a C type *)
       with Not_found -> ctxt.error loc "\\result meaningless")
    | PLnull -> Tnull, c_void_star
    | PLcast (ty, t) ->
      let t = term env t in
      (* no casts of tsets in grammar *)
      let ct = Logic_const.unroll_ltdef (logic_type ctxt loc env ty) in
      let { term_node; term_type } = mk_cast ~explicit:true t ct in
      (term_node, term_type)
    | PLrel (t1, (Eq | Neq | Lt | Le | Gt | Ge as op), t2) ->
      let f _ op t1 t2 =
        (TBinOp(binop_of_rel op, t1, t2),
         Ltype(ctxt.find_logic_type Utf8_logic.boolean,[]))
      in
      type_relation ctxt env f t1 op t2
    | PLtrue ->
      let ctrue = ctxt.find_logic_ctor "\\true" in
      TDataCons(ctrue,[]), Ltype(ctrue.ctor_type,[])
    | PLfalse ->
      let cfalse = ctxt.find_logic_ctor "\\false" in
      TDataCons(cfalse,[]), Ltype(cfalse.ctor_type,[])
    | PLlambda(prms,e) ->
      let (prms, env) = add_quantifiers ctxt loc ~kind:LVFormal prms env in
      let e = term env e in
      Tlambda(prms,e),Larrow(List.map (fun x -> x.lv_type) prms,e.term_type)
    | PLnot t ->
      let t = type_bool_term ctxt env t in
      TUnOp(LNot,t), Ltype (ctxt.find_logic_type Utf8_logic.boolean,[])
    | PLand (t1,t2) ->
      let t1 = type_bool_term ctxt env t1 in
      let t2 = type_bool_term ctxt env t2 in
      TBinOp(LAnd,t1,t2), Ltype (ctxt.find_logic_type Utf8_logic.boolean,[])
    | PLor (t1,t2) ->
      let t1 = type_bool_term ctxt env t1 in
      let t2 = type_bool_term ctxt env t2 in
      TBinOp(LOr,t1,t2), Ltype (ctxt.find_logic_type Utf8_logic.boolean,[])
    | PLtypeof t1 ->
      let t1 = term env t1 in
      Ttypeof t1, Ltype (ctxt.find_logic_type "typetag",[])
    | PLtype ty ->
      begin match logic_type  ctxt loc env ty with
        | Ctype ty -> Ttype ty, Ltype (ctxt.find_logic_type "typetag",[])
        | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
          ctxt.error loc "cannot take type tag of logic type"
      end
    | PLlet (ident, def, body) ->
      let tdef = term env def in
      (* At least for now, the type is supposed to be fully instantiated.
         No generalization is needed. *)
      let var = Cil_const.make_logic_info_local ident in
      let tdef = normalize_lambda_term env tdef in
      let args, tdef =
        match tdef.term_node with
        | Tlambda(args,term) -> args, term
        | _ -> [],tdef
      in
      var.l_type <- Some tdef.term_type;
      var.l_var_info.lv_type <- tdef.term_type;
      var.l_profile <- args;
      var.l_body <- LBterm tdef;
      let env = Lenv.add_logic_info ident var env in
      let tbody = term env body in
      Tlet(var,tbody), tbody.term_type
    | PLcomprehension(t,quants,pred) ->
      let quants, env = add_quantifiers ctxt loc ~kind:LVQuant quants env in
      let t = term env t in
      if is_set_type t.term_type then begin
        ctxt.error loc "sets of sets are not supported yet"
      end else begin
        let pred = Extlib.opt_map (predicate env) pred in
        Tcomprehension(t,quants,pred), (make_set_type t.term_type)
      end
    | PLempty
    | PLset [] ->
      fresh_type#reset();
      let typ = make_set_type (fresh_type_var "_") in
      Tempty_set,typ
    | PLset l ->
      fresh_type#reset();
      let typ = make_set_type (fresh_type_var "_") in
      let locs,typ = locations_set ctxt ~lift_set:false env loc l typ in
      Tunion locs, typ
    | PLunion l ->
      fresh_type#reset();
      let typ = make_set_type (fresh_type_var "_") in
      let locs,typ = locations_set ctxt ~lift_set:true env loc l typ in
      Tunion locs, typ
    | PLinter l ->
      fresh_type#reset();
      let typ = make_set_type (fresh_type_var "_") in
      let locs,typ = locations_set ctxt ~lift_set:true env loc l typ in
      Tinter locs, typ
    | PLlist l ->
      fresh_type#reset();
      let empty_list,typ_items =
        let empty_list,typ = lfun_app ctxt env loc "\\Nil" [] [] in
        empty_list,(type_of_list_elem typ)
      in
      let l,typ_items = List.fold_left
          (fun (l,typ_items) t ->
             let t' = term env t in
             let typ_items' = list_conversion t'.term_loc t' typ_items t'.term_type env in
             (t'::l), typ_items') ([],typ_items) l
      in
      let typ_items = list_promotion typ_items in
      let add_ahead (tnode, typ) e =
        let t' = { term_node = tnode; term_loc=loc;
                   term_type=typ; term_name = [] }
        in
        fresh_type#reset();
        lfun_app ctxt env loc "\\Cons" [] [(list_coercion typ_items e);t']
      in
      List.fold_left add_ahead (empty_list, (make_type_list_of typ_items)) l
    | PLrepeat (t1,t2) ->
      let t1 = term env t1 in
      let t2 = term env t2 in
      fresh_type#reset ();
      lfun_app ctxt env loc "\\repeat" [] [t1; t2]
    | PLrange (t1,t2) ->
      (* we allow range of floats/real.  *)
      let t1,ty1 = type_num_term_option ctxt env t1 in
      let t2,ty2 = type_num_term_option ctxt env t2 in
      (Trange(t1,t2),
       Ltype(ctxt.find_logic_type "set", [arithmetic_conversion ty1 ty2]))
    | PLvalid _ | PLvalid_read _ | PLvalid_function _
    | PLfresh _ | PLallocable _ | PLfreeable _
    | PLinitialized _ | PLdangling _ | PLexists _ | PLforall _
    | PLimplies _ | PLiff _
    | PLxor _ | PLseparated _ ->
      if ctxt.silent then raise Backtrack;
      ctxt.error loc "syntax error (expression expected but predicate found)"
  and type_relation:
    'a. _ -> _ -> (_ -> _ -> _ -> _ -> 'a) -> _ -> _ -> _ -> 'a =
    fun ctxt env f t1 op t2 ->
    let loc1 = t1.lexpr_loc in
    let loc2 = t2.lexpr_loc in
    let loc = loc_join t1.lexpr_loc t2.lexpr_loc in
    let t1 = ctxt.type_term ctxt env t1 in
    let ty1 = t1.term_type in
    let t2 = ctxt.type_term ctxt env t2 in
    let ty2 = t2.term_type in
    let rel = match op with
      | Eq -> "eq"
      | Neq -> "ne"
      | Le -> "le"
      | Lt -> "lt"
      | Ge -> "ge"
      | Gt -> "gt"
    in
    let conditional_conversion t1 t2 =
      let env,t,ty1,ty2 =
        conditional_conversion loc env (Some rel) t1 t2
      in
      let t1 = { t1 with term_type = instantiate env t1.term_type } in
      let _,t1 =
        implicit_conversion ~overloaded:false loc1 t1 t1.term_type ty1
      in
      let t2 = { t2 with term_type = instantiate env t2.term_type } in
      let _,t2 =
        implicit_conversion ~overloaded:false loc2 t2 t2.term_type ty2
      in
      f loc op (mk_cast t1 t) (mk_cast t2 t)
    in
    begin match op with
      | _ when plain_arithmetic_type ty1 && plain_arithmetic_type ty2 ->
        conditional_conversion t1 t2
      | Eq | Neq when isLogicPointer t1 && isLogicNull t2 ->
        let t1 = mk_logic_pointer_or_StartOf t1 in
        let t2 =
          (* in case of a set, we perform two conversions: first from
             integer to pointer, then from pointer to set of pointer. *)
          if is_set_type t1.term_type then
            mk_cast t2 (type_of_set_elem t1.term_type)
          else t2
        in
        f loc op t1 (mk_cast t2 t1.term_type)
      | Eq | Neq when isLogicPointer t2 && isLogicNull t1 ->
        let t2 = mk_logic_pointer_or_StartOf t2 in
        let t1 =
          if is_set_type t2.term_type then
            mk_cast t1 (type_of_set_elem t2.term_type)
          else t1
        in
        f loc op (mk_cast t1 t2.term_type) t2
      | Eq | Neq when isLogicArrayType ty1 && isLogicArrayType ty2 ->
        if is_same_logic_array_type ty1 ty2 then f loc op t1 t2
        else
          ctxt.error loc "comparison of incompatible types %a and %a"
            Cil_printer.pp_logic_type ty1 Cil_printer.pp_logic_type ty2
      | _ when isLogicPointer t1 && isLogicPointer t2 ->
        let t1 = mk_logic_pointer_or_StartOf t1 in
        let t2 = mk_logic_pointer_or_StartOf t2 in
        if is_same_logic_ptr_type ty1 ty2 ||
           ((op = Eq || op = Neq) &&
            (isLogicVoidPointerType t1.term_type ||
             isLogicVoidPointerType t2.term_type))
        then f loc op t1 t2
        else if (op=Eq || op = Neq) then conditional_conversion t1 t2
        else
          ctxt.error loc "comparison of incompatible types: %a and %a"
            Cil_printer.pp_logic_type t1.term_type
            Cil_printer.pp_logic_type t2.term_type
      | Eq | Neq -> conditional_conversion t1 t2
      | _ ->
        ctxt.error loc "comparison of incompatible types: %a and %a"
          Cil_printer.pp_logic_type t1.term_type
          Cil_printer.pp_logic_type t2.term_type
    end

  and term_lval f t =
    let check_lval t =
      match t.term_node with
        TLval lv
      | TLogic_coerce(_,{term_node = TLval lv })
      | Tat({term_node = TLval lv},_) -> f lv t
      | TStartOf lv
      | Tat ({term_node = TStartOf lv}, _) ->
        f lv t
      | TAddrOf lv when is_fun_ptr t.term_type ->
        f lv
          { t with
            term_type = type_of_pointed t.term_type;
            term_node = TLval lv }
      | _ -> C.error t.term_loc "not a left value: %a"
               Cil_printer.pp_term t
    in
    lift_set check_lval t

  and type_logic_app env loc f labels ttl =
    (* support for overloading *)
    let infos =
      try [Lenv.find_logic_info f env]
      with Not_found ->
        C.find_all_logic_functions f in
    match infos with
    | [] -> C.error loc "unbound function %s" f
    | [info] ->
      begin
        let labels = List.map (find_logic_label loc env) labels in
        let params = List.map (fun x -> fresh x.lv_type) info.l_profile in
        let env, tl =
          type_arguments ~overloaded:false env loc params ttl
        in
        let label_assoc = labels_assoc loc f env info.l_labels labels in
        match info.l_type with
        | Some t ->
          let t = fresh t in
          let t = instantiate env t in
          info, label_assoc, tl, Some t
        | None ->
          info, label_assoc, tl, None
      end
    | _ ->
      (* overloading *)
      let l =
        List.fold_left
          (fun acc info ->
             try
               let labels = List.map (find_logic_label loc env) labels in
               let params =
                 List.map (fun x -> fresh x.lv_type) info.l_profile
               in
               let env, tl =
                 type_arguments ~overloaded:true env loc params ttl
               in
               let tl =
                 List.combine (List.map (instantiate env) params) tl
               in
               let label_assoc = labels_assoc loc f env info.l_labels labels
               in
               match info.l_type with
               | Some t ->
                 let t = fresh t in
                 let t =
                   try instantiate env t
                   with _ -> raise Not_applicable
                 in
                 (info, label_assoc, tl, Some t)::acc
               | None ->
                 (info, label_assoc, tl, None)::acc
             with Not_applicable -> acc)
          [] infos
      in
      (* remove non-minimal calls *)
      let l = List.fold_left filter_non_minimal_arguments [] l in
      match l with
      | [] ->
        let tl = List.map (fun t -> t.term_type) ttl in
        C.error loc "no such predicate or logic function %s(%a)" f
          (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_logic_type) tl
      | [x,y,z,t] -> (x,y,(List.map (fun (t, e) -> mk_cast e t) z),t)
      | _ ->
        let tl = List.map (fun t -> t.term_type) ttl in
        C.error loc "ambiguous logic call to %s(%a)" f
          (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_logic_type) tl

  and type_int_term ctxt env t =
    let module C = struct end in
    let tt = ctxt.type_term ctxt env t in
    if not (plain_integral_type tt.term_type) then
      ctxt.error t.lexpr_loc
        "integer expected but %a found" Cil_printer.pp_logic_type tt.term_type;
    tt

  and type_bool_term ctxt env t =
    let module C = struct end in
    let tt = ctxt.type_term ctxt env t in
    if not (plain_boolean_type tt.term_type) then
      ctxt.error t.lexpr_loc "boolean expected but %a found"
        Cil_printer.pp_logic_type tt.term_type;
    mk_cast tt (Ltype (ctxt.find_logic_type Utf8_logic.boolean,[]))

  and type_num_term_option ctxt env t =
    let module C = struct end in
    match t with
      None -> None, Linteger (* Warning: should be an hybrid of integer
                                and float. *)
    | Some t -> let t = type_num_term ctxt env t in Some t, t.term_type

  and type_num_term ctxt env t =
    let module C = struct end in
    let tt = ctxt.type_term ctxt env t in
    if not (is_arithmetic_type tt.term_type) then
      ctxt.error t.lexpr_loc "integer or float expected";
    tt

  (* type_arguments checks if argument list tl is well-typed for the
     formal parameter list at *)
  and type_arguments ~overloaded env loc at tl =
    let rec type_list env = function
      | [], [] -> env, []
      | et :: etl, ({term_loc=tloc} as t) :: tl ->
        let env, _,et' = instantiate_app ~overloaded tloc t et env in
        let env, l = type_list env (etl, tl) in env, et' :: l
      | [], _ ->
        if overloaded then raise Not_applicable
        else C.error loc "too many arguments"
      | _, [] ->
        if overloaded then raise Not_applicable
        else C.error loc "partial application"
    in
    let rec conversion env = function
      | [], [] -> []
      | et::etl, ({term_loc=tloc} as t) :: tl ->
        let iet = instantiate env et in
        let _,t = implicit_conversion ~overloaded tloc t t.term_type iet in
        let t = if overloaded then t else mk_cast t iet in
        let l = conversion env (etl,tl) in
        t::l
      | _ -> assert false (* captured by first auxiliary function *)
    in
    let env, args = type_list env (at, tl) in
    (* perform conversion triggered by latter args over the former ones *)
    let res = conversion env (at,args) in
    env, res

  and boolean_to_predicate ctxt env p0 =
    boolean_term_to_predicate (ctxt.type_term ctxt env p0)

  and abstract_predicate ctxt env p0 =
    let loc = p0.lexpr_loc in
    match p0.lexpr_node with
      PLlambda (args,p) ->
      let (prms,env) = add_quantifiers ctxt loc ~kind:LVFormal args env in
      let other_prms, p = abstract_predicate ctxt env p in
      (other_prms @ prms), p
    | _ -> [], ctxt.type_predicate ctxt env p0

  let term_lval_assignable ctxt ~accept_formal env t =
    let module C = struct end in
    let t = ctxt.type_term ctxt env t in
    if not (check_lval_kind { lval_assignable_mode with accept_formal } t) then
      ctxt.error t.term_loc "not an assignable left value: %a"
        Cil_printer.pp_term t;
    lift_set (term_lval (fun _ t -> t)) t

  let term ctxt env t =
    let module C = struct end in
    match t.lexpr_node with
    | PLnamed(name,t) ->
      let t = ctxt.type_term ctxt env t in
      { t with term_name = name :: t.term_name }
    | _ ->
      let t', ty = term_node ctxt env t.lexpr_loc t.lexpr_node in
      { term_node = t'; term_loc=t.lexpr_loc; term_type=ty; term_name = [] }

  let predicate ctxt env p0 =
    let module C = struct end in
    let loc = p0.lexpr_loc in
    let predicate = ctxt.type_predicate ctxt in
    let term = ctxt.type_term ctxt in
    (* Auxiliary functions for valid, valid_read, initialized, dangling,
       separated, freeable and allocable. For the last two predicates, don't
       check that the pointer is not void. *)
    let term_ptr ~check_non_void t =
      let t = term env t in
      check_logic_pointer ~check_non_void loc t;
      t
    in
    let predicate_label_ptr ~check_non_void fpred label t =
      let t = term_ptr ~check_non_void t in
      let l = find_current_logic_label loc env label in
      (* higher-order funs do not mix well with (optional) labels,
         hence the binding below. *)
      let loc = Some loc in
      fpred ?loc (l,t)
    in
    match p0.lexpr_node with
    | PLfalse -> unamed ~loc Pfalse
    | PLtrue -> unamed ~loc Ptrue
    | PLrel (t1, (Eq | Neq | Lt | Le | Gt | Ge as op), t2) ->
      let f loc op t1 t2 = prel ~loc (type_rel op, t1, t2) in
      type_relation ctxt env f t1 op t2
    | PLand (p1, p2) -> pand ~loc (predicate env p1, predicate env p2)
    | PLor (p1, p2) -> por ~loc (predicate env p1, predicate env p2)
    | PLxor (p1, p2) -> pxor ~loc (predicate env p1, predicate env p2)
    | PLimplies (p1, p2) -> pimplies ~loc (predicate env p1, predicate env p2)
    | PLiff (p1, p2) -> piff ~loc (predicate env p1, predicate env p2)
    | PLnot p ->
      (match (predicate env p) with
       | {pred_content = Prel (Cil_types.Rneq, t, z)} when isLogicZero z ->
         prel ~loc (Cil_types.Req, t, Cil.lzero ~loc ())
       | p -> pnot ~loc p)
    | PLapp (p, labels, tl) ->
      let ttl= List.map (term env) tl in
      let info, label_assoc, tl, t = type_logic_app env loc p labels ttl in
      begin
        match t with
        | Some t ->
          (* error loc "%s is a function, not a predicate" p *)
          boolean_term_to_predicate
            { term_loc = loc; term_node = Tapp(info, label_assoc, tl);
              term_type = t ; term_name = []}
        | None -> papp ~loc (info, label_assoc, tl)
      end
    | PLif (t, p1, p2) ->
      begin try
          let t = type_bool_term { ctxt with silent = true } env t in
          pif ~loc (t, predicate env p1, predicate env p2)
        with Backtrack ->
          (* p1 ? p2 : p3 is syntactic sugar
                    for (p1 ==> p2) && (!p1 ==> p3) *)
          predicate env {lexpr_node =
                           (PLand ({lexpr_node = (PLimplies (t, p1)); lexpr_loc = loc},
                                   {lexpr_node =
                                      (PLimplies ({lexpr_node = PLnot t; lexpr_loc = loc},
                                                  p2));
                                    lexpr_loc = loc}));
                         lexpr_loc = loc}
      end
    | PLforall (q, p) ->
      let q, env' = add_quantifiers ctxt loc ~kind:LVQuant q env in
      pforall ~loc (q, predicate env' p)
    | PLexists (q, p) ->
      let q, env' = add_quantifiers ctxt loc ~kind:LVQuant q env in
      pexists ~loc (q, predicate env' p)
    | PLfresh (l12,t,n) ->
      let l1,l2=
        match l12 with
        | None ->
          (find_logic_label loc env "Old"),(find_current_label loc env )
        | Some (l1,l2) ->
          (find_logic_label loc env l1),(find_logic_label loc env l2)
      in
      let tloc = t.lexpr_loc in
      if l1 == l2 then
        ctxt.error tloc "\\fresh requires two different labels";
      let t = term_ptr ~check_non_void:false t in
      let n = term env n in
      pfresh ~loc (l1, l2, t, n)
    | PLfreeable (l, t) ->
      predicate_label_ptr ~check_non_void:false pfreeable l t
    | PLallocable (l, t) ->
      predicate_label_ptr ~check_non_void:false pallocable l t
    | PLvalid_read (l, t) ->
      predicate_label_ptr ~check_non_void:true pvalid_read l t
    | PLvalid (l,t) ->
      predicate_label_ptr ~check_non_void:true pvalid l t
    | PLvalid_function t ->
      let t = term env t in
      if isLogicPointer t then begin
        check_fun_ptr loc t.term_type;
        let t = mk_logic_pointer_or_StartOf t in
        pvalid_function ~loc t
      end else
        ctxt.error loc "subscripted value is neither array nor pointer"
    | PLinitialized (l,t) ->
      predicate_label_ptr ~check_non_void:true pinitialized l t
    | PLdangling (l,t) ->
      predicate_label_ptr ~check_non_void:true pdangling l t
    | PLold p ->
      let lab = find_old_label loc env in
      let env = Lenv.set_current_logic_label lab env in
      (* could be Tat(t,lab) *)
      pold ~loc (predicate env p)
    | PLat (p, l) ->
      let lab = find_logic_label loc env l in
      let env = Lenv.set_current_logic_label lab env in
      pat ~loc (predicate env p, lab)
    | PLvar x ->
      (try
         let def = ctxt.find_macro x
         in predicate env def
       with Not_found ->
         let make_app info =
           match info.l_type with
           | None ->
             let labels = match info.l_labels with
                 [] -> []
               | [_] -> [find_current_label loc env]
               | _ ->
                 ctxt.error loc
                   "%s labels must be explicitly instantiated" x
             in
             papp ~loc (info,labels,[])
           | Some _ -> boolean_to_predicate ctxt env p0
         in
         try make_app (Lenv.find_logic_info x env)
         with Not_found ->
           (try
              let info =
                List.find
                  (fun x -> x.l_profile = [])
                  (ctxt.find_all_logic_functions x)
              in make_app info
            with Not_found -> boolean_to_predicate ctxt env p0))
    | PLlet(x,def,body) ->
      let typ, args, tdef =
        try
          let tdef = ctxt.type_term { ctxt with silent = true} env def in
          let tdef = normalize_lambda_term env tdef in
          (match tdef.term_node with
             Tlambda(args,t) -> Some t.term_type, args, LBterm t
           | _ -> Some tdef.term_type,[], LBterm tdef)
        with Backtrack ->
          let args, tdef = abstract_predicate ctxt env def in
          None, args, LBpred tdef
      in
      let var = Cil_const.make_logic_info_local x in
      var.l_profile <- args;
      var.l_var_info.lv_type <-
        (match typ with
           None -> Ctype (Cil.voidType)
         | Some t -> t);
      var.l_type <- typ;
      var.l_body <- tdef;
      let env = Lenv.add_logic_info x var env in
      let tbody = predicate env body in
      { pred_name = []; pred_loc = loc; pred_content = Plet(var,tbody) }
    | PLcast _ | PLblock_length _ | PLbase_addr _ | PLoffset _
    | PLrepeat _ | PLlist _ | PLarrget _ | PLarrow _
    | PLdot _ | PLbinop _ | PLunop _ | PLconstant _
    | PLnull | PLresult | PLsizeof _
    | PLsizeofE _ | PLlambda _
    | PLupdate _ | PLinitIndex _ | PLinitField _
    | PLtypeof _ | PLtype _ -> boolean_to_predicate ctxt env p0
    | PLrange _ -> ctxt.error loc "cannot use operator .. within a predicate"
    | PLnamed (n, p) ->
      let p = predicate env p in { p with pred_name = n::p.pred_name }
    | PLseparated seps ->
      let seps = List.map (term_ptr ~check_non_void:true) seps in
      pseparated ~loc seps
    | PLcomprehension _ | PLset _ | PLunion _ | PLinter _ | PLempty ->
      ctxt.error loc "expecting a predicate and not tsets"

  let type_from ctxt ~accept_formal env (l,d) =
    let module C = struct end in
    (* Yannick: [assigns *\at(\result,Post)] should be allowed *)
    let tl =
      term_lval_assignable ctxt ~accept_formal env l
    in
    let tl = Logic_const.new_identified_term tl in
    match d with
      FromAny -> (tl,Cil_types.FromAny)
    | From f ->
      let tf =
        List.map (term_lval_assignable ctxt ~accept_formal:true env) f
      in
      let tf =
        List.map
          (fun td ->
             if Logic_utils.contains_result td then
               ctxt.error td.term_loc "invalid \\result in dependencies";
             Logic_const.new_identified_term td)
          tf
      in
      (tl, Cil_types.From tf)

  let type_assign ctxt ~accept_formal env a =
    let module C = struct end in
    match a with
      WritesAny -> Cil_types.WritesAny
    | Writes l ->
      let res = List.map (type_from ctxt ~accept_formal env) l in
      (* we drop assigns \result; and assigns \exit_status; without from
         clause, as this does not convey any information.
      *)
      let res =
        List.filter
          (fun (l,f) ->
             not (Logic_const.is_result l.it_content
                  || Logic_const.is_exit_status l.it_content)
             || f <> Cil_types.FromAny)
          res
      in
      Cil_types.Writes res

  let base_ctxt env =
    make_typing_context
      ~pre_state:env
      ~post_state:(fun _ -> env)
      ~assigns_env:env
      ~logic_type:logic_type
      ~type_predicate:predicate
      ~type_term:term
      ~type_assigns:type_assign

  let type_term = term
  let type_predicate = predicate

  let term env t = term (base_ctxt env) env t

  let predicate env t = predicate (base_ctxt env) env t

  let type_variant env = function
    | (t, None) -> (type_int_term (base_ctxt env) env t, None)
    | (t, r) -> (term env t, r)

  let id_predicate env pred = Logic_const.new_predicate (predicate env pred)

  let id_term_ptr env t =
    let loc = t.lexpr_loc in
    let t = term env t in
    check_logic_pointer ~check_non_void:false loc t;
    Logic_const.new_identified_term (t)

  let plain_logic_type loc env t = logic_type (base_ctxt env) loc env t

  (* For Widen_hints and Widen_variables, we check that the arguments of the
     pragma can be understood later. Keep this code synchronized with
     src/plugins/value/utils/widen.ml. *)
  let loop_pragma env p =
    let accept_int = function
        { term_node = TConst (Integer _)} -> true | _ -> false
    in
    let accept_var = function
        { term_node = TLval (TVar {lv_origin = Some _}, _)} -> true | _ -> false
    in
    (* fail when the translation of [p] does not verify the predicate [accept]*)
    let term_accept accept p =
      let t = term env p in
      if not (accept t) then
        C.error t.term_loc "invalid pragma '%a'" Cil_printer.pp_term t;
      t
    in
    match p with
    | Unroll_specs l -> Cil_types.Unroll_specs (List.map (term env) l)
    | Widen_variables l -> Cil_types.Widen_variables (List.map (term_accept accept_var) l)
    | Widen_hints l ->
      let accept t = accept_int t || accept_var t in
      Cil_types.Widen_hints (List.map (term_accept accept) l)

  let type_annot loc ti =
    let env = append_here_label (append_init_label (Lenv.empty())) in
    let this_type = plain_logic_type loc env ti.this_type in
    let v = Cil_const.make_logic_var_formal ti.this_name this_type in
    let env = Lenv.add_var ti.this_name v env in
    let body = predicate env ti.inv in
    let infos = Cil_const.make_logic_info ti.inv_name in
    infos.l_profile <- [v];
    infos.l_labels <- [Logic_const.here_label];
    infos.l_body <- LBpred body;
    add_logic_function loc infos; infos

  let model_annot loc ti =
    let env = Lenv.empty() in
    let model_for_type =
      plain_logic_type loc env ti.model_for_type |> c_type_of loc in
    if has_field ti.model_name model_for_type then
      C.error loc "Cannot add model field %s for type %a: it already exists"
        ti.model_name Cil_printer.pp_typ model_for_type
    else begin
      let model_type = plain_logic_type loc env ti.model_type in
      let infos =
        { mi_name = ti.model_name;
          mi_base_type = model_for_type;
          mi_field_type = model_type;
          mi_decl = loc;
          mi_attr = [];
        }
      in
      Logic_env.add_model_field infos; infos
    end

  let check_behavior_names loc existing_behaviors names =
    List.iter
      (fun x -> if not (List.mem x existing_behaviors) then
          C.error loc "reference to unknown behavior %s" x) names

  let check_unique_behavior_names loc old_behaviors behaviors =
    List.fold_left
      (fun names b ->
         let open Cil_types in
         if b.b_name = Cil.default_behavior_name then names
         else begin
           if (List.mem b.b_name names) then
             C.error loc "behavior %s already defined" b.b_name ;
           b.b_name::names
         end)
      old_behaviors
      behaviors

  let type_extended ~typing_context ~loc (name,ps) =
    let loc = match ps with
      | [] -> loc
      | p::_ -> p.lexpr_loc
    in
    if Extensions.is_extension name then
      let status , kind = Extensions.typer name ~typing_context ~loc ps in
      Logic_const.new_acsl_extension name loc status kind
    else
      C.error
        loc "No type-checking function registered for extension %s" name

  (* This module is used to sort the list of behaviors in [complete] and
     [disjoint] clauses, in order to remove duplicate clauses. *)
  module StringListSet =
    FCSet.Make(
    struct
      type t = string list
      let compare s1 s2 =
        Transitioning.Stdlib.(compare (List.sort compare s1) (List.sort compare s2))
    end)

  let type_spec old_behaviors loc is_stmt_contract result env s =
    let env = append_here_label (append_init_label env) in
    let env_with_result = add_result env result in
    let env_with_result_and_exit_status = add_exit_status env_with_result in
    (* assigns_env is a bit special:
       - both \result and \exit_status (in a \at(_,Post) term are admissible)
       - Old and Post labels are admissible
       - Default label is Old (Assigns are evaluated in Pre-state
     * allocates is also using assigns_env
    *)
    let assigns_env = env_with_result_and_exit_status in
    let assigns_env = append_old_and_post_labels assigns_env in
    let old = Lenv.find_logic_label "Old" assigns_env in
    let assigns_env =
      Lenv.set_current_logic_label old assigns_env in
    let assigns_env =
      Lenv.exit_post_state (Lenv.enter_post_state assigns_env Exits)
    in
    let post_state_env k =
      let env = match k with
        | Returns -> env_with_result
        | Normal -> if is_stmt_contract then env else env_with_result
        | Exits -> add_exit_status env
        | Breaks | Continues -> env
      in
      Lenv.enter_post_state (append_old_and_post_labels env) k
    in
    let rec multiple_post_clauses_state_env l =
      match l with
      | [] -> env
      | [x] -> post_state_env x (* Usual case*)

      (* The two cases below are used in the ACSL importer plugin *)
      | (Returns|Normal)::r ->
        add_result (multiple_post_clauses_state_env r) result
      | (Exits|Breaks|Continues)::r ->
        Lenv.enter_post_state (multiple_post_clauses_state_env r) Exits
    in
    let spec_behavior =
      let spec_behavior = s.spec_behavior
      in if spec_behavior = [] then
        (* at least allocates \nothing *)
        [Cabshelper.mk_behavior ()]
      else spec_behavior
    in
    let b = List.map
        (fun {b_assigns= ba; b_name; b_post_cond=be; b_assumes= bas;
              b_allocation=bfa; b_requires=br; b_extended=bext} ->
          let typing_context = make_typing_context
              ~pre_state:env
              ~post_state:multiple_post_clauses_state_env
              ~assigns_env:assigns_env
              ~logic_type
              ~type_predicate
              ~type_term
              ~type_assigns:type_assign
          in
          let b_assumes = List.map (id_predicate env) bas in
          let b_requires= List.map (id_predicate env) br in
          let b_post_cond =
            List.map
              (fun (k,p)->
                 let p' = id_predicate (post_state_env k) p in (k,p')) be
          in
          let b_assigns =
            type_assign typing_context ~accept_formal:is_stmt_contract assigns_env ba
          in
          let b_allocation=
            match bfa with
            | FreeAllocAny -> Cil_types.FreeAllocAny
            | FreeAlloc(f,a) ->
              Cil_types.FreeAlloc((List.map (id_term_ptr env) f),
                                  List.map (id_term_ptr (post_state_env Normal)) a)
          in
          let b_extended = List.map (type_extended ~typing_context ~loc) bext in
          { Cil_types.b_name; b_assumes; b_requires; b_post_cond;
            b_assigns; b_allocation; b_extended }
        )
        spec_behavior
    in
    let none_for_stmt_contract clause = function
      | None -> None
      | (Some _) as x ->
        if is_stmt_contract then
          C.error
            loc "%s clause isn't allowed into statement contract" clause;
        x
    in
    let v = Extlib.opt_map (type_variant env)
        (none_for_stmt_contract "decreases" s.spec_variant) in
    let t = Extlib.opt_map (id_predicate env)
        (none_for_stmt_contract "terminates" s.spec_terminates) in
    let my_names = check_unique_behavior_names loc [] b in
    let bnames = old_behaviors @ my_names in
    let expand_my_names = function
      | [] ->
        if my_names = [] then
          C.error loc
            "complete or disjoint behaviors clause in a contract with empty \
             list of behavior"
        else my_names
      | l -> l
    in
    let complete = List.map expand_my_names s.spec_complete_behaviors in
    let disjoint = List.map expand_my_names s.spec_disjoint_behaviors in
    List.iter (check_behavior_names loc bnames) complete;
    List.iter (check_behavior_names loc bnames) disjoint;
    let cleanup_duplicate l =
      StringListSet.(elements (List.fold_left (fun acc e -> add e acc) empty l))
    in
    let complete = cleanup_duplicate complete in
    let disjoint = cleanup_duplicate disjoint in
    { Cil_types.spec_behavior = b;
      spec_variant = v;
      spec_terminates = t;
      spec_complete_behaviors = complete;
      spec_disjoint_behaviors = disjoint;
    }

  let funspec old_behaviors vi formals typ s =
    let env = append_pre_label (Lenv.funspec()) in
    let log_return_typ = Ctype (Cil.getReturnType typ) in
    let env =
      match formals with
      | None -> (* This is the spec of a function declaration *)
        let add_formal env v =
          Lenv.add_var v.vname (Cil.cvar_to_lvar v) env
        in
        begin try
            List.fold_left add_formal env (Cil.getFormalsDecl vi)
          with Not_found -> env (*declaration with an empty list of argument*)
        end
      | Some formals ->
        let add_formal env v =
          Lenv.add_var v.vname (Cil.cvar_to_lvar v) env in
        List.fold_left add_formal env formals
    in type_spec old_behaviors vi.vdecl false log_return_typ env s

  let slice_pragma env = function
      SPexpr t -> Cil_types.SPexpr (term env t)
    | SPctrl -> Cil_types.SPctrl
    | SPstmt -> Cil_types.SPstmt

  let impact_pragma env = function
      IPexpr t -> Cil_types.IPexpr (term env t)
    | IPstmt -> Cil_types.IPstmt

  let code_annot_env () =
    let env = append_here_label (append_pre_label (append_init_label
                                                     (Lenv.empty()))) in
    if C.is_loop () then append_loop_labels env else env

  let loop_annot_env () =
    append_loop_labels (append_here_label (append_pre_label (append_init_label
                                                               (Lenv.empty()))))

  let assertion_kind =
    function Assert -> Cil_types.Assert | Check -> Cil_types.Check

  let code_annot loc current_behaviors current_return_type ca =
    let source = fst loc in
    let annot = match ca with
      | AAssert (behav,k,p) ->
        check_behavior_names loc current_behaviors behav;
        Cil_types.AAssert(behav,assertion_kind k,predicate (code_annot_env()) p)
      | APragma (Impact_pragma sp) ->
        Cil_types.APragma
          (Cil_types.Impact_pragma (impact_pragma (code_annot_env()) sp))
      | APragma (Slice_pragma sp) ->
        Cil_types.APragma
          (Cil_types.Slice_pragma (slice_pragma (code_annot_env()) sp))
      | APragma (Loop_pragma lp) ->
        Cil_types.APragma
          (Cil_types.Loop_pragma (loop_pragma (code_annot_env()) lp))
      | AStmtSpec (behav,s) ->
        (* function behaviors and statement behaviors are not at the
           same level. Do not mix them in a complete or disjoint clause
           here.
        *)
        check_behavior_names loc current_behaviors behav;
        let env = append_pre_label (Lenv.empty()) in
        let my_spec =
          type_spec [] loc true current_return_type env s
        in
        ignore
          (check_unique_behavior_names
             loc current_behaviors my_spec.Cil_types.spec_behavior);
        Cil_types.AStmtSpec (behav,my_spec)
      | AVariant v ->
        Cil_types.AVariant (type_variant (loop_annot_env ()) v)
      | AInvariant (behav,f,i) ->
        let env = if f then loop_annot_env () else code_annot_env () in
        check_behavior_names loc current_behaviors behav;
        Cil_types.AInvariant (behav,f,predicate env i)
      | AAllocation (behav,fa) ->
        check_behavior_names loc current_behaviors behav;
        Cil_types.AAllocation(behav,
                              (match fa with
                               | FreeAllocAny -> Cil_types.FreeAllocAny
                               | FreeAlloc(f,a) ->
                                 Cil_types.FreeAlloc((List.map (id_term_ptr (loop_annot_env())) f),
                                                     List.map (id_term_ptr (loop_annot_env())) a)));
      | AAssigns (behav,a) ->
        let env = loop_annot_env () in
        let ctxt = base_ctxt env in
        Cil_types.AAssigns(behav, type_assign ctxt ~accept_formal:true env a)
      | AExtended (behav, is_loop, (name, _ as ext)) ->
        let kind = Logic_env.extension_category name in
        let pre_state, post_state =
          match kind,is_loop with
          | Some (Ext_code_annot Ext_here), false ->
            code_annot_env (), fun _ -> Lenv.empty ()
          | Some (Ext_code_annot (Ext_next_stmt | Ext_next_both)), false ->
            let env = append_old_and_post_labels (code_annot_env()) in
            (env, function [Normal] -> env | _ -> Lenv.empty ())
          | Some (Ext_code_annot (Ext_next_loop | Ext_next_both)), true ->
            loop_annot_env(), fun _ -> Lenv.empty ()
          | Some (Ext_code_annot Ext_next_loop), false ->
            Kernel.(
              warning
                ~source ~wkey:wkey_acsl_extension
                "%s is a loop annotation extension, \
                 but used here as code annotation" name);
            code_annot_env (), fun _ -> Lenv.empty()
          | Some (Ext_code_annot (Ext_here | Ext_next_stmt)), true ->
            Kernel.(
              warning
                ~source ~wkey:wkey_acsl_extension
                "%s is a code annotation extension, \
                 but used here as loop annotation" name);
            code_annot_env (), fun _ -> Lenv.empty()
          | (Some (Ext_global | Ext_contract) | None),_ ->
            Kernel.(
              warning
                ~source ~wkey:wkey_acsl_extension
                "%s is not a code annotation extension" name);
            code_annot_env (), fun _ -> Lenv.empty()
        in
        let assigns_env = Lenv.empty() in
        let type_assigns = type_assign in
        let typing_context =
          make_typing_context
            ~pre_state ~post_state ~assigns_env
            ~logic_type ~type_predicate ~type_term ~type_assigns
        in
        Cil_types.AExtended
          (behav, is_loop, type_extended ~typing_context ~loc ext)
    in Logic_const.new_code_annotation annot

  let formals loc env p =
    let add_var (p,env) (t,x) =
      let lt = plain_logic_type loc env t in
      let var = Cil_const.make_logic_var_formal x lt in
      (var::p, Lenv.add_var x var env)
    in
    let (p,env) = List.fold_left add_var ([],env) p in
    List.rev p, env

  let init_type_variables loc l =
    List.fold_left
      (fun env x ->
         try
           ignore (Lenv.find_type_var x env);
           C.error loc "duplicated type variable in annotation"
         with Not_found -> Lenv.add_type_var x (Lvar x) env)
      (Lenv.empty()) l

  let rec is_cyclic_typedef s = function
    | None -> false
    | Some (LTsum _) -> false
    | Some (LTsyn typ) -> is_cyclic_typedef_aux s typ
  and is_cyclic_typedef_aux s = function
    | Ltype ({ lt_name = s'; lt_def = d },_) ->
      s = s' || is_cyclic_typedef s d
    | Larrow  (prm,rt) ->
      List.exists (is_cyclic_typedef_aux s) prm ||
      is_cyclic_typedef_aux s rt
    | _ -> false

  (* checks whether all the type variable contained in the return type t of
     a logic function are bound in a parameter's type
     (p being the list of formals). type-checking error otherwise
  *)
  let check_polymorphism loc ?return_type p =
    let obj known_vars =
      let update_known_vars s =
        known_vars:= Datatype.String.Set.add s !known_vars
      in object inherit Cil.nopCilVisitor
        method! vlogic_type = function
            Lvar s -> update_known_vars s; Cil.DoChildren
          | _ -> Cil.DoChildren
      end
    in let rt_vars = ref Datatype.String.Set.empty
    in let prm_vars = ref Datatype.String.Set.empty
    in
    ignore(Extlib.opt_map (Cil.visitCilLogicType (obj rt_vars)) return_type);
    List.iter
      (fun v -> ignore (Cil.visitCilLogicType (obj prm_vars) v.lv_type)) p;
    if not (Datatype.String.Set.subset !rt_vars !prm_vars) then
      C.error loc "some type variable appears only in the return type. \
                   All type variables need to occur also in the parameters types."

  let annot_env loc labels poly =
    let env = init_type_variables loc poly in
    let labels,env =
      List.fold_right
        (fun l (labs,e) ->
           try
             let _ = Lenv.find_logic_label l e in
             C.error loc "multiple defined label `%s'" l
           with Not_found ->
             let lab = FormalLabel l in
             (lab::labs,Lenv.add_logic_label l lab e))
        labels ([],env)
    in
    let env =
      match labels with
      | [lab] ->
        (* if there is exactly one label, it is the default label *)
        Lenv.set_current_logic_label lab env
      | _ -> env
    in
    labels,env

  let logic_decl loc f labels poly ?return_type p =
    let labels,env = annot_env loc labels poly in
    let t = match return_type with
      | None -> None;
      | Some t -> Some (plain_logic_type loc env t)
    in
    let p, env = formals loc env p in
    check_polymorphism loc ?return_type:t p;
    let info = Cil_const.make_logic_info f in
    (* Should we add implicitly a default label for the declaration? *)
    let labels = match !Lenv.default_label with
        None -> labels
      | Some lab -> [lab]
    in
    (* Quick fix for bug 428, but this is far from perfect
       - Predicates still have a varinfo with Ctype Void
       - Polymorphism is not reflected on the lvar level.
       - However, such lvar should rarely if at all be seen under a Tvar.
    *)
    (match p,t with
       _,None -> ()
     | [], Some t ->
       info.l_var_info.lv_type <- t
     | _,Some t ->
       let typ = Larrow (List.map (fun x -> x.lv_type) p,t) in
       info.l_var_info.lv_type <- typ);
    info.l_tparams <- poly;
    info.l_profile <- p;
    info.l_type <- t;
    info.l_labels <- labels;
    add_logic_function loc info;
    env,info

  let type_datacons loc env type_info (name,params) =
    (try
       let info = C.find_logic_ctor name in
       C.error loc "type constructor %s is already used by type %s"
         name info.ctor_type.lt_name
     with Not_found -> ());
    let tparams = List.map (plain_logic_type loc env) params in
    let my_info =
      { ctor_name = name;
        ctor_type = type_info;
        ctor_params = tparams
      }
    in
    C.add_logic_ctor name my_info;
    my_info

  let typedef loc env my_info def =
    match def with
    | TDsum cons -> LTsum (List.map (type_datacons loc env my_info) cons)
    | TDsyn typ -> LTsyn (plain_logic_type loc env typ)

  let rec annot in_axiomatic a =
    let loc = a.decl_loc in
    Cil.CurrentLoc.set loc;
    match a.decl_node with
    | LDlogic_reads (f, labels, poly, t, p, l) ->
      let env,info = logic_decl loc f labels poly ~return_type:t p in
      info.l_body <-
        (match l with
         | Some l ->
           let l =
             List.map (fun x ->
                 new_identified_term
                   (update_term_wrt_default_label (term env x))) l
           in
           LBreads l
         | None -> LBnone);
      (* potential creation of label w.r.t. reads clause *)
      update_info_wrt_default_label info;
      Dfun_or_pred (info,loc)
    | LDpredicate_reads (f, labels, poly, p, l) ->
      let env,info = logic_decl loc f labels poly p in
      info.l_body <-
        (match l with
         | Some l ->
           let l =
             List.map (fun x ->
                 new_identified_term
                   (update_term_wrt_default_label (term env x))) l
           in
           LBreads l
         | None -> LBnone);
      (* potential creation of label w.r.t. reads clause *)
      update_info_wrt_default_label info;
      Dfun_or_pred (info,loc)
    | LDlogic_def(f, labels, poly,t,p,e) ->
      let env,info = logic_decl loc f labels poly ~return_type:t p in
      let rt = match info.l_type with
        | None -> assert false
        | Some t -> t
      in
      let e = term env e in
      let _,new_typ,new_term =
        instantiate_app ~overloaded:false loc e rt env in
      if is_same_type new_typ rt then begin
        info.l_body <- LBterm (update_term_wrt_default_label new_term);
        (* potential creation of label w.r.t. def *)
        update_info_wrt_default_label info;
        Dfun_or_pred (info,loc)
      end else
        C.error loc
          "return type of logic function %s is %a but %a was expected"
          f
          Cil_printer.pp_logic_type new_typ
          Cil_printer.pp_logic_type rt
    | LDpredicate_def (f, labels, poly, p, e) ->
      let env,info = logic_decl loc f labels poly p in
      let e = update_predicate_wrt_default_label (predicate env e) in
      info.l_body <- LBpred e;
      (* potential creation of label w.r.t. def *)
      update_info_wrt_default_label info;
      Dfun_or_pred (info,loc)
    | LDinductive_def (f, input_labels, poly, p, indcases) ->
      let _env,info = logic_decl loc f input_labels poly p in
      (* env is ignored: because params names are indeed useless...*)
      let (global_default, l) =
        List.fold_left
          (fun (global_default, acc) (id,labels,poly,e) ->
             Lenv.default_label := None;
             let labels,env = annot_env loc labels poly in
             let p = predicate env e in
             let res = update_ind_case_wrt_default_label (id, labels, poly, p)
             in
             let global_default =
               match global_default, !Lenv.default_label with
               | _, None -> global_default
               | Some _, _ -> global_default
               | None, (Some _ as lab) -> lab
             in
             (global_default, res::acc))
          (!Lenv.default_label,[]) indcases
      in
      Lenv.default_label := global_default;
      (* After all inductive cases has been processed, we might have added
         an implicit label for the predicate at some point.
         Update the inductive cases that need it (i.e. do not define
         their own label(s)).
      *)
      let l =
        List.rev_map update_ind_case_wrt_default_label l
      in
      info.l_body <- LBinductive l;
      update_info_wrt_default_label info;
      Dfun_or_pred (info,loc)
    | LDaxiomatic(id,decls) ->
      if in_axiomatic then
        (* Not supported yet. See issue 43 on ACSL's github repository. *)
        C.error loc "Nested axiomatic. Ignoring body of %s" id
      else
        let l = List.map (annot true) decls in
        Daxiomatic(id,l,[],loc)
    | LDtype(s,l,def) ->
      let env = init_type_variables loc l in
      let my_info =
        { lt_name = s;
          lt_params = l;
          lt_def = None; (* will be updated later *)
          lt_attr = [];
        }
      in
      add_logic_type loc my_info;
      let tdef = Extlib.opt_map (typedef loc env my_info) def in
      if is_cyclic_typedef s tdef then
        C.error loc "Definition of %s is cyclic" s;
      my_info.lt_def <- tdef;
      Dtype (my_info,loc)
    | LDlemma (x,is_axiom, labels, poly, e) ->
      if Logic_env.Lemmas.mem x then begin
        let old_def = Logic_env.Lemmas.find x in
        let old_loc = Cil_datatype.Global_annotation.loc old_def in
        let is_axiom =
          match old_def with
          | Dlemma(_, is_axiom, _, _, _, _, _) -> is_axiom
          | _ ->
            Kernel.fatal ~current:true
              "Logic_env.get_lemma must return Dlemma"
        in
        C.error loc "%s is already registered as %s (%a)"
          x (if is_axiom then "axiom" else "lemma")
          Cil_datatype.Location.pretty old_loc
      end;
      let labels,env = annot_env loc labels poly in
      let p = predicate env e in
      let labels = match !Lenv.default_label with
        | None -> labels
        | Some lab -> [lab]
      in
      let def = Dlemma (x,is_axiom, labels, poly,  p, [], loc) in
      Logic_env.Lemmas.add x def;
      def
    | LDinvariant (s, e) ->
      let env = append_here_label (append_init_label (Lenv.empty())) in
      let p = predicate env e in
      let li = Cil_const.make_logic_info s in
      li.l_labels <- [Logic_const.here_label];
      li.l_body <- LBpred p;
      add_logic_function loc li;
      Dinvariant (li,loc)
    | LDtype_annot l ->
      Dtype_annot (type_annot loc l,loc)
    | LDmodel_annot l ->
      Dmodel_annot (model_annot loc l,loc);
    | LDvolatile (tsets, (rd_opt, wr_opt)) ->
      let env = Lenv.empty () in
      let ctxt = base_ctxt env in
      let tsets =
        List.map
          (term_lval_assignable ctxt ~accept_formal:false env) tsets
      in
      let checks_tsets_type fct ctyp =
        List.iter
          (fun t ->
             let check t = match Logic_utils.unroll_type t with
               | Ctype ctyp' -> Cil_datatype.Typ.equal ctyp ctyp'
               | _ -> false
             in
             if not (Logic_const.plain_or_set check t.term_type) then
               C.error t.term_loc "incompatible prototype of '%s' with %a"
                 fct Cil_printer.pp_term t )
          tsets
      in
      let prototype_error s fct =
        C.error loc
          "incompatible prototype of '%s' with volatile %s declaration"
          fct s
      in
      let volatile_type ret_typ arg1 error =
        (* note: type pointed to by arg1 may differ from the
           return type with respect to qualifiers *)
        if not (isPointerType arg1) then error ();
        let vol_typ = typeOf_pointed arg1 in
        if not (Cil.isVolatileType vol_typ
                && Cil_datatype.Typ.equal ret_typ
                  (Cil.type_remove_qualifier_attributes vol_typ))
        then error ();
        vol_typ
      in
      let checks_reads_fct fct ty =
        let error () = prototype_error "reads" fct
        in let ret,args,is_varg_arg,_attrib =
             if not (Cil.isFunctionType ty) then
               error ();
             Cil.splitFunctionType ty
        in match args with
        | Some [_,arg1,_] when
            (not (isVoidType ret || is_varg_arg))
          -> (* matching prototype: T fct (volatile T *arg1) *)
          let vol_typ = volatile_type ret arg1 error in
          if Cil.isConstType vol_typ then
            Kernel.warning ~current:true "Access function '%s' writes to volatile const locations" fct;
          checks_tsets_type fct vol_typ (* tsets should have type: volatile T *)
        | _ ->
          error ()
      in
      let checks_writes_fct fct ty =
        let error () = prototype_error "writes" fct
        in let ret,args,is_varg_arg,_attrib =
             if not (Cil.isFunctionType ty) then
               error ();
             Cil.splitFunctionType ty
        in match args with
        | Some ((_,arg1,_)::[_,arg2,_]) when
            (not (isVoidType ret || is_varg_arg))
            && Cil_datatype.Typ.equal ret (Cil.type_remove_qualifier_attributes arg2)
          -> (* matching prototype: T fct (volatile T *arg1, T arg2) *)
          let vol_typ = volatile_type ret arg1 error in
          checks_tsets_type fct vol_typ (* tsets should have type: volatile T *)
        | _ ->
          error ()
      in
      let get_volatile_fct checks_type = function
        | None -> None
        | Some fct ->
          try (match (C.find_var fct).lv_origin
               with
               | None -> raise Not_found
               | Some vi as vi_opt-> checks_type fct vi.vtype ; vi_opt)
          with Not_found ->
            C.error loc "cannot find function '%s' for volatile clause" fct
      in
      let tsets = List.map (Logic_const.new_identified_term) tsets in
      let rvi_opt = get_volatile_fct checks_reads_fct rd_opt in
      let wvi_opt = get_volatile_fct checks_writes_fct wr_opt in
      Dvolatile (tsets, rvi_opt, wvi_opt, [], loc)
    | LDextended (kind, content) ->
      let typing_context = base_ctxt (Lenv.empty ()) in
      let status,tcontent = Extensions.typer kind ~typing_context ~loc content in
      let textended = Logic_const.new_acsl_extension kind loc status tcontent in
      Dextended (textended, [], loc)

  let annot a =
    start_transaction ();
    let res = annot false a in
    finish_transaction (); res

  let annot = C.on_error annot rollback_transaction

  let custom _c = CustomDummy

end

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
