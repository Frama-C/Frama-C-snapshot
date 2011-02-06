(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cilutil
open Cil_types
open Logic_ptree
open Logic_const
open Logic_utils
open Format
open Cil

let dloc = Lexing.dummy_pos,Lexing.dummy_pos
let error (b,e) fstring =
  CurrentLoc.set (b,e) ;
  Cil.abort_loc
    (b.Lexing.pos_fname,b.Lexing.pos_lnum)
    (fstring ^^ " in annotation.")

let loc_join (b,_) (_,e) = (b,e)

let unescape s =
  let b = Buffer.create (String.length s) in
    Logic_lexer.chr b (Lexing.from_string s)

let wcharlist_of_string s =
  let res = ref [] in
  let i = ref 0 in
  let rec treat_escape_octal n nb_pass =
    if nb_pass > 2 then res:= n::!res
    else if !i >= String.length s then res:= n::!res
    else match s.[!i] with
        x when '0' <= x && x <= '9' ->
          incr i;
          treat_escape_octal
            (Int64.add (Int64.mul (Int64.of_int 8) n)
               (Int64.of_int (Char.code x - Char.code '0'))) (nb_pass + 1)
      | _ -> res:= n::!res
  in
  let rec treat_escape_hexa n =
    if !i >= String.length s then res:= n::!res
    else match s.[!i] with
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
      Cil.warning "Ill-formed escape sequence in wide string"
    else begin
      match s.[!i] with
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
        | c -> incr i; Cil.warning "Ill-formed escape sequence in wide string";
            res:= Int64.of_int (Char.code c) :: !res
    end
  in
  while (!i < String.length s) do
    match s.[!i] with
      | '\\' -> incr i; treat_escape_sequence ()
      | c -> res := Int64.of_int (Char.code c)::!res; incr i
  done;
  List.rev (!res)

let lift_set f loc =
let rec aux loc =
  match loc.term_node with
      Tcomprehension(t,q,p) -> { loc with term_node = Tcomprehension(aux t,q,p)}
    | Tunion l -> {loc with term_node = Tunion(List.map aux l)}
    | Tinter l -> {loc with term_node = Tinter(List.map aux l)}
    | Tempty_set -> loc
    | _ -> f loc
in aux loc

let is_same_type t1 t2 =
  Logic_utils.is_same_type
    (Logic_utils.unroll_type t1) (Logic_utils.unroll_type t2)

(* Logical environments *)

module Lenv = struct
(* locals: logic variables (e.g. quantified variables in \forall, \exists) *)

module Smap = Map.Make(String)

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

let fresh_var env name typ =
  let name =
    let exists name =
      Smap.mem name env.local_vars ||
        Smap.mem name env.local_logic_info ||
        (Logic_env.find_all_logic_functions name <> [])
    in
    let rec aux i =
      let name' = name ^ "_" ^ (string_of_int i) in
      if exists name' then aux (i+1) else name'
    in if exists name then aux 0 else name
  in Cil_const.make_logic_var name typ

let no_label env = Smap.is_empty env.logic_labels

let enter_post_state env kind =
  let real_kind =
    match kind, env.enclosing_post_state with
      | _, None -> kind
      | Normal, Some kind -> kind
      | _, Some _ -> Cil.fatal "Inconsistent logic labels env stack"
  in
  { env with
      is_post_state = Some real_kind; enclosing_post_state = Some real_kind
  }

let exit_post_state env = { env with is_post_state = None }

let current_post_state env = env.is_post_state

let is_post_state env =
  match env.is_post_state with None -> false | Some _ -> true

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
      LogicLabel (_,"Post") -> enter_post_state env Normal
    | LogicLabel (_,("Pre" | "Old")) | StmtLabel _ -> exit_post_state env
    | LogicLabel (_,"Here") -> env
    | LogicLabel _ -> exit_post_state env

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


let append_here_label env =
  let env = Lenv.add_logic_label "Here" Logic_const.here_label env in
  Lenv.set_current_logic_label Logic_const.here_label env

let append_pre_label ~pre_is_old env =
  let l =
    if pre_is_old then Logic_const.old_label
    else Logic_const.pre_label in
  Lenv.add_logic_label "Pre" l env

let append_old_and_post_labels env =
  Lenv.add_logic_label "Post" Logic_const.post_label
    (Lenv.add_logic_label "Old" Logic_const.old_label env)

let append_post_label env =
  Lenv.add_logic_label "Post" Logic_const.post_label env

let add_var var info env = Lenv.add_var var info env

let add_result env typ =
  if Logic_utils.isLogicVoidType typ then env
  else
    let v = Cil_const.make_logic_var "\\result" typ in
    Lenv.add_var "\\result" v env

let enter_post_state env kind = Lenv.enter_post_state env kind

let post_state_env kind typ =
  let env = Lenv.funspec () in
  let env = append_here_label env in
  let env = append_old_and_post_labels env in
  (* NB: this allows to have \result and Exits as termination kind *)
  let env = add_result env typ in
  let env = enter_post_state env kind in
  env

type typing_context = {
  annonCompFieldName : string;
  conditionalConversion : typ -> typ -> typ;
  find_macro : string -> lexpr;
  find_var : string -> logic_var;
  find_enum_tag : string -> exp * typ;
  find_comp_type : kind:string -> string -> typ;
  find_comp_field: compinfo -> string -> offset;
  find_type : string -> typ;
  find_label : string -> stmt ref;
  remove_logic_function : string -> unit;
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
  type_predicate:Lenv.t -> Logic_ptree.lexpr -> predicate named;
  type_term:Lenv.t -> Logic_ptree.lexpr -> term;
  type_assigns:
    accept_formal:bool ->
    Lenv.t ->
    Logic_ptree.lexpr Cil_types.assigns -> identified_term Cil_types.assigns;
  error: 'a. location -> ('a,formatter,unit) format -> 'a
}

module Extensions = struct
  let typer_tbl = Hashtbl.create 5
  let find_typer name= Hashtbl.find typer_tbl name
  let register name typer =
    Hashtbl.add typer_tbl name typer
  let typer name ~typing_context:typing_context ~loc bhv p =
    try let typ = find_typer name in
    typ ~typing_context ~loc bhv p
    with Not_found -> error loc "unsupported clause of name '%s'" name

end
let register_behavior_extension = Extensions.register

module Make
  (C :
    sig
      val annonCompFieldName : string
      val conditionalConversion : typ -> typ -> typ
      val find_macro : string -> lexpr
      val find_var : string -> logic_var
      val find_enum_tag : string -> exp * typ
      val find_comp_type : kind:string -> string -> typ
      val find_comp_field: compinfo -> string -> offset
      val find_type : string -> typ
      val find_label : string -> stmt ref
      val remove_logic_function : string -> unit
      val remove_logic_type: string -> unit
      val remove_logic_ctor: string -> unit
      val add_logic_function: logic_info -> unit
      val add_logic_type: string -> logic_type_info -> unit
      val add_logic_ctor: string -> logic_ctor_info -> unit
      val find_all_logic_functions: string -> logic_info list
      val find_logic_type: string -> logic_type_info
      val find_logic_ctor: string -> logic_ctor_info
    end) =
struct

  let make_typing_context ~pre_state ~post_state ~assigns_env
      ~type_predicate ~type_term ~type_assigns = {
    pre_state=pre_state;
    post_state=post_state;
    assigns_env=assigns_env;
    type_predicate= type_predicate;
    type_term= type_term;
    type_assigns = type_assigns;
    annonCompFieldName = C.annonCompFieldName;
    conditionalConversion = C.conditionalConversion;
    find_macro = C.find_macro;
    find_var = C.find_var;
    find_enum_tag = C.find_enum_tag;
    find_comp_type = C.find_comp_type;
    find_comp_field = C.find_comp_field;
    find_type = C.find_type ;
    find_label = C.find_label;
    remove_logic_function = C.remove_logic_function;
    remove_logic_type = C.remove_logic_type;
    remove_logic_ctor = C.remove_logic_ctor;
    add_logic_function = C.add_logic_function;
    add_logic_type = C.add_logic_type;
    add_logic_ctor = C.add_logic_ctor;
    find_all_logic_functions = C.find_all_logic_functions;
    find_logic_type = C.find_logic_type;
    find_logic_ctor = C.find_logic_ctor;
    error = error;
  }

  let is_logic_ctor c =
    try ignore (C.find_logic_ctor c);true with Not_found -> false
  let is_logic_function c =
    match C.find_all_logic_functions c with _::_ -> true | [] -> false

  let prefix p s =
    let lp = String.length p in
    let ls = String.length s in
    lp <= ls && String.sub s 0 lp = p

  let plain_type_of_field loc f = function
    | Ctype ty ->
        begin match Cil.unrollType ty with
          | TComp (comp, _, attrs) ->
              (try
                 let attrs = Cil.filter_qualifier_attributes attrs in
                 let field = C.find_comp_field comp f in
                 let typ = Cil.typeOffset ty field in
                 Logic_utils.offset_to_term_offset ~cast:false field,
                 Ctype (Cil.typeAddAttributes attrs typ)
               with Not_found -> error loc "cannot find field %s" f)
          | _ ->
	      error loc "expected a struct with field %s" f
       end
    | _ ->
        error loc "expected a struct with field %s" f

  let type_of_field loc f = function
    | Ltype ({lt_name = "set"} as lt,[t]) ->
        let offs,typ = plain_type_of_field loc f t in offs, Ltype(lt,[typ])
    | t -> plain_type_of_field loc f t

  let c_int = Ctype (TInt (IInt, []))
  let c_float = Ctype (TFloat (FDouble, []))
  let c_void_star = Ctype (TPtr (TVoid [], []))

  let instantiate env ty =
    let obj = object
      inherit Cil.nopCilVisitor
      method vlogic_type t =
        match t with
            Lvar s ->
              (try Cil.ChangeDoChildrenPost(Lenv.find_type_var s env, fun x -> x)
               with Not_found -> assert false
                 (* All type variables are supposed to be bound somewhere. *)
              )
          | _ -> Cil.DoChildren
    end
    in Cil.visitCilLogicType obj ty

  let rec logic_type loc env = function
    | LTvoid -> Ctype (TVoid [])
    | LTint ikind -> Ctype (TInt (ikind, []))
    | LTfloat fkind -> Ctype (TFloat (fkind, []))
    | LTarray (ty,length) ->
        let size =
          match length with
              Some (IntConstant s) -> Some (parseInt ~loc s)
            | Some (FloatConstant _ | StringConstant _ | WStringConstant _) ->
                error loc "size of array must be an integral value"
            | None -> None
        in
        Ctype (TArray (c_logic_type loc env ty, size,
                       Cil.empty_size_cache (),[]))
    | LTpointer ty -> Ctype (TPtr (c_logic_type loc env ty, []))
    | LTenum e ->
        (try Ctype (C.find_comp_type "enum" e)
         with Not_found -> error loc "no such enum %s" e)
    | LTstruct s ->
        (try Ctype (C.find_comp_type "struct" s)
         with Not_found -> error loc "no such struct %s" s)
    | LTunion u ->
        (try Ctype (C.find_comp_type "union" u)
         with Not_found -> error loc "no such union %s" u)
    | LTarrow (prms,rt) ->
        (* For now, our only function types are C function pointers. *)
        let prms = List.map (fun x -> "", c_logic_type loc env x, []) prms in
        let rt = c_logic_type loc env rt in
        (match prms with
             [] -> Ctype (TFun(rt,None,false,[]))
           | _ -> Ctype (TFun(rt,Some prms,false,[])))
    | LTnamed (id,[]) ->
        (try Lenv.find_type_var id env
         with Not_found ->
           try Ctype (C.find_type id) with Not_found ->
             try
               let info = C.find_logic_type id in
               if info.lt_params <> [] then
                 error loc "wrong number of parameter for type %s" id
               else Ltype (info,[])
             with Not_found ->
               error loc "no such type %s" id)
    | LTnamed(id,l) ->
        (try
           let info = C.find_logic_type id in
           if List.length info.lt_params <> List.length l then
             error loc "wrong number of parameter for type %s" id
           else Ltype (info,List.map (logic_type loc env) l)
         with Not_found ->
           error loc "no such type %s" id)
    | LTinteger -> Linteger
    | LTreal -> Lreal

  and c_logic_type loc env t = match logic_type loc env t with
    | Ctype t -> t
    | Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ -> error loc "not a C type"

  let rec add_offset toadd = function
    | TNoOffset -> toadd
    | TField(fid', offset) -> TField(fid', add_offset toadd offset)
    | TIndex(e, offset) -> TIndex(e, add_offset toadd offset)

  let add_offset_lval toadd (b, off) = b, add_offset toadd off

  let dummy_loc = Lexing.dummy_pos,Lexing.dummy_pos

  let rec type_of_pointed t =
    match unroll_type t with
      Ctype ty when isPointerType ty -> Ctype (Cil.typeOf_pointed ty)
    | Ltype ({lt_name = "set"} as lt,[t]) ->
        Ltype(lt,[type_of_pointed t])
    | _ ->
        Cilmsg.fatal "type %a is not a pointer type" d_logic_type t

  let type_of_array_elem =
    plain_or_set
      (fun t ->
         match unroll_type t with
           Ctype ty when isArrayType ty -> Ctype (Cil.typeOf_array_elem ty)
         | _ ->
             error (CurrentLoc.get()) "type %a is not an array type"
               d_logic_type t)

  let plain_mk_mem ?loc t ofs = match t.term_node with
    | TAddrOf lv -> add_offset_lval ofs lv
    | TStartOf lv -> add_offset_lval (TIndex (Cil.lzero ?loc (), ofs)) lv
    | _ -> TMem t, ofs

  let mk_mem ?loc t ofs =
    lift_set
      (fun t -> term ?loc (TLval (plain_mk_mem ?loc t ofs))
         (type_of_pointed t.term_type))
      t

  let mk_logic_access env t =
    match t.term_node with
        TLval _ -> t
      | _ ->
          let var = Lenv.fresh_var env "tmp" t.term_type in
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
              ~loc (TLval (add_offset_lval f_ofs lv)) f_type
	| Told t1 ->
            Logic_const.term
              ~loc (Told (t_dot_x t1)) f_type
	| Tat (t1,l) ->
            Logic_const.term
              ~loc (Tat (t_dot_x t1,l)) f_type
	| _ ->
          let var = Lenv.fresh_var env "tmp" t.term_type in
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

  let mk_shift loc env idx t_elt t =
    let add_offset array =
      Logic_const.term ~loc
	(TLval
           (add_offset_lval (TIndex (idx, TNoOffset)) array)) t_elt
    in
      match t.term_node with
	| TStartOf array -> add_offset array
	| TLval array when isLogicArrayType t.term_type ->
            add_offset array
	| Tlet (def, ({ term_node = TLval array} as t))
            when isLogicArrayType t.term_type ->
            Logic_const.term ~loc (Tlet (def, add_offset array)) t_elt
        | Tat({term_node = TStartOf (TVar { lv_origin = Some v},_ as lv)},lab)
            when v.vformal && lab = old_label && env.Lenv.is_funspec ->
            Logic_const.tat ~loc (add_offset lv,lab)
        | Tat({term_node = TLval (TVar { lv_origin = Some v},_ as lv)},lab)
            when v.vformal && lab = old_label && env.Lenv.is_funspec &&
              isLogicArrayType t.term_type ->
            Logic_const.tat ~loc (add_offset lv,lab)
	| _ ->
	    let b =
              { term_node = TBinOp (IndexPI, t, idx); term_name = [];
		term_loc = loc;
		term_type = set_conversion t.term_type idx.term_type }
	    in
	      mk_mem b TNoOffset

  let plain_arithmetic_type t =
    match unroll_type t with
      | Ctype ty -> Cil.isArithmeticType ty
      | Linteger | Lreal -> true
      | Ltype _ | Lvar _ | Larrow _ -> false

  let plain_integral_type t =
    match unroll_type t with
      | Ctype ty -> Cil.isIntegralType ty
      | Linteger -> true
      | Ltype _ | Lreal | Lvar _ | Larrow _ -> false

  let plain_boolean_type t =
    match unroll_type t with
      | Ctype ty -> isIntegralType ty
      | Linteger -> true
      | Ltype ({lt_name = name},[]) ->
          name = Utf8_logic.boolean
      | Lreal | Ltype _ | Lvar _ | Larrow _ -> false

  let plain_non_void_ptr loc typ =
    match unroll_type typ with
        Ctype (TPtr(ty,_) | TArray(ty,_,_,_)) ->
          not (Cil.isVoidType ty)
      | _ -> error loc "not a pointer or array type"

  let is_arithmetic_type = plain_or_set plain_arithmetic_type

  let is_integral_type = plain_or_set plain_integral_type

  (* can we have sets of boolean as for now ? *)
  let is_boolean_type = plain_or_set plain_boolean_type

  let is_non_void_ptr loc = plain_or_set (plain_non_void_ptr loc)

  let check_non_void_ptr loc typ =
    if not (is_non_void_ptr loc typ) then
      error loc "expecting a non-void pointer"

  let is_set_type t =
    match unroll_type t with
        Ltype ({lt_name = "set"},[_]) -> true
      | _ -> false

  (* Make an AddrOf. Given an lval of type T will give back an expression of
   * type ptr(T)  *)
  let mk_AddrOf lval t =
    let loc = t.term_loc in
    match lval with
        TMem e, TNoOffset -> term ~loc e.term_node e.term_type
      | b, TIndex(z, TNoOffset) when isLogicZero z ->
          term ~loc (TStartOf (b, TNoOffset))
            (Ctype (TPtr (logicCType t.term_type,[]))) (* array *)
      | _ ->
          term ~loc (TAddrOf lval)
            (Ctype (TPtr (logicCType t.term_type,[])))

  let mkAddrOfAndMark (b,off as lval) t =
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
      | TField(fi,_) -> fi.faddrof <- true
    end;
    mk_AddrOf lval t

  let c_mk_cast e oldt newt =
    if Cilutil.equals (type_sig_logic oldt) (type_sig_logic newt) then begin
      e
    end else begin
      (* Watch out for constants *)
      if isPointerType newt && isLogicNull e && not (isLogicZero e) then e
      else if isPointerType newt && isArrayType oldt && is_C_array e then
        mk_logic_StartOf e
      else
        match newt, e.term_node with
          | TInt(newik, []), TConst (CInt64(i, _, _)) ->
	      { e with term_node = TConst (CInt64 (i, newik, None)) }
          | _ ->
              { e with term_node = TCastE (newt, e); term_type = Ctype newt }
    end

  let is_same_ptr_type ctyp1 ctyp2 =
    (isPointerType ctyp1) &&
      (isPointerType ctyp2) &&
      (Cilutil.equals (type_sig_logic (typeOf_pointed ctyp1))
         (type_sig_logic (typeOf_pointed ctyp2)))

  let is_same_array_type ctyp1 ctyp2 =
    (isArrayType ctyp1) && (isArrayType ctyp2) &&
      (Cilutil.equals
         (type_sig_logic (typeOf_array_elem ctyp1))
         (type_sig_logic (typeOf_array_elem ctyp2)))

  let is_same_logic_ptr_type ty1 ty2 =
    match (ty1,ty2) with
        Ctype t1, Ctype t2 -> is_same_ptr_type t1 t2
      | _ -> false

  let is_same_logic_array_type ty1 ty2 =
    match (ty1,ty2) with
        Ctype t1, Ctype t2 -> is_same_array_type t1 t2
      | _ -> false

  let is_function_pointer ty =
    try
      Cil.isFunctionType (Cil.typeOf_pointed  ty)
    with Assert_failure _ -> false

  let is_implicit_pointer_conversion term ctyp1 ctyp2 =
    let same_pointed () =
      Cilutil.equals
        (type_sig_logic (typeOf_pointed ctyp1))
        (type_sig_logic (typeOf_pointed ctyp2))
    in
    let same_array_elt () =
      Cilutil.equals
        (type_sig_logic (typeOf_array_elem ctyp1))
        (type_sig_logic (typeOf_array_elem ctyp2))
    in
    let compatible_pointed () =
      same_pointed () ||
        (isVoidPtrType ctyp2 && not (is_function_pointer ctyp1))
    in
    (isArrayType ctyp1 && isArrayType ctyp2 && same_array_elt ()) ||
    (isPointerType ctyp1 && isPointerType ctyp2 &&
     (compatible_pointed() || isLogicNull term))

  let rec mk_cast e newt =
    let loc = e.term_loc in
    if is_same_type e.term_type newt then e
    else begin
      match
        (unroll_type e.term_type),
        (* If any, use the typedef itself in the inserted cast *)
        (unroll_type ~unroll_typedef:false newt)
      with
        | Ctype oldt, Ctype newt ->
            c_mk_cast e oldt newt
        | t1, Ltype ({lt_name = name},[])
            when name = Utf8_logic.boolean && is_integral_type t1 ->
            { e with
                term_node = TBinOp(Cil_types.Ne,e,lzero ~loc ());
                term_type = Ltype(C.find_logic_type Utf8_logic.boolean,[]) }
        | Ltype({lt_name = "set"},[ty1]), Ltype({lt_name="set"},[ty2]) ->
            let e = mk_cast {e with term_type = ty1} ty2 in
            { e with term_type = make_set_type e.term_type}
        | ty1 , Ltype({lt_name =  "set"},[ ty2 ]) ->
            let e = mk_cast e ty2 in
            { e with term_type = make_set_type ty1}
        | Linteger, Linteger | Lreal, Lreal -> e
        | Linteger, Ctype t when isLogicPointerType newt && isLogicNull e ->
            c_mk_cast e intType t
        | Linteger, Ctype _ | Lreal, Ctype _ ->
            error loc "invalid implicit cast from %a to C type %a"
              d_logic_type e.term_type d_logic_type newt
        | Ctype t, Linteger when Cil.isIntegralType t ->
            { e with term_type = Linteger}
        | Ctype t, Lreal when isArithmeticType t -> e
        | Ctype _, (Lreal | Linteger) ->
            error loc "invalid implicit cast from %a to logic type %a"
              d_logic_type e.term_type d_logic_type newt
        | Linteger, Lreal -> e
        | Lreal, Linteger ->
            error loc
              "invalid cast from real to integer. \
         Use conversion functions instead"
        | Ltype _, _ | _, Ltype _
        | Lvar _,_ | _,Lvar _
        | Larrow _,_ | _,Larrow _ ->
            error loc "invalid cast from %a to %a"
              d_logic_type e.term_type d_logic_type newt
    end

  let rec c_cast_to ot nt e =
    if Cilutil.equals (type_sig_logic ot) (type_sig_logic nt) then
      (ot, e)
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
            when Cilutil.equals (type_sig_logic told) (type_sig_logic tnew)
              -> result
        | (TPtr _ | TArray _), (TPtr _ | TArray _)
            when isLogicNull e -> result
        | TPtr _, TPtr _ when isVoidPtrType nt -> (nt, e)
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
            Cil.warnOpt "Casting %a to __builtin_va_list" Cil.d_type ot;
            result
        | TPtr _, TEnum _ ->
            Cil.warnOpt "Casting a pointer into an enumeration type";
            result
        | (TInt _ | TEnum _ | TPtr _ ), TVoid _ ->
            (ot, e)
        | TComp (comp1, _, _), TComp (comp2, _, _)
            when comp1.ckey = comp2.ckey ->
            (nt, e)
        | _ ->
	    Cil.fatal "Logic_typing.c_cast_to: %a -> %a@." Cil.d_type ot Cil.d_type nt
    end

  (* for overloading: raised when an arguments list does not fit a
     formal parameter list *)
  exception Not_applicable

  (* keep in sync with fresh_type below *)
  let generated_var s = String.contains s '#'

  let rec partial_unif ~overloaded loc ot nt env =
    match (unroll_type ot),(unroll_type nt) with
      | Lvar s1, Lvar s2 ->
          if generated_var s1 then
            try
              let ot = Lenv.find_type_var s1 env in
              partial_unif ~overloaded loc ot nt env
            with Not_found ->
              if generated_var s2 then
                try let nt = Lenv.find_type_var s2 env in
                partial_unif ~overloaded loc ot nt env
                with Not_found ->
                  if s1 < s2 then Lenv.add_type_var s2 ot env,ot,ot
                  else if s2 < s1 then Lenv.add_type_var s1 nt env,nt,nt
                  else env,ot,ot (* same type anyway *)
              else Lenv.add_type_var s1 nt env, nt, nt
          else
            if generated_var s2 then
              try
                let nt = Lenv.find_type_var s2 env in
                partial_unif ~overloaded loc ot nt env
              with Not_found ->
                Lenv.add_type_var s2 ot env, ot, ot
            else if s1 = s2 then env, ot, ot (* same type *)
            else error loc "implicit unification of type variables %s and %s" s1 s2
      | Lvar s1, _ when generated_var s1 ->
          (try let ot = Lenv.find_type_var s1 env in
           partial_unif ~overloaded loc ot nt env
           with Not_found ->
             Lenv.add_type_var s1 nt env, nt, nt)
      | _, Lvar s2 when generated_var s2 ->
          (try
             let nt = Lenv.find_type_var s2 env in
             partial_unif ~overloaded loc ot nt env
           with Not_found ->
             Lenv.add_type_var s2 ot env, ot, ot)
      | Ltype(t1,l1), Ltype(t2,l2) when t1.lt_name = t2.lt_name ->
          let env,l1,l2 =
            List.fold_right2
              (fun ot nt (env,l1,l2) ->
                 let (env,ot,nt) = partial_unif ~overloaded loc ot nt env in
                 (env,ot::l1,nt::l2))
              l1 l2 (env,[],[])
          in env,Ltype(t1,l1),Ltype(t2,l2)
      | Larrow(args1,rt1), Larrow(args2,rt2)
          when List.length args1 = List.length args2 ->
          let env, args1, args2 =
            List.fold_right2
              (fun ot nt (env, args1, args2) ->
                 let (env,ot,nt) = partial_unif ~overloaded loc ot nt env in
                 (env,ot::args1,nt::args2))
              args1 args2 (env,[],[])
          in
          let env, rt1, rt2 = partial_unif ~overloaded loc rt1 rt2 env in
          env, Larrow(args1,rt1), Larrow(args2,rt2)
      | t1, Ltype ({lt_name = "set"},[t2]) ->
          let (env,ot,nt) = partial_unif ~overloaded loc t1 t2 env in
          env, make_set_type ot, make_set_type nt
      | t1,t2 when plain_boolean_type t1 && plain_boolean_type t2 ->
          env,ot,nt
      | ((Ctype _ | Linteger | Lreal | Ltype ({lt_name = "boolean"},[])),
         (Ctype _ | Linteger | Lreal | Ltype ({ lt_name = "boolean"},[]))) ->
          env,ot,nt
      | (Ltype _|Larrow _|Lvar _), _ | _, (Larrow _| Ltype _|Lvar _) ->
          if overloaded then raise Not_applicable
	  else error loc "incompatible types %a and %a"
            d_logic_type ot
            d_logic_type nt

  (*
    convert term [oterm] of type [ot] to type [nt].
    when overloaded is true,
    raise exception Not_applicable if conversion not possible,
    otherwise print an error message with location [loc]
   *)
  let rec implicit_conversion ~overloaded loc oterm ot nt =
    match (unroll_type ot), (unroll_type nt) with
      | Ctype ty1, Ctype ty2 ->
          if Cilutil.equals (type_sig_logic ty1) (type_sig_logic ty2)
          then
            ot, oterm
          else
            let sz1 = bitsSizeOf ty1 in
            let sz2 = bitsSizeOf ty2 in
            if (isIntegralType ty1 && isIntegralType ty2 &&
                  (sz1 < sz2
                   || (sz1 = sz2 && (isSignedInteger ty1 = isSignedInteger ty2))
                  ))
              || is_implicit_pointer_conversion oterm ty1 ty2
              || (match unrollType ty1, unrollType ty2 with
                    | (TFloat (f1,_), TFloat (f2,_)) ->
                        f1 <= f2
                          (*[BM]
                            relies on internal representation of OCaml constant
                            constructors.*)
                    | _ -> false)
            then begin
              let t,e = c_cast_to ty1 ty2 oterm in Ctype t, e
            end else
	      if overloaded then raise Not_applicable
	      else
                error loc "invalid implicit conversion from '%a' to '%a'"
                  d_type ty1 d_type ty2
      | Ctype ty, Linteger when Cil.isIntegralType ty -> Linteger, oterm
      | Ctype ty, Lreal when Cil.isArithmeticType ty -> Lreal, oterm
      | Linteger, Lreal when not overloaded -> Lreal, oterm
          (* Integer 0 is also a valid pointer. *)
      | Linteger, Ctype ty when Cil.isPointerType ty && isLogicNull oterm ->
          nt, { oterm with
                  term_node = TCastE(ty,oterm);
                  term_type = nt }
            (* can convert implicitly a singleton into a set,
               but not the reverse. *)
      | Ltype (t1,l1), Ltype (t2,l2) when t1.lt_name = t2.lt_name ->
          (* not sure this is really what we want: can foo<int> be implicitly
             converted into foo<integer> ? *)
          let l =
	    List.map2 (fun x y -> fst (implicit_conversion ~overloaded loc oterm x y)) l1 l2
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
	      (fun x y -> fst (implicit_conversion ~overloaded loc oterm x y)) args2 args1
          in
          let rt,_ = implicit_conversion ~overloaded loc oterm rt1 rt2 in
          Larrow(args,rt), oterm
      | ((Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _),
         (Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _)) ->
          if overloaded then raise Not_applicable
          else
	    error loc "invalid implicit conversion from %a to %a"
              d_logic_type ot d_logic_type nt

  let instantiate_app ~overloaded loc oterm nt env =
    let ot = oterm.term_type in
    let env, ot, nt = partial_unif ~overloaded loc ot nt env in
    let t,e = implicit_conversion ~overloaded loc oterm ot nt in
    env, t, e

  let convertible t1 t2 =
    try
      let _ =
        implicit_conversion
          ~overloaded:true
	  Cil_datatype.Location.unknown
	  t1
	  t1.term_type
	  t2.term_type
      in true
    with Not_applicable -> false

  let convertible_non_null t1 t2 =
    match (unroll_type t1.term_type, unroll_type t2.term_type) with
        | Ctype ty1, Ctype ty2 when
            isPointerType ty1 && isPointerType ty2 && isLogicNull t1 ->
            isVoidPtrType ty2
        | _ -> convertible t1 t2

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
    assert (l <> []);
    l

  let rec logic_arithmetic_promotion t =
    match unroll_type t with
      | Ctype ty when Cil.isIntegralType ty -> Linteger
      | Linteger -> Linteger
      | Lreal -> Lreal
      | Ctype ty ->
          (match Cil.unrollType ty with
               TFloat _ -> Lreal
             | _ ->
                 Cilmsg.fatal
                   "logic arithmetic promotion on non-arithmetic type %a"
                   d_logic_type t)
      | Ltype ({lt_name="set"} as lt,[t]) ->
          Ltype(lt,[logic_arithmetic_promotion t])
      | Ltype _ | Lvar _ | Larrow _ ->
          Cilmsg.fatal "logic arithmetic promotion on non-arithmetic type %a"
            d_logic_type t

  let rec integral_promotion t =
    match unroll_type t with
    | Ctype ty when isIntegralType ty ->
        Linteger
    | Linteger -> Linteger
    | Ltype ({lt_name="set"} as lt,[t]) -> Ltype(lt,[integral_promotion t])
    | Ltype _ | Lreal | Lvar _ | Larrow _ | Ctype _ ->
        Cilmsg.fatal
          "logic integral promotion on non-integral type %a"
          d_logic_type t

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
      | Ltype ({lt_name="set"} as lt,[t1]),t2
      | t1,Ltype ({lt_name="set"} as lt,[t2]) ->
          Ltype(lt,[arithmetic_conversion t1 t2])
      | _ ->
          Cilmsg.fatal
            "arithmetic conversion between non arithmetic types %a and %a"
            d_logic_type ty1 d_logic_type ty2

  let conditional_conversion loc env t1 t2 =
    let _, t1, t2 = partial_unif ~overloaded:false loc t1 t2 env in
    let rt =
      match (unroll_type t1), (unroll_type t2) with
        | _ when is_same_type t1 t2 -> t1
        | Ctype ty1, Ctype ty2 ->
            if isIntegralType ty1 && isIntegralType ty2 then
              Linteger
            else if isArithmeticType ty1 && isArithmeticType ty2 then
              Lreal
            else if is_same_ptr_type ty1 ty2 || is_same_array_type ty1 ty2 then
              Ctype (C.conditionalConversion ty1 ty2)
            else if
              (isPointerType ty1 || isArrayType ty1) &&
                (isPointerType ty2 || isArrayType ty2)
            then error loc "types %a and %a are not convertible"
              d_type ty1 d_type ty2
            else (* pointer to integer conversion *)
              Ctype (C.conditionalConversion ty1 ty2)
        | (Linteger, Ctype t | Ctype t, Linteger) when Cil.isIntegralType t
            ->
            Linteger
        | (Ltype({lt_name = name},[]), t
          | t, Ltype({lt_name = name},[]))
            when is_integral_type t && name = Utf8_logic.boolean ->
            Ltype(C.find_logic_type Utf8_logic.boolean,[])
        | Lreal, Ctype ty | Ctype ty, Lreal ->
            (match Cil.unrollType ty with
                 TFloat _ -> Lreal
               | _ -> error loc "types %a and %a are not convertible"
                   d_logic_type t1 d_logic_type t2)
        | Ltype (s1,l1), Ltype (s2,l2)  when s1.lt_name = s2.lt_name &&
            List.for_all2 is_same_type l1 l2 -> t1
        | Lvar s1, Lvar s2 when s1 = s2 -> t1
        | Linteger, Linteger -> Linteger
        | (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
        | _ ->
            error loc "types %a and %a are not convertible"
              d_logic_type t1 d_logic_type t2
    in
    rt,t1,t2

  let location_to_char_ptr t =
    let convert_one_location t =
      let ptd_type = type_of_pointed t.term_type in
      if isLogicCharType ptd_type then t
      else if isLogicVoidType ptd_type then error t.term_loc
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

  let location_set_conversion loc transform_pointer_set ot nt env =
    let ot = set_conversion ot nt in
    if is_same_type ot nt then transform_pointer_set, ot
    else if isLogicArithmeticType ot && isLogicArithmeticType nt then
      let typ = arithmetic_conversion ot nt in transform_pointer_set, typ
    else if isLogicPointerType ot && isLogicPointerType nt then
      true, make_set_type (Ctype Cil.charPtrType)
    else
      let _,_,t = partial_unif ~overloaded:false loc ot nt env in
      transform_pointer_set,t

  let rec mk_ptr_type t =
    match unroll_type t with
      Ctype ty -> Ctype (TPtr(ty,[]))
    | Ltype ({lt_name = "set"} as lt,[t]) -> Ltype (lt,[mk_ptr_type t])
    | _ ->
        error Cil_datatype.Location.unknown
          "cannot take the address of non-C values (type %a)" d_logic_type t

  let binop_of_rel = function
      Eq -> Cil_types.Eq
    | Neq -> Cil_types.Ne
    | Ge -> Cil_types.Ge
    | Gt -> Cil_types.Gt
    | Le -> Cil_types.Le
    | Lt -> Cil_types.Lt

  (* Typing terms *)

  let parseInt s =
    let explode s =
      let l = ref [] in
      String.iter (fun c -> l:=Int64.of_int (Char.code c) :: !l) s;
      List.rev !l
    in
    match s.[0] with
      | 'L' -> (* L'wide_char' *)
          let content = String.sub s 2 (String.length s - 3) in
          let tokens = explode content in
          let value = Cil.reduce_multichar Cil.theMachine.Cil.wcharType tokens
          in
          Cil.kinteger64 
            ~loc:Cil_datatype.Location.unknown Cil.theMachine.Cil.wcharKind value
      | '\'' -> (* 'char' *)
          let content = String.sub s 1 (String.length s - 2) in
          let tokens = explode content in
          let value,_= Cil.interpret_character_constant tokens in
          dummy_exp (Const value)
      | _ -> Cil.parseInt ~loc:Cil_datatype.Location.unknown s

  let find_logic_label loc env l =
    try Lenv.find_logic_label l env
    with Not_found ->
      (* look for a C label *)
      try
        let lab = C.find_label l in
        StmtLabel lab
      with Not_found ->
        error loc "logic label `%s' not found" l

  let find_old_label loc env =
    try Lenv.find_logic_label "Old" env
    with Not_found ->
      error loc "\\old undefined in this context"

  let find_current_label loc env =
    match env.Lenv.current_logic_label with
      | Some lab -> lab
      | None ->
          if Lenv.no_label env then begin
            match !Lenv.default_label with
                None ->
                  let lab = LogicLabel (None, "L") in
                  Lenv.default_label := Some lab; lab
              | Some lab -> lab
          end else
	    error loc
              "no label in the context. (\\at or explicit label missing?)"

  let check_current_label loc env = ignore (find_current_label loc env)

  let labels_assoc loc id env fun_labels effective_labels =
    match fun_labels, effective_labels with
        [lf], [] -> [lf, find_current_label loc env]
      | _ ->
	  try
	    List.map2
	      (fun l1 l2 -> (l1,l2))
	      fun_labels effective_labels
	  with Invalid_argument _ ->
	    error loc "wrong number of labels for %s" id

  let add_quantifiers loc q env =
    let (tq,env) =
      List.fold_left
        (fun (tq,env) (ty, id) ->
	   let ty = unroll_type (logic_type loc env ty) in
           let v = Cil_const.make_logic_var id ty in
           (v::tq, Lenv.add_var id v env))
        ([],env) q
    in
    (List.rev tq,env)

  class rename_variable v1 v2 =
    object
      inherit Cil.nopCilVisitor
        method vlogic_var_use v =
          if v.lv_id = v1.lv_id then ChangeTo v2 else SkipChildren
    end

  (* rename v1 into v2 in t *)
  let rename_variable t v1 v2 =
    visitCilTerm (new rename_variable v1 v2) t

  (* keep in sync with generated_var above*)
  class fresh_type_var =
  object(self)
    inherit Cil.nopCilVisitor
    val alpha_rename = Hashtbl.create 7
    val mutable count = 0
    method private fresh_s s =
      count <- succ count; Printf.sprintf "%s#%d" s count
    method vlogic_type = function
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
               i+1, Cil_const.make_logic_var ("x_" ^ (string_of_int i)) x ::l)
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
            let args =
              List.fold_right (fun x l -> x.lv_type :: l) quants args
            in
            { term with
                term_node = Tlambda (quants @ quants', term);
                term_type = Larrow (args,rt_typ) }
        | Tlambda _ , _ -> fatal "\\lambda with a non-arrow type"
        | _,typ ->
            { term with
                term_node = Tlambda(quants, term);
                term_type = Larrow(List.map (fun x -> x.lv_type) quants,typ)
            }
    in
    let rec aux known_vars kont term =
      match term.term_node with
        | TLval(TVar v, TNoOffset) ->
            known_vars, kont (eta_expand term.term_loc term.term_name env v)
        | TConst _ | TLval _ | TSizeOf _ | TSizeOfE _
        | TSizeOfStr _ | TAlignOf _ | TAlignOfE _
        | TUnOp _ | TBinOp _ | TCastE _ | TAddrOf _ | TStartOf _
        | Tapp _  | TDataCons _ | Tbase_addr _
        | Tblock_length _ | Tnull | TCoerce _ | TCoerceE _
        | TUpdate _ | Ttypeof _ | Ttype _ | Tempty_set
              (* [VP] I suppose that an union of functions
                 is theoretically possible but I'm not sure that we want to
                 lift the lambda anyway, even though this contradicts the
                 idea that you can always replace a term by a set of terms
               *)
        | Tunion _ | Tinter _ | Tcomprehension _
        | Trange _

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
                      Cilmsg.verify(List.length quants1 = List.length quants2)
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
                    fatal "Branches of conditional have different number \
                             of \\lambda"
                | _,_ -> term
            in known_vars, kont term
        | Told t ->
            let push_old t = match t.term_node with
                Tlambda(quants,t) ->
                  { term with
                      term_node =
                      Tlambda(quants, {t with term_node = Told t})}
              | _ -> term
            in
            aux known_vars (kont $ push_old) t
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

  let check_func_labels loc env info =
    match info.l_labels with
        [] -> ()
      | [_] -> check_current_label loc env;
      | _ -> error loc
          "logic function or predicate %a has multiple labels. \
         They must be instantiated explicitly." Cil.d_logic_var info.l_var_info

  let has_extra_offset_to_TField loc t_type = function
      (* used for functional update of field under anonymous type *)
    | PLpathField f ->
	let f_ofs, _ = plain_type_of_field loc f t_type in
	let result = match f_ofs with
	  | TField (_,TNoOffset) -> false
	  | TField _ -> true ; 
	  | _ -> assert false
	in result
    | PLpathIndex _ -> false

  let updated_offset_term idx_typing check_type mk_field mk_idx loc t_type = function
    | PLpathField f ->
	let f_ofs, ofs_type = plain_type_of_field loc f t_type in
	let f_ofs, ofs_type = match f_ofs with
	  | TField (f,TNoOffset) ->( mk_field f),ofs_type
	  | TField (f,_) -> (mk_field f), 
	      (* f is an anonymous field, find its type *)
	      (let c_type = match t_type with
		 | Ctype c_type -> c_type
		 | _ -> assert false in
		 Ctype (Cil.typeOffset c_type (Field (f,NoOffset))))
	  | _ -> assert false
	in f_ofs,ofs_type
    | PLpathIndex idx ->
	let idx = idx_typing idx in
	let ofs_type =
	  if isLogicArrayType t_type && check_type idx.term_type
	  then type_of_array_elem t_type
	  else error loc "subscripted value is not an array"
	in mk_idx idx, ofs_type

  let normalize_updated_offset_term idx_typing env loc t normalizing_cont toff =
   let t_type = t.term_type in
    let mk_let_info name t t_off2 type2 =
      match t with
	| { term_node = TConst _} -> (* just a copy *)
	    assert (t_off2 = TNoOffset) ;
	    (fun id -> id), t, { t with term_node = t.term_node }
	| { term_node = TLval((TVar _,_) as lv)} -> (* just a copy *)
	    (fun id -> id), t,
	    { t with term_node = TLval(add_offset_lval t_off2 lv);
		term_type = type2}
	| _ -> (* to build a let *)
	    let var = Lenv.fresh_var env name t.term_type in
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
	|| error loc "range is only allowed for lastest offset"
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
    in updated, t_type

  let rec term env t =
    match t.lexpr_node with
      | PLnamed(name,t) ->
	  let t = term env t in
	  { t with term_name = name :: t.term_name }
      | _ ->
	  let t', ty = term_node env t.lexpr_loc t.lexpr_node in
	  { term_node = t'; term_loc=t.lexpr_loc; term_type=ty; term_name = [] }

  and normalize_update_term env loc t v = function
      (* Transform terms like {x \with .c[idx] = v}
	 into {x \with .c = {x.c \with [idx] = v}}.
	 \let expressions can be introduced. *)
    | [] -> assert false (* parsing invariant *)
    | (toff::tail) as offs -> 
	begin 
	  let t_type = t.term_type in
	  let tail =
	    if has_extra_offset_to_TField loc t_type toff then
	      offs (* fields under an anonymous field are not removed *)
	    else
	      tail
	  in match tail with
	    | [] ->
		let toff, ofs_type =
		  let mk_field f = TField (f, TNoOffset)
		  and mk_idx idx = TIndex(idx,TNoOffset)
		  and idx_typing idx = term env idx
		  in updated_offset_term
		       idx_typing is_integral_type mk_field mk_idx loc t_type toff
		in
		let v = term env v in
		let v = mk_cast v ofs_type in
		let updated = TUpdate(t,toff,v)
		in updated, t_type
	    | toffs ->
		let idx_typing idx = term env idx
		and normalizing_cont t2 = normalize_update_term env loc t2 v toffs
		in normalize_updated_offset_term idx_typing env loc t normalizing_cont toff
	end
  and normalize_update_cont env loc t =
    function
      | [],_ -> assert false (* parsing invariant *)
      | _,[] -> assert false (* parsing invariant *)
      | ((contoffs,PLupdateTerm v)::[]),toffs ->
	  (* {x \with .c1 = {\with .c2 = v}} =
	     {x \with .c1.c2 = v} *)
	  normalize_update_term env loc t v (toffs@contoffs)
      | ((contoffs,PLupdateCont v)::[]),toffs ->
	  (* {x \with .c1 = {\with .c2 = {\with...}}} =
	     {x \with .c1.c2 = {\with...}} *)
	  normalize_update_cont env loc t (v,(toffs@contoffs))
      | (cont::conts),toff::[] ->
	  (* {x \with .c1 = {\with .c2 = v2, ..., c22 = v22}} =
	     {x \with .c1 = {...{x.c1 \with .c2 = v2} .. \with c22 = v22} *)
	  let idx_typing idx = term env idx in
	  let normalizing_cont t2 =
	    let normalize t = function
	      | contoffs,PLupdateTerm v -> normalize_update_term env loc t v contoffs
	      | contoffs,PLupdateCont cont -> normalize_update_cont env loc t (cont, contoffs)
	    in
	    let normalize_folding (tn,typ) cont =  normalize (Logic_const.term ~loc tn typ) cont
	    in List.fold_left normalize_folding (normalize t2 cont) conts
	  in normalize_updated_offset_term idx_typing env loc t normalizing_cont toff
      | cont,toff::toffs ->
	  (* {x \with .c1.c2 = {\with...}} =
	     {x \with .c1 = { x.c1 \with .c2 = {\with...}}} *)
	  let idx_typing idx = term env idx
	  and normalizing_cont t2 = normalize_update_cont env loc t2 (cont,toffs)
	  in normalize_updated_offset_term idx_typing env loc t normalizing_cont toff

  and term_node env loc pl =
    match pl with
      | PLinitIndex _ ->
	  error loc "unsupported aggregated array construct"
      | PLinitField _ ->
	  error loc "unsupported aggregated field construct"
      | PLupdate (t, toff, PLupdateCont cont) ->
	  let t = term env t in
            normalize_update_cont env loc t (cont, toff)
      | PLupdate (t, toff, PLupdateTerm v) ->
	  let t = term env t in
            normalize_update_term env loc t v toff

      | PLsizeof typ ->
	  (match logic_type loc env typ with
               Ctype t -> TSizeOf t,Linteger
             | _ -> error loc "sizeof can only handle C types")
	    (* NB: don't forget to add the case of literal string
               when they are authorized in the logic *)
      | PLsizeofE lexpr ->
          let t = term env lexpr in
          TSizeOfE t, Linteger
      | PLnamed _ -> assert false (* should be captured by term *)
      | PLconstant (IntConstant s) ->
          begin match (parseInt s).enode with
            | Const (CInt64 (_,_,_) as c) -> TConst c, Linteger
            | Const ((CChr _) as c) -> (* a char literal has type int *)
                TConst c, Linteger
            | _ -> assert false
          end
      | PLconstant (FloatConstant str) ->
          let hasSuffix str =
            let l = String.length str in
            fun s ->
              let ls = String.length s in
              l >= ls && s = String.uppercase (String.sub str (l - ls) ls)
          in
          (* Maybe it ends in U or UL. Strip those *)
          let l = String.length str in
          let hasSuffix = hasSuffix str in
          let baseint, kind =
            if  hasSuffix "L" or hasSuffix "l" then
              String.sub str 0 (l - 1), FLongDouble
            else if hasSuffix "F" or hasSuffix "f" then
              String.sub str 0 (l - 1), FFloat
            else if hasSuffix "D" or hasSuffix "d" then
              String.sub str 0 (l - 1), FDouble
            else
              str, FDouble
          in
          begin
	    try
	      TConst(CReal(float_of_string baseint, kind, Some str)),
	      Lreal
	    with Failure _ as e ->
	      Cil.abort "float_of_string %s (%s)" str (Printexc.to_string e)
          end
      | PLconstant (StringConstant s) ->
          TConst (CStr (unescape s)), Ctype Cil.charPtrType
      | PLconstant (WStringConstant s) ->
          TConst (CWStr (wcharlist_of_string s)),
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
            (if x = "\\exit_status" then
               match Lenv.current_post_state env with
                 | Some Exits -> ()
                 | _ ->
                     Cil.abort
                       "\\exit_status can only be found in exits clause");
	  try
	     let def = C.find_macro x 
	     in term_node env loc def.lexpr_node
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
                       Cil.abort "Variable %s is bound to a predicate, \
                                  not a term" x
                   | _ -> old_val lv)
	    with Not_found ->
              try
	        let info = C.find_var x in
                (match info.lv_origin with
                   | Some lv ->
                       check_current_label loc env;
                       lv.vreferenced <- true
                   | None -> ());
                old_val info
	      with Not_found ->
	        try
	          let e,t = C.find_enum_tag x in
	          begin match (Cil.constFold true e).enode with
	            | Const c -> TConst c, Ctype t
	            | _ -> assert false
	          end
	        with Not_found ->
                  try
                    fresh_type#reset ();
                    let info = C.find_logic_ctor x in
                    match info.ctor_params with
                        [] ->
                          TDataCons(info,[]),
                          Ltype(info.ctor_type,
                                List.map
                                  (fun x -> fresh (Lvar x))
                                  info.ctor_type.lt_params)
                      | _ ->
                          error loc "Data constructor %s needs arguments"
                            info.ctor_name
                  with Not_found ->
                    (* We have a global logic variable. It may depend on
                       a single state (multiple labels need to be explicitely
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
                          | Some t, l ->
                              fresh
                                (Larrow (List.map (fun x -> x.lv_type) l, t))
                          | None, _ -> error loc "%s is not a logic variable" x
                      in
                      match f.l_labels with
                          [] ->
                            TLval (TVar(f.l_var_info),TNoOffset), typ
                        | [l] ->
                            let curr = find_current_label loc env in
                            Tapp(f,[l,curr],[]), typ
                        | _ ->
                            error loc
                              "%s labels must be explicitely instantiated" x
                    in
                    match C.find_all_logic_functions x with
                        [] -> error loc "unbound logic variable %s" x
                      | [f] -> make_expr f
                      | l ->
                          (try
                             let f =
                               List.find (fun info -> info.l_profile = []) l
                             in make_expr f
                           with Not_found ->
                               error loc
                                 "invalid use of overloaded function \
                                  %s as constant" x)
          end
      | PLapp (f, labels, tl) ->
          fresh_type#reset ();
          let ttl = List.map (term env) tl in
          begin
	    try
              let info = C.find_logic_ctor f in
              if labels <> [] then error loc "symbol %s is a data constructor. \
                                         It cannot have logic labels" f;
              let params = List.map fresh info.ctor_params in
              let env, tl =
                type_arguments ~overloaded:false env loc params ttl
              in
              let t = Ltype(info.ctor_type,
			    List.map (fun x -> fresh (Lvar x))
                              info.ctor_type.lt_params)
              in
              let t = instantiate env t in
              TDataCons(info,tl), t
	    with Not_found ->
	      let info, label_assoc, tl, t =
                type_logic_app env loc f labels ttl
              in
	      match t with
	        | None -> error loc "symbol %s is a predicate, not a function" f
	        | Some t -> Tapp(info, label_assoc, tl), t
          end
      | PLunop (Ubw_not, t) ->
          let t = type_int_term env t in
          TUnOp (BNot, t), logic_arithmetic_promotion t.term_type
      | PLunop (Uminus, t) ->
          let t = type_num_term env t in
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
            error loc "invalid type %a for `unary *'" d_logic_type t.term_type
          end
      | PLunop (Uamp, t) ->
          check_current_label loc env;
          let t = term_lval mkAddrOfAndMark (term env t) in
          t.term_node, t.term_type
      | PLbinop (t1, (Badd | Bsub | Bmul | Bdiv | Bmod
	         | Bbw_and | Bbw_or | Bbw_xor | Blshift | Brshift as op), t2) ->
          let t1 = term env t1 in
          let ty1 = t1.term_type in
          let t2 = term env t2 in
          let ty2 = t2.term_type in
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
          in
          let binop op tr =	TBinOp (op, mk_cast t1 tr, mk_cast t2 tr),
            logic_arithmetic_promotion tr
          in
          begin match op with
            | Bmul | Bdiv when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	        binop (type_binop op) (arithmetic_conversion ty1 ty2)
            | Bmod when is_integral_type ty1 && is_integral_type ty2 ->
                binop (type_binop op) (arithmetic_conversion ty1 ty2)
            | Badd | Bsub when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	        binop (type_binop op) (arithmetic_conversion ty1 ty2)
            | Bbw_and | Bbw_or | Bbw_xor
	          when is_integral_type ty1 && is_integral_type ty2 ->
	        binop (type_binop op) (arithmetic_conversion ty1 ty2)
            | Blshift | Brshift
                  when is_integral_type ty1 && is_integral_type ty2 ->
                binop (type_binop op) (arithmetic_conversion ty1 ty2)
            | Badd when isLogicPointer t1 && is_integral_type ty2 ->
                let t1 = mk_logic_pointer_or_StartOf t1 in
                let ty1 = t1.term_type in
	        TBinOp (PlusPI, t1, mk_cast t2 (integral_promotion ty2)),
                set_conversion ty1 ty2
            | Badd when is_integral_type ty1 && isLogicPointer t2 ->
                let t2 = mk_logic_pointer_or_StartOf t2 in
                let ty2 = t2.term_type in
                assert (isLogicPointerType t2.term_type);
	        TBinOp (PlusPI, t2, mk_cast t1 (integral_promotion ty1)),
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
	        error loc
                  "invalid operands to binary %a; unexpected %a and %a"
                  d_binop (type_binop op) d_logic_type ty1 d_logic_type ty2
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
          let t = mk_logic_pointer_or_StartOf t in
          let struct_type = type_of_pointed t.term_type in
          let f_ofs, f_type = type_of_field loc f struct_type in
          (mk_mem ~loc t f_ofs).term_node, f_type

      | PLarrget (t1, t2) -> (* TODO: take into account logic arrays. *)
          check_current_label loc env;
          (* memory access need a current label to have some semantics *)
          let t1 = term env t1 in
          let t2 = term env t2 in
          (* access to a C value (either array or pointer) *)
          let t'1, t'2, tres =
            if isLogicPointer t1 && is_integral_type t2.term_type then
              let t1 = mk_logic_pointer_or_StartOf t1 in
              check_non_void_ptr t1.term_loc t1.term_type;
              (t1, t2,
               set_conversion (type_of_pointed t1.term_type) t2.term_type)
            else if is_integral_type t1.term_type && isLogicPointer t2
            then
              let t2 = mk_logic_pointer_or_StartOf t2 in
              check_non_void_ptr t2.term_loc t2.term_type;
              (t2, t1,
               set_conversion (type_of_pointed t2.term_type) t1.term_type)
            else if (* purely logical array access. *)
              isLogicArrayType t1.term_type && is_integral_type t2.term_type
            then
              mk_logic_access env t1, t2, type_of_array_elem t1.term_type
            else if
              isLogicArrayType t1.term_type && is_integral_type t2.term_type
            then
              mk_logic_access env t2, t1, type_of_array_elem t2.term_type
            else error loc "subscripted value is neither array nor pointer"
          in
          let t = lift_set (mk_shift loc env t'2 tres) t'1 in
            t.term_node, t.term_type

      | PLif (t1, t2, t3) ->
          let t1 = type_bool_term env t1 in
          let t2 = term env t2 in
          let t3 = term env t3 in
          let ty,ty2,ty3 =
            conditional_conversion loc env t2.term_type t3.term_type in
          let t2 = { t2 with term_type = ty2 } in
          let t3 = { t3 with term_type = ty3 } in
          Tif (t1, mk_cast t2 ty, mk_cast t3 ty), ty

      | PLold t ->
          let lab = find_old_label loc env in
          let env = Lenv.set_current_logic_label lab env in
          let t = term env t in
          (* could be Tat(t,lab) *)
          Told t, t.term_type
      | PLat (t, l) ->
          let lab = find_logic_label loc env l in
          let env = Lenv.set_current_logic_label lab env in
          let t = term env t in
          Tat (t, lab), t.term_type
      | PLbase_addr t ->
          check_current_label loc env;
          let t = term env t in
          if isLogicPointer t then
            let t =
              lift_set
                (fun t -> Logic_const.term (Tbase_addr t)
                   (Ctype Cil.charPtrType)) (mk_logic_pointer_or_StartOf t)
            in t.term_node, t.term_type
          else error loc "subscripted value is neither array nor pointer"
      | PLblock_length t ->
          check_current_label loc env;
          let t = term env t in
          if isLogicPointer t then
            let t =
              lift_set (fun t -> Logic_const.term (Tblock_length t) Linteger)
                (mk_logic_pointer_or_StartOf t)
            in t.term_node, t.term_type
          else error loc "subscripted value is neither array nor pointer"
      | PLresult ->
          (try let t = Lenv.find_var "\\result" env in
           match t.lv_type with
               Ctype ty ->
                 TLval(TResult ty,TNoOffset), t.lv_type
             | _ -> Cilmsg.fatal "\\result associated to non-C type"
                 (* \\result is the value returned by a C function.
                    It has always a C type *)
           with Not_found -> error loc "\\result meaningless")
      | PLnull -> Tnull, c_void_star
      | PLcast (ty, t) ->
          let t = term env t in
          (* no casts of tsets in grammar *)
          (match unroll_type ~unroll_typedef:false (logic_type loc env ty) with
             | (Ctype ty) as cty
               ->
                 (*TODO: Use c_mk_cast or handle conversion from array to ptr*)
                 TCastE (ty, t), cty
             | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
                 error loc "cannot cast to logic type")
      | PLcoercion (t,ty) ->
          let t = term env t in
          (match unroll_type ~unroll_typedef:false (logic_type loc env ty) with
             | Ctype ty as cty
               -> TCoerce (t, ty), cty
             | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
                 error loc "cannot cast to logic type")
      | PLcoercionE (t,tc) ->
          let t = term env t in
          let tc = term env tc in
          TCoerceE (t, tc), tc.term_type
      | PLrel (t1, (Eq | Neq | Lt | Le | Gt | Ge as op), t2) ->
          let conditional_conversion t1 t2 =
            let t,ty1,ty2 =
              conditional_conversion loc env t1.term_type t2.term_type in
            TBinOp (binop_of_rel op,
                    mk_cast { t1 with term_type = ty1} t,
                    mk_cast { t2 with term_type = ty2} t)
          in
          let loc = loc_join t1.lexpr_loc t2.lexpr_loc in
          let t1 = term env t1 in
          let ty1 = t1.term_type in
          let t2 = term env t2 in
          let ty2 = t2.term_type in
          if not (is_plain_type ty1) || not (is_plain_type ty2) then
            error loc "comparison of incompatible types %a and %a"
              d_logic_type ty1 d_logic_type ty2
          else
          let expr = match op with
            | _ when plain_arithmetic_type ty1 && plain_arithmetic_type ty2 ->
                let tr = arithmetic_conversion ty1 ty2 in
	        TBinOp(binop_of_rel op,mk_cast t1 tr,mk_cast t2 tr)
            | Eq | Neq when isLogicPointer t1 && isLogicNull t2 ->
                let t1 = mk_logic_pointer_or_StartOf t1 in
	        TBinOp (binop_of_rel op, t1, mk_cast t2 ty1)
            | Eq | Neq when isLogicPointer t2 && isLogicNull t1 ->
                let t2 = mk_logic_pointer_or_StartOf t2 in
	        TBinOp (binop_of_rel op, mk_cast t1 ty2, t2)
            | Eq | Neq when
                isLogicArrayType t1.term_type && isLogicArrayType t2.term_type
                ->
                if is_same_logic_array_type t1.term_type t2.term_type then
                  TBinOp(binop_of_rel op, t1,t2)
                else
                  error loc "comparison of incompatible types %a and %a"
                    d_logic_type t1.term_type d_logic_type t2.term_type
            | _ when isLogicPointer t1 && isLogicPointer t2 ->
                let t1 = mk_logic_pointer_or_StartOf t1 in
                let t2 = mk_logic_pointer_or_StartOf t2 in
                if is_same_logic_ptr_type t1.term_type t2.term_type then
                  TBinOp (binop_of_rel op, t1, t2)
                else if
                  (op = Eq || op = Neq) &&
                  (isLogicVoidPointerType t1.term_type ||
                   isLogicVoidPointerType t2.term_type)
                then
	          TBinOp (binop_of_rel op, t1, t2)
                else if (op = Eq || op = Neq) then conditional_conversion t1 t2
                else
                  error loc "comparison of incompatible types %a and %a"
                    d_logic_type ty1 d_logic_type ty2
            | Eq | Neq -> conditional_conversion t1 t2
            | _ ->
	        error loc "comparison of incompatible types %a and %a"
                  d_logic_type ty1 d_logic_type ty2
          in expr, Ltype(C.find_logic_type Utf8_logic.boolean,[])
      | PLtrue ->
          let ctrue = C.find_logic_ctor "\\true" in
          TDataCons(ctrue,[]), Ltype(ctrue.ctor_type,[])
      | PLfalse ->
          let cfalse = C.find_logic_ctor "\\false" in
          TDataCons(cfalse,[]), Ltype(cfalse.ctor_type,[])
      | PLlambda(prms,e) ->
          let (prms, env) = add_quantifiers loc prms env in
          let e = term env e in
          Tlambda(prms,e),Larrow(List.map (fun x -> x.lv_type) prms,e.term_type)
      | PLnot t ->
          let t = type_bool_term env t in
          TUnOp(LNot,t), Ltype (C.find_logic_type Utf8_logic.boolean,[])
      | PLand (t1,t2) ->
          let t1 = type_bool_term env t1 in
          let t2 = type_bool_term env t2 in
          TBinOp(LAnd,t1,t2), Ltype (C.find_logic_type Utf8_logic.boolean,[])
      | PLor (t1,t2) ->
          let t1 = type_bool_term env t1 in
          let t2 = type_bool_term env t2 in
          TBinOp(LOr,t1,t2), Ltype (C.find_logic_type Utf8_logic.boolean,[])
      | PLtypeof t1 ->
          let t1 = term env t1 in
          Ttypeof t1, Ltype (C.find_logic_type "typetag",[])
      | PLtype ty ->
          begin match logic_type loc env ty with
            | Ctype ty -> Ttype ty, Ltype (C.find_logic_type "typetag",[])
            | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
                error loc "cannot take type tag of logic type"
          end
      | PLlet (ident, def, body) ->
          let tdef = term env def in
          (* At least for now, the type is supposed to be fully instantiated.
             No generalization is needed.
           *)
          let var = Cil_const.make_logic_info ident in
          let tdef = normalize_lambda_term env tdef in
          let args, tdef =
            match tdef.term_node with
                Tlambda(args,term) -> args, term
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
          let quants, env = add_quantifiers loc quants env in
          let t = term env t in
          let pred = Extlib.opt_map (predicate env) pred in
          Tcomprehension(t,quants,pred),
          Ltype(C.find_logic_type "set",[t.term_type])
      | PLsingleton t ->
          let t = term env t in
	  if is_set_type t.term_type then
            error loc "syntax error (set of set is not yet implemented)" ;
	  Tunion [t], (* lifting to a set can be used for non-set type *)
	  Ltype(C.find_logic_type "set",[t.term_type])
      | PLunion l ->
          fresh_type#reset();
          let init_type = visitCilLogicType (fresh_type:>cilVisitor)
            (make_set_type (Lvar "_"))
          in
          let convert_ptr,locs, typ =
            List.fold_left
              (fun (convert_ptr,locs,typ) t ->
                 let loc = term env t in
                 let convert_ptr, typ =
                   location_set_conversion
                     loc.term_loc convert_ptr loc.term_type typ env
                 in convert_ptr,loc::locs, typ)
              (false,[], init_type) l
          in
          let locs =
            if convert_ptr then List.rev_map location_to_char_ptr locs
            else List.rev locs
          in Tunion locs, typ
      | PLinter l ->
          fresh_type#reset();
          let init_type = visitCilLogicType (fresh_type:>cilVisitor)
            (make_set_type (Lvar "_"))
          in
          let convert_ptr, locs, typ =
            List.fold_left
              (fun (convert_ptr,locs,typ) t ->
                 let loc = term env t in
                 let convert_ptr, typ =
                   location_set_conversion
                     loc.term_loc convert_ptr loc.term_type typ env
                 in (convert_ptr,loc::locs, typ))
              (false,[], init_type) l
          in let locs =
            if convert_ptr then List.rev_map location_to_char_ptr locs
            else List.rev locs
          in Tinter locs, typ
      | PLempty ->
          let typ =
            fresh_type#reset();
            visitCilLogicType(fresh_type:>cilVisitor) (make_set_type (Lvar "_"))
          in
          Tempty_set,typ
      | PLrange (t1,t2) ->
          (* we allow range of floats/real.  *)
          let t1,ty1 = type_num_term_option env t1 in
          let t2,ty2 = type_num_term_option env t2 in
          (Trange(t1,t2),
           Ltype(C.find_logic_type "set", [arithmetic_conversion ty1 ty2]))
      | PLvalid _ | PLvalid_index _ | PLvalid_range _ | PLfresh _
      | PLexists _ | PLforall _  | PLimplies _ | PLiff _
      | PLxor _ | PLsubtype _ | PLseparated _ ->
          error loc "syntax error (expression expected but predicate found)"

  and term_lval f t =
    let check_lval t =
        match t.term_node with
            TLval (h,_ as lv) | TCastE(_,{term_node = TLval (h,_ as lv)})
          | Tat({term_node = TLval(h,_ as lv)},_)
            ->
              (match h with
                   TVar { lv_name = v; lv_origin = None } ->
                       error t.term_loc "not an assignable left value: %s" v
                         (* Tresult only exists when typing C functions and
                            Tmem would lead to an error earlier if applied
                            to pure logic expression.
                          *)
                 | TVar _ | TResult _ | TMem _ -> f lv t)
          | TStartOf lv | TCastE(_,{term_node = TStartOf lv})
          | Tat ({term_node = TStartOf lv}, _) ->
              f lv t
          | _ -> error t.term_loc "not a left value: %a"
              Cil.d_term t
    in
    lift_set check_lval t

  and type_logic_app env loc f labels ttl =
    (* support for overloading *)
    let infos =
      try [Lenv.find_logic_info f env]
      with Not_found ->
        C.find_all_logic_functions f in
    match infos with
      | [] -> error loc "unbound function %s" f
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
	        error loc "no such predicate or logic function %s(%a)" f
		  (Pretty_utils.pp_list ~sep:",@ " d_logic_type) tl
	    | [x] -> x
	    | _ ->
	        let tl = List.map (fun t -> t.term_type) ttl in
	        error loc "ambiguous logic call to %s(%a)" f
		  (Pretty_utils.pp_list ~sep:",@ " d_logic_type) tl

  and type_int_term env t =
    let tt = term env t in
    if not (plain_integral_type tt.term_type) then
      error t.lexpr_loc
        "integer expected but %a found" d_logic_type tt.term_type;
    tt

  and type_bool_term env t =
    let tt = term env t in
    if not (plain_boolean_type tt.term_type) then
      error t.lexpr_loc "boolean expected but %a found"
        d_logic_type tt.term_type;
    mk_cast tt (Ltype (C.find_logic_type Utf8_logic.boolean,[]))

  and type_num_term_option env t =
    match t with
        None -> None, Linteger (* Warning: should be an hybrid of integer
                                  and float. *)
      | Some t -> let t = type_num_term env t in Some t, t.term_type

  and type_num_term env t =
    let tt = term env t in
    if not (is_arithmetic_type tt.term_type) then
      error t.lexpr_loc "integer or float expected";
    tt

  (* type_arguments checks if argument list tl is well-typed for the
     formal parameter list at *)
  and type_arguments ~overloaded env loc at tl =
    let rec type_list env = function
      | [], [] -> env, []
      | et :: etl, ({term_loc=tloc} as t) :: tl ->
	  let env, _,t' = instantiate_app ~overloaded tloc t et env in
	  let env, l = type_list env (etl, tl) in env, t' :: l
      | [], _ ->
	  if overloaded then raise Not_applicable
	  else error loc "too many arguments"
      | _, [] ->
	  if overloaded then raise Not_applicable
	  else error loc "partial application"
    in
    type_list env (at, tl)

  and boolean_term_to_predicate t =
    let loc = t.term_loc in
    match unroll_type t.term_type with
        Ctype _ | Linteger | Lreal ->
	  prel ~loc (Cil_types.Rneq, t, Cil.lzero ~loc ())
      | Ltype ({lt_name = name},[]) when name = Utf8_logic.boolean ->
	  let ctrue = C.find_logic_ctor "\\true" in
	  prel ~loc
	    (Cil_types.Req,t,
             { term_node = TDataCons(ctrue,[]);
               term_loc = loc;
               term_type = Ltype(ctrue.ctor_type,[]);
               term_name = [];
             })
      | Ltype _ | Lvar _ | Larrow _ ->
	  error loc "expecting a predicate and not a term"

  and boolean_to_predicate env p0 =
    boolean_term_to_predicate (term env p0)

  and abstract_predicate env p0 =
    let loc = p0.lexpr_loc in
    match p0.lexpr_node with
        PLlambda (args,p) ->
          let (prms,env) = add_quantifiers loc args env in
          let other_prms, p = abstract_predicate env p in
          (other_prms @ prms), p
      | _ -> [], predicate env p0


  and predicate env p0 =
    let loc = p0.lexpr_loc in
    match p0.lexpr_node with
      | PLfalse -> pfalse
      | PLtrue -> ptrue
      | PLrel (t1, (Eq | Neq | Lt | Le | Gt | Ge as op), t2) ->
          let loc = loc_join t1.lexpr_loc t2.lexpr_loc in
          let t1 = term env t1 in
          let ty1 = t1.term_type in
          let t2 = term env t2 in
          let ty2 = t2.term_type in
          let type_binop = function
	    | Eq -> Cil_types.Req
	    | Neq -> Cil_types.Rneq
	    | Lt -> Cil_types.Rlt
	    | Le -> Cil_types.Rle
	    | Gt -> Cil_types.Rgt
	    | Ge -> Cil_types.Rge
          in
          let binop op tr =
            prel ~loc (op, mk_cast t1 tr, mk_cast t2 tr) in
          let conditional_conversion t1 t2 =
            let t,ty1,ty2 =
              conditional_conversion loc env t1.term_type t2.term_type
            in
            prel ~loc
              (type_binop op,
               mk_cast { t1 with term_type = ty1} t,
               mk_cast { t2 with term_type = ty2} t)
          in
          begin match op with
 	    | _ when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	        binop (type_binop op) (arithmetic_conversion ty1 ty2)
	    | Eq | Neq when isLogicPointer t1 && isLogicNull t2 ->
                let t1 = mk_logic_pointer_or_StartOf t1 in
	        prel ~loc (type_binop op, t1, mk_cast t2 ty1)
	    | Eq | Neq when isLogicPointer t2 && isLogicNull t1 ->
                let t2 = mk_logic_pointer_or_StartOf t2 in
	        prel ~loc (type_binop op, mk_cast t1 ty2, t2)
            | Eq | Neq when
                isLogicArrayType ty1 && isLogicArrayType ty2
                ->
                if is_same_logic_array_type ty1 ty2 then
                  prel ~loc (type_binop op, t1, t2)
                else
                  error loc "comparison of incompatible types %a and %a"
                    d_logic_type ty1 d_logic_type ty2
            | _ when isLogicPointer t1 && isLogicPointer t2 ->
                let t1 = mk_logic_pointer_or_StartOf t1 in
                let t2 = mk_logic_pointer_or_StartOf t2 in
                if is_same_logic_ptr_type ty1 ty2 ||
                  ((op = Eq || op = Neq) &&
                     (isLogicVoidPointerType t1.term_type ||
                        isLogicVoidPointerType t2.term_type))
                then
                  prel ~loc (type_binop op, t1, t2)
                else if (op=Eq || op = Neq) then
                  conditional_conversion t1 t2
                else
                  error loc "comparison of incompatible types: %a and %a"
                    d_logic_type t1.term_type d_logic_type t2.term_type
            | Eq | Neq -> conditional_conversion t1 t2
	    | _ ->
	        error loc "comparison of incompatible types: %a and %a"
                  d_logic_type t1.term_type d_logic_type t2.term_type
          end
      | PLand (p1, p2) ->
          pand ~loc:p0.lexpr_loc (predicate env p1, predicate env p2)
      | PLor (p1, p2) ->
          por ~loc:p0.lexpr_loc (predicate env p1, predicate env p2)
      | PLxor (p1, p2) ->
          pxor ~loc:p0.lexpr_loc (predicate env p1, predicate env p2)
      | PLimplies (p1, p2) ->
          pimplies ~loc:p0.lexpr_loc (predicate env p1, predicate env p2)
      | PLiff (p1, p2) ->
          piff ~loc:p0.lexpr_loc (predicate env p1, predicate env p2)
      | PLnot p ->
          (match (predicate env p) with
	     | {content = Prel (Cil_types.Rneq, t, z)} when isLogicZero z ->
	         prel ~loc:p0.lexpr_loc (Cil_types.Req, t, Cil.lzero ~loc ())
	     | p -> pnot ~loc:p0.lexpr_loc p)
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
	      | None ->
	          papp ~loc:p0.lexpr_loc (info, label_assoc, tl)
          end

      | PLif (t, p1, p2) ->
          begin try
            let t = type_bool_term env t in
            pif ~loc:p0.lexpr_loc (t, predicate env p1, predicate env p2)
          with _ ->
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
          let q, env' = add_quantifiers p0.lexpr_loc q env in
          pforall ~loc:p0.lexpr_loc (q, predicate env' p)
      | PLexists (q, p) ->
          let q, env' = add_quantifiers p0.lexpr_loc q env in
          pexists ~loc:p0.lexpr_loc (q, predicate env' p)
      | PLfresh (t) ->
          if not (Lenv.is_post_state env) then
            error loc "\\fresh can only be used in a post-condition";
          let tloc = t.lexpr_loc in
          let t = term env t in
          if isLogicPointerType t.term_type then pfresh ~loc:p0.lexpr_loc (t)
          else error tloc "subscripted value is not a pointer"
      | PLvalid (t) ->
          check_current_label loc env;
          let loc = t.lexpr_loc in
          let t = term env t in
          let t = mk_logic_pointer_or_StartOf t in
          check_non_void_ptr loc t.term_type;
          pvalid ~loc:p0.lexpr_loc t
      | PLvalid_index (t,a) ->
          check_current_label loc env;
          let t = term env t in
          let a = type_int_term env a in
          let t = mk_logic_pointer_or_StartOf t in
          check_non_void_ptr t.term_loc t.term_type;
          pvalid_index ~loc:p0.lexpr_loc (t,a)
      | PLvalid_range (t,a,b) ->
          check_current_label loc env;
          let t = term env t in
          let a = type_int_term env a in
          let b = type_int_term env b in
          let t = mk_logic_pointer_or_StartOf t in
          check_non_void_ptr t.term_loc t.term_type;
          pvalid_range ~loc:p0.lexpr_loc (t,a,b)
      | PLold p ->
          let lab = find_old_label p0.lexpr_loc env in
          let env = Lenv.set_current_logic_label lab env in
          (* could be Tat(t,lab) *)
          pold ~loc:p0.lexpr_loc (predicate env p)
      | PLat (p, l) ->
          let lab = find_logic_label p0.lexpr_loc env l in
          let env = Lenv.set_current_logic_label lab env in
          pat ~loc:p0.lexpr_loc (predicate env p, lab)
      | PLvar x ->
	  (try
	     let def = C.find_macro x 
	     in predicate env def
	   with Not_found ->
             let loc = p0.lexpr_loc in
               (* TODO: accept a predicate with arguments here (see terms) *)
               (try
		  let info =
		    List.find
                      (fun x -> x.l_profile = []) (C.find_all_logic_functions x)
		  in
		    match info.l_type with
		      | None ->
			  let labels = match info.l_labels with
			      [] -> []
			    | [l] -> [l,find_current_label loc env]
			    | _ ->
				error loc
				  "%s labels must be explicitely instantiated" x
			  in
			    papp ~loc (info,labels,[])
		      | Some _ -> boolean_to_predicate env p0
		with Not_found -> boolean_to_predicate env p0))
      | PLlet(x,def,body) ->
          let typ, args, tdef =
            try
              let tdef = term env def in
              let tdef = normalize_lambda_term env tdef in
              (match tdef.term_node with
                   Tlambda(args,t) -> Some t.term_type, args, LBterm t
                 | _ -> Some tdef.term_type,[], LBterm tdef)
            with Log.AbortError _ ->
              let args, tdef = abstract_predicate env def in
              None, args, LBpred tdef
          in
          let var = Cil_const.make_logic_info x in
          var.l_profile <- args;
          var.l_var_info.lv_type <-
            (match typ with
                 None -> Ctype (Cil.voidType)
               | Some t -> t);
          var.l_type <- typ;
          var.l_body <- tdef;
          let env = Lenv.add_logic_info x var env in
          let tbody = predicate env body in
          { name = []; loc = p0.lexpr_loc;
            content = Plet(var,tbody) }
      | PLcast _ | PLblock_length _ | PLbase_addr _ | PLarrget _ | PLarrow _
      | PLdot _ | PLbinop _ | PLunop _ | PLconstant _
      | PLnull | PLresult | PLcoercion _ | PLcoercionE _ | PLsizeof _
      | PLsizeofE _ | PLlambda _
      | PLupdate _ | PLinitIndex _ | PLinitField _
      | PLtypeof _ | PLtype _ -> boolean_to_predicate env p0
      | PLrange _ ->
          error p0.lexpr_loc "cannot use operator .. within a predicate"
      | PLnamed (n, p) ->
          let p = predicate env p in { p with name = n::p.name }
      | PLsubtype (t,tc) ->
          let t = term env t in
          let tc = term env tc in
          psubtype ~loc:p0.lexpr_loc (t,tc)
      | PLseparated seps ->
          let type_loc loc =
            let res = term env loc in
            let res = mk_logic_pointer_or_StartOf res in
            check_non_void_ptr res.term_loc res.term_type;
            res
          in
          let seps = List.map type_loc seps in
          pseparated ~loc:p0.lexpr_loc seps
      | PLcomprehension _ | PLsingleton _ | PLunion _ | PLinter _ | PLempty ->
          error p0.lexpr_loc "expecting a predicate and not tsets"

  let type_variant env = function
    | (t, None) -> (type_int_term env t, None)
    | (t, r) -> (term env t, r)

  (* checks if the given offset points to a location inside a formal. *)
  let is_substructure off =
    let rec aux is_array_field off =
      match off with
          TNoOffset -> true
        | TField (f,o) -> aux (Cil.isArrayType f.ftype) o
        | TIndex(_,o) -> 
          (* if we are in the an array field, the element is still part of
             the structure. Otherwise, this is an index to a memory cell 
             outside of the current region.
           *)
          is_array_field && aux is_array_field o
       (* The formal is never an array by definition: 
          start recursion with false. *)
    in aux false off

  let term_lval_assignable ~accept_formal env t =
    let f t =
      if isLogicArrayType t.term_type then
        error t.term_loc "not an assignable left value: %a" d_term t
      else begin
        match t.term_node with
          | Tapp _ -> t (* allow to use footprint functions in assigns. *)
          | _ ->
              term_lval
                (fun _ t ->
                   match t.term_node with
                       TStartOf lv | TCastE(_,{ term_node = TStartOf lv}) ->
                         error t.term_loc "not an assignable left value: %a"
                           Cil.d_term_lval lv
                     | TLval (TVar v, o) when not accept_formal ->
                       (match v.lv_origin with
                           None -> t
                         | Some v ->
                           if v.vformal && is_substructure o then
                             error t.term_loc 
                               "can not assign part of a formal parameter: %a"
                               Cil.d_term t
                           else t)
                     | _ -> t
                )
                t
      end
    in lift_set f (term env t)

  let type_from ~accept_formal env (l,d) =
    (* Yannick: [assigns *\at(\result,Post)] should be allowed *)
    let tl = 
      term_lval_assignable ~accept_formal env l
    in
    let tl = Logic_const.new_identified_term tl in
    match d with
        FromAny -> (tl,FromAny)
      | From f ->

        let tf = 
          List.map (term_lval_assignable ~accept_formal:true env) f 
        in
        let tf = 
          List.map
            (fun td -> 
              if Logic_utils.contains_result td then
                error td.term_loc "invalid \\result in dependencies";
            Logic_const.new_identified_term td)
            tf
        in
        (tl, From tf)

  let type_assign ~accept_formal env a =
    match a with
        WritesAny -> WritesAny
      | Writes l -> Writes (List.map (type_from ~accept_formal env) l)

  let id_predicate env pred = Logic_const.new_predicate (predicate env pred)

  let loop_pragma env = function
    | Unroll_level t -> (Unroll_level (term env t))
    | Widen_hints l -> (Widen_hints (List.map (term env) l))
    | Widen_variables l -> (Widen_variables (List.map (term env) l))

  let type_annot loc ti =
    let env = append_here_label (Lenv.empty()) in
    let this_type = logic_type loc env ti.this_type in
    let v = Cil_const.make_logic_var ti.this_name this_type in
    let env = Lenv.add_var ti.this_name v env in
    let body = predicate env ti.inv in
    let infos = Cil_const.make_logic_info ti.inv_name in
    infos.l_profile <- [v];
    infos.l_labels <- [Logic_const.here_label];
    infos.l_body <- LBpred body;
    C.add_logic_function infos; infos

  let check_behavior_names loc existing_behaviors names =
    List.iter
      (fun x -> if not (List.mem x existing_behaviors) then
         error loc "reference to unknown behavior %s" x) names

  let check_unique_behavior_names loc old_behaviors behaviors =
    List.fold_left
      (fun names b ->
         if b.b_name = Cil.default_behavior_name then names
         else begin
           if (List.mem b.b_name names) then
             error loc "behavior %s already defined" b.b_name ;
	   b.b_name::names
         end)
      old_behaviors
      behaviors

  let type_extended ~typing_context ~loc behavior extensions =
    List.iter
      (fun (name,_,ps) ->
         let loc = match ps with
	   | [] -> loc
	   | p::_ -> p.lexpr_loc in
         Extensions.typer name ~typing_context ~loc  behavior ps)
      extensions



  let type_spec old_behaviors loc is_stmt_contract result env s =
    let env = append_here_label env in
    let env_with_result = add_result env result in
    (* assigns_env is a bit special:
       - both \result and \exit_status (in a \at(_,Post) term are admissible)
       - Old and Post labels are admissible
       - Default label is Old (Assigns are evaluated in Pre-state
     *)

    let assigns_env = append_old_and_post_labels env_with_result in
    let old = Lenv.find_logic_label "Old" assigns_env in
    let assigns_env =
      Lenv.set_current_logic_label old assigns_env in
    let assigns_env =
      Lenv.exit_post_state (Lenv.enter_post_state assigns_env Exits)
    in
    let post_state_env k =
      let env = match k with
        | Returns -> env_with_result
        | Normal when is_stmt_contract -> env
        | Normal -> env_with_result
        | Exits | Breaks | Continues -> env
      in
      Lenv.enter_post_state (append_old_and_post_labels env) k
    in
    let rec full_post_state_env l =
      match l with
      | [] -> env
      | [x] -> post_state_env x
      | (Returns|Normal)::r ->
          add_result (full_post_state_env r) result
      | (Exits|Breaks|Continues)::r ->
          Lenv.enter_post_state (full_post_state_env r) Exits
    in

    let b = List.map
      (fun {b_assigns= ba; b_name = bn; b_post_cond=be; b_assumes= bas;
	    b_requires=br; b_extended=bext} ->
         let result = 
           { b_assigns= 
               type_assign ~accept_formal:is_stmt_contract assigns_env ba;
             b_name = bn;
             b_post_cond =
               List.map
                 (fun (k,p)->
                   let p' = id_predicate (post_state_env k) p in (k,p')) be;
             b_assumes= List.map (id_predicate env) bas;
             b_requires= List.map (id_predicate env) br;
             b_extended= []}
         in
	 let typing_context = make_typing_context
	   ~pre_state:env
           ~post_state:full_post_state_env
           ~assigns_env:assigns_env
           ~type_predicate:predicate
           ~type_term:term
           ~type_assigns:type_assign
         in
         type_extended
	   ~typing_context
           ~loc
           result
           bext;
         result)
      s.spec_behavior
    in
    let none_for_stmt_contract clause = function
      | None -> None
      | (Some _) as x ->
	  if is_stmt_contract then
	    error loc "%s clause isn't allowed into statement contract" clause;
	  x
    in
    let v = Extlib.opt_map (type_variant env)
      (none_for_stmt_contract "decreases" s.spec_variant) in
    let t = Extlib.opt_map (id_predicate env)
      (none_for_stmt_contract "terminates" s.spec_terminates) in
    let bnames = old_behaviors @ (check_unique_behavior_names loc [] b) in
    List.iter (check_behavior_names loc bnames) s.spec_complete_behaviors;
    List.iter (check_behavior_names loc bnames) s.spec_disjoint_behaviors;
    { spec_behavior = b;
      spec_variant = v;
      spec_terminates = t;
      spec_complete_behaviors = s.spec_complete_behaviors;
      spec_disjoint_behaviors = s.spec_disjoint_behaviors;
    }

  let funspec old_behaviors vi formals typ s =
    let env = append_pre_label ~pre_is_old:true (Lenv.funspec()) in
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
      SPexpr t -> SPexpr (term env t)
    | (SPctrl | SPstmt) as sp -> sp

  let impact_pragma env = function
      IPexpr t -> IPexpr (term env t)
    | IPstmt as ip -> ip

  let code_annot_env () =
    append_here_label (append_pre_label ~pre_is_old:false (Lenv.empty()))

  let code_annot loc current_behaviors current_return_type ca =
    let annot = match ca with
      | AAssert (behav,p) ->
          check_behavior_names loc current_behaviors behav;
          AAssert (behav,predicate (code_annot_env()) p)
      | APragma (Impact_pragma sp) ->
	  APragma (Impact_pragma (impact_pragma (code_annot_env()) sp))
      | APragma (Slice_pragma sp) ->
	  APragma (Slice_pragma (slice_pragma (code_annot_env()) sp))
      | APragma (Loop_pragma lp) ->
	  APragma (Loop_pragma (loop_pragma (code_annot_env()) lp))
      | AStmtSpec s ->
          (* function behaviors and statement behaviors are not at the
             same level. Do not mix them in a complete or disjoint clause
             here.
           *)
          let env = append_pre_label ~pre_is_old:false (Lenv.empty()) in
          let my_spec =
            type_spec [] loc true current_return_type env s
          in
          ignore
            (check_unique_behavior_names
               loc current_behaviors my_spec.spec_behavior);
	  AStmtSpec my_spec
      | AVariant v -> AVariant (type_variant (code_annot_env ()) v)
      | AInvariant (behav,f,i) ->
          check_behavior_names loc current_behaviors behav;
          AInvariant (behav,f,predicate (code_annot_env()) i)
      | AAssigns (behav,a) -> 
        AAssigns (behav,type_assign ~accept_formal:true (code_annot_env()) a)
    in Logic_const.new_code_annotation annot

  let formals loc env p =
    let add_var (p,env) (t,x) =
      let lt = logic_type loc env t in
      let var = Cil_const.make_logic_var x lt in
      (var::p, Lenv.add_var x var env)
    in
    let (p,env) = List.fold_left add_var ([],env) p in
    List.rev p, env

  let init_type_variables loc l =
    List.fold_left
      (fun env x ->
         try
           ignore (Lenv.find_type_var x env);
           error loc "duplicated type variable in annotation"
         with Not_found -> Lenv.add_type_var x (Lvar x) env)
      (Lenv.empty()) l

  (* checks whether all the type variable contained in the return type t of
     a logic function are bound in a parameter's type
     (p being the list of formals). type-checking error otherwise
   *)
  let check_polymorphism loc ?return_type p =
    let obj known_vars =
      let update_known_vars s =
	known_vars:= Datatype.String.Set.add s !known_vars
      in object inherit Cil.nopCilVisitor
                method vlogic_type = function
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
      error loc "some type variable appears only in the return type. \
               All type variables need to occur also in the parameters types."

  let annot_env loc labels poly =
    let env = init_type_variables loc poly in
    let labels,env =
      List.fold_right
        (fun l (labs,e) ->
	   try
	     let _ = Lenv.find_logic_label l e in
	     error loc "multiply defined label `%s'" l
	   with Not_found ->
	     let lab = LogicLabel (None, l) in
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
      | Some t -> Some (logic_type loc env t)
    in
    let p, env = formals loc env p in
    check_polymorphism loc ?return_type:t p;
    let info = Cil_const.make_logic_info f in
    (* Should we add implicitely a default label for the declaration? *)
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
    begin
      C.add_logic_function info;
      env,info
    end

  let type_datacons loc env type_info (name,params) =
    let tparams = List.map (logic_type loc env) params in
    let my_info =
      { ctor_name = name;
        ctor_type = type_info;
        ctor_params = tparams
      }
    in
    C.add_logic_ctor name my_info;
    my_info

  let typedef loc env my_info = function
    | TDsum cons -> LTsum (List.map (type_datacons loc env my_info) cons)
    | TDsyn typ -> LTsyn (logic_type loc env typ)

  let rec annot a =
    let loc = a.decl_loc in
    Cil.CurrentLoc.set loc;
    match a.decl_node with
      | LDlogic_reads (f, labels, poly, t, p, l) ->
	  let env,info = logic_decl loc f labels poly ~return_type:t p in
          info.l_body <-
            (match l with
               | Some l ->
	           let l =
                     List.map (fun x -> new_identified_term (term env x)) l
                   in
	           LBreads l
              | None -> LBnone);
          Dfun_or_pred (info,loc)
      | LDpredicate_reads (f, labels, poly, p, l) ->
	  let env,info = logic_decl loc f labels poly p in
          info.l_body <-
	    (match l with
               | Some l ->
                   let l = 
                     List.map (fun x -> new_identified_term (term env x)) l
                   in
	           LBreads l
               | None -> LBnone);
          Dfun_or_pred (info,loc)
      | LDlogic_def(f, labels, poly,t,p,e) ->
	  let env,info = logic_decl loc f labels poly ~return_type:t p in
	  let redefinition = false in
	  let rt = match info.l_type with
	    | None -> assert false
	    | Some t -> t
	  in
          (try
             let e = term env e in
             let _,new_typ,new_term =
               instantiate_app ~overloaded:false loc e rt env in
             if is_same_type new_typ rt then begin
               info.l_body <- LBterm new_term;
               Dfun_or_pred (info,loc)
             end else
               error loc
                 "return type of logic function %s is %a but %a was expected"
                 f d_logic_type new_typ d_logic_type rt
           with e when not redefinition ->
             C.remove_logic_function f; raise e)
      | LDpredicate_def (f, labels, poly, p, e) ->
	  let env,info = logic_decl loc f labels poly p in
	  let e = predicate env e in
          (match !Lenv.default_label with
               None -> ()
             | Some lab -> info.l_labels <- [lab]);
          info.l_body <- LBpred e;
	  Dfun_or_pred (info,loc)
      | LDinductive_def (f, labels, poly, p, indcases) ->
	  let _env,info = logic_decl loc f labels poly p in
	  (* env is ignored: because params names are indeed useless...*)
          let l =
	    List.map
	      (fun (id,labels,poly,e) ->
                 let labels,env = annot_env loc labels poly in
                 let p = predicate env e in
                 let labels = match !Lenv.default_label with
                     None -> labels
                   | Some lab -> [lab]
                 in (id, labels, poly, p))
              indcases
	  in
          info.l_body <- LBinductive l;
	  Dfun_or_pred (info,loc)
      | LDaxiomatic(id,decls) ->
          (*
	    Format.eprintf "Typing axiomatic %s@." id;
           *)
	  let l = List.map annot decls in
	  Daxiomatic(id,l,loc)

      | LDtype(s,l,def) ->

          let env = init_type_variables loc l in
          let my_info =
            { lt_name = s;
              lt_params = l;
              lt_def = None; (* will be updated later *)
            }
          in
          C.add_logic_type s my_info;
          (try
             let tdef =
               Extlib.opt_map (typedef loc env my_info) def
             in
             my_info.lt_def <- tdef;
             Dtype (my_info,loc)
           with e ->
             (* clean up the env in case we are in continue mode *)
             C.remove_logic_type s;
             Cilutil.opt_iter
               (function
                    TDsum cons ->
                      List.iter
                        (fun (name,_) -> C.remove_logic_ctor name) cons
                  | TDsyn _ -> ())
               def;
             raise e)
      | LDlemma (is_axiom, x, labels, poly, e) ->
          let labels,env = annot_env loc labels poly in
          let p = predicate env e in
          let labels = match !Lenv.default_label with
            | None -> labels
            | Some lab -> [lab]
          in
          Dlemma (is_axiom, x, labels, poly,  p, loc)
      | LDinvariant (s, e) ->
          let env = append_here_label (Lenv.empty()) in
          let p = predicate env e in
          let li = Cil_const.make_logic_info s in
	  li.l_labels <- [Logic_const.here_label];
          li.l_body <- LBpred p;
          C.add_logic_function li;
	  Dinvariant (li,loc)
      | LDtype_annot l ->
          Dtype_annot (type_annot loc l,loc)
      | LDvolatile (_tsets, (_rd_opt, _wr_opt)) ->
          error loc "Volatile clauses are not yet implemented"

end

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
