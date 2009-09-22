(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
    ("Error during annotations analysis: " ^^ fstring)

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

(* Logical environments *)

module Lenv = struct
(* locals: logic variables (e.g. quantified variables in \forall, \exists) *)

module Smap = Map.Make(String)

type t = {
  local_vars: Cil_types.logic_var Smap.t;
  type_vars: Cil_types.logic_type Smap.t;
  logic_labels: Cil_types.logic_label Smap.t;
  current_logic_label: Cil_types.logic_label option;
  is_post_state: bool
}

let empty = {
  local_vars = Smap.empty;
  type_vars = Smap.empty;
  logic_labels = Smap.empty;
  current_logic_label = None;
  is_post_state = false
}
let enter_post_state env = { env with is_post_state = true }

let exit_post_state env = { env with is_post_state = false }

let add_var v var env =
  { env with local_vars = Smap.add v var env.local_vars }
let find_var v env = Smap.find v env.local_vars
let add_type_var v typ env =
  { env with type_vars = Smap.add v typ env.type_vars }
let find_type_var v env = Smap.find v env.type_vars

(* logic labels *)
let add_logic_label l lab env =
  { env with logic_labels = Smap.add l lab env.logic_labels }
let find_logic_label l env = Smap.find l env.logic_labels

let set_current_logic_label lab env =
  let env =
    { env with current_logic_label = Some lab }
  in match lab with
      LogicLabel "Post" -> enter_post_state env
    | LogicLabel ("Pre" | "Old") | StmtLabel _ -> exit_post_state env
    | LogicLabel "Here" -> env
    | LogicLabel _ -> exit_post_state env
end

let append_here_label env =
  let env = Lenv.add_logic_label "Here" (LogicLabel "Here") env in
  Lenv.set_current_logic_label (LogicLabel "Here") env

let make_pre_label () = Lenv.add_logic_label "Pre" (LogicLabel "Pre") Lenv.empty

let append_old_and_post_labels env =
  Lenv.add_logic_label "Post" (LogicLabel "Post")
    (Lenv.add_logic_label "Old" (LogicLabel "Old") env)

let append_post_label env =
  Lenv.add_logic_label "Post" (LogicLabel "Post") env

let make_here_label () =
  append_here_label Lenv.empty

module Make
  (C :
    sig
      val annonCompFieldName : string
      val conditionalConversion : typ -> typ -> typ
      val find_var : string -> logic_var
      val find_enum_tag : string -> exp * typ
      val find_comp_type : kind:string -> string -> typ
      val find_type : string -> typ
      val find_label : string -> stmt ref
      val remove_logic_function : string -> unit
      val remove_logic_type: string -> unit
      val remove_logic_ctor: string -> unit
      val add_logic_function: logic_info -> unit
      val add_logic_type: string -> logic_type_info -> unit
      val add_logic_ctor: string -> logic_ctor_info -> unit

(*
      val find_logic_function: string -> logic_info
*)
      val find_all_logic_functions: string -> logic_info list
      val find_logic_type: string -> logic_type_info
      val find_logic_ctor: string -> logic_ctor_info

    end) =
struct

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
      | TComp (comp, _, _) ->
	  let rec search = function
	    | [] ->
                raise Exit
	    | fid :: _ when fid.fname = f ->
		TField(fid, TNoOffset), Ctype fid.ftype
	    | fid :: rest
		when prefix C.annonCompFieldName fid.fname ->
		  begin match Cil.unrollType fid.ftype with
		    | TComp (ci, _, _) ->
			(try
			   let off, t = search ci.cfields in
			   TField (fid, off), t
			 with Exit -> search rest (* Continue searching *))
		    | _ ->
                        error loc "unnamed field type is not a struct/union"
		  end
	    | _ :: rest ->
		search rest
	  in
	  (try search comp.cfields
	   with Exit -> error loc "cannot find field %s" f)
      | _ ->
	  error loc "expected a struct with field %s" f
      end
  | _ ->
      error loc "expected a struct with field %s" f

let type_of_field loc f = function
  | Ltype ({lt_name = "set"} as lt,[t]) ->
      let offs,typ = plain_type_of_field loc f t in offs, Ltype(lt,[typ])
  | t -> plain_type_of_field loc f t

let unroll_type = function
  | Ctype ty -> Ctype (Cil.unrollType ty)
  | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ as ty  -> ty

let array_to_ptr ty =
  match unroll_type ty with
      Ctype(TArray(ty,lo,_,attr)) ->
        let rec aux = function
            TArray(ty,lo,s,attr) -> TArray(aux ty,lo,s,attr)
          | ty -> Cil.typeAddAttributes attr ty
        in
        let length_attr =
          match lo with
              None -> []
            | Some l -> begin
                try
                  let la = Cil.expToAttrParam l in
                  [ Attr("arraylen",[la])]
                with Cil.NotAnAttrParam _ ->
                  Cil.warning
                    "Cannot represent the length of array as an attribute";
                  []
              end
        in
        Ctype(TPtr(aux ty,length_attr))
    | ty -> ty

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
  | LTarray ty -> Ctype (TPtr (c_logic_type loc env ty, []))
  | LTpointer ty -> Ctype (TPtr (c_logic_type loc env ty, []))
  | LTenum e ->
      (try Ctype (C.find_comp_type "enum" e)
       with Not_found -> error loc "no such enum")
  | LTstruct s ->
      (try Ctype (C.find_comp_type "struct" s)
       with Not_found -> error loc "no such structure")
  | LTunion u ->
      (try Ctype (C.find_comp_type "union" u)
       with Not_found -> error loc "no such union")
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
             error loc "no such named type")
  | LTnamed(id,l) ->
      (try
         let info = C.find_logic_type id in
         if List.length info.lt_params <> List.length l then
           error loc "wrong number of parameter for type %s" id
         else Ltype (info,List.map (logic_type loc env) l)
       with Not_found ->
         error loc "no such named type")
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

let rec type_of_pointed = function
    Ctype ty -> Ctype (Cil.typeOf_pointed ty)
  | Ltype ({lt_name = "set"} as lt,[t]) ->
      Ltype(lt,[type_of_pointed t])
  | t ->
      error Cilutil.locUnknown "type %a is not a pointer type" d_logic_type t

let plain_mk_mem ?loc t ofs = match t.term_node with
  | TAddrOf lv -> add_offset_lval ofs lv
  | TStartOf lv -> add_offset_lval (TIndex (Cil.lzero ?loc (), ofs)) lv
  | _ -> TMem t, ofs

let mk_mem ?loc t ofs =
  lift_set
    (fun t -> term ?loc (TLval (plain_mk_mem ?loc t ofs))
       (type_of_pointed t.term_type))
    t

let plain_arithmetic_type = function
  | Ctype ty -> Cil.isArithmeticType ty
  | Linteger | Lreal -> true
  | Ltype _ | Lvar _ | Larrow _ -> false

let plain_pointer_type = function
  | Ctype ty -> Cil.isPointerType ty
  | Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ -> false

let plain_integral_type = function
  | Ctype ty -> Cil.isIntegralType ty
  | Linteger -> true
  | Ltype _ | Lreal | Lvar _ | Larrow _ -> false

let plain_boolean_type = function
  | Ctype ty -> isIntegralType ty
  | Linteger | Ltype ({lt_name = "boolean"},[]) -> true
  | Lreal | Ltype _ | Lvar _ | Larrow _ -> false

let plain_non_void_ptr loc typ =
  match unroll_type typ with
      Ctype (TPtr(ty,_) | TArray(ty,_,_,_)) ->
        not (Cil.isVoidType ty)
    | _ -> error loc "not a pointer or array type"

let is_arithmetic_type = plain_or_set plain_arithmetic_type

let is_pointer_type = plain_or_set plain_pointer_type

let is_integral_type = plain_or_set plain_integral_type

(* can we have sets of boolean as for now ? *)
let is_boolean_type = plain_or_set plain_boolean_type

let is_non_void_ptr loc = plain_or_set (plain_non_void_ptr loc)

let check_non_void_ptr loc typ =
  if not (is_non_void_ptr loc typ) then error loc "expecting a non-void pointer"

let is_set_type = function
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
	  (* Do not mark arrays as having their address taken. *)
	  begin match vi.lv_origin with None -> () | Some vi ->
	    if not (isArrayType vi.vtype) then
	      vi.vaddrof <- true
	  end
      | _ -> ())
  | TIndex _ -> ()
  | TField(fi,_) -> fi.faddrof <- true
  end;
  mk_AddrOf lval t

(* compute type signature and removes unnecessary attributes *)
let type_sig_logic ty =
  let doattr = Cil.dropAttributes ["const"; "restrict"; "declspec" ] in
  typeSigWithAttrs doattr ty

let c_mk_cast e oldt newt =
  if Cilutil.equals (type_sig_logic oldt) (type_sig_logic newt) then begin
    e
  end else begin
    (* Watch out for constants *)
    if isPointerType newt && isLogicNull e && not (isLogicZero e) then e
    else
      match newt, e.term_node with
        | TInt(newik, []), TConst (CInt64(i, _, _)) ->
	    { e with term_node = TConst (CInt64 (i, newik, None)) }
        | _ ->
            { e with term_node = TCastE (newt, e); term_type = Ctype newt }
  end

let is_same_ptr_type ctyp1 ctyp2 =
  (isArrayType ctyp1 || isPointerType ctyp1) &&
    (isArrayType ctyp2 || isPointerType ctyp2) &&
    (Cilutil.equals (type_sig_logic (typeOf_pointed ctyp1))
       (type_sig_logic (typeOf_pointed ctyp2)))

let is_same_logic_ptr_type ty1 ty2 =
  match (ty1,ty2) with
      Ctype t1, Ctype t2 -> is_same_ptr_type t1 t2
    | _ -> false

let is_implicit_pointer_conversion term ctyp1 ctyp2 =
  (isArrayType ctyp1 || isPointerType ctyp1) &&
    (isVoidPtrType ctyp2 ||
       ((isArrayType ctyp2 || isPointerType ctyp2) &&
          (Cilutil.equals
             (type_sig_logic (typeOf_pointed ctyp1))
             (type_sig_logic (typeOf_pointed ctyp2))
           || isLogicNull term)))

let rec mk_cast e newt =
  let loc = e.term_loc in
  if is_same_type e.term_type newt then e
  else begin
    match e.term_type, newt with
      | Ctype oldt, Ctype newt ->
          c_mk_cast e oldt newt
      | t1, Ltype ({lt_name = "boolean"},[]) when is_integral_type t1 ->
          { e with
              term_node = TBinOp(Cil_types.Ne,e,lzero ~loc ());
              term_type = Ltype(C.find_logic_type "boolean",[]) }
      | Ltype({lt_name = "set"},[ty1]), Ltype({lt_name="set"},[ty2]) ->
          let e = mk_cast {e with term_type = ty1} ty2 in
          { e with term_type = make_set_type e.term_type}
      | ty1 , Ltype({lt_name =  "set"},[ ty2 ]) ->
          let e = mk_cast e ty2 in
          { e with term_type = make_set_type ty1}
      | Linteger, Linteger | Lreal, Lreal -> e
      | Linteger, Ctype t when is_pointer_type newt && isLogicNull e ->
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
      | TComp (comp1, _, _), TComp (comp2, _, _) when comp1.ckey = comp2.ckey ->
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
  match ot,nt with
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
        (try let ot = Lenv.find_type_var s1 env in partial_unif ~overloaded loc ot nt env
         with Not_found ->
           Lenv.add_type_var s1 nt env, nt, nt)
    | _, Lvar s2 when generated_var s2 ->
        (try
           let nt = Lenv.find_type_var s2 env in partial_unif ~overloaded loc ot nt env
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
        in env, Larrow(args1,rt1), Larrow(args2,rt2)
    | t1, Ltype ({lt_name = "set"},[t2]) ->
        let (env,ot,nt) = partial_unif ~overloaded loc t1 t2 env in
        env, make_set_type ot, make_set_type nt
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
  when overloaded is true, raise exception Not_applicable if conversion not possible,
  otherwise print an error message with location [loc]
*)
let rec implicit_conversion ~overloaded loc oterm ot nt =
  match ot, nt with
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
                  f1 <= f2 (*[BM] relies on internal representation of OCaml constant
                             constructors.*)
              | _ -> false)
        then
          let t,e = c_cast_to ty1 ty2 oterm in Ctype t, e
        else
	  if overloaded then raise Not_applicable
	  else
            error loc "invalid implicit conversion from %a to %a"
              d_type ty1 d_type ty2
  | Ctype ty, Linteger when Cil.isIntegralType ty -> Linteger, oterm
  | Ctype ty, Lreal when Cil.isArithmeticType ty -> Lreal, oterm
  | Linteger, Lreal when not overloaded -> Lreal, oterm
   (* Integer 0 is also a valid pointer. *)
  | Linteger, Ctype ty when Cil.isPointerType ty && isLogicNull oterm ->
      nt, { oterm with
              term_node = TCastE(ty,oterm);
              term_type = nt }
  (* can convert implicitly a singleton into a set, but not the reverse. *)
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
      implicit_conversion ~overloaded:true Cilutil.locUnknown t1 t1.term_type t2.term_type
    in true
  with Not_applicable -> false

let filter_non_minimal_arguments l ((_,_,tl,_) as p) =
  let rec aux acc l =
    match l with
      | [] -> p::acc
      | ((_,_,tl',_) as p')::r ->
	  if List.for_all2 convertible tl tl' then
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




let rec logic_arithmetic_promotion = function
  | Ctype ty when Cil.isIntegralType ty -> Linteger
  | Linteger -> Linteger
  | Lreal -> Lreal
  | Ctype ty ->
      (match Cil.unrollType ty with TFloat _ -> Lreal | _ -> assert false)
  | Ltype ({lt_name="set"} as lt,[t]) ->
      Ltype(lt,[logic_arithmetic_promotion t])
  | Ltype _ | Lvar _ | Larrow _ -> assert false

let rec integral_promotion = function
  | Ctype ty when isIntegralType ty ->
      Linteger
  | Linteger -> Linteger
  | Ltype ({lt_name="set"} as lt,[t]) -> Ltype(lt,[integral_promotion t])
  | Ltype _ | Lreal | Lvar _ | Larrow _ | Ctype _ -> assert false

let rec arithmetic_conversion ty1 ty2 =
match ty1, ty2 with
| Ctype ty1, Ctype ty2 ->
    if isIntegralType ty1 && isIntegralType ty2
    then Linteger
    else Lreal
| (Linteger, Ctype t | Ctype t, Linteger) when isIntegralType t -> Linteger
| (Linteger, Ctype t | Ctype t , Linteger) when isArithmeticType t-> Lreal
| (Lreal, Ctype ty | Ctype ty, Lreal) when isArithmeticType ty -> Lreal
| Linteger, Linteger -> Linteger
| (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
| Ltype ({lt_name="set"} as lt,[t1]),t2
| t1,Ltype ({lt_name="set"} as lt,[t2]) ->
    Ltype(lt,[arithmetic_conversion t1 t2])
| _ -> assert false

(*
let rec conditional_conversion loc t1 t2 = match t1, t2 with
| Ctype ty1, Ctype ty2 ->
    if isIntegralType ty1 && isIntegralType ty2 then
      Linteger
    else if isArithmeticType ty1 && isArithmeticType ty2 then
      Lreal
    else if is_same_ptr_type ty1 ty2 then
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
| (Ltype({lt_name = "boolean"},[]), t | t, Ltype({lt_name = "boolean"},[]))
        when is_integral_type t ->
    Ltype(C.find_logic_type "boolean",[])
| Lreal, Ctype ty | Ctype ty, Lreal ->
    (match Cil.unrollType ty with
       TFloat _ -> Lreal
     | _ -> error loc "types %a and %a are not convertible"
         d_logic_type t1 d_logic_type t2)
| Ltype (({lt_name = "set"} as lt),[t1]), t2
| t1, Ltype (({lt_name = "set"} as lt),[t2]) ->
    Ltype(lt,[conditional_conversion loc t1 t2])
| Ltype (s1,l1), Ltype (s2,l2)  when s1.lt_name = s2.lt_name &&
    List.for_all2 is_same_type l1 l2 -> t1
| Lvar s1, Lvar s2 when s1 = s2 -> t1
| Linteger, Linteger -> Linteger
| (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
| _ ->
    error loc "types %a and %a are not convertible"
      d_logic_type t1 d_logic_type t2
    *)

let conditional_conversion loc env t1 t2 =
  let _, t1, t2 = partial_unif ~overloaded:false loc t1 t2 env in
  let rt =
    match t1, t2 with
      | Ctype ty1, Ctype ty2 ->
          if isIntegralType ty1 && isIntegralType ty2 then
            Linteger
          else if isArithmeticType ty1 && isArithmeticType ty2 then
            Lreal
          else if is_same_ptr_type ty1 ty2 then
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
      | (Ltype({lt_name = "boolean"},[]), t | t, Ltype({lt_name = "boolean"},[]))
          when is_integral_type t ->
          Ltype(C.find_logic_type "boolean",[])
      | Lreal, Ctype ty | Ctype ty, Lreal ->
          (match Cil.unrollType ty with
               TFloat _ -> Lreal
             | _ -> error loc "types %a and %a are not convertible"
                 d_logic_type t1 d_logic_type t2)
            (*    | Ltype (({lt_name = "set"} as lt),[t1]), t2
                  | t1, Ltype (({lt_name = "set"} as lt),[t2]) ->
                  Ltype(lt,[conditional_conversion loc env t1 t2])
             *)
      | Ltype (s1,l1), Ltype (s2,l2)  when s1.lt_name = s2.lt_name &&
          List.for_all2 Logic_utils.is_same_type l1 l2 -> t1
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

let rec mk_ptr_type = function
    Ctype ty -> Ctype (TPtr(ty,[]))
  | Ltype ({lt_name = "set"} as lt,[t]) -> Ltype (lt,[mk_ptr_type t])
  | t ->
    error Cilutil.locUnknown "cannot take the address of non-C values (type %a)"
      d_logic_type t

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
        let value = Cil.reduce_multichar Cil.theMachine.Cil.wcharType tokens in
        Cil.kinteger64 Cil.theMachine.Cil.wcharKind value
    | '\'' -> (* 'char' *)
        let content = String.sub s 1 (String.length s - 2) in
        let tokens = explode content in
        let value,_= Cil.interpret_character_constant tokens in
        dummy_exp (Const value)
    | _ -> Cil.parseInt s

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
	error loc "no label in the context. (\\at or explicit label missing ?)"


let labels_assoc loc id cur_label fun_labels effective_labels =
  match cur_label, fun_labels, effective_labels with
    | Some l, [lf], [] -> [lf,l]
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

let rec term env t =
  match t.lexpr_node with
    | PLnamed(name,t) ->
	let t = term env t in
	{ t with term_name = name :: t.term_name }
    | _ ->
	let t', ty = term_node env t.lexpr_loc t.lexpr_node in
	{ term_node = t'; term_loc=t.lexpr_loc; term_type=ty; term_name = [] }

and term_node env loc pl =
  let typed_term_node,typ = match pl with
    | PLsizeof typ ->
	(match logic_type loc env typ with
             Ctype t -> TSizeOf t,Linteger
           | _ -> error loc "sizeof can only handle C types")
	  (* NB: don't forget to add the case of literal string when they are authorized
	     in the logic *)
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
      begin
	try
	  let lv = Lenv.find_var x env in TLval (TVar lv, TNoOffset), lv.lv_type
	with Not_found ->
          try
	    let info = C.find_var x in
(*             (match info.lv_origin with *)
(*              | Some lv when lv.vglob -> ignore (find_current_label loc env) *)
(*              | _ -> ()); *)
	    TLval (TVar info, TNoOffset), info.lv_type
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
                              (fun x -> visitCilLogicType
                                 (fresh_type:>cilVisitor) (Lvar x))
                              info.ctor_type.lt_params)
                  | _ ->
                      error loc "Data constructor %s needs arguments"
                        info.ctor_name
              with Not_found ->
		match C.find_all_logic_functions x with
		  | [info] ->
		      begin
			match info.l_type with
			  | Some t -> Tapp(info,[],[]), t
			  | None -> error loc "%s is not a logic variable" x
		      end
		  | _ ->
		      error loc "unbound logic variable %s" x
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
          let env, tl = type_arguments ~overloaded:false env loc params ttl in
          let t = Ltype(info.ctor_type,
			List.map (fun x -> fresh (Lvar x))
                          info.ctor_type.lt_params)
          in
          let t = instantiate env t in
          TDataCons(info,tl), t
	with Not_found ->
	  let info, label_assoc, tl, t = type_logic_app env loc f labels ttl in
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
(*       let _lab = find_current_label loc env in *)
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      if is_pointer_type t.term_type then begin
          check_non_void_ptr loc t.term_type;
        let t = mk_mem t TNoOffset in
	t.term_node, t.term_type
      end else begin
        error loc "invalid type %a for `unary *'" d_logic_type t.term_type
      end
  | PLunop (Uamp, t) ->
      let t = term_lval false mkAddrOfAndMark (term env t) in
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
      | Badd when is_pointer_type ty1 && is_integral_type ty2 ->
	  TBinOp (PlusPI, t1, mk_cast t2 (integral_promotion ty2)),
          set_conversion ty1 ty2
      | Badd when is_integral_type ty1 && is_pointer_type ty2 ->
	  TBinOp (PlusPI, t2, mk_cast t1 (integral_promotion ty1)),
          set_conversion ty2 ty1
      | Bsub when is_pointer_type ty1 && is_integral_type ty2 ->
	  TBinOp (MinusPI, t1, mk_cast t2 (integral_promotion ty2)),
           set_conversion ty1 ty2
      | Bsub when is_pointer_type ty1 && is_pointer_type ty2 ->
	  TBinOp (MinusPP, t1, mk_cast t2 ty1), Linteger
      | _ ->
	  error loc "invalid operands to binary %a; unexpected %a and %a" d_binop (type_binop op)
            d_logic_type ty1 d_logic_type ty2
      end
  | PLdot (t, f) ->
      let t = term env t in
      let f_ofs, f_type = type_of_field loc f t.term_type in
      let t_dot_x t =
        match t.term_node with
      | TLval lv
      | TCastE (_, {term_node=TLval lv}) ->
          Logic_const.term ~loc:t.term_loc (TLval (add_offset_lval f_ofs lv))
            f_type
      | _ -> error loc "field access expects a left value"
      in
      let t = lift_set t_dot_x t in
      t.term_node, t.term_type

  | PLupdate (s, f, v) ->
      let s = term env s in
      (* we do not update sets *)
      let f_ofs, f_type = plain_type_of_field loc f s.term_type in
      let f_ofs = match f_ofs with
      | TField (f,_) -> f
      | _ -> assert false
      in
      let v = mk_cast (term env v)  f_type in
      let updated = TUpdate(s,f_ofs,v)
      in
      updated, s.term_type

  | PLarrow (t, f) ->
(*       let _lab = find_current_label loc env in *)
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      let struct_type = type_of_pointed t.term_type in
      let f_ofs, f_type = type_of_field loc f struct_type in
      (mk_mem ~loc t f_ofs).term_node, f_type
  | PLarrget (t1, t2) ->
(*       let _lab = find_current_label loc env in *)
      (* memory access need a current label to have some semantics *)
      let t1 = term env t1 in
      let t2 = term env t2 in
      let t'1, t'2, tres =
        if is_pointer_type t1.term_type && is_integral_type t2.term_type then
          t1,t2, set_conversion (type_of_pointed t1.term_type) t2.term_type
        else
          if is_integral_type t1.term_type && is_pointer_type t2.term_type then
            t2,t1,set_conversion (type_of_pointed t2.term_type) t1.term_type
          else error loc "subscripted value is neither array nor pointer"
      in
      check_non_void_ptr t'1.term_loc t'1.term_type;
      let shift t = match t.term_node with
        | TStartOf array ->
	 Logic_const.term ~loc
           (TLval (add_offset_lval (TIndex (t'2, TNoOffset)) array)) tres
        | _ ->
	    let b = { term_node = TBinOp (IndexPI, t'1, t'2); term_name = [];
		      term_loc = loc; term_type = t'1.term_type }
	    in
	    mk_mem b TNoOffset
      in
      let t = lift_set shift t'1 in
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
      let _lab = find_old_label loc env in
      let t = term env t in
      (* could be Tat(t,lab) *)
      Told t, t.term_type
  | PLat (t, l) ->
      let lab = find_logic_label loc env l in
      let env = Lenv.set_current_logic_label lab env in
      let t = term env t in
      Tat (t, lab), t.term_type
  | PLbase_addr t ->
      let t = term env t in
      if is_pointer_type t.term_type then
        let t =
          lift_set
            (fun t -> Logic_const.term (Tbase_addr t)
               (Ctype Cil.charPtrType)) t
        in t.term_node, t.term_type
      else error loc "subscripted value is neither array nor pointer"
  | PLblock_length t ->
      let t = term env t in
      if is_pointer_type t.term_type then
        let t =
          lift_set (fun t -> Logic_const.term (Tblock_length t) Linteger) t
        in t.term_node, t.term_type
      else error loc "subscripted value is neither array nor pointer"
  | PLresult ->
      (try let t = Lenv.find_var "\\result" env in
       match t.lv_type with
           Ctype ty ->
             TLval(TResult ty,TNoOffset), t.lv_type
         | _ -> assert false (* \\result is the value returned by a C function.
                                It has always a C type *)
       with Not_found -> error loc "\\result meaningless")
  | PLnull -> Tnull, c_void_star
  | PLcast (ty, t) ->
      let t = term env t in
      (* no casts of tsets in grammar *)
        (match logic_type loc env ty with
           | (Ctype ty) as cty
             -> TCastE (ty, t), cty
           | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
              error loc "cannot cast to logic type")
  | PLcoercion (t,ty) ->
      let t = term env t in
      (match logic_type loc env ty with
         | Ctype ty as cty
             -> TCoerce (t, ty), cty
         | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
              error loc "cannot cast to logic type")
  | PLcoercionE (t,tc) ->
      let t = term env t in
      let tc = term env tc in
      TCoerceE (t, tc), tc.term_type
  | PLrel (t1, (Eq | Neq | Lt | Le | Gt | Ge as op), t2) ->
      let loc = loc_join t1.lexpr_loc t2.lexpr_loc in
      let t1 = term env t1 in
      let ty1 = t1.term_type in
      let t2 = term env t2 in
      let ty2 = t2.term_type in
      let expr = match op with
      | _ when plain_arithmetic_type ty1 && plain_arithmetic_type ty2 ->
          let tr = arithmetic_conversion ty1 ty2 in
	  TBinOp(binop_of_rel op,mk_cast t1 tr,mk_cast t2 tr)
      | Eq | Neq when plain_pointer_type ty1 && isLogicNull t2 ->
	  TBinOp (binop_of_rel op, t1, mk_cast t2 ty1)
      | Eq | Neq when plain_pointer_type ty2 && isLogicNull t1 ->
	  TBinOp (binop_of_rel op, mk_cast t1 ty2, t2)
      | _ when plain_pointer_type ty1 && plain_pointer_type ty2 &&
          is_same_logic_ptr_type ty1 ty2 ->
          let ty,ty1,ty2 = conditional_conversion loc env ty1 ty2 in
          let t1 = { t1 with term_type = ty1 } in
          let t2 = { t2 with term_type = ty2 } in
          TBinOp (binop_of_rel op,  mk_cast t1 ty, mk_cast t2 ty)
      | Eq | Neq ->
	  let ty,ty1,ty2 = conditional_conversion loc env ty1 ty2 in
          let t1 = { t1 with term_type = ty1 } in
          let t2 = { t2 with term_type = ty2 } in
	  TBinOp (binop_of_rel op, mk_cast t1 ty, mk_cast t2 ty)
      (* Can we allow comparison between sets of terms (as notation for
         inclusion)?
       *)
      | _ ->
	  error loc "comparison of incompatible types %a and %a"
            d_logic_type ty1 d_logic_type ty2
      in expr, Ltype(C.find_logic_type "boolean",[])
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
      TUnOp(LNot,t), Ltype (C.find_logic_type "boolean",[])
  | PLand (t1,t2) ->
      let t1 = type_bool_term env t1 in
      let t2 = type_bool_term env t2 in
      TBinOp(LAnd,t1,t2), Ltype (C.find_logic_type "boolean",[])
  | PLor (t1,t2) ->
      let t1 = type_bool_term env t1 in
      let t2 = type_bool_term env t2 in
      TBinOp(LOr,t1,t2), Ltype (C.find_logic_type "boolean",[])
  | PLtypeof t1 ->
      let t1 = term env t1 in
      Ttypeof t1, Ltype (C.find_logic_type "typetag",[])
  | PLtype ty ->
      begin match logic_type loc env ty with
      | Ctype ty -> Ttype ty, Ltype (C.find_logic_type "typetag",[])
      | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
          error loc "cannot take type tag of logic type"
      end
  | PLcomprehension(t,quants,pred) ->
      let quants, env = add_quantifiers loc quants env in
      let t = term env t in
      let pred = Cilutil.opt_map (predicate env) pred in
      Tcomprehension(t,quants,pred),
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
  in
  match typed_term_node, unroll_type typ with
    | (TLval lval|TCastE(_, {term_node = TLval lval})),
      ((Ctype (TArray (t,_,_,_)) |
        Ltype ({lt_name = "set"},[Ctype(TArray(t,_,_,_))])) as maybeset) ->
        TStartOf lval, set_conversion (Ctype (TPtr(t,[]))) maybeset
    | _, Ctype (TArray (_,_,_,_)) -> assert false
    | _ -> typed_term_node,typ

and term_lval accept_logic_lval f t =
  let check_lval t =
  match t.term_node with
      TLval (h,_ as lv) | TCastE(_,{term_node = TLval (h,_ as lv)}) ->
        (match h with
             TVar { lv_name = v; lv_origin = None }
               when not accept_logic_lval ->
               error t.term_loc "not an assignable left value: %s" v
                 (* Tresult only exists when typing C functions and
                    Tmem would lead to an error earlier if applied to pure
                    logic expression.
                  *)
           | TVar _ | TResult _ | TMem _ -> f lv t)
    | TStartOf lv | TCastE(_,{term_node = TStartOf lv}) ->
        f lv t
    | _ -> error t.term_loc "not a left value:%a"
        Cil.d_term t
  in
  lift_set check_lval t

and type_logic_app env loc f labels ttl =
  (* support for overloading *)
  let infos = C.find_all_logic_functions f in
  match infos with
    | [] -> error loc "unbound function %s" f
    | [info] ->
	begin
	  let labels = List.map (find_logic_label loc env) labels in
	  let params = List.map (fun x -> fresh x.lv_type) info.l_profile in
	  let env, tl =
	    type_arguments ~overloaded:false env loc params ttl
	  in
	  let label_assoc =
	    labels_assoc loc f
	      env.Lenv.current_logic_label info.l_labels labels
	  in
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
		 let label_assoc =
		   labels_assoc loc f
		     env.Lenv.current_logic_label info.l_labels labels
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
	  | [x] ->
	      (*
		Format.eprintf "overloading resolved: %s(%d)(%a)@." f
		(Obj.magic info)
		(fprintfList ~sep:",@ " d_logic_type)
		(List.map (fun v -> v.lv_type) info.l_profile);
	      *)
	      x
	  | _ ->
	      let tl = List.map (fun t -> t.term_type) ttl in
	      error loc "ambiguous logic call to %s(%a)" f
		(Pretty_utils.pp_list ~sep:",@ " d_logic_type) tl

and type_int_term env t =
  let tt = term env t in
  if not (plain_integral_type tt.term_type) then
    error t.lexpr_loc "integer expected but %a found" d_logic_type tt.term_type;
  tt

and type_bool_term env t =
  let tt = term env t in
  if not (plain_boolean_type tt.term_type) then
    error t.lexpr_loc "boolean expected but %a found" d_logic_type tt.term_type;
  mk_cast tt (Ltype (C.find_logic_type "boolean",[]))

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

(* type_arguments checks if argument list tl is well-typed for the formal parameter list at *)
and type_arguments ~overloaded env loc at tl =
  let rec type_list env = function
    | [], [] ->
	env, []
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
  (match t.term_type with
       Ctype _ | Linteger | Lreal ->
	 prel ~loc (Cil_types.Rneq, t, Cil.lzero ~loc ())
     | Ltype ({lt_name = "boolean"},[]) ->
	 let ctrue = C.find_logic_ctor "\\true" in
	 prel ~loc
	   (Cil_types.Req,t,
            { term_node = TDataCons(ctrue,[]);
              term_loc = loc;
              term_type = Ltype(ctrue.ctor_type,[]);
              term_name = [];
            })
     | Ltype _ | Lvar _ | Larrow _ ->
	 error loc "expecting a predicate and not a term")

and boolean_to_predicate env p0 =
  boolean_term_to_predicate (term env p0)

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
      let binop op tr =	prel ~loc:p0.lexpr_loc (op, mk_cast t1 tr, mk_cast t2 tr) in
      begin match op with
	| _ when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	    binop (type_binop op) (arithmetic_conversion ty1 ty2)
	| Eq | Neq when is_pointer_type ty1 && isLogicNull t2 ->
	    prel ~loc:p0.lexpr_loc
	      (type_binop op, t1, mk_cast t2 ty1)
	| Eq | Neq when is_pointer_type ty2 && isLogicNull t1 ->
	    prel ~loc:p0.lexpr_loc
	      (type_binop op, mk_cast t1 ty2, t2)
        | _ when is_pointer_type ty1 && is_pointer_type ty2 &&
            is_same_logic_ptr_type ty1 ty2 ->
            let ty,ty1,ty2 = conditional_conversion p0.lexpr_loc env ty1 ty2 in
            let t1 = { t1 with term_type = ty1 } in
            let t2 = { t2 with term_type = ty2 } in
            prel ~loc:p0.lexpr_loc
	      (type_binop op, mk_cast t1 ty, mk_cast t2 ty)
	| Eq | Neq ->
	    let ty,ty1,ty2 = conditional_conversion loc env ty1 ty2 in
            let t1 = { t1 with term_type = ty1 } in
            let t2 = { t2 with term_type = ty2 } in
	    prel ~loc:p0.lexpr_loc (type_binop op, mk_cast t1 ty, mk_cast t2 ty)
	| _ ->
	    error loc "comparison of incompatible types"
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
(*
    begin try
	let info =
	  match C.find_all_logic_functions p with
	    | [info] -> info
	    | _ -> raise Not_found
	in
	match info.l_type with
	  | None ->
	      let labels =
		List.map (find_logic_label p0.lexpr_loc env) labels
	      in
              fresh_type#reset();
              let params =
                List.map (fun x ->
                            visitCilLogicType (fresh_type:>cilVisitor) x.lv_type)
                  info.l_profile
              in
	      let _, tl = type_arguments ~overloaded:false env p0.lexpr_loc params ttl in
	      let label_assoc =
		labels_assoc loc p env.Lenv.current_logic_label
		  info.l_labels labels
	      in
	      papp ~loc:p0.lexpr_loc (info, label_assoc, tl)
	  | Some _ -> boolean_to_predicate env p0
      with Not_found ->
	error p0.lexpr_loc "unknown predicate %s" p
      end
  (* see below for implicit conversion of terms into predicate *)
*)

  | PLif (t, p1, p2) ->
      begin try
	(*TODO: Error message are still printed. *)
        let t = type_bool_term env t in
        pif ~loc:p0.lexpr_loc (t, predicate env p1, predicate env p2)
      with _ ->
	(* p1 ? p2 : p3 is syntactic sugar for (p1 ==> p2) && (!p1 ==> p3) *)
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
      let tloc = t.lexpr_loc in
      let t = term env t in
      if is_pointer_type t.term_type then pfresh ~loc:p0.lexpr_loc (t)
      else error tloc "subscripted value is neither array nor pointer"
  | PLvalid (t) ->
      let loc = t.lexpr_loc in
      let t = term env t in
      check_non_void_ptr loc t.term_type;
      pvalid ~loc:p0.lexpr_loc t
  | PLvalid_index (t,a) ->
      let t = term env t in
      let a = type_int_term env a in
      check_non_void_ptr t.term_loc t.term_type;
      pvalid_index ~loc:p0.lexpr_loc (t,a)
  | PLvalid_range (t,a,b) ->
      let t = term env t in
      let a = type_int_term env a in
      let b = type_int_term env b in
      check_non_void_ptr t.term_loc t.term_type;
      pvalid_range ~loc:p0.lexpr_loc (t,a,b)
  | PLold p ->
      let _lab = find_old_label p0.lexpr_loc env in
      (* could be Tat(t,lab) *)
      pold ~loc:p0.lexpr_loc (predicate env p)
  | PLat (p, l) ->
      let lab = find_logic_label p0.lexpr_loc env l in
      let env = Lenv.set_current_logic_label lab env in
      pat ~loc:p0.lexpr_loc (predicate env p, lab)
  | PLvar x ->
      (try
         let info =
	   match C.find_all_logic_functions x with
	     | [info] -> info
	     | _ -> raise Not_found
	 in
	 match info.l_type with
	   | None -> papp ~loc:p0.lexpr_loc (info,[],[])
	   | Some _ -> boolean_to_predicate env p0
       with Not_found -> boolean_to_predicate env p0)
  | PLcast _ | PLblock_length _ | PLbase_addr _ | PLarrget _ | PLarrow _
  | PLdot _ | PLbinop _ | PLunop _ | PLconstant _
  | PLnull | PLresult | PLcoercion _ | PLcoercionE _ | PLsizeof _
  | PLsizeofE _ | PLupdate _ | PLlambda _
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
        check_non_void_ptr res.term_loc res.term_type;
        res
      in
      let seps = List.map type_loc seps in
      pseparated ~loc:p0.lexpr_loc seps
  | PLcomprehension _ | PLunion _ | PLinter _ | PLempty ->
      error p0.lexpr_loc "expecting a predicate and not tsets"

let type_variant env = function
  | (t, None) -> (type_int_term env t, None)
  | (t, r) -> (term env t, r)

let term_lval_assignable accept_logic_lval env t =
  let f t =
    match t.term_node with
      | Tapp _ -> t (* allow to use footprint functions in assigns. *)
      | _ ->
          term_lval accept_logic_lval
            (fun _ t -> match t.term_node with
                 TStartOf lv | TCastE(_,{ term_node = TStartOf lv}) ->
                   error t.term_loc "not an assignable left value: %a"
             Cil.d_term_lval lv
               | _ -> t
            )
            t
  in lift_set f (term env t)

let type_zone env =
  function
      Nothing -> Nothing
    | Location a ->
        let tset = term_lval_assignable false env a
        in
        try
          ignore (Logic_utils.logicCType tset.term_type);
          Location (Logic_const.new_identified_term tset)
        with Failure _ ->
          error tset.term_loc "expected memory location, got logical value"


let get_zone_loc = function
    Nothing -> Cilutil.locUnknown
  | Location l -> l.lexpr_loc

let type_assign env (a,f) =
  let ta = type_zone env a in
(* Yannick: [assigns *\at(\result,Post)] should be allowed *)
  let tf =
    List.map
      (fun d ->
         let td = type_zone env d in
         (match td with
              Nothing -> ()
            | Location td ->
                if Logic_utils.contains_result td.it_content then
                  error (get_zone_loc d)
                    "invalid \\result in dependencies"); td)
      f
  in (ta,tf)

let id_predicate env pred = Logic_const.new_predicate (predicate env pred)

let loop_pragma env = function
  | Unroll_level t -> (Unroll_level (term env t))
  | Widen_hints l -> (Widen_hints (List.map (term env) l))
  | Widen_variables l -> (Widen_variables (List.map (term env) l))

let type_annot loc ti =
  let env = make_here_label () in
  let this_type = logic_type loc env ti.this_type in
  let v = Cil_const.make_logic_var ti.this_name this_type in
  let env = Lenv.add_var ti.this_name v env in
  let body = predicate env ti.inv in
  let infos = Cil_const.make_logic_info ti.inv_name in
  infos.l_profile <- [v];
  infos.l_labels <- [LogicLabel "Here"];
  infos.l_body <- LBpred body;
  C.add_logic_function infos; infos

let type_spec result env s =
  let env = append_here_label env in
  let p = List.map (id_predicate env) s.spec_requires in
  let env_with_result = match result with
    | None -> env
    | Some ty ->
	let v = Cil_const.make_logic_var "\\result" ty in
	Lenv.add_var "\\result" v env
  in
  let assigns_env = append_old_and_post_labels env_with_result in
  let post_state_env =
    Lenv.enter_post_state assigns_env in
  let b = List.map
    (fun {b_assigns= ba;
          b_name = bn; b_ensures=be;
          b_assumes= bas} ->
       {b_assigns= List.map (type_assign assigns_env) ba;
        b_name = bn;
        b_ensures= List.map (id_predicate post_state_env) be;
        b_assumes= List.map (id_predicate env) bas})
    s.spec_behavior
  in
  let v = Cilutil.opt_map (type_variant env) s.spec_variant in
  let t = Cilutil.opt_map (id_predicate env) s.spec_terminates in

  { spec_requires = p;
    spec_behavior = b;
    spec_variant = v;
    spec_terminates = t;
    spec_complete_behaviors = s.spec_complete_behaviors;
    spec_disjoint_behaviors = s.spec_disjoint_behaviors;
  }

let funspec vi formals typ s =
  let env = make_pre_label () in
  try
    let log_return_typ = match typ with
    | TFun (TVoid _ , _, _, _) -> None
    | TFun (typ , _, _, _) -> Some (Ctype typ)
    | typ ->
	Cil.warning "Ignoring unexpected function annotation on %a" Cil.d_type typ ;
        raise Not_found
    in
    let env =
      match formals with
        | None -> (* This is the spec of a function declaration *)
            let add_formal env v =
                Lenv.add_var v.vname (Cil.cvar_to_lvar v) env
            in
            (try
               List.fold_left add_formal env (Cil.getFormalsDecl vi)
             with Not_found -> env)
      | Some formals ->
          let add_formal env v =
            Lenv.add_var v.vname (Cil.cvar_to_lvar v) env in
          List.fold_left add_formal env formals
    in type_spec log_return_typ env s
  with Not_found -> Cil.empty_funspec ()

let slice_pragma env = function
    SPexpr t -> SPexpr (term env t)
  | (SPctrl | SPstmt) as sp -> sp

let impact_pragma env = function
    IPexpr t -> IPexpr (term env t)
  | IPstmt as ip -> ip

let code_annot_env = append_here_label (make_pre_label())

let code_annot ca =
  let annot = match ca with
    | AAssert (behav,p,s) -> AAssert (behav,predicate code_annot_env p,s)
    | APragma (Impact_pragma sp) ->
	APragma (Impact_pragma (impact_pragma code_annot_env sp))
    | APragma (Slice_pragma sp) ->
	APragma (Slice_pragma (slice_pragma code_annot_env sp))
    | APragma (Loop_pragma lp) ->
	APragma (Loop_pragma (loop_pragma code_annot_env lp))
    | AStmtSpec s ->
	(* TODO: right env for labels *)
	AStmtSpec (type_spec None Lenv.empty s)
    | AVariant v -> AVariant (type_variant code_annot_env v)
    | AInvariant (behav,f,i) -> AInvariant (behav,f,predicate code_annot_env i)
    | AAssigns (behav,a) -> AAssigns (behav,type_assign code_annot_env a)
(*
    | ALoopBehavior(behav,invs,assigns) ->
	ALoopBehavior(behav,
		      List.map (predicate code_annot_env) invs,
		      List.map (type_assign code_annot_env) assigns)
*)
  in Logic_const.new_code_annotation annot

let formals loc env p =
  let add_var (p,env) (t,x) =
    let lt = logic_type loc env t in
    let lt = array_to_ptr lt in
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
    Lenv.empty l

(* checks whether all the type variable contained in the return type t of
   a logic function are bound in a parameter's type
   (p being the list of formals). type-checking error otherwise
*)
let check_polymorphism loc ?return_type p =
  let obj known_vars =
    let update_known_vars s = known_vars:= Cilutil.StringSet.add s !known_vars
    in object inherit Cil.nopCilVisitor
      method vlogic_type = function
          Lvar s -> update_known_vars s; Cil.DoChildren
        | _ -> Cil.DoChildren
    end
  in let rt_vars = ref Cilutil.StringSet.empty
  in let prm_vars = ref Cilutil.StringSet.empty
  in
  ignore(Cilutil.opt_map (Cil.visitCilLogicType (obj rt_vars)) return_type);
  List.iter
    (fun v -> ignore (Cil.visitCilLogicType (obj prm_vars) v.lv_type)) p;
  if not (Cilutil.StringSet.subset (!rt_vars) (!prm_vars)) then
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
	   let lab = LogicLabel l in
	   (lab::labs,Lenv.add_logic_label l lab e))
      labels ([],env)
  in
  let env =
    match labels with
      | [lab] ->
	  (* is exactly one label, it is the default label *)
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
  info.l_tparams <- poly;
  info.l_profile <- p;
  info.l_type <- t;
  info.l_labels <- labels;
(* this is taken care in add_logic_function
  if is_logic_function f then begin
    let old_info =
      match C.find_all_logic_functions f with
	| [info] -> info
	| _ -> raise Not_found
    in
    if Logic_utils.is_same_logic_info old_info info then begin
      env, old_info
    end
    else
       error loc "logic function %s has incompatible declarations" f
  end else
*)
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


let rec annot loc a =
  Cil.CurrentLoc.set loc;
  match a with
    | LDlogic_reads (f, labels, poly, t, p, l) ->
	let env,info = logic_decl loc f labels poly ~return_type:t p in
	let l = List.map (fun x -> new_identified_term (term env x)) l in
	info.l_body <- LBreads l;
        Dfun_or_pred info
    | LDlogic_def(f, labels, poly,t,p,e) ->
	let env,info = logic_decl loc f labels poly ~return_type:t p in
	let redefinition = false in
	let rt = match info.l_type with
	  | None -> assert false
	  | Some t -> t
	in
        (try
          let e = term env e in
          let _,new_typ,new_term = instantiate_app ~overloaded:false loc e rt env in
          if is_same_type new_typ rt then begin
            info.l_body <- LBterm new_term;
            Dfun_or_pred info
          end else
            error loc "return type of logic function %s is %a but %a was expected"
              f d_logic_type new_typ d_logic_type rt
         with e when not redefinition ->
           C.remove_logic_function f; raise e)
    | LDaxiomatic(id,decls) ->
(*
	Format.eprintf "Typing axiomatic %s@." id;
*)
	let l = List.map (annot loc) decls in
	Daxiomatic(id,l)

    | LDpredicate_reads (f, labels, poly, p, l) ->
	let env,info = logic_decl loc f labels poly p in
	let l = List.map
          (fun x ->
             new_identified_term (term_lval_assignable true env x)) l
        in
	info.l_body <- LBreads l;
        Dfun_or_pred info
    | LDpredicate_def (f, labels, poly, p, e) ->
	let env,info = logic_decl loc f labels poly p in
	let e = predicate env e in
        info.l_body <- LBpred e;
	Dfun_or_pred info
    | LDinductive_def (f, labels, poly, p, indcases) ->
	let _env,info = logic_decl loc f labels poly p in
	(* env is ignored: because params names are indeed useless...*)
        let l =
	  List.map
	    (fun (id,labels,poly,e) ->
               let labels,env = annot_env loc labels poly in
	       (id, labels, poly, predicate env e)) indcases
	in
        info.l_body <- LBinductive l;
	Dfun_or_pred info
    | LDtype(s,l,cons) ->
        let env = init_type_variables loc l in
        let my_info =
          { lt_name = s;
            lt_params = l;
            lt_ctors = None; (* will be updated later *)
          }
        in
        C.add_logic_type s my_info;
        (try
           let tcons =
             Cilutil.opt_map (List.map (type_datacons loc env my_info)) cons
           in
           my_info.lt_ctors <- tcons;
           Dtype my_info
         with e ->
           C.remove_logic_type s;
           Cilutil.opt_iter
             (List.iter (fun (name,_) -> C.remove_logic_ctor name))
             cons;
           raise e)
    | LDlemma (is_axiom, x, labels, poly, e) ->
        let labels,env = annot_env loc labels poly in
        Dlemma (is_axiom, x, labels, poly,  predicate env e)
    | LDinvariant (s, e) ->
        let env = make_here_label () in
        let p = predicate env e in
        let li = Cil_const.make_logic_info s in
	li.l_labels <- [LogicLabel "Here"];
        li.l_body <- LBpred p;
        C.add_logic_function li;
	Dinvariant li
    | LDtype_annot l ->
        Dtype_annot (type_annot loc l)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
