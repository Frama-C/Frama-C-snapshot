(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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
open Format
open Cil

let dloc = Lexing.dummy_pos,Lexing.dummy_pos
let error (b,e) fstring =
  let f fmt =
    kfprintf (fun _ ->
                currentLoc := (b,e);
                raise Errormsg.Error) fmt (fstring ^^ "@\n@]")
  in
  kfprintf f err_formatter
    "@[File %s, line %d, characters %d-%d:@\n\
     Error during analysis of annotation: "
    b.Lexing.pos_fname b.Lexing.pos_lnum
    (b.Lexing.pos_cnum - b.Lexing.pos_bol)
    (e.Lexing.pos_cnum - b.Lexing.pos_bol)

let loc_join (b,_) (_,e) = (b,e)

(* Logical environments *)

module Lenv = struct
(* locals: logic variables (e.g. quantified variables in \forall, \exists) *)

module Smap = Map.Make(String)

type t = {
  local_vars: Cil_types.logic_var Smap.t;
  type_vars: Cil_types.logic_type Smap.t;
  logic_labels: Cil_types.logic_label Smap.t;
  current_logic_label: Cil_types.logic_label option;
}

let empty = {
  local_vars = Smap.empty;
  type_vars = Smap.empty;
  logic_labels = Smap.empty;
  current_logic_label = None;
}
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
  { env with current_logic_label = Some lab }
end

let append_here_label env =
  let env = Lenv.add_logic_label "Here" (LogicLabel "Here") env in
  Lenv.set_current_logic_label (LogicLabel "Here") env
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
      val add_logic_function: logic_info -> unit
      val add_predicate: predicate_info -> unit
      val add_logic_type: string -> logic_type_info -> unit
      val add_logic_ctor: string -> logic_ctor_info -> unit

      val find_logic_function: string -> logic_info
      val find_predicate: string -> predicate_info
      val find_logic_type: string -> logic_type_info
      val find_logic_ctor: string -> logic_ctor_info

    end) =
struct

  let is_logic_ctor c =
    try ignore (C.find_logic_ctor c);true with Not_found -> false
  let is_logic_function c =
    try ignore (C.find_logic_function c);true with Not_found -> false
  let is_predicate c =
    try ignore (C.find_predicate c);true with Not_found -> false


let prefix p s =
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p

let type_of_field loc f = function
  | Ctype ty ->
      begin match Cil.unrollType ty with
      | TComp (comp, _) ->
	  let rec search = function
	    | [] ->
                raise Exit
	    | fid :: _ when fid.fname = f ->
		TField(fid, TNoOffset), Ctype fid.ftype
	    | fid :: rest
		when prefix C.annonCompFieldName fid.fname ->
		  begin match Cil.unrollType fid.ftype with
		    | TComp (ci, _) ->
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

let type_of_tsfield loc f = function
  | Ctype ty ->
      begin match Cil.unrollType ty with
      | TComp (comp, _) ->
	  let rec search = function
	    | [] ->
		raise Exit
	    | fid :: _ when fid.fname = f ->
		TSField(fid, TSNo_offset), Ctype fid.ftype
	    | fid :: rest
		when prefix C.annonCompFieldName fid.fname ->
		  begin match Cil.unrollType fid.ftype with
		    | TComp (ci, _) ->
			(try
			   let off, t = search ci.cfields in
			   TSField (fid, off), t
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

let unroll_type = function
  | Ctype ty -> Ctype (Cil.unrollType ty)
  | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ as ty  -> ty

let array_to_ptr ty =
  match unroll_type ty with
      Ctype(TArray(ty,lo,attr)) ->
        let rec aux = function
            TArray(ty,lo,attr) -> TArray(aux ty,lo,attr)
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
                  Cil.warn
                    "Cannot represent the length of array as an attribute";
                  []
              end
        in
        Ctype(TPtr(aux ty,length_attr))
    | ty -> ty

let check_non_void_ptr loc typ =
  match unroll_type typ with
      Ctype (TPtr(ty,_) | TArray(ty,_,_)) ->
        if Cil.isVoidType ty then
          error loc
            "can not access location with a pointer to void. \
             Cast it to (char*) first. "
        else ()
    | _ -> error loc "not a pointer or array type"

let rec partial_unif loc env t1 t2 =
  match unroll_type t1, unroll_type t2 with
  | _, Lvar s2 ->
      (try
         if Logic_const.is_same_type t1 (Lenv.find_type_var s2 env) then env
         else error loc "cannot instantiate %a to %a" d_logic_type t1 d_logic_type t2
       with Not_found -> Lenv.add_type_var s2 t1 env)
  | Ctype _, Ctype _ when Logic_const.is_same_type t1 t2 -> env
  | Linteger, Linteger | Lreal, Lreal -> env
  | Ltype(s1,l1), Ltype(s2,l2) when s1 = s2->
      List.fold_left2 (partial_unif loc) env l1 l2
  | _,_ -> error loc "incompatible types"

let c_addr = Ltype ("base_addr",[])
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
             if info.nb_params <> 0 then
               error loc "wrong number of parameter for type %s" id
             else Ltype (id,[])
           with Not_found ->
             error loc "no such named type")
  | LTnamed(id,l) ->
      (try
         let info = C.find_logic_type id in
         if info.nb_params <> List.length l then
           error loc "wrong number of parameter for type %s" id
         else Ltype (id,List.map (logic_type loc env) l)
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

let rec add_tsoffset toadd = function
   | TSNo_offset -> toadd
   | TSField(fid, offset) -> TSField(fid, add_tsoffset toadd offset)
   | TSIndex(e, offset) -> TSIndex(e, add_tsoffset toadd offset)
   | TSRange(l,h,offset) -> TSRange(l,h,add_tsoffset toadd offset)

let add_offset_lval toadd (b, off) = b, add_offset toadd off

let add_offset_tslval toadd (b,off) = b, add_tsoffset toadd off

let dummy_loc = Lexing.dummy_pos,Lexing.dummy_pos

let mk_mem ?loc t ofs = match t.term_node with
  | TAddrOf lv -> add_offset_lval ofs lv
  | TStartOf lv -> add_offset_lval (TIndex (Cil.lzero ?loc (), ofs)) lv
  | _ -> TMem t, ofs

let mk_tsmem loc typ t ofs =
  check_non_void_ptr loc typ;
  match t with
      TSStartOf lv -> add_offset_tslval (TSIndex(Cil.lzero(),ofs)) lv
    | _ -> TSMem t, ofs

let rec is_zero t = match t.term_node with
  | TConst (CInt64 (n,_,_)) -> n = 0L
  | TConst (CChr c) -> Char.code c = 0
  | TCastE(_, t) -> is_zero t
  | _ -> false

let is_arithmetic_type = function
  | Ctype ty -> Cil.isArithmeticType ty
  | Linteger | Lreal -> true
  | Ltype _ | Lvar _ | Larrow _ -> false

let is_pointer_type = function
  | Ctype ty -> Cil.isPointerType ty
  | Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ -> false

let is_integral_type = function
  | Ctype ty -> Cil.isIntegralType ty
  | Linteger -> true
  | Ltype _ | Lreal | Lvar _ | Larrow _ -> false

let is_boolean_type = function
  | Ctype ty -> isIntegralType ty
  | Linteger | Ltype ("boolean",[]) -> true
  | Lreal | Ltype _ | Lvar _ | Larrow _ -> false

(* Make an AddrOf. Given an lval of type T will give back an expression of
 * type ptr(T)  *)
let mk_AddrOf lval =
  match lval with
    TMem e, TNoOffset -> e.term_node
  | b, TIndex(z, TNoOffset) when is_zero z ->
      TStartOf (b, TNoOffset)(* array *)
  | _ -> TAddrOf lval

let mkAddrOfAndMark ((b, off) as lval) : term_node =
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
  mk_AddrOf lval

let c_mk_cast e oldt newt =
  (* let oldt = Cil.typeRemoveAttributes ["const"] oldt in
  let newt = Cil.typeRemoveAttributes ["const"] newt in *)
  if Cilutil.equals (Cil.typeSig oldt) (Cil.typeSig newt) then begin
    e
  end else begin
    (* Watch out for constants *)
    match newt, e.term_node with
      | TInt(newik, []), TConst (CInt64(i, _, _)) ->
	  { e with term_node = TConst (CInt64 (i, newik, None)) }
      | _ ->
          { e with term_node = TCastE (newt, e); term_type = Ctype newt }
  end

let mk_cast e newt =
  if Logic_const.is_same_type e.term_type newt then e
  else begin
    match e.term_type, newt with
      | Ctype oldt, Ctype newt ->
          c_mk_cast e oldt newt
      | t1, Ltype ("boolean",[]) when is_integral_type t1 -> e
      | Linteger, Linteger | Lreal, Lreal -> e
      | Linteger, Ctype _ | Lreal, Ctype _ ->
          error e.term_loc "invalid implicit cast from %a to C type %a"
            d_logic_type e.term_type d_logic_type newt
      | Ctype t, Linteger when Cil.isIntegralType t ->
          { e with term_type = Linteger}
      | Ctype t, Lreal when isFloatingType t -> { e with term_type = Lreal }
      | Ctype _, (Lreal | Linteger) ->
          error e.term_loc "invalid implicit cast from %a to logic type %a"
            d_logic_type e.term_type d_logic_type newt
      | Linteger, Lreal | Lreal, Linteger ->
          error e.term_loc
            "invalid cast between real and integer. \
         Use conversion functions instead"
      | Ltype _, _ | _, Ltype _
      | Lvar _,_ | _,Lvar _
      | Larrow _,_ | _,Larrow _ ->
          error e.term_loc "invalid cast from %a to %a"
            d_logic_type e.term_type d_logic_type newt
  end

let rec c_cast_to ot nt e =
  if Cilutil.equals (Cil.typeSig ot) (Cil.typeSig nt) then
    (ot, e)
  else begin
    let result = (nt, mk_cast e (Ctype nt)) in
    match ot, nt with
      | TNamed(r, _), _ -> c_cast_to r.ttype nt e
      | _, TNamed(r, _) -> c_cast_to ot r.ttype e
      | TInt(_ikindo,_), TInt(_ikindn,_) ->
          result
      | TPtr (_told, _), TPtr(_tnew, _) -> result
      | TInt _, TPtr _ -> result
      | TPtr _, TInt _ -> result
      | TArray _, TPtr _ -> result
      | TArray(t1,_,_), TArray(t2,None,_)
	when Cil.typeSig t1 = Cil.typeSig t2 ->
	  (nt, e)
      | TPtr _, TArray(_,_,_) -> (nt, e)
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
          ignore (Cil.warnOpt "Casting %a to __builtin_va_list" Cil.d_type ot);
          result
      | TPtr _, TEnum _ ->
          ignore (Cil.warnOpt "Casting a pointer into an enumeration type");
          result
      | (TInt _ | TEnum _ | TPtr _ ), TVoid _ ->
          (ot, e)
      | TComp (comp1, _a1), TComp (comp2, _a2) when comp1.ckey = comp2.ckey ->
          (nt, e)
      | _ ->
	  bug "Logic_typing.c_cast_to: %a -> %a@."
	    Cil.d_type ot Cil.d_type nt
  end

let instantiate_app loc oterm nt env =
  let ot = oterm.term_type in
  let rec implicit_conversion ot nt = match ot, nt with
  | Ctype ty1, Ctype ty2 ->
      if Cilutil.equals (typeSig ty1) (typeSig ty2) then
        ot, oterm
      else
        let sz1 = bitsSizeOf ty1 in
        let sz2 = bitsSizeOf ty2 in
        if (isIntegralType ty1 && isIntegralType ty2 &&
              (sz1 < sz2
               || (sz1 = sz2 && (isSignedInteger ty1 = isSignedInteger ty2))
              ))
          || ((isArrayType ty1 || isPointerType ty1) && isVoidPtrType ty2)
          || ((isArrayType ty1 || isPointerType ty1) &&
              (isArrayType ty2 || isPointerType ty2) &&
              (Cilutil.equals (typeSig (typeOf_pointed ty1)) (typeSig (typeOf_pointed ty2))
               || isLogicNull oterm))
          || (match unrollType ty1, unrollType ty2 with
              | (TFloat (f1,_), TFloat (f2,_)) ->
                  f1 <= f2 (*[BM] relies on internal representation of OCaml constant
                             constructors.*)
              | _ -> false)
        then
          let t,e = c_cast_to ty1 ty2 oterm in Ctype t, e
        else
          error loc "invalid implicit conversion from %a to %a"
            d_type ty1 d_type ty2
  | Ctype ty, Linteger when Cil.isIntegralType ty -> Linteger, oterm
  | Ctype (TFloat _), Lreal -> Lreal, oterm
  | Ltype (t1,l1), Ltype (t2,l2) when t1 = t2 ->
      let l = List.map2 (fun x y -> fst (implicit_conversion x y)) l1 l2 in
      Ltype(t1,l),oterm
  | Linteger, Linteger | Lreal, Lreal -> ot, oterm
  | Lvar s1, Lvar s2 when s1 = s2 -> ot, oterm
  | Larrow(args1,rt1), Larrow(args2,rt2)
      when List.length args1 = List.length args2 ->
      (* contravariance. *)
      let args =
        List.map2 (fun x y -> fst (implicit_conversion x y)) args2 args1 in
      let rt,_ = implicit_conversion rt1 rt2 in
      Larrow(args,rt), oterm
  | ((Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _),
     (Ctype _| Linteger | Lreal | Ltype _ | Lvar _ | Larrow _)) ->
      error loc "invalid implicit conversion from %a to %a"
        d_logic_type ot d_logic_type nt
  in

  let rec instantiate ot nt env =
    match ot,nt with
    | _, Lvar s2 -> (* we only perform instantiation, as all
                       types are supposed to be declared. *)
        (try
           let nt = Lenv.find_type_var s2 env in (env, ot, nt)
         with Not_found ->
           Lenv.add_type_var s2 ot env, ot, ot)
    | Ltype(t1,l1), Ltype(t2,l2) when t1 = t2 ->
        let env,l1,l2 =
          List.fold_right2
            (fun ot nt (env,l1,l2) ->
               let (env,ot,nt) = instantiate ot nt env in
               (env,ot::l1,nt::l2))
            l1 l2 (env,[],[])
        in env,Ltype(t1,l1),Ltype(t2,l2)
    | Larrow(args1,rt1), Larrow(args2,rt2)
        when List.length args1 = List.length args2 ->
        let env, args1, args2 =
          List.fold_right2
            (fun ot nt (env, args1, args2) ->
               let (env,ot,nt) = instantiate ot nt env in
               (env,ot::args1,nt::args2))
            args1 args2 (env,[],[])
        in env, Larrow(args1,rt1), Larrow(args2,rt2)
    | (Ltype _|Larrow _|Lvar _), _ | _, (Larrow _| Ltype _) ->
        error loc "cannot instantiate %a to %a"
          d_logic_type ot
          d_logic_type nt
    | ((Ctype _ | Linteger | Lreal), (Ctype _ | Linteger | Lreal)) ->
        env,ot,nt
  in
  let env, ot, nt = instantiate ot nt env in
  let t,e = implicit_conversion ot nt in
  env, t, e

let logic_arithmetic_promotion = function
  | Ctype ty when Cil.isIntegralType ty -> Linteger
  | Linteger -> Linteger
  | Lreal -> Lreal
  | Ctype ty ->
      (match Cil.unrollType ty with TFloat _ -> Lreal | _ -> assert false)
  | Ltype _ | Lvar _ | Larrow _ -> assert false

let integral_promotion = function
  | Ctype ty when isIntegralType ty ->
      Linteger
  | Linteger -> Linteger
  | Ltype _ | Lreal | Lvar _ | Larrow _ | Ctype _ -> assert false

let arithmetic_conversion ty1 ty2 = match ty1, ty2 with
| Ctype ty1, Ctype ty2 ->
    if isIntegralType ty1 && isIntegralType ty2
    then Linteger
    else Lreal
| (Linteger, Ctype t | Ctype t, Linteger) when isIntegralType t ->
    Linteger
| Lreal, Ctype ty | Ctype ty, Lreal ->
    (match Cil.unrollType ty with TFloat _ -> Lreal | _ -> assert false)
| Linteger, Linteger -> Linteger
| (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
| _ -> assert false

let conditional_conversion loc t1 t2 = match t1, t2 with
| Ctype ty1, Ctype ty2 ->
    if isIntegralType ty1 && isIntegralType ty2 then
      Linteger
    else if isArithmeticType ty1 && isArithmeticType ty2 then
      Lreal
    else
      Ctype (C.conditionalConversion ty1 ty2)
| (Linteger, Ctype t | Ctype t, Linteger) when Cil.isIntegralType t
    ->
    Linteger
| (Ltype("boolean",[]), t | t, Ltype("boolean",[])) when is_integral_type t ->
    Ltype("boolean",[])
| Lreal, Ctype ty | Ctype ty, Lreal ->
    (match Cil.unrollType ty with
       TFloat _ -> Lreal
     | _ -> error loc "types %a and %a are not convertible"
         d_logic_type t1 d_logic_type t2)
| Ltype (s1,l1), Ltype (s2,l2)  when s1 = s2 && l1 = l2 -> t1
| Lvar s1, Lvar s2 when s1 = s2 -> t1
| Linteger, Linteger -> Linteger
| (Lreal | Linteger) , (Lreal | Linteger) -> Lreal
| _ ->
    error loc "types %a and %a are not convertible"
      d_logic_type t1 d_logic_type t2

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
        let value = Cil.reduce_multichar !Cil.wcharType tokens in
        Cil.kinteger64 !Cil.wcharKind value
    | '\'' -> (* 'char' *)
        let content = String.sub s 1 (String.length s - 2) in
        let tokens = explode content in
        let value,_= Cil.interpret_character_constant tokens in
        Const value
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

let is_pointer_type = function
    Ctype t -> Cil.isPointerType t
  | Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ -> false

let add_quantifiers loc q env =
  let (tq,env) =
    List.fold_left
      (fun (tq,env) (ty, id) ->
	 let ty = unroll_type (logic_type loc env ty) in
         let v = Cil.make_logic_var id ty in
         (v::tq, Lenv.add_var id v env))
      ([],env) q
  in
  (List.rev tq,env)

let rec term env t =
  match t.lexpr_node with
  | PLnamed(name,t) ->
      let t = term env t in { t with term_name = name :: t.term_name }
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
      begin match parseInt s with
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
	with e ->
          ignore (Errormsg.log "float_of_string %s (%s)\n" str
		    (Printexc.to_string e));
          TConst(CStr "booo CONS_FLOAT"), Lreal
      end
  | PLvar x ->
      begin
	try
	  let lv = Lenv.find_var x env in TLval (TVar lv, TNoOffset), lv.lv_type
	with Not_found ->
          try
	    let info = C.find_var x in
            (match info.lv_origin with
             | Some lv when lv.vglob -> ignore (find_current_label loc env)
             | _ -> ());
	    TLval (TVar info, TNoOffset), info.lv_type
	  with Not_found ->
	    try
	      let e,t = C.find_enum_tag x in
	      begin match Cil.constFold true e with
	      | Const c -> TConst c, Ctype t
	      | _ -> assert false
	      end
	    with Not_found ->
	      try
                let info = C.find_logic_function x in
                Tapp(info,[],[]), info.l_type
              with Not_found ->
                error loc "unbound logic variable %s" x
      end
  | PLapp (f, labels, tl) ->
      (try
	 let info = C.find_logic_function f in
	 let labels = List.map (find_logic_label loc env) labels in
	 let tl = type_terms env loc info.l_profile tl in
	 let label_assoc =
	   labels_assoc loc f env.Lenv.current_logic_label info.l_labels labels
	 in
	 Tapp (info, label_assoc, tl), info.l_type
       with Not_found ->
	 error loc "unbound function %s" f)
  | PLunop (Ubw_not, t) ->
      let t = type_int_term env t in
      TUnOp (BNot, t), logic_arithmetic_promotion t.term_type
  | PLunop (Uminus, t) ->
      let t = type_num_term env t in
      TUnOp (Neg, t), logic_arithmetic_promotion t.term_type
  | PLunop (Ustar, t) ->
      let _lab = find_current_label loc env in
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      begin match t.term_type with
      | Ctype (TPtr (ty,_))  ->
          check_non_void_ptr t.term_loc t.term_type;
	  TLval (mk_mem t TNoOffset), Ctype ty
      | _ -> error loc "invalid type %a for `unary *'" d_logic_type t.term_type
      end
  | PLunop (Uamp, t) ->
      let t, (ty:Cil_types.typ) = term_lval env t in
      mkAddrOfAndMark t,Ctype (TPtr(ty,[]))
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
      | Bmod when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	  (match arithmetic_conversion ty1 ty2 with
	   | Ctype ty as cty ->
	       (match Cil.unrollType ty with
		| TInt _  -> binop Mod cty
		| _ -> error loc "integer type expected")
	   | Linteger ->  binop Mod Linteger
           | _ -> error loc "integer type expected")
      | Badd | Bsub when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	  binop (type_binop op) (arithmetic_conversion ty1 ty2)
      | Bbw_and | Bbw_or | Bbw_xor
	    when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
	  binop (type_binop op) (arithmetic_conversion ty1 ty2)
      | Blshift | Brshift
            when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
          binop (type_binop op) (arithmetic_conversion ty1 ty2)
      | Badd when is_pointer_type ty1 && is_integral_type ty2 ->
	  TBinOp (PlusPI, t1, mk_cast t2 (integral_promotion ty2)), ty1
      | Badd when is_integral_type ty1 && is_pointer_type ty2 ->
	  TBinOp (PlusPI, t2, mk_cast t1 (integral_promotion ty1)), ty2
      | Bsub when is_pointer_type ty1 && is_integral_type ty2 ->
	  TBinOp (MinusPI, t1, mk_cast t2 (integral_promotion ty2)), ty1
      | Bsub when is_pointer_type ty1 && is_pointer_type ty2 ->
	  TBinOp (MinusPP, t1, mk_cast t2 ty1), c_int
      | _ ->
	  error loc "invalid operands to binary %a"d_binop (type_binop op)
      end
  | PLdot (t, f) ->
      let t = term env t in
      let f_ofs, f_type = type_of_field loc f t.term_type in
      let t_dot_x = match t.term_node with
      | TLval lv
      | TCastE (_, {term_node=TLval lv}) -> TLval (add_offset_lval f_ofs lv)
      | _ -> error loc "field access expects a left value"
      in
      t_dot_x, f_type

  | PLupdate (s, f, v) ->
      let s = term env s in
      let f_ofs, f_type = type_of_field loc f s.term_type in
      let f_ofs = match f_ofs with
      | TField (f,_) -> f
      | _ -> assert false
      in
      let v = mk_cast (term env v)  f_type in
      let updated = TUpdate(s,f_ofs,v)
      in
      updated, s.term_type

  | PLarrow (t, f) ->
      let _lab = find_current_label loc env in
      (* memory access need a current label to have some semantics *)
      let t = term env t in
      begin match t.term_type with
      | Ctype ctyp ->
          begin match Cil.unrollType ctyp with
          | (TPtr (ty, _) | TArray (ty, _, _)) ->
              let f_ofs, f_type = type_of_field loc f (Ctype ty) in
	      TLval (mk_mem ~loc t f_ofs), f_type
          | t -> error loc "invalid C type argument of `->': %a" d_type t

          end
      | t ->
	  error loc "invalid type argument of `->': %a" d_logic_type t
      end
  | PLarrget (t1, t2) ->
      let _lab = find_current_label loc env in
      (* memory access need a current label to have some semantics *)
      let t1 = term env t1 in
      let t2 = term env t2 in
      let t'1, t'2, tres =
	match unroll_type t1.term_type, unroll_type t2.term_type with
	| Ctype (TPtr (ty,_) | TArray (ty,_,_)),
	  (Ctype (TInt _ | TEnum _) | Linteger) -> t1, t2, ty
	| (Ctype (TInt _ | TEnum _) | Linteger),
	    Ctype (TPtr (ty,_) | TArray (ty,_,_)) ->
	    t2, t1, ty
	| _ ->
	    error loc "subscripted value is neither array nor pointer"
      in
      check_non_void_ptr t'1.term_loc t'1.term_type;
      begin match t'1.term_node with
      | TStartOf array ->
	  TLval (add_offset_lval (TIndex (t'2, TNoOffset)) array)
      | _ ->
	  let b = { term_node = TBinOp (IndexPI, t'1, t'2); term_name = [];
		    term_loc = loc; term_type = t'1.term_type }
	  in
	  TLval (mk_mem b TNoOffset)
      end,
      Ctype tres
  | PLif (t1, t2, t3) ->
      let t1 = type_bool_term env t1 in
      let t2 = term env t2 in
      let t3 = term env t3 in
      Tif (t1, t2, t3),
      conditional_conversion loc t2.term_type t3.term_type
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
      (match t.term_type with
       | Ctype (TArray _ | TPtr _) -> Tbase_addr t, c_addr
       | _ -> error loc "subscripted value is neither array nor pointer")
  | PLblock_length t ->
      let t = term env t in
      (match t.term_type with
       | Ctype (TArray _ | TPtr _) ->
           check_non_void_ptr t.term_loc t.term_type;
           Tblock_length t, c_int
       | _ -> error loc "subscripted value is neither array nor pointer")
  | PLresult ->
      (try let t = Lenv.find_var "\\result" env in
       TLval(TResult,TNoOffset), t.lv_type
       with Not_found -> error loc "\\result meaningless")
  | PLnull ->
      Tnull, c_void_star
  | PLcast (ty, t) ->
      let t = term env t in
      begin match logic_type loc env ty with
      | Ctype ty as cty -> TCastE (ty, t), cty
      | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
          error loc "cannot cast to logic type"
      end
  | PLcoercion (t,ty) ->
      let t = term env t in
      begin match logic_type loc env ty with
      | Ctype ty as cty -> TCoerce (t,ty), cty
      | Linteger | Lreal | Ltype _
      | Lvar _ | Larrow _ -> error loc "cannot coerce to logic type"
      end
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
      | _ when is_arithmetic_type ty1 && is_arithmetic_type ty2 ->
          let tr = arithmetic_conversion ty1 ty2 in
	  TBinOp(binop_of_rel op,mk_cast t1 tr,mk_cast t2 tr)
      | _ when is_pointer_type ty1 && is_pointer_type ty2 ->
	  TBinOp(binop_of_rel op, t1,t2)
      | Eq | Neq when is_pointer_type ty1 && is_zero t2 ->
	  TBinOp (binop_of_rel op, t1, mk_cast (Cil.lzero ~loc ()) ty1)
      | Eq | Neq when is_pointer_type ty2 && is_zero t1 ->
	  TBinOp (binop_of_rel op, mk_cast (Cil.lzero ~loc ()) ty2, t2)
      | Eq | Neq ->
	  let ty = conditional_conversion loc ty1 ty2 in
	  TBinOp (binop_of_rel op, mk_cast t1 ty, mk_cast t2 ty)
      | _ ->
	  error loc "comparison of incompatible types %a and %a"
            d_logic_type ty1 d_logic_type ty2
      in expr, Ltype("boolean",[])
  | PLtrue ->
      TDataCons(C.find_logic_ctor "\\true",[]), Ltype ("boolean",[])
  | PLfalse ->
      TDataCons(C.find_logic_ctor "\\false",[]), Ltype ("boolean",[])
  | PLlambda(prms,e) ->
      let (prms, env) = add_quantifiers loc prms env in
      let e = term env e in
      Tlambda(prms,e),Larrow(List.map (fun x -> x.lv_type) prms,e.term_type)
  | PLnot t ->
      let t = type_bool_term env t in TUnOp(LNot,t), Ltype ("boolean",[])
  | PLand (t1,t2) ->
      let t1 = type_bool_term env t1 in
      let t2 = type_bool_term env t2 in
      TBinOp(LAnd,t1,t2), Ltype ("boolean",[])
  | PLor (t1,t2) ->
      let t1 = type_bool_term env t1 in
      let t2 = type_bool_term env t2 in
      TBinOp(LOr,t1,t2), Ltype ("boolean",[])
  | PLtypeof t1 ->
      let t1 = term env t1 in
      Ttypeof t1, Ltype ("typetag",[])
  | PLtype ty ->
      begin match logic_type loc env ty with
      | Ctype ty -> Ttype ty, Ltype ("typetag",[])
      | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
          error loc "cannot take type tag of logic type"
      end
  | PLvalid _ | PLvalid_index _ | PLvalid_range _ | PLfresh _
  | PLexists _ | PLforall _  | PLimplies _ | PLiff _
  | PLxor _
  | PLsubtype _ ->
      error loc "syntax error (expression expected but predicate found)"
  | PLcomprehension _ | PLunion _ | PLinter _ | PLempty | PLrange _ ->
      error loc "syntax error (expression expected but tsets found)"
  in
  match typed_term_node, unroll_type typ with
  | (TLval lval|TCastE(_, {term_node = TLval lval})), Ctype (TArray (t,_,_)) ->
      TStartOf lval, Ctype (TPtr(t,[]))
  | _, Ctype (TArray (_,_,_)) -> assert false
  | _ -> typed_term_node,typ

and type_int_term env t =
  let tt = term env t in
  if not (is_integral_type tt.term_type) then
    error t.lexpr_loc "integer expected but %a found" d_logic_type tt.term_type;
  tt

and type_bool_term env t =
  let tt = term env t in
  if not (is_boolean_type tt.term_type) then
    error t.lexpr_loc "boolean expected but %a found" d_logic_type tt.term_type;
  tt

and type_int_term_option env = Cilutil.opt_map (type_int_term env)

and type_num_term env t =
  let tt = term env t in
  if not (is_arithmetic_type tt.term_type) then
    error t.lexpr_loc "integer or float expected";
  tt

and type_num_pointer_term env t =
  let tt = term env t in
  if not (is_arithmetic_type tt.term_type || is_pointer_type tt.term_type) then
    error t.lexpr_loc "integer, float or pointer expected";
  tt
    (* only for C lvalue. Use logic_term_lval below for cases where any
       (C and purely logical) lval can happen *)
and term_lval env t =
  let t = term env t in
  match t.term_node, t.term_type with
  | (TLval lv|TStartOf lv), Ctype ty -> lv, ty
  | TCastE(_,{term_node = (TLval lv|TStartOf lv)}), Ctype ty -> lv, ty
  | TLval _, _ -> assert false
  | _ -> error t.term_loc "not a left value"

and type_terms env loc at tl =
  let obj =
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
    end
  in
  let rec type_list env = function
    | [], [] ->
	env, []
    | { lv_type = et } :: etl, ({lexpr_loc=tloc} as t) :: tl ->
	let arg = term env t in
        let nt = Cil.visitCilLogicType obj et in
	let env, _,t' = instantiate_app tloc arg nt env in
	let env, l = type_list env (etl, tl) in env, t' :: l
    | [], _ ->
	error loc "too many arguments"
    | _, [] ->
	error loc "partial application"
  in
  snd (type_list env (at, tl))

(* Typing predicates *)
let rec tsets_lval env pl =
  let loc = pl.lexpr_loc in
  let elem,typ =
    match pl.lexpr_node with
      | PLconstant (IntConstant s) ->
          begin match parseInt s with
            | Const (CInt64 (_,_,_) as c) -> TSConst c, Linteger
            | Const ((CChr _) as c) -> (* a char literal has type int *)
                TSConst c, Linteger
            | _ -> assert false
          end
      | PLvar x ->
          begin
	    try
	      let lv = Lenv.find_var x env in
              TSLval (TSVar lv, TSNo_offset),lv.lv_type
	    with Not_found ->
	      try
	        let info = C.find_var x in
	        TSLval (TSVar info, TSNo_offset),info.lv_type
	      with Not_found ->
                try
	          let e,t = C.find_enum_tag x in
	          begin match Cil.constFold true e with
	            | Const c -> TSConst c, Ctype t
	            | _ -> assert false
	          end
	        with Not_found ->
                                (* TODO: global logic constants in tsets... *)
                  try
                    let _info = C.find_logic_function x in
                    error loc "Global logic constants are unsupported in locations"
                  with Not_found ->
                  error loc "unbound logic variable %s" x
          end
      | PLunop(Ustar,e) ->
	  let _lab = find_current_label loc env in
	  (* memory access need a current label to have some semantics *)
          let e,typ = tsets_lval env e in
          begin match typ with
              Ctype (TPtr(ty,_) | TArray(ty,_,_)) ->
                TSLval(mk_tsmem loc typ e TSNo_offset), Ctype ty
            | _ ->
                error loc "cannot dereference a non-pointer value"
          end
      | PLbinop (t,Badd,{ lexpr_node = PLrange(low,high)})
      | PLbinop({lexpr_node = PLrange(low,high)},Badd,t) ->
          let low = Cilutil.opt_map (type_int_term env) low in
          let high = Cilutil.opt_map (type_int_term env) high in
          let elem,typ = tsets_lval env t in
          if is_pointer_type typ then TSAdd_range(elem,low,high), typ
          else error t.lexpr_loc "pointer or array expected"
      | PLbinop (t1, Badd, t2) ->
          let ptr,ptr_type,idx =
            try
              let ptr, ptr_type = tsets_lval env t1  in
              if is_pointer_type ptr_type then
                let idx = type_int_term env t2 in
                ptr,ptr_type,idx
              else
                let idx = type_int_term env t1 in
                let ptr, ptr_type = tsets_lval env t2 in
                if is_pointer_type ptr_type then
                  ptr,ptr_type,idx
                else error loc "not a tsets element"
            with Errormsg.Error -> (*TODO: refine error type*)
              let idx = type_int_term env t1 in
              let ptr, ptr_type = tsets_lval env t2 in
              ptr, ptr_type, idx
          in TSAdd_index(ptr,idx), ptr_type
      | PLdot (t, f) ->
          let elem,typ = tsets_lval env t in
          let f_ofs, f_type = type_of_tsfield loc f typ in
          let t_dot_x = match elem with
            | TSLval lv
            | TSCastE (_, TSLval lv) -> TSLval (add_offset_tslval f_ofs lv)
            | _ -> error loc "PLdot: expected a left value"
          in
          t_dot_x, f_type
      | PLarrow (t, f) ->
	  let _lab = find_current_label loc env in
	  (* memory access need a current label to have some semantics *)
          let elem,typ = tsets_lval env t in
          begin match typ with
            | Ctype ctyp ->
                begin match Cil.unrollType ctyp with
                  | (TPtr (ty, _) | TArray (ty, _, _)) ->
                      let f_ofs, f_type = type_of_tsfield loc f (Ctype ty) in
	              TSLval (mk_tsmem loc typ elem f_ofs), f_type
                  | t ->
                      error loc "invalid C type argument of `->': %a"
                        Cil.d_type t
                end
            | t ->
	        error loc "invalid type argument of `->': %a" Cil.d_logic_type t
          end
      | PLarrget(t,{lexpr_node = PLrange(low,high)})
      | PLarrget({lexpr_node = PLrange(low,high)},t) ->
	  let _lab = find_current_label loc env in
	  (* memory access need a current label to have some semantics *)
          let elem,typ = tsets_lval env t in
          let low = Cilutil.opt_map (type_int_term env) low in
          let high = Cilutil.opt_map (type_int_term env) high in
          let content_type =
            match typ with
                Ctype (TPtr (ty,_) | TArray (ty,_,_)) -> Ctype ty
              | _ -> error loc "subscripted value is neither array nor pointer"
          in
          check_non_void_ptr loc typ;
          begin
            match elem with
                TSStartOf array ->
                  TSLval(add_offset_tslval
                           (TSRange(low,high,TSNo_offset)) array),
                  content_type
            | _  ->
                TSLval(mk_tsmem loc typ
                         (TSAdd_range(elem,low,high)) TSNo_offset),
                content_type
          end
      | PLarrget (t1, t2) ->
	  let _lab = find_current_label loc env in
	  (* memory access need a current label to have some semantics *)
          let t'1, t'2, ptr_type =
            try
              let t'1, typ1 = tsets_lval env t1 in
              if is_pointer_type typ1 then
                let t'2 = type_int_term env t2 in
                t'1,t'2,typ1
              else
                let t'1,typ1 = tsets_lval env t2 in
                let t'2 = type_int_term env t1 in
                t'1,t'2,typ1
            with Errormsg.Error -> (* TODO: refine error type *)
              let t'1,typ1 = tsets_lval env t2 in
              let t'2 = type_int_term env t1 in
              t'1,t'2,typ1
          in
          check_non_void_ptr loc ptr_type;
          let tres = match unroll_type ptr_type with
              Ctype (TPtr (ty,_) | TArray (ty,_,_)) -> ty
            | _ ->
	        error loc "subscripted value is neither array nor pointer"
          in
          begin match t'1 with
            | TSStartOf array ->
	        TSLval (add_offset_tslval (TSIndex (t'2, TSNo_offset)) array)
            | _ ->
	        TSLval (mk_tsmem loc ptr_type
                          (TSAdd_index(t'1,t'2)) TSNo_offset)
          end,
          Ctype tres
  | PLresult ->
      (try let t = Lenv.find_var "\\result" env in
       TSLval(TSResult,TSNo_offset), t.lv_type
       with Not_found -> error loc "\\result meaningless")
  | PLcast (ty, t) ->
      let t,_ = tsets_lval env t in
      begin match logic_type loc env ty with
      | Ctype ty as cty -> TSCastE (ty, t), cty
      | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _ ->
          error loc "cannot cast to logic type"
      end
  | PLat(loc,l) ->
      let lab = find_logic_label loc.lexpr_loc env l in
      let env = Lenv.set_current_logic_label lab env in
      let elem,typ = tsets_lval env loc in
      TSAt(elem,lab), typ

  | PLvalid _ | PLvalid_index _ | PLvalid_range _ | PLfresh _
  | PLexists _ | PLforall _ | PLnot _ | PLimplies _ | PLiff _
  | PLor _ | PLxor _ | PLand _
  | PLsubtype _ | PLtrue | PLfalse ->
      error loc "syntax error (location expected but predicate found)"
  | PLcomprehension _ | PLunion _ | PLinter _ | PLempty | PLrange _ ->
      error loc "syntax error (single location expected but tsets found)"
  | PLapp _ | PLunop _ | PLbinop _ | PLold _ | PLbase_addr _ | PLblock_length _
  | PLnull | PLcoercion _ | PLcoercionE _ | PLrel _ | PLif _ | PLnamed _
  | PLconstant _ | PLsizeof _ | PLsizeofE _ | PLupdate _ | PLlambda _ 
  | PLtypeof _ | PLtype _ ->
      error loc "syntax error (location expected but term found)"
 in
  match elem, unroll_type typ with
  | (TSLval lval|TSCastE(_, TSLval lval)), Ctype (TArray (t,_,_)) ->
      TSStartOf lval, Ctype (TPtr(t,[]))
  | _, Ctype (TArray (_,_,_)) -> assert false
  | _ -> elem,typ

(* Normal form of a location set: union of comprehension of terms *)
let normalize_location loc =
  let rec aux transf pl =
    let new_term t = { pl with lexpr_node = t } in
    match pl.lexpr_node with
      | PLvar _ | PLresult | PLempty -> transf pl
      | PLat (elem,lab) ->
          aux (fun x -> transf (new_term (PLat(x,lab)))) elem
      | PLunop(Ustar,pl) -> aux
          (fun x -> transf (new_term (PLunop(Ustar,x)))) pl
      | PLbinop(pl1, Badd, pl2) ->
          let pl1' = aux (fun x -> x) pl1 in
          aux (fun x -> transf (new_term (PLbinop(pl1',Badd,x)))) pl2
      | PLdot(pl1,field) ->
          aux (fun x -> transf (new_term (PLdot (x,field)))) pl1
      | PLarrow(pl1,field) ->
          aux (fun x -> transf (new_term (PLarrow (x,field)))) pl1
      | PLarrget(pl1,pl2) ->
          let pl1' = aux (fun x -> x) pl1 in
          aux (fun x -> transf (new_term (PLarrget(pl1',x)))) pl2
      | PLcomprehension(pl,quant,pred) ->
          new_term
            (PLcomprehension (aux transf pl, quant, pred))
      | PLunion pls ->
          let pls = List.map (aux transf) pls in
          new_term (PLunion pls)
      | PLinter pls ->
          let pls = List.map (aux transf) pls in
          new_term (PLinter pls)
      (* Grammar forbids to have union or comprehension inside
         those expr.
       *)
      | PLapp _ | PLconstant _ | PLunop _ | PLbinop _ | PLold _
      | PLbase_addr _ | PLnull | PLcast _ | PLcoercion _ | PLcoercionE _
      | PLfalse | PLtrue | PLrel _ | PLand _ | PLor _ | PLxor _
      | PLnot _ | PLif _ | PLblock_length _   | PLrange _ | PLupdate _
      | PLlambda _ | PLtypeof _ | PLtype _ ->
          transf pl
            (* tsets are terms, not predicates *)
      | PLimplies _ | PLiff _ | PLforall _ | PLexists _ | PLvalid _
      | PLvalid_index _ | PLvalid_range _ | PLfresh _ | PLnamed _
      | PLsubtype _ | PLsizeof _ | PLsizeofE _ ->
          error pl.lexpr_loc "expecting tsets and not a predicate"
  in
  aux (fun x -> x) loc

let boolean_to_predicate env p0 =
  let loc = p0.lexpr_loc in
  let t = term env p0 in
  (match t.term_type with
     Ctype _ | Linteger | Lreal ->
       prel ~loc:p0.lexpr_loc (Cil_types.Rneq, t, Cil.lzero ~loc ())
   | Ltype ("boolean",[]) ->
       prel ~loc: p0.lexpr_loc
	 (Cil_types.Req,t,
          { term_node =
              TDataCons(C.find_logic_ctor "\\true",[]);
            term_loc = t.term_loc;
            term_type = Ltype("boolean",[]);
            term_name = [];
          })
   | Ltype _ | Lvar _ | Larrow _ ->
       error p0.lexpr_loc "expecting a predicate and not a term")


let rec type_tset ?(check_type=fun _ ->()) env loc0 =
  let loc = normalize_location loc0 in
  let rec aux env loc =
    match loc.lexpr_node with
        PLcomprehension(t,quant,pred) ->
          let q, env' = add_quantifiers loc0.lexpr_loc quant env in
          let v = aux env' t in
          let p = match pred with
              None -> None
            | Some p -> Some (predicate env' p)
          in TSComprehension(v,q,p)
      | PLunion locs ->
          let ls = List.map (aux env) locs in
          TSUnion(ls)
      | PLinter locs ->
          let ls = List.map (aux env) locs in
          TSInter(ls)
      | PLempty -> TSEmpty
      | _ -> let (v,typ) = tsets_lval env loc in
        check_type typ;
        TSSingleton v
  in aux env loc

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
	| _ when is_pointer_type ty1 && is_pointer_type ty2 ->
	    prel ~loc:p0.lexpr_loc
	      (type_binop op, t1, t2)
	| Eq | Neq when is_pointer_type ty1 && is_zero t2 ->
	    let tr = ty1 (* Ctype !Cil.upointType *) in
	    prel ~loc:p0.lexpr_loc
	      (type_binop op, t1, mk_cast (Cil.lzero ~loc ()) tr)
	| Eq | Neq when is_pointer_type ty2 && is_zero t1 ->
	    let tr = ty2 (* Ctype !Cil.upointType *) in
	    prel ~loc:p0.lexpr_loc
	      (type_binop op, mk_cast (Cil.lzero ~loc ()) tr, t2)
	| Eq | Neq ->
	    let ty = conditional_conversion loc ty1 ty2 in
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
	 | {content = Prel (Cil_types.Rneq, t, z)} when is_zero z ->
	     prel ~loc:p0.lexpr_loc (Cil_types.Req, t, Cil.lzero ~loc ())
	 | p -> pnot ~loc:p0.lexpr_loc p)
  | PLapp (p, labels, tl) when is_predicate p ->
      let info = C.find_predicate p in
      let labels = List.map (find_logic_label p0.lexpr_loc env) labels in
      let tl = type_terms env p0.lexpr_loc info.p_profile tl in
      let label_assoc =
	labels_assoc loc p env.Lenv.current_logic_label info.p_labels labels
      in
      papp ~loc:p0.lexpr_loc (info, label_assoc, tl)
  | PLapp (p,_, _) when not (is_logic_function p ||
                               is_logic_ctor p) ->
      error p0.lexpr_loc "unknown predicate %s" p
  (* see below for implicit conversion of terms into predicate *)
  | PLif (t, p1, p2) ->
      begin try
        let t = type_bool_term env t in
        pif ~loc:p0.lexpr_loc (t, predicate env p1, predicate env p2)
      with Errormsg.Error ->
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
      let t = type_tset ~check_type:(check_non_void_ptr t.lexpr_loc) env t in
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
         let info = C.find_predicate x in
         papp ~loc:p0.lexpr_loc (info,[],[])
       with Not_found -> boolean_to_predicate env p0)
  | PLapp _
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
  | PLcomprehension _ | PLunion _ | PLinter _ | PLempty ->
      error p0.lexpr_loc "expecting a predicate and not tsets"

let type_variant env = function
  | (t, None) -> (type_int_term env t, None)
  | (t, r) -> (term env t, r)

let logic_term_lval env t =
  let t' = term env t in
  match t'.term_node with
      TLval lv | TStartOf lv
    | TCastE(_,{term_node = (TLval lv|TStartOf lv)}) -> (lv,t'.term_type)
    | _ -> error t.lexpr_loc "not a left value:%a"
        Cil.d_term t'

let logic_term_lhost env t =
  let ((h,o),typ) = logic_term_lval env t in
  match o with
      TNoOffset -> h,typ
    | _ -> error t.lexpr_loc "not a lhost"
        (*FIXME:update tsets type to support e.g. t[0..2]->x *)


let type_zone env =
  function
      Nothing -> Nothing
    | Location a -> Location (Logic_const.new_location (type_tset env a))

let get_zone_loc = function
    Nothing -> Cil.locUnknown
  | Location l -> l.lexpr_loc

let type_assign env (a,f) =
  let ta = type_zone env a in
    (match ta with
         Location ta ->
           if Logic_const.tsets_contains_result ta.its_content &&
             not (Logic_const.tsets_is_result ta.its_content) then
               error (get_zone_loc a) "invalid \\result in location"
       | Nothing -> ());
  let tf =
    List.map
      (fun d ->
         let td = type_zone env d in
         (match td with
              Nothing -> ()
            | Location td ->
                if Logic_const.tsets_contains_result td.its_content then
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
  let v = Cil.make_logic_var ti.this_name this_type in
  let env = Lenv.add_var ti.this_name v env in
  let body = predicate env ti.inv in
  let infos = {p_name = ti.inv_name;
               p_profile = [v];
	       p_labels = [LogicLabel "Here"];
               p_body = PDefinition body}
  in C.add_predicate infos; infos

let type_spec result env s =
  let env = append_here_label env in
  let p = List.map (id_predicate env) s.spec_requires in
  let env_with_result = match result with
    | None -> env
    | Some ty ->
	let v = Cil.make_logic_var "\\result" ty in
	Lenv.add_var "\\result" v env
  in
  let post_state_env =
    Lenv.add_logic_label "Old" (LogicLabel "Old") env_with_result in
  let b = List.map
    (fun {b_assigns= ba;
          b_name = bn; b_ensures=be;
          b_assumes= bas} ->
       {b_assigns= List.map (type_assign post_state_env) ba;
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

let funspec ~id ~formals typ s =
  try
    let log_return_typ = match typ with
    | TFun (TVoid _ , _, _, _) -> None
    | TFun (typ , _, _, _) -> Some (Ctype typ)
    | typ ->
      ignore (Cil.warn "ignoring unexpected function annotation on %a" Cil.d_type typ);
        raise Not_found
    in
    let env =
      match formals with
        | None -> (* This is the spec of a function declaration *)
         (* begin match typ with
          | TFun (_, None, _, _) -> Lenv.empty
          | TFun (_ , Some all_formals, _, _) ->
	      (* JS : TODO *)
              let add_formal env (n,t,_) =
                Lenv.add n (Cil.make_logic_var n (Ctype t)) env
              in
              List.fold_left add_formal Lenv.empty all_formals
          | _ -> assert false
          end *)
            let add_formal env v =
                Lenv.add_var v.vname (Cil.cvar_to_lvar v) env
            in
            (try
               List.fold_left add_formal Lenv.empty (Cil.getFormalsDecl id)
             with Not_found -> Lenv.empty)
      | Some formals ->
          let add_formal env v =
            Lenv.add_var v.vname (Cil.cvar_to_lvar v) env in
          List.fold_left add_formal Lenv.empty formals
    in type_spec log_return_typ env s
  with Not_found -> Cil.empty_funspec ()

let slice_pragma env = function
    SPexpr t -> SPexpr (term env t)
  | (SPctrl | SPstmt) as sp -> sp

let impact_pragma env = function
    IPexpr t -> IPexpr (term env t)
  | IPstmt as ip -> ip

let code_annot_env =
  let env = make_here_label () in
  Lenv.add_logic_label "Pre" (LogicLabel "Pre") env

let code_annot ca =
  let annot = match ca with
    | AAssert (behav,p) -> AAssert (behav,predicate code_annot_env p)
    | AAssume p -> AAssume (predicate code_annot_env p)
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
    | AAssigns a -> AAssigns (type_assign code_annot_env a)
  in Logic_const.new_code_annotation annot

let formals loc env p =
  let add_var (p,env) (t,x) =
    let lt = logic_type loc env t in
    let lt = array_to_ptr lt in
    let var = Cil.make_logic_var x lt in
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
let check_polymorphism loc t p =
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
  ignore (Cil.visitCilLogicType (obj rt_vars) t);
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

let annot loc annot =
  Cil.currentLoc:= loc;
  match annot with
    | LDlogic_reads (f, labels, poly, t, p, l) ->
        let labels,env = annot_env loc labels poly in
        let t = logic_type loc env t in
        let p, env = formals loc env p in
        check_polymorphism loc t p;
        let l = List.map (type_tset env) l in
        let info =
          {l_name = f;
           l_profile = p;
           l_type = t;
           l_reads= l;
	   l_labels = labels;
           l_definition = None}
        in
        let info =
          if is_logic_function f then begin
            let old_info = C.find_logic_function f in
            if Logic_const.is_same_logic_signature old_info info then begin
              Logic_const.merge_logic_reads old_info info; old_info
            end else
              error loc "logic function %s has incompatible declarations" f
          end else begin
            C.add_logic_function info;
            info
          end
        in
        Dlogic_reads (info, poly, p, t, l)
    | LDlogic_def(f, labels, poly,t,p,e) ->
        let labels,env = annot_env loc labels poly in
        let t = logic_type loc env t in
        let p, env = formals loc env p in
        check_polymorphism loc t p;
        let info =
          { l_name = f; l_profile = p; l_type = t; l_labels = labels;
            l_reads = []; l_definition = None (*for the time being*)}
        in
        let info,redefinition =
          if is_logic_function f then begin
            let old_info = C.find_logic_function f in
            if Logic_const.is_same_logic_signature old_info info then
              (match old_info.l_definition with
                   None -> old_info,true
                 | Some _ ->
                     error loc "logic function %s has multiple definitions" f)
            else
              error loc "logic function %s has incompatible declarations" f
          end else begin
            C.add_logic_function info; info,false
          end
        in
        (try 
          let e = term env e in
          let _,new_typ,new_term = instantiate_app loc e t env in
          if Logic_const.is_same_type new_typ t then begin
            info.l_definition <- Some new_term;
            Dlogic_def (info,poly,p,t,e)
          end else
            error loc "return type of logic function %s is %a but %a was expected" 
              f d_logic_type new_typ d_logic_type t
         with Errormsg.Error as e when not redefinition -> 
           C.remove_logic_function f; raise e)

    | LDpredicate_reads (f, labels, poly, p, l) ->
        let labels,env = annot_env loc labels poly in
        let p, env = formals loc env p in
        let l = List.map (type_tset env) l in
        let info = {p_name = f; p_profile = p; p_body = PReads l;
		    p_labels = labels}
        in C.add_predicate info;
        Dpredicate_reads (info, poly, p, l)
    | LDpredicate_def (f, labels, poly, p, e) ->
        let labels,env = annot_env loc labels poly in
        let p, env = formals loc env p in
        (* allow recursive definitions *)
        let pinfo =
          {p_name = f;
           p_profile = p;
	   p_labels = labels;
           p_body = PReads [] (* updated in 3 lines *)};
        in
        C.add_predicate pinfo;
        let e = predicate env e in
        pinfo.p_body <- PDefinition e;
        Dpredicate_def (pinfo, poly, p, e)
    | LDtype(s,l) ->
        ignore (init_type_variables loc l);
        C.add_logic_type s {nb_params = (List.length l)}; Dtype (s,l)
    | LDlemma (is_axiom, x, labels, poly, e) ->
        let labels,env = annot_env loc labels poly in
        Dlemma (is_axiom, x, labels, poly,  predicate env e)
    | LDinvariant (s, e) ->
        let env = make_here_label () in
        let p = predicate env e in
        let infos =
          {p_name = s;
           p_profile = [];
	   p_labels = [LogicLabel "Here"];
           p_body = PDefinition p}
        in C.add_predicate infos; Dinvariant infos
    | LDtype_annot l ->
        Dtype_annot (type_annot loc l)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
