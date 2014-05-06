(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Variable Analysis                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Cil_types
open Cil_datatype

(* -------------------------------------------------------------------------- *)
(* --- Var Info Accesses                                                  --- *)
(* -------------------------------------------------------------------------- *)

type var = Result | Cvar of varinfo | Lvar of logic_var

module Var =
struct
  type t = var
  let hash = function
    | Result -> 0
    | Cvar x -> 2 * x.vid
    | Lvar x -> 3 * x.lv_id
  let compare x y = match x,y with
    | Result , Result -> 0
    | Result , _ -> (-1)
    | _ , Result -> 1
    | Cvar x , Cvar y -> Varinfo.compare x y
    | Lvar x , Lvar y -> Logic_var.compare x y
    | Cvar _ , Lvar _ -> (-1)
    | Lvar _ , Cvar _ -> 1
  let equal x y = (compare x y = 0)
  let pretty fmt = function
    | Result -> Format.fprintf fmt "\\result"
    | Cvar x -> Format.fprintf fmt "C%d:%s" x.vid x.vname
    | Lvar x -> Format.fprintf fmt "L%d:%s" x.lv_id x.lv_name
end

type access =
  | NoAccess
  | ByAddr    (* The expression ["&x"] *)
  | ByValue   (* The expression ["x"], equal to [load(&x)] *)
  | ByArray   (* The expression ["x[_]"], equal to [load(shift(load(&x),_))] *)
  | ByRef     (* The expression ["*x"], equal to [load(load(&x))] *)
      
module Access :
sig
  type t = access
  (* val is_bot : t -> bool *) (* unused for now *)
  (*val leq : t -> t -> bool*) (* unused for now *)
  val cup : t -> t -> t
  (* val pretty : var -> Format.formatter -> t -> unit *) (* unused for now *)
end =
struct
  type t = access
  (* unused for now *)
  (* let is_bot = function NoAccess -> true | _ -> false *)
  (* unused for now *)
  (* let pretty x fmt = function
    | NoAccess -> Format.fprintf fmt "-"
    | ByValue -> Var.pretty fmt x
    | ByAddr -> Format.fprintf fmt "&%a" Var.pretty x
    | ByRef -> Format.fprintf fmt "*%a" Var.pretty x
    | ByArray -> Format.fprintf fmt "%a[_]" Var.pretty x
  *)
  let rank = function
    | NoAccess -> 0
    | ByRef -> 1
    | ByArray -> 2
    | ByValue -> 3
    | ByAddr -> 4
(*  let leq a b = (rank a) <= (rank b)*) (* unused for now *)
  let cup a b = if rank a < rank b then b else a
end

(* -------------------------------------------------------------------------- *)
(* --- Expressions & Memory Model                                         --- *)
(* -------------------------------------------------------------------------- *)

module E :
sig
  type t
  val bot : t
  val cup : t -> t -> t
  (* val leq : t -> t -> bool *) (* unused for now *)
  (* val lcup : t list -> t *) (* unused for now *)
  val fcup : ('a -> t) -> 'a list -> t
  val get : var -> t -> access
  val access : var -> access -> t -> t
  val bind : logic_var list -> t -> t
end =
struct

  module Xmap = Qed.Mergemap.Make(Var)
  type t = access Xmap.t
      
  let bot = Xmap.empty
  let cup = Xmap.union (fun _ -> Access.cup)
  (* unused for now *)
  (* let leq = Xmap.subset (fun _ -> Access.leq) *)

  (* unused for now *)
(*  let rec lcup = function [] -> bot | [x] -> x | x::xs -> cup x (lcup xs)*)
  let rec fcup f = function [] -> bot | [x] -> f x | x::xs -> cup (f x) (fcup f xs)

  let get x e = try Xmap.find x e with Not_found -> NoAccess
  let access x u e = 
    Xmap.add x (try Access.cup (Xmap.find x e) u with Not_found -> u) e

  let rec bind xs e = match xs with
    |  [] -> e | x::xs -> bind xs (Xmap.remove (Lvar x) e)
end

type value = E.t
type model =
  | L (* Logic, same as E.bot *)
  | E of value (* E *)
  | Loc_var of varinfo  (* &x *)
  | Loc_shift of varinfo * value (* &x.[...] *)
  | Val_var of var  (* x *)
  | Val_shift of var * value (* (x + E) *)

let vcup a b = E (E.cup a b)
(* let lcup xs = E (E.lcup xs) *) (* unused for now *)
let fcup f xs = E (E.fcup f xs)

let value = function
  | Loc_var x -> E.access (Cvar x) ByAddr E.bot 
  | Loc_shift(x,e) -> E.access (Cvar x) ByAddr e
  | Val_var x -> E.access x ByValue E.bot
  | Val_shift(x,e) -> E.access x ByValue e
  | E e -> e
  | L -> E.bot

let cvar x = Loc_var x
let shift (l:model) (k:value) = match l with
  | Loc_var x -> Loc_shift(x,k)
  | Val_var x -> Val_shift(x,k)
  | Loc_shift(x,e) -> Loc_shift(x,E.cup e k)
  | Val_shift(x,e) -> Val_shift(x,E.cup e k)
  | E e -> E (E.cup e k)
  | L -> E k
let field (l:model) = match l with
  | Loc_var x -> Loc_shift(x,E.bot)
  | Loc_shift _ -> l
  | Val_var x -> E (E.access x ByValue E.bot)
  | Val_shift(x,e) -> E (E.access x ByValue e)
  | E _ | L -> l

let load = function
  | Loc_var x -> Val_var (Cvar x)
  | Loc_shift(x,e) -> E (E.access (Cvar x) ByValue e)
  | Val_var x -> E (E.access x ByRef E.bot)
  | Val_shift(x,e) -> E (E.access x ByArray e)
  | (E _ | L) as m -> m

(* for \\valid and \\separated : no variable escape, excepts for shifts *)
let reference = function
  | L | Loc_var _ | Val_var _ -> E.bot
  | E e | Loc_shift(_,e) | Val_shift(_,e) -> e

(* -------------------------------------------------------------------------- *)
(* --- Casts                                                              --- *)
(* -------------------------------------------------------------------------- *)

type cast =
  | Identity
  | Convert
  | Cast

let cast cv e = match cv with
  | Identity -> e
  | Convert | Cast -> E (value e)

let cast_obj tgt src =
  match tgt , src with
    | (C_int _ | C_float _) , (C_int _ | C_float _) -> Convert
    | C_pointer tr , C_pointer te ->
	let obj_r = Ctypes.object_of tr in
	let obj_e = Ctypes.object_of te in
	if Ctypes.compare obj_r obj_e = 0 
	then Identity
	else Cast
    | _ -> if Ctypes.equal tgt src then Identity else Cast

let cast_ctyp tgt src = cast_obj (Ctypes.object_of tgt) (Ctypes.object_of src)
let cast_ltyp tgt src = match Logic_utils.unroll_type src with
  | Ctype src -> cast_ctyp tgt src
  | _ -> Cast

(* -------------------------------------------------------------------------- *)
(* --- Call                                                               --- *)
(* -------------------------------------------------------------------------- *)

let param a m = match a with
  | NoAccess | ByAddr -> E.bot (* should never arise *)
  | ByValue -> value m
  | ByRef -> value (load m)
  | ByArray -> value (load (shift m E.bot))

let rec call f xs ms = match xs , ms with
  | [] , _ | _ , [] -> E.bot
  | x::xs , m::ms ->
      let a = E.get x f in
      E.cup (param a m) (call f xs ms)

type context = {
  mutable locals : Logic_var.Set.t ;
  mutable logic : E.t Logic_info.Map.t ;
  mutable spec : E.t Kernel_function.Map.t ;
  mutable code : E.t Kernel_function.Map.t ;
  mutable w_kf : Kernel_function.Set.t ;
  mutable w_lg : Logic_info.Set.t ;
}

let call_kf context kf ms =
  try
    context.w_kf <- Kernel_function.Set.add kf context.w_kf ;
    let phi = Kernel_function.Map.find kf context.spec in
    let xs = List.map (fun x -> Cvar x) (Kernel_function.get_formals kf) in
    call phi xs ms
  with Not_found -> E.bot

let call_lg context f ms =
  try
    context.w_lg <- Logic_info.Set.add f context.w_lg ;
    let phi = Logic_info.Map.find f context.logic in
    let xs = List.map (fun x -> Lvar x) f.l_profile in
    call phi xs ms
  with Not_found -> E.bot

(* -------------------------------------------------------------------------- *)
(* --- Compilation of C-Expressions                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec vexpr e = value (expr e) 

and expr (e:Cil_types.exp) : model = match e.enode with

    (* Logics *)
    | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> L

    (* Unary *)
    | UnOp((Neg|BNot|LNot),e,_) | Info(e,_) -> expr e

    (* Binary *)
    | BinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
	     |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
	     |Lt|Gt|Le|Ge|Eq|Ne), a,b,_ ) 
      -> vcup (vexpr a) (vexpr b)
	
    (* Shifts *)
    | BinOp((PlusPI|IndexPI|MinusPI),a,b,_) -> shift (expr a) (vexpr b)

    (* Casts *)
    | CastE(ty_tgt,e) -> cast (cast_ctyp ty_tgt (Cil.typeOf e)) (expr e)
	  
    (* Address *)
    | AddrOf lval | StartOf lval -> lvalue lval

    (* Load *)
    | Lval lval -> load (lvalue lval)

and lvalue (h,ofs) = offset (host h) ofs
and host = function
  | Var x -> cvar x
  | Mem e -> expr e

and offset (l:model) = function
  | NoOffset -> l
  | Field(_,ofs) -> offset (field l) ofs
  | Index(e,ofs) -> offset (shift l (vexpr e)) ofs

(* -------------------------------------------------------------------------- *)
(* --- Compilation of ACSL-Terms                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec vterm (env:context) t = value (term env t)

and vtermopt (env:context) = function None -> E.bot | Some t -> vterm env t

and term (env:context) (t:term) : model = match t.term_node with

  (* Logics *)
  | TConst _ 
  | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ 
  | Ttypeof _ | Ttype _ -> L

  (* Unary *)
  | TUnOp((Neg|BNot|LNot),t) -> term env t

  (* Binary *)
  | TBinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
	    |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
	    |Lt|Gt|Le|Ge|Eq|Ne), a,b ) 
    -> vcup (vterm env a) (vterm env b)

  (* Shifts *)
  | TBinOp((PlusPI|IndexPI|MinusPI),a,b) -> shift (term env a) (vterm env b)

  (* Casts *)
  | TCastE(ty_tgt,t) -> cast (cast_ltyp ty_tgt t.term_type) (term env t)

  (* Term L-Values *)
  | TLval tlv -> term_lval env tlv
  | TAddrOf tlv | TStartOf tlv -> addr_lval env tlv
  | TUpdate(s,ofs,t) ->
      let v = vterm env s in
      let e = vterm env t in
      let k = value (term_indices env L ofs) in
      E (E.cup v (E.cup e k))

  (* Call *)
  | Tapp(phi,_,ts) -> E (call_lg env phi (List.map (term env) ts))

  (* Operators *)
  | TDataCons(_,ts) -> fcup (vterm env) ts
  | Tif(e,a,b) -> fcup (vterm env) [e;a;b]
  | Trange(a,b) -> fcup (vtermopt env) [a;b]
  | Tat(t,_) -> term env t
  | Toffset(_,t) | Tbase_addr(_,t) -> E (vterm env t)
  | Tnull | Tempty_set -> L
  | Tunion ts | Tinter ts -> fcup (vterm env) ts
  
  (* Binders *)
  | Tlambda(xs,b) -> E (E.bind xs (vterm env b))
  | Tcomprehension(t,xs,None) -> E (E.bind xs (vterm env t))
  | Tcomprehension(t,xs,Some p) -> E (E.bind xs (E.cup (vterm env t) (pred env p)))

  (* Jessie *)
  | TCoerce _ | TCoerceE _ -> Wp_parameters.fatal "Jessie Coercions"

  | _ -> assert false

and term_lval env (h,ofs) = match h with
  | TResult _ -> term_indices env (Val_var Result) ofs
  | TVar( {lv_origin=None} as x ) -> term_indices env (Val_var (Lvar x)) ofs
  | TVar( {lv_origin=Some x} ) -> load (term_offset env (Loc_var x) ofs)
  | TMem t -> load (term_offset env (load (term env t)) ofs)

and term_indices env m = function
  | TNoOffset -> m
  | TModel(_,ofs) | TField(_,ofs) -> term_indices env (E (value m)) ofs
  | TIndex(e,ofs) -> term_indices env (vcup (vterm env e) (value m)) ofs

and term_offset env (l:model) = function
  | TNoOffset -> l
  | TField(_,ofs) -> term_offset env (field l) ofs
  | TIndex(e,ofs) -> term_offset env (shift l (vterm env e)) ofs
  | TModel _ -> Wp_parameters.not_yet_implemented "Model fields"

and addr_lval env (h,ofs) = match h with
  | TResult _ -> Wp_parameters.fatal "Address of \\result"
  | TMem t -> term_offset env (term env t) ofs
  | TVar( {lv_origin=Some x} ) -> term_offset env (Loc_var x) ofs
  | TVar( {lv_origin=None} as x ) -> 
      Wp_parameters.fatal "Address of logic variable (%a)"
	Logic_var.pretty x

and pred (_:context) _ = E.bot

(*
and body (_:context) _ = E.bot
*)

(* -------------------------------------------------------------------------- *)
(* --- OCaml 4.0 Warnings (module under dev.)                             --- *)
(* -------------------------------------------------------------------------- *)

let _ = reference
let _ = call_kf 
let _ = call_lg 
let _ = expr 
let _ = term 
let _ = pred 
