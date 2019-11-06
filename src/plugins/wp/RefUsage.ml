(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* ---------------------------------------------------------------------- *)
(* --- Variable Analysis                                              --- *)
(* ---------------------------------------------------------------------- *)

open Ctypes
open Cil_types
open Cil_datatype

(* ---------------------------------------------------------------------- *)
(* --- Varinfo Accesses                                               --- *)
(* ---------------------------------------------------------------------- *)

(** By lattice order of usage *)
type access =
  | NoAccess (** Never used *)
  | ByRef   (** Only used as ["*x"],   equals to [load(shift(load(&x),0))] *)
  | ByArray (** Only used as ["x[_]"], equals to [load(shift(load(&x),_))] *)
  | ByValue (** Only used as ["x"],    equals to [load(&x)] *)
  | ByAddr  (** Widely used, potentially up to ["&x"] *)

module Access :
sig
  type t = access
  val is_bot : t -> bool
  val cup : t -> t -> t
  val pretty : varinfo -> Format.formatter -> t -> unit
end =
struct
  type t = access
  let is_bot = function NoAccess -> true | _ -> false
  let pretty x fmt = function
    | NoAccess -> Format.fprintf fmt "-%a" Varinfo.pretty x
    | ByRef -> Format.fprintf fmt "*%a" Varinfo.pretty x
    | ByArray -> Format.fprintf fmt "%a[]" Varinfo.pretty x
    | ByValue -> Format.fprintf fmt "%a" Varinfo.pretty x
    | ByAddr -> Format.fprintf fmt "&%a" Varinfo.pretty x

  let rank = function
    | NoAccess -> 0
    | ByRef -> 1
    | ByArray -> 2
    | ByValue -> 3
    | ByAddr -> 4

  let cup a b = if rank a < rank b then b else a
end

(* ---------------------------------------------------------------------- *)
(* --- Expressions & Memory Model                                     --- *)
(* ---------------------------------------------------------------------- *)

module E :
sig
  type t
  val bot : t
  val is_bot : t -> bool
  val cup : t -> t -> t
  val cup_differ : t -> t -> t * bool
  (* val leq : t -> t -> bool *) (* unused for now *)
  (* val lcup : t list -> t *) (* unused for now *)
  val fcup : ('a -> t) -> 'a list -> t
  val get : varinfo -> t -> access
  val access : varinfo -> access -> t -> t

  val partition_formals_vs_others : t -> t*t

  val pretty : Format.formatter -> t -> unit

  val iter: (varinfo -> access -> unit) -> t -> unit
end =
struct

  module Xmap = Qed.Mergemap.Make(Varinfo)

  type t = access Xmap.t

  let pretty fmt m =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      Xmap.iter
        (fun x e ->
           if e <> NoAccess then
             ( Format.pp_print_space fmt () ; Access.pretty x fmt e )
        ) m ;
      Format.fprintf fmt " }@]" ;
    end

  let bot = Xmap.empty
  let is_bot = Xmap.is_empty
  let cup = Xmap.union (fun _ -> Access.cup)
  let cup_differ e1 e2 =
    let r = cup e1 e2 in
    let is_modified = not (r==e1) in
    (* Format.printf "cup_differ %a %a = %a,%b@."
                     pretty e1 pretty e2 pretty r is_modified; *)
    r, is_modified
  (* unused for now *)
  (* let leq = Xmap.subset (fun _ -> Access.leq) *)

  (* unused for now *)
  (* let rec lcup = function [] -> bot |[x] -> x |x::xs -> cup x (lcup xs)*)
  let rec fcup f = function
      [] -> bot | [x] -> f x | x::xs -> cup (f x) (fcup f xs)

  let get vi e = try Xmap.find vi e with Not_found -> NoAccess
  let access vi u e =
    if Access.is_bot u then e
    else Xmap.insert (fun _ u old -> Access.cup old u) vi u e

  let partition_formals_vs_others e =
    Xmap.partition (fun vi _a -> vi.vformal) e

  let iter = Xmap.iter

end

type value = E.t
type model =
  | E of value (* E *)
  | Loc_var of varinfo  (* &x *)
  | Loc_shift of varinfo * value (* &x.[...] *)
  | Val_var of varinfo  (* x *)
  | Val_comp of varinfo * value (* x.f[_].g... *)
  | Val_shift of varinfo * value (* (x + E) *)

[@@@ warning "-32" ]
let pp_model fmt = function
  | E v -> E.pretty fmt v
  | Loc_var x -> Format.fprintf fmt "&%a" Varinfo.pretty x
  | Loc_shift(x,v) -> Format.fprintf fmt "&%a.(%a)" Varinfo.pretty x E.pretty v
  | Val_var x -> Varinfo.pretty fmt x
  | Val_comp(x,v) -> Format.fprintf fmt "%a.(%a)" Varinfo.pretty x E.pretty v
  | Val_shift(x,v) -> Format.fprintf fmt "%a+(%a)" Varinfo.pretty x E.pretty v
[@@@ warning "+32" ]

let nothing = E E.bot
let v_model v = if E.is_bot v then nothing else E v

let vcup (a:value) (b:value) : model = v_model (E.cup a b)

let share_vcup (m:model) ~old (b:value) : model =
  (* requires m = E(old) *)
  (* ensures \result == vcup old b *)
  let e = E.cup old b in if e == old then m else v_model e

(* let lcup xs = m_value (E.lcup xs) *) (* unused for now *)
(* let fcup f xs = m_value (E.fcup f xs) *) (* unused for now *)

let e_value = function
  | Loc_var x -> E.access x ByAddr E.bot
  | Loc_shift(x,e) -> E.access x ByAddr e
  | Val_var x -> E.access x ByValue E.bot
  | Val_comp(x,e) | Val_shift(x,e) -> E.access x ByValue e
  | E e -> e

let m_value = function
  | E _ as m -> m
  | m -> E (e_value m)

let m_vcup =
  let m_vcup m = vcup (e_value m) in
  function
  | E old as m -> (* better sharing than vcup (e_value m) b *)
      share_vcup m ~old
  | _ as m -> m_vcup m

let m_fcup f =
  let m_fcup f = E.fcup (fun x -> e_value (f x)) in
  function  (* better sharing than E.fcup (fun x -> e_value (f x)) *)
  | [] -> nothing
  | [x] -> m_value (f x)
  | x::xs -> m_vcup (f x) (m_fcup f xs)

let cval x = Val_var x
let cvar x = Loc_var x
let shift (m:model) (k:value) =
  let share ~old mk e = (* for a better sharing between maps *)
    if e == old then m else mk e
  in
  match m with
  | Loc_var x -> Loc_shift(x,k)
  | Loc_shift(x,e) -> share ~old:e (fun k -> Loc_shift(x,k)) (E.cup e k)
  | Val_var x -> Val_shift(x,k)
  | Val_comp(x,e) -> share ~old:e (fun k -> Val_comp(x,k)) (E.cup e k)
  | Val_shift(x,e) -> share ~old:e (fun k -> Val_shift(x,k)) (E.cup e k)
  | E old -> share_vcup m old k

let field = function
  | Val_var x -> Val_comp(x,E.bot)
  | (Val_comp _ | Val_shift _) as m -> m
  | m -> shift m E.bot

let load = function
  | Loc_var x -> Val_var x (* E.access x ByValue E.bot *)
  | Loc_shift(x,e) ->
      if Cil.isArithmeticOrPointerType x.vtype then
        E (E.access x ByAddr e)
      else
        E (E.access x ByValue e)
  | Val_var x -> E (E.access x ByRef E.bot)
  | Val_comp(x,e) -> E (E.access x ByRef e)
  | Val_shift(x,e) -> E (E.access x ByArray e)
  | E _ as m -> m

(* for \\valid, \\separated, \\block_length: no variable escape,
   excepts for shifts *)
let unescape = function (* better than e_value (load m) *)
  | Loc_var x -> E.access x ByValue E.bot
  | Loc_shift(x,e) -> E.access x ByValue e
  | Val_var x -> E.access x ByRef E.bot
  | Val_comp(x,e) -> E.access x ByRef e
  | Val_shift(x,e) -> E.access x ByArray e
  | E e -> e

(* ---------------------------------------------------------------------- *)
(* --- Casts                                                          --- *)
(* ---------------------------------------------------------------------- *)

type cast =
  | Identity
  | Convert
  | Cast

let cast cv m = match cv with
  | Identity -> m
  | Convert | Cast -> m_value m

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

let cast_ctyp tgt src =
  cast_obj (Ctypes.object_of tgt) (Ctypes.object_of src)
let cast_ltyp tgt src =
  match Logic_utils.unroll_type ~unroll_typedef:false src with
  | Ctype src -> cast_ctyp tgt src
  | _ -> Cast

(* ---------------------------------------------------------------------- *)
(* --- Environment                                                    --- *)
(* ---------------------------------------------------------------------- *)

module KFmap = Qed.Mergemap.Make(Kernel_function)
module KFset = Qed.Mergeset.Make(Kernel_function)
module LVmap = Qed.Mergemap.Make(Logic_var)
module LFset = Qed.Mergeset.Make(Logic_info)

type global_ctx = {

  (** Variable accesses from C code and code annotations *)
  mutable code : value ;

  (** Accesses of formal variables from function specs *)
  mutable spec_formals : value ;

  (** Accesses of global variables from function specs *)
  mutable spec_globals : value ;

  (** A map to a list (since a same kf can be called more than ones)
      to a list of models for each arg_exp of the call to the kf. *)
  mutable cphi : (model list list) KFmap.t ;

  (** Logical function/predicate used directly and indirectly by
      specs/annots of a C function *)
  mutable lphi : LFset.t ;
}
let mk_global_ctx () =
  { code = E.bot ; spec_formals = E.bot ; spec_globals = E.bot ;
    cphi = KFmap.empty ; lphi = LFset.empty }

(* Temporary local context *)
type local_ctx = {

  mutable tlet : model LVmap.t; (* for \\let var bound to a term *)
  mutable plet : value LVmap.t; (* for \\let var bound to a predicate *)
  mutable spec : value; (* for formals and globals of of spec,
                           before partitioning the result *)
}
let mk_local_ctx () = { tlet=LVmap.empty ; plet=LVmap.empty ; spec=E.bot }

type ctx = { local:local_ctx ; global:global_ctx }
let mk_ctx () = { global = mk_global_ctx () ; local = mk_local_ctx () }

(* ---------------------------------------------------------------------- *)
(* --- Tlet                                                           --- *)
(* ---------------------------------------------------------------------- *)

(* For \\let binding a predicate *)
let get_tlet (env:local_ctx) (lv:logic_var) =
  try LVmap.find lv env.tlet with Not_found -> assert (false) (* nothing *)

let add_tlet (env:local_ctx) (lv:logic_var) (m:model) =
  env.tlet <- LVmap.insert (fun _ _ _old -> assert false) lv m env.tlet

let rem_tlet (env:local_ctx) (lv:logic_var) =
  env.tlet <- LVmap.remove lv env.tlet

(* ---------------------------------------------------------------------- *)
(* --- Plet                                                           --- *)
(* ---------------------------------------------------------------------- *)

(* For \\let binding a predicate *)
let get_plet (env:local_ctx) (lv:logic_var) =
  try LVmap.find lv env.plet
  with Not_found -> e_value (get_tlet env lv)

let add_plet (env:local_ctx) (lv:logic_var) (e:value) =
  env.plet <- LVmap.insert (fun _ _ _old -> assert false) lv e env.plet

let rem_plet (env:local_ctx) (lv:logic_var) =
  env.plet <- LVmap.remove lv env.plet

(* ---------------------------------------------------------------------- *)
(* --- Compilation of C-Expressions                                   --- *)
(* ---------------------------------------------------------------------- *)

let rec vexpr (e:Cil_types.exp) : value = e_value (expr e)

and mexpr (e:Cil_types.exp) : model = (* better sharing than E (vexpr e) *)
  m_value (expr e)

and expr (e:Cil_types.exp) : model = match e.enode with

  (* Logics *)
  | Const _
  | SizeOf _ | SizeOfE _ | SizeOfStr _  | AlignOf _ | AlignOfE _ -> nothing

  (* Unary *)
  | UnOp((Neg|BNot|LNot),e,_) -> mexpr e

  (* Jessie *)
  | Info(e,_) -> expr e

  (* Binary *)
  | BinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
           |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
           |Lt|Gt|Le|Ge|Eq|Ne), a,b,_ ) -> m_vcup (expr a) (vexpr b)

  (* Shifts *)
  | BinOp((PlusPI|IndexPI|MinusPI),a,b,_) -> shift (expr a) (vexpr b)

  (* Casts *)
  | CastE(ty_tgt,e) -> cast (cast_ctyp ty_tgt (Cil.typeOf e)) (expr e)

  (* Address *)
  | AddrOf lval -> lvalue lval
  | StartOf lval -> startof (lvalue lval) (Cil.typeOfLval lval)

  (* Load *)
  | Lval lval -> load (lvalue lval)

and lvalue (h,ofs) = offset (host h) ofs
and host = function
  | Var x -> cvar x
  | Mem e -> expr e

and offset (m:model) = function
  | NoOffset -> m
  | Field(_,ofs) -> offset (field m) ofs
  | Index(e,ofs) -> offset (shift m (vexpr e)) ofs

and startof (m:model) typ =
  if Cil.isArrayType typ then shift m E.bot else m

(* ---------------------------------------------------------------------- *)
(* --- Compilation of ACSL-Terms                                      --- *)
(* ---------------------------------------------------------------------- *)

let rec vterm (env:ctx) (t:term) : value = e_value (term env t)

and mterm (env:ctx) (t:term) : model =
  m_value (term env t) (* better sharing than E (vterm env e) *)

and termopt (env:ctx) = function None -> nothing | Some t -> term env t

(* --- Expr --- *)
and term (env:ctx) (t:term) : model = match t.term_node with

  (* Logics *)
  | TConst _
  | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _
  | Ttypeof _ | Ttype _ -> nothing

  (* Unary *)
  | TUnOp((Neg|BNot|LNot),t) -> mterm env t

  (* Binary *)
  | TBinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
            |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
            |Lt|Gt|Le|Ge|Eq|Ne), a,b )  -> m_vcup (term env a) (vterm env b)

  (* Shifts *)
  | TBinOp((PlusPI|IndexPI|MinusPI),a,b) -> shift (term env a) (vterm env b)

  (* Casts *)
  | TCastE(ty_tgt,t) -> cast (cast_ltyp ty_tgt t.term_type) (term env t)
  | TLogic_coerce (_lt,t) -> term env t


  (* Term L-Values *)
  | TLval tlv -> term_lval env tlv
  | TAddrOf tlv | TStartOf tlv -> addr_lval env tlv
  | TUpdate(s,ofs,t) ->
      let v = term env s in
      let k = term_indices env E.bot ofs in
      let e = vterm env t in
      m_vcup (m_vcup v k) e

  (* Operators *)
  | Tat(t,_) -> term env t
  | Tunion ts | Tinter ts | TDataCons(_,ts) -> m_fcup (term env) ts
  | Tif(e,a,b) -> m_fcup (term env) [e;a;b]
  | Trange(a,b) -> m_fcup (termopt env) [a;b]
  | Toffset(_,t) | Tbase_addr(_,t) -> mterm env t
  | Tnull | Tempty_set -> nothing

  (* Binders *)
  | Tlambda(_xs,b) -> mterm env b
  | Tcomprehension(t,_xs,None) -> mterm env t
  | Tcomprehension(t,_xs,Some p) -> m_vcup (term env t) (pred env p)
  | Tlet({l_var_info; l_body = LBterm def},t) ->
      let m_def = term env def in
      add_tlet env.local l_var_info m_def;
      let m = term env t in
      rem_tlet env.local l_var_info;
      m
  | Tlet(_,_t) ->
      Wp_parameters.not_yet_implemented "unknown \\let construct"

  (* No escape *)
  | Tblock_length(_, t) ->
      E (unescape ((term env) t))

  (* Call *)
  | Tapp({l_var_info=({lv_origin=None; lv_kind=LVLocal} as lvar)},[],[]) ->
      (* var bound by a \\let *)
      get_tlet env.local lvar
  | Tapp(phi,_,ts) -> v_model (v_lphi env phi ts)

(* --- Lvalues --- *)
and term_lval env (h,ofs) = match h with
  | TResult _ | TVar{lv_name="\\exit_status"} -> nothing
  | TVar( {lv_origin=None ; lv_kind=LVLocal} as lvar) ->
      (* var bound by a \\let *)
      load (term_offset env (get_tlet env.local lvar) ofs)
  | TVar( {lv_origin=None} ) -> (* logic variable *)   nothing
  | TVar( {lv_origin=Some x} ) -> load (term_offset env (Loc_var x) ofs)
  | TMem t -> load (term_offset env (load (term env t)) ofs)

and term_indices env v = function
  | TNoOffset -> v
  | TModel(_,ofs) | TField(_,ofs) -> term_indices env v ofs
  | TIndex(e,ofs) -> term_indices env (E.cup v (vterm env e)) ofs

and term_offset env (l:model) = function
  | TNoOffset -> l
  | TField(_,ofs) -> term_offset env (field l) ofs
  | TIndex(e,ofs) -> term_offset env (shift l (vterm env e)) ofs
  | TModel _ -> Wp_parameters.not_yet_implemented "Model fields"

and addr_lval env (h,ofs) = match h with
  | TResult _ -> Wp_parameters.abort ~current:true "Address of \\result"
  | TVar{lv_name="\\exit_status"} ->
      Wp_parameters.abort ~current:true "Address of \\exit_status"
  | TMem t -> term_offset env (term env t) ofs
  | TVar( {lv_origin=Some x} ) -> term_offset env (Loc_var x) ofs
  | TVar( {lv_origin=None} as x ) ->
      Wp_parameters.abort ~current:true
        "Address of logic variable (%a)" Logic_var.pretty x

(* --- Predicates --- *)
and pred (env:ctx) p : value = match p.pred_content with
  | Pfalse | Ptrue ->  E.bot

  (* Unary *)
  | Pat(p,_)
  | Pnot p -> (pred env) p

  (* Binary *)
  | Pand(p1,p2) | Por(p1,p2) | Pxor(p1,p2) | Piff(p1,p2)
  | Pimplies(p1,p2) -> E.fcup (pred env) [p1; p2]
  | Pif (t,p1,p2) -> E.cup ((vterm env) t) (E.fcup (pred env) [p1; p2])
  | Prel(_,t1,t2) -> E.fcup (vterm env) [t1; t2]

  (* Binders *)
  | Pforall(_,p) | Pexists(_,p) -> (pred env) p
  | Plet({l_var_info; l_body = LBterm def},p) ->
      let m_def = term env def in
      add_tlet env.local l_var_info m_def;
      let e = pred env p in
      rem_tlet env.local l_var_info; e
  | Plet({l_var_info; l_body = LBpred def},p) ->
      let e_def = pred env def in
      add_plet env.local l_var_info e_def;
      let e = pred env p in
      rem_plet env.local l_var_info; e
  | Plet(_,_t) ->
      Wp_parameters.not_yet_implemented "unknown \\let construct"

  (* Call *)
  | Papp({l_var_info=({lv_origin=None; lv_kind=LVLocal} as lvar)},[],[]) ->
      (* var bound by a \\let *)
      get_plet env.local lvar
  | Papp(phi,_,ts) -> v_lphi env phi ts

  (* No escape *)
  | Pinitialized(_, t) | Pdangling(_,t)
  | Pallocable(_, t) | Pfreeable(_, t)
  | Pvalid(_,t) | Pvalid_read (_,t) | Pvalid_function t ->
      unescape ((term env) t)
  | Pseparated ts ->
      E.fcup (fun t -> unescape ((term env) t)) ts
  | Pfresh(_, _, t1, t2) ->
      E.fcup (fun t -> unescape ((term env) t)) [t1;t2]

(* --- Call to Logical functions/Predicates --- *)
and v_lphi (env:ctx) (lphi:logic_info) ts : value =
  let not_yet_implemented s =
    Wp_parameters.not_yet_implemented "unknown construct with %s" s
  in
  match lphi.l_var_info.lv_kind with
  | LVC -> not_yet_implemented "LVC"
  | LVFormal -> not_yet_implemented "LVFormal"
  | LVQuant -> not_yet_implemented "LVQuant"
  | LVLocal -> not_yet_implemented "LVLocal"
  | LVGlobal ->
      let v_body = (* get the accesses the globals *)
        if not (LFset.mem lphi env.global.lphi) then begin
          env.global.lphi <- LFset.add lphi env.global.lphi;
          v_body env lphi.l_body
        end
        else E.bot
      and v_param =
        E.fcup (vterm env) ts (* usage of the parameter of the application *)
      in E.cup v_param v_body
and v_body (env:ctx) = (* locals of the logical function are removed *)
  let vglob v = snd (E.partition_formals_vs_others v) in
  function
  | LBnone -> E.bot
  | LBreads(its) -> E.fcup (fun it -> vglob ((vterm env) it.it_content)) its
  | LBterm(t) -> vglob (vterm env t)
  | LBpred(p) -> vglob (pred env p)
  | LBinductive(inds) -> E.fcup (fun (_,_,_,p) -> vglob (pred env p)) inds

(* ---------------------------------------------------------------------- *)
(* --- Compilation of C Function                                      --- *)
(* ---------------------------------------------------------------------- *)

let cinit vi init =
  let update_code_env a v = E.cup a v in
  let einit (m:model) a exp =
    update_code_env a (E.cup (e_value m) (vexpr exp))
  in
  let rec aux (m: model) a = function
    | SingleInit (exp) ->  einit m a exp
    | CompoundInit(_,loi) ->
        List.fold_left (fun a (ofs,init) -> aux (offset m ofs) a init)
          a loi
  in aux (cval vi) E.bot init

let cfun_code env kf = (* Visits term/pred of code annotations and C exp *)
  let update_code_env v = env.global.code <- E.cup env.global.code v in
  let do_term t = update_code_env (vterm env t) in
  let do_pred p = update_code_env (pred env p) in
  let do_code =
    let do_arg arg =      (* normalizing model: taking out access map *)
      match expr arg with (* in order to put it code_env *)
      | (Loc_var _ | Val_var _) as m -> m
      | (Loc_shift(_,e) | Val_shift(_,e)) as m when E.is_bot e -> m
      | Loc_shift(x,e) -> update_code_env e; Loc_shift(x,E.bot)
      | Val_shift(x,e) -> update_code_env e; Val_shift(x,E.bot)
      | Val_comp(x,e) -> update_code_env e; Val_comp(x,E.bot)
      | m when m == nothing -> m
      | E e -> update_code_env e ; nothing
    in
    let do_args kf args =
      env.global.cphi <- KFmap.insert (fun _ u old -> u @ old)
          kf [(List.map do_arg args)] env.global.cphi in
    let do_exp exp = update_code_env (vexpr exp) in
    let do_lval lval = update_code_env (e_value (load (lvalue lval))) in
    let do_lval_opt = function
      | None -> ()
      | Some lval -> do_lval lval in

    function
    | Block _ | Break _ | Continue _ | Goto _
    | Loop _ | UnspecifiedSequence _ | TryFinally _
    | Return (None,_)
    | Instr(Asm _)
    | Instr(Skip _)
    | Instr(Code_annot _)
      -> ()

    | Throw _ | TryCatch _ | TryExcept _ ->
        Wp_parameters.warning "RefUsage: throw/try-catch not implemented"

    | Instr(Set(lval,exp,_)) -> do_lval lval ; do_exp exp
    | Instr(Call(lval_opt,fun_exp,args_list,_)) ->
        begin
          do_lval_opt lval_opt ;
          match Kernel_function.get_called fun_exp with
          | None -> List.iter do_exp (fun_exp::args_list)
          | Some called_kf -> do_args called_kf args_list
        end
    | Instr(Local_init (v,AssignInit i,_)) -> update_code_env (cinit v i)
    | Instr(Local_init (v,ConsInit (f,args,kind),_)) ->
        let kf = Globals.Functions.get f in
        (match kind with
         | Constructor -> do_args kf (Cil.mkAddrOfVi v :: args)
         | Plain_func ->
             update_code_env (e_value (cval v));
             do_args kf args)
    | Return(Some exp,_)
    | If (exp,_,_,_)
    | Switch (exp,_,_,_) -> do_exp exp
  in
  let visitor = object
    inherit Visitor.frama_c_inplace as super
    method! vstmt stmt = do_code stmt.skind; super#vstmt stmt
    (* vpredicate and vterm are called from vcode_annot *)
    method !vpredicate p = do_pred p ; Cil.SkipChildren
    method !vterm t = do_term t ; Cil.SkipChildren
    (* speed up: skip non interesting subtrees *)
    method! vloop_pragma _ =  Cil.SkipChildren (* no need *)
    method! vvdec _ = Cil.SkipChildren (* done via stmt *)
    method! vexpr _ = Cil.SkipChildren (* done via stmt *)
    method! vlval _ = Cil.SkipChildren (* done via stmt *)
    method! vattr _ = Cil.SkipChildren (* done via stmt *)
    method! vinst _ =  Cil.SkipChildren (* done via stmt *)
  end
  in
  try
    let definition = Kernel_function.get_definition kf in
    ignore (Cil.visitCilFunction (visitor:>Cil.cilVisitor) definition)
  with Not_found -> ()

let cfun_spec env kf =
  let update_spec_env v = env.local.spec <- E.cup env.local.spec v ;
    Cil.SkipChildren
  in
  let visitor = object
    inherit Cil.nopCilVisitor
    method !vpredicate p = update_spec_env (pred env p)
    method !vterm t = update_spec_env (vterm env t)
  end in
  let spec = Annotations.funspec kf in
  ignore (Cil.visitCilFunspec (visitor:>Cil.cilVisitor) spec) ;
  (* Partitioning the accesses of the spec for formals vs globals *)
  let formals,globals = E.partition_formals_vs_others env.local.spec in
  env.global.spec_formals <- formals ;
  env.global.spec_globals <- globals

let cfun kf =
  let env = mk_ctx () in
  (* Skipping frama-c builtins?
     if not (Cil.is_builtin (Kernel_function.get_vi kf)) then *)
  begin
    if Kernel_function.is_definition kf then cfun_code env kf ;
    cfun_spec env kf
  end ;
  env.global

let cvarinit vi initinfo env =
  match initinfo.init with
  | None -> env
  | Some init -> E.cup env (cinit vi init)

(* ---------------------------------------------------------------------- *)
(* --- Compilation                                                    --- *)
(* ---------------------------------------------------------------------- *)

let mk_context () = KFmap.empty

let param a m = match a with
  | NoAccess | ByAddr -> E.bot (* should never arise *)
  | ByValue -> e_value m
  | ByRef -> e_value (load m)
  | ByArray -> e_value (load (shift m E.bot))

let update_call_env (env:global_ctx) v =
  let r,differ = E.cup_differ env.code v in
  env.code <- r ; differ

let call_kf (env:global_ctx) (formals:access list) (models:model list) (reached:bool) =
  let unmodified = ref reached in
  let rec call xs ms = match xs, ms with
    | x::xs , m::ms ->
        let actual = param x m in
        if update_call_env env actual then unmodified := false;
        call xs ms
    | _ -> ()
  in call formals models;
  !unmodified

type callee = KFset.t
type callees = callee KFmap.t

type fp_t = { mutable todo: unit KFmap.t ; mutable redo: unit KFmap.t }

let compute_usage () =
  Wp_parameters.feedback ~ontty:`Transient "Collecting variable usage" ;
  (* initial state from variable initializers *)
  let u_init = Globals.Vars.fold cvarinit E.bot in
  (* initial state by kf *)
  let usage = Globals.Functions.fold (fun kf env ->
      KFmap.insert (fun _ _u _old -> assert false) kf (cfun kf) env)
      (mk_context ())
  in
  (* inverse table of function calls *)
  let callees =
    KFmap.fold (fun kf v (a:callees) ->
        KFmap.fold (fun called_kf _ (a:callees) ->
            KFmap.insert (fun _ v (old:callee) -> KFset.union old v)
              called_kf (KFset.add kf KFset.empty) a)
          v.cphi a)
      usage KFmap.empty
  in
  (* extract kf map to be fixed (the callers). *)
  let callers = KFmap.mapq
      (fun _kf v -> if KFmap.is_empty v.cphi then None else Some v) usage
  in
  (* extract it as a working list to be fixed *)
  let todo = KFmap.map (fun _ -> ()) callers in
  (* the todo map is used to intersect the callers map *)
  let kf_fp state_fp kf env _ =
    let kf_calls called_kf calls (reached:bool) =
      let called =
        try KFmap.find called_kf usage with Not_found -> assert false
      in
      (* update from accesses of globals of the called spec *)
      let reached =
        if update_call_env env called.spec_globals then false else reached
      in
      (* update from accesses of formals of the called spec for each calls*)
      let specs_formals = called.spec_formals in
      let params = Kernel_function.get_formals called_kf in
      let formals = List.map (fun vi -> E.get vi specs_formals) params in
      let kf_call reached call = call_kf env formals call reached in
      List.fold_left kf_call reached calls
    in
    state_fp.todo <- KFmap.remove kf state_fp.todo ;
    let cphi = env.cphi in
    let reached = KFmap.fold kf_calls cphi true in
    if not reached then begin
      let callers = try KFmap.find kf callees with Not_found -> KFset.empty
      in
      KFset.iter (fun kf_caller ->
          try ignore (KFmap.find kf_caller todo)
          (* kf_caller is still into the current remaining working list *)
          with Not_found ->
            (* kf_caller must be added to the next working list. *)
            state_fp.redo <- KFmap.add kf_caller () state_fp.redo) callers
    end;
    (* the intersect result is not used *)
    None
  in
  let rec fixpoint todo =
    if not (KFmap.is_empty todo) then
      let state_fp = {redo=KFmap.empty; todo} in
      ignore (KFmap.interf (kf_fp state_fp) callers todo);
      fixpoint state_fp.todo
  in fixpoint todo ;
  (* TODO[LC]: prendre en compte la compilation des fonctions logiques et predicats ; Cf. add_lphi *)
  let usage =
    KFmap.map
      (fun ctx -> E.cup (E.cup ctx.code ctx.spec_globals) ctx.spec_formals)
      usage
  in u_init, usage

(* ---------------------------------------------------------------------- *)
(* --- Projectified Analysis Result                                   --- *)
(* ---------------------------------------------------------------------- *)

module D = Datatype.Make
    (struct
      type t = E.t * E.t KFmap.t
      include Datatype.Serializable_undefined
      let reprs = [E.bot,KFmap.empty]
      let name = "RefUsage.usage"
    end)

module S = State_builder.Option_ref(D)
    (struct
      let name = "RefUsage.Analysis"
      let dependencies = [ Ast.self ]
    end)

(* compute_usage is called once per project *)
let usage () = S.memo compute_usage

(* ---------------------------------------------------------------------- *)
(* --- API                                                            --- *)
(* ---------------------------------------------------------------------- *)

let iter ?kf ?(init=false) f =
  let u_init, usage = usage () in
  let kf_access = match kf with
    | None -> E.bot
    | Some kf ->
        (try KFmap.find kf usage
         with Not_found -> E.bot)
  in
  let access = if init then E.cup kf_access u_init else kf_access in
  E.iter f access

let get ?kf ?(init=false) vi =
  let u_init, usage = usage () in
  let kf_access = match kf with
    | None -> NoAccess
    | Some kf ->
        (try E.get vi (KFmap.find kf usage)
         with Not_found -> NoAccess)
  in
  if init then Access.cup kf_access (E.get vi u_init) else kf_access

let compute () = ignore (usage ())

let print x m fmt = Access.pretty x fmt m

let dump () =
  Log.print_on_output
    begin fun fmt ->
      Format.fprintf fmt ".................................................@\n" ;
      Format.fprintf fmt "... Ref Usage@\n" ;
      Format.fprintf fmt ".................................................@\n" ;

      let a_init, a_usage = usage ()
      in Format.fprintf fmt "@[<hv 0>Init:@ %a@]@." E.pretty a_init ;
      KFmap.iter (fun kf m ->
          (* Do not dump results for frama-c builtins *)
          if not (Cil.is_builtin (Kernel_function.get_vi kf)) then
            Format.fprintf fmt "@[<hv 0>Function %a:@ %a@]@."
              Kernel_function.pretty kf E.pretty m ;
        ) a_usage;
      Format.fprintf fmt ".................................................@\n" ;
    end
