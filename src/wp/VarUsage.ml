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
open Cil
open Cil_types
open Cil_datatype

module WpMain = Wp_parameters
let dkey = Wp_parameters.register_category "vardebug"

(* -------------------------------------------------------------------------- *)
(* --- Dimension Utilities                                                --- *)
(* -------------------------------------------------------------------------- *)

exception NoSize

let pp_box fmt = function
  | [] -> ()
  | k::ks -> 
      Format.fprintf fmt "%d" k ; 
      List.iter (fun k -> Format.fprintf fmt ":%d" k) ks

let pp_dim fmt ks =
  List.iter (fun k -> Format.fprintf fmt "[%d]" k) ks

let size_of_char c =
  match Cil.charConstToInt c with
    | CInt64(k,_,_) -> k
    | _ -> raise NoSize

let size e =
  match (Cil.constFold true e).enode with
    | Const(CInt64(k,_,_)) -> k
    | Const(CChr c) -> size_of_char c
    | _ -> raise NoSize

let size_int e = Integer.to_int (size e)

let _merge_dim ds1 ds2 = (* Unused *)
  if ds1=[] then ds2 else
    if ds2=[] then ds1 else
      try
	let rec verify ds1 ds2 rs1 rs2 =
	  match rs1 , rs2 with
	    | [] , _ -> ds2
	    | _ , [] -> ds1
	    | d1::rs1 , d2::rs2 -> 
		if d1=d2 then verify ds1 ds2 rs1 rs2
		else raise Exit
	in verify ds1 ds2 (List.rev ds1) (List.rev ds2)
      with Exit -> []

(* boxes are dimensions in reverse order *)
let merge_box box1 box2 =
  try
    let rec verify ds1 ds2 rs1 rs2 =
      match rs1 , rs2 with
	| [] , _ -> ds2
	| _ , [] -> ds1
	| d1::rs1 , d2::rs2 -> 
	    if d1=d2 then verify ds1 ds2 rs1 rs2
	    else raise Exit
    in verify box1 box2 box1 box2
  with Exit -> []

let rec leq_box box1 box2 = (* merge box1 box2 = box2 *)
  match box1 , box2 with
    | [] , _ -> true
    | _::_ , [] -> false
    | d1::rs1 , d2::rs2 -> d1 = d2 && leq_box rs1 rs2

let rec addbox_of_type box typ =
  match Cil.unrollType typ with
    | TArray(te,Some d,_,_) -> addbox_of_type (size_int d::box) te
    | _ -> box

let box_of_type typ = addbox_of_type [] typ

let rec dim_of_type typ =
  match Cil.unrollType typ with
    | TArray(te,Some d,_,_) -> size_int d :: dim_of_type te
    | TArray(_,None,_,_) -> raise NoSize
    | _ -> []

let rec cells_in_type typ =
  match Cil.unrollType typ with
    | TArray(te,Some d,_,_) -> Integer.mul (size d) (cells_in_type te)
    | TArray(_,None,_,_) -> raise NoSize
    | _ -> Integer.one

let rec type_of_cells typ =
  match Cil.unrollType typ with
    | TArray(te,_,_,_) -> type_of_cells te
    | te -> te

let alloc_for_type typ = 
  match Cil.unrollType typ with
    | TArray(te,Some d,_,_) -> size_int d :: dim_of_type te
    | TArray(te,None,_,_) -> 0 :: dim_of_type te
    | _ -> []

let rec degree_of_type typ =
  match Cil.unrollType typ with
    | TArray(te,_,_,_) -> succ (degree_of_type te)
    | _ -> 0

let shape typ =
  let rec destruct box typ =
    match Cil.unrollType typ with
      | TArray(te,Some d,_,_) -> destruct (size_int d :: box) te
      | te -> te , box
  in match Cil.unrollType typ with
    | TArray(te,_,_,_) -> Some (destruct [] te)
    | TPtr(te,_) -> Some(destruct [] te)
    | _ -> None

let rec compatible s t =
  match Ctypes.object_of s , Ctypes.object_of t with
    | C_int i1 , C_int i2 -> i1 = i2
    | C_float f1 , C_float f2 -> f1 = f2
    | C_pointer t1 , C_pointer t2 -> compatible t1 t2
    | C_comp s1 , C_comp s2 -> Compinfo.equal s1 s2
    | _ -> false (* arrays are already destructured by shape *)

let reshape ty_src ty_tgt =
  match shape ty_src , shape ty_tgt with
    | None , _ | _ , None -> None
    | Some (s,ds) , Some (t,dt) ->
	if compatible s t then Some (merge_box ds dt) else None
	  
(* -------------------------------------------------------------------------- *)
(* --- Root Variables                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Root =
struct
  type t = Cvar of varinfo | Lvar of logic_var
  let compare x y = match x,y with
    | Cvar x , Cvar y -> Varinfo.compare x y
    | Lvar x , Lvar y -> Logic_var.compare x y
    | Cvar _ , Lvar _ -> (-1)
    | Lvar _ , Cvar _ -> 1
  let pretty fmt = function
    | Cvar x -> Format.fprintf fmt "'%s'(C%d)" x.vname x.vid (* user's pretty print *)
    | Lvar x -> Format.fprintf fmt "'%s'(L%d)" x.lv_name x.lv_id (* user's pretty print *)
end

(* -------------------------------------------------------------------------- *)
(* --- Abstract Access Model                                              --- *)
(* -------------------------------------------------------------------------- *)

module Model =
struct

  type value =
    | BASE     (* base-address *)
    | VALUE    (* load of base-address *)
    | REFERENCE (* load of load of base-address *)
    | INDEX of int list (* shift on base-address *)
    | ARRAY of int list (* load of shift on base-address *)
    | REF_INDEX of int list (* shift on value *)
    | REF_ARRAY of int list (* load of shift on value *)
    | TERM of value (* an arbitrary term build upon the given value *)

  (* all dimensions are in reverse order (boxes) *)

  let field = function
    | (TERM _) as t -> t
    | u -> TERM u

  let shift tbox = function
    | BASE -> INDEX tbox
    | VALUE -> REF_INDEX tbox
    | INDEX box -> INDEX(merge_box box tbox)
    | REF_INDEX box -> REF_INDEX(merge_box box tbox)
    | (REFERENCE|ARRAY _|REF_ARRAY _) as loc -> TERM loc
    | (TERM _) as t -> t

  let load = function
    | BASE -> VALUE
    | VALUE -> REFERENCE
    | INDEX box -> ARRAY box
    | REF_INDEX box -> REF_ARRAY box
    | (REFERENCE|ARRAY _|REF_ARRAY _) as loc -> TERM loc
    | (TERM _) as t -> t

end

module Context =
struct
  
  type delta =
    | Dload
    | Dfield (* BY: unused constructor: always filtered, never created *)
    | Dshift of int list (* box *)
  let _ = Dfield (* VP: silence Ocaml's 4 warning, see BY remark above *)

  let apply loc = function
    | Dload -> Model.load loc
    | Dfield -> Model.field loc
    | Dshift box -> Model.shift box loc

  let eval : delta list -> Model.value = List.fold_left apply Model.BASE

  type target =
    | Memory
    | Validity
    | Fcall of kernel_function * varinfo
    | Logic of logic_info * logic_var

  type t = target * delta list (* first operation to apply at head *)

  let epsilon : t = (Memory,[])
  let assigned : t = (Memory,[Dload])
  let validity : t = (Validity,[])
  let load (target,delta) = target , Dload :: delta
  let shift ty (target,delta) = target , Dshift (box_of_type ty) :: delta
  let cast ty_src ty_tgt (target,delta) =
    match reshape ty_src ty_tgt with
      | Some ds -> (target,Dshift ds :: delta)
      | None -> (target,[])
  let function_param kf x = (Fcall(kf,x),[])
  let logic_param phi x = (Logic(phi,x),[])

  let in_spec = ref false

  let on_spec e = 
    in_spec := true ; ChangeDoChildrenPost(e,fun e -> in_spec := false ; e)

  let pp_target fmt = function
    | Memory -> Format.fprintf fmt "memory"
    | Validity -> Format.fprintf fmt "valid"
    | Fcall(kf,x) -> Format.fprintf fmt "call %a:%a" 
	Kernel_function.pretty kf Varinfo.pretty x
    | Logic(phi,x) -> Format.fprintf fmt "logic %a:%a"
	Logic_var.pretty phi.l_var_info Logic_var.pretty x

  let pp_access fmt ds = 
    List.iter
      (function
	 | Dload -> Format.fprintf fmt "L"
	 | Dfield -> Format.fprintf fmt "F"
	 | Dshift box -> Format.fprintf fmt "{%a}" pp_box box
      ) ds

  (*let pretty fmt (target,access) =
    Format.fprintf fmt "[%a] %a" pp_target target pp_access access
   *)
end

module Usage =
struct

  type domain =
    | Bot (* value if never used *)
    | Top (* value must be allocated in heap *)
    | Value  (* always accessed by [load(base)] *)
    | RefValue (* always accessed by [load(load(base))] *) 
    | Array of int list (* always accessed by [load(shift(base))] *)
    | RefArray of int list (* always accessed by [load(shift(load(base)))] *)
	(* for arrays : empty list means flatten array *)
	(* for arrays : non-empty list may start with [0] for unknown size *)
	(* dimensions are given in reverse order (boxes) *)


(*
  Usage Lattice Diagram

           Top 
            |
          Array           Justification comes from:
            |      (b)          
          Value            (a) For any operation (f), f(u) <= u
            |      (a)
         RefArray          (b) 0-shift of any dimension is identity
            |      (b)
         RefValue
            |
           Bot

*)

  let _print fmt = function
    | Bot -> Format.pp_print_string fmt "-"
    | Top -> Format.pp_print_string fmt "&"
    | Value -> Format.pp_print_string fmt "(=)"
    | RefValue -> Format.pp_print_string fmt "(\042)" (* '\042'='*' *)
    | Array box -> Format.fprintf fmt "@{%a}" pp_box box
    | RefArray box -> Format.fprintf fmt "(\042){%a}" pp_box box (* '\042'='*' *)

  let pretty ~name fmt = function
    | Bot -> Format.fprintf fmt "%s not used" name
    | Top -> Format.fprintf fmt "&%s" name
    | Value -> Format.fprintf fmt "%s" name
    | RefValue -> Format.fprintf fmt "*%s" name
    | Array box -> Format.fprintf fmt "%s[]%a" name pp_dim (List.rev box)
    | RefArray box -> Format.fprintf fmt "(%s[])%a" name pp_dim (List.rev box)

  let rec of_value = function
    | Model.BASE -> Top
    | Model.VALUE -> Value
    | Model.REFERENCE -> RefValue
    | Model.INDEX _ -> Top
    | Model.ARRAY box -> Array box
    | Model.REF_INDEX _ -> Value
    | Model.REF_ARRAY box -> RefArray box
    | Model.TERM t -> of_value t

  let of_context context = of_value (Context.eval context)

  let merge u v =
    match u , v with
      | Bot , w | w , Bot -> w
      | Top , _ | _ , Top -> Top
	  (* same levels *)
      | Value , Value -> Value
      | RefValue , RefValue -> RefValue
      | Array a , Array b -> Array(merge_box a b)
      | RefArray a , RefArray b -> RefArray(merge_box a b)
	  (* Array level *)
      | (Array _ as w) , _ | _ , (Array _ as w) -> w
	  (* Value level *)
      | Value , _ | _ , Value -> Value
	  (* RefArray level *)
      | (RefArray _ as w) , _ | _ , (RefArray _ as w) -> w
	  (* RefValue level *)

  let leq u v = (* merge u v = v *)
    match u,v with
      | Bot,_ -> true
      | _,Bot -> false
      | _,Top -> true
      | Top,_ -> false
	  (* RefValue level and upper *)
      | RefValue,_ -> true
      | _,RefValue -> false
	  (* RefArray level and upper *)
      | RefArray a,RefArray b -> leq_box a b
      | RefArray _,_ -> true
      | _,RefArray _ -> false
	  (* Value level and upper *)
      | Value,_ -> true
      | _,Value -> false
	  (* Array level and upper *)
      | Array a,Array b -> leq_box a b

  let call formal ds =
    match formal with
      | Bot -> Bot
      | Top -> of_context ds
      | Value -> of_context ds
      | RefValue -> of_context (ds @ [Context.Dload])
      | RefArray box -> of_context (ds @ [Context.Dshift box;Context.Dload])
      | Array _ -> WpMain.fatal "Usage of formal as an array"

end

(* -------------------------------------------------------------------------- *)
(* --- Occurences Collecting                                              --- *)
(* -------------------------------------------------------------------------- *)

module Occur =
struct

  type t = {
    mutable valid : bool ; (* address used for validity and separation *)
    mutable value : Usage.domain ; (* in scope of definition *)
    mutable param : Usage.domain ; (* in specification for formal parameter *)
    mutable calls : (bool * Root.t * Context.delta list) list ; (* calls *)
  }

  let empty () = {
    valid = false ;
    value = Usage.Bot ;
    param = Usage.Bot ;
    calls = [] ;
  }

  let merge_with usage context = Usage.merge usage (Usage.of_context context)

  open Context
  open Root
    
  let update occur inspec target context =
    match target with
      | Memory -> 
	  if inspec then occur.param <- merge_with occur.param context ;
	  occur.value <- merge_with occur.value context
      | Validity -> occur.valid <- true
      | Fcall(_,x) -> occur.calls <- (inspec,Cvar x,context)::occur.calls
      | Logic(_,x) -> occur.calls <- (inspec,Lvar x,context)::occur.calls

  let propagate modified occur (phi : Root.t -> Usage.domain)  =
    List.iter
      (fun (inspec,x,w) -> 
	 let u = Usage.call (phi x) w in
	 if not (Usage.leq u occur.value) then
	   begin
	     occur.value <- Usage.merge occur.value u ;
	     modified := true ;
	   end ;
	 if inspec && not (Usage.leq u occur.param) then
	   begin
	     occur.param <- Usage.merge occur.param u ;
	     modified := true ;
	   end
      ) occur.calls

end

(* -------------------------------------------------------------------------- *)
(* --- Fixpoint Computation                                               --- *)
(* -------------------------------------------------------------------------- *)

module Omap = FCMap.Make(Root)
module Domain = Datatype.Make
  (struct
     type t = Occur.t Omap.t
     include Datatype.Serializable_undefined
     let reprs = [Omap.empty]
     let name = "Wp.VarUsage.Domain"
   end)

module U = State_builder.Ref(Domain)
  (struct
     let name = "Wp.VarUsage.Analysis"
     let dependencies = 
       [ Ast.self; 
	 (* [JS 2012/02/08] put all annotations state, but unsure that this
	    state actually depends on all of them. *)
	 Annotations.code_annot_state;
	 Annotations.funspec_state;
	 Annotations.global_state ]
     let default () = Omap.empty
   end)

let occur r = 
  let omap = U.get () in
  try Omap.find r omap
  with Not_found -> 
    let occ = Occur.empty () in
    U.set (Omap.add r occ omap) ; occ

let get_formal r =
  try 
    let occ = Omap.find r (U.get()) in
    match r with
      | Root.Cvar _ -> occ.Occur.param
      | Root.Lvar _ -> occ.Occur.value
  with Not_found -> Usage.Bot

let occurrence (target,access) root =
  let in_spec = match root with
    | Root.Cvar x -> x.vformal && !Context.in_spec
    | Root.Lvar _ -> false
  in 
  WpMain.debug ~dkey ~current:true "%s %a : %a <- %a"
    (if in_spec then "Spec" else "Code")
    Root.pretty root Context.pp_target target Context.pp_access access ;
  Occur.update (occur root) in_spec target access

let fixpoint () =
  let modified = ref true in
  let omap = U.get () in
  while !modified do
    modified := false ;
    Omap.iter
      (fun _ occ -> Occur.propagate modified occ get_formal)
      omap ;
  done

(* -------------------------------------------------------------------------- *)
(* --- C-Expressions Visitor                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec expr (context:Context.t) (e:Cil_types.exp) =
  match e.enode with
    | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> ()
    | UnOp((Neg|BNot|LNot),e,_) -> expr Context.epsilon e
    | BinOp((PlusPI|IndexPI|MinusPI),a,b,_) -> 
	let ty = Cil.typeOf_pointed (Cil.typeOf a) in
	expr (Context.shift ty context) a ; 
	expr Context.epsilon b
    | BinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
	     |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
	     |Lt|Gt|Le|Ge|Eq|Ne), a,b,_ ) ->
	expr Context.epsilon a ; 
	expr Context.epsilon b
    | CastE(ty_tgt,e) -> 
	let ty_src = Cil.typeOf e in
	expr (Context.cast ty_src ty_tgt context) e
    | AddrOf lval -> lvalue context lval
    | StartOf lval -> lvalue context lval
    | Lval lval -> lvalue (Context.load context) lval
    | Info(e,_) -> expr context e

and lvalue context (host,offset) = 
  let ty_host = match host with 
    | Var x -> x.vtype 
    | Mem e -> Cil.typeOf_pointed (Cil.typeOf e) in
  let context = lval_offset context ty_host offset in
  lval_host context host

and lval_option context = function None -> () | Some lv -> lvalue context lv

and lval_host context = function
  | Var x -> occurrence context (Root.Cvar x)
  | Mem e -> expr context e

and lval_offset context ty = function
  | NoOffset -> context
  | Field(f,offset) -> 
      lval_offset Context.epsilon f.ftype offset
  | Index(e,offset) -> 
      expr Context.epsilon e ;
      let telt = Cil.typeOf_array_elem ty in
      Context.shift telt (lval_offset context telt offset)

let rec funcall_params kf xs es =
  match xs , es with
    | _ , [] | [] , _ -> ()
    | x::xs , e::es -> 
	expr (Context.function_param kf x) e ; 
	funcall_params kf xs es

let funcall (ef:Cil_types.exp) (es:Cil_types.exp list) =
  match Kernel_function.get_called ef with
    | None -> 
	expr Context.epsilon ef ; 
	List.iter (expr Context.epsilon) es
    | Some kf -> 
	funcall_params kf (Kernel_function.get_formals kf) es

(* -------------------------------------------------------------------------- *)
(* --- Term Visitor                                                       --- *)
(* -------------------------------------------------------------------------- *)
	
let rec term (context:Context.t) (t:term) =
  match t.term_node with
    | TConst _ 
    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ 
    | TAlignOf _ | TAlignOfE _ -> ()
    | TUnOp((Neg|BNot|LNot),t) -> 
	term Context.epsilon t
    | TBinOp((PlusPI|IndexPI|MinusPI),a,b) -> 
	let ty = Logic_typing.ctype_of_pointed a.term_type in
	term (Context.shift ty context) a ; 
	term Context.epsilon b
    | TBinOp( (MinusPP|PlusA|MinusA|Mult|Div|Mod
	     |Shiftlt|Shiftrt|BAnd|BXor|BOr|LAnd|LOr
	     |Lt|Gt|Le|Ge|Eq|Ne), a,b ) ->
	term Context.epsilon a ; 
	term Context.epsilon b
    | TCastE(ty_tgt,t) ->
	begin
	  match Logic_utils.unroll_type t.term_type with
	    | Ctype ty_src -> term (Context.cast ty_src ty_tgt context) t
	    | _ -> term Context.epsilon t
	end
    | TAddrOf tlv -> term_lval context tlv
    | TStartOf tlv -> term_lval context tlv
    | TLval tlv -> term_lval (Context.load context) tlv
    | Tapp(phi,_,ts) -> logic_call phi phi.l_profile ts
    | Tlambda(_,t) -> term Context.epsilon t
    | TDataCons(_,ts) -> List.iter (term Context.epsilon) ts
    | Tif(e,a,b) -> term Context.epsilon e ; term context a ; term context b
    | Tat(t,_) -> term context t
    | Tbase_addr (_,t) -> term Context.epsilon t
    | Toffset (_,t) -> term Context.epsilon t
    | Tblock_length (_,t) -> term Context.validity t
    | Tnull -> ()
    | TCoerce _ | TCoerceE _ -> WpMain.fatal "Jessie Coercions"
    | TUpdate(s,offset,t) ->
	term Context.epsilon s ;
	term Context.epsilon t ;
	term_indices offset
    | Ttypeof _ | Ttype _ -> ()
    | Tempty_set -> ()
    | Tunion ts | Tinter ts -> List.iter (term context) ts
    | Tcomprehension( t , _ , None ) ->
	term context t
    | Tcomprehension( t , _ , Some p ) -> 
	term context t ; named_predicate p
    | Trange( a , b ) ->
	term_option Context.epsilon a ;
	term_option Context.epsilon b
    | Tlet( phi , a ) ->
	logic_body phi.l_body ;
	term context a
     | TLogic_coerce (_,t) -> term context t

and term_option context = function None -> () | Some e -> term context e

and term_lval context (host,offset) =
  match host with
      
    (* Logic value + field/array-index offset *)
    | TVar ({lv_origin=None} as x) ->
	occurrence context (Root.Lvar x) ; term_indices offset
	  
    (* Cases where host has a C-type *)
    | TResult typ
    | TVar {lv_origin=Some {vtype=typ}}
      -> term_host (term_coffset context typ offset) host
	
    (* Case where host is a pointer *)
    | TMem e ->
	let te = Logic_typing.ctype_of_pointed e.term_type in
	term_host (term_coffset context te offset) host

and term_coffset context ty = function
  | TNoOffset -> context
  | TField(f,offset) -> 
      term_coffset Context.epsilon f.ftype offset
  | TModel _ ->
     Wp_parameters.not_yet_implemented "Model field"
  | TIndex(e,offset) -> 
      term Context.epsilon e ;
      let telt = Cil.typeOf_array_elem ty in
      Context.shift telt (term_coffset context telt offset)

and term_indices = function
  | TNoOffset -> ()
  | TField(_,offset) | TModel(_,offset) -> term_indices offset
  | TIndex(k,offset) -> term Context.epsilon k ; term_indices offset

and term_host context = function
  | TResult _ -> ()
  | TVar {lv_origin=Some x} -> occurrence context (Root.Cvar x)
  | TVar x -> occurrence context (Root.Lvar x)
  | TMem t -> term context t

and logic_call phi xs ts =
  match xs , ts with
    | [] , _ | _ , [] -> ()
    | x::xs , t::ts -> 
	term (Context.logic_param phi x) t ; 
	logic_call phi xs ts

and identified_term context t = term context t.it_content

(* -------------------------------------------------------------------------- *)
(* --- Pred Visitor                                                       --- *)
(* -------------------------------------------------------------------------- *)

and named_predicate p = predicate p.content
and predicate = function
  | Psubtype _ | Pfalse | Ptrue -> ()
  | Papp(phi,_,ts) -> 
      logic_call phi phi.l_profile ts
  | Pseparated ts -> 
      List.iter (term Context.validity) ts
  | Pvalid (_,t) 
  | Pvalid_read (_,t)
  | Pallocable (_,t)
  | Pfreeable (_,t) ->
      term Context.validity t
  | Pinitialized (_,t) ->
      term Context.validity t
  | Pfresh (_,_,t,n) ->
      term Context.validity t ;
      term Context.validity n
  | Prel(_,a,b) -> 
      term Context.epsilon a ; 
      term Context.epsilon b
  | Pand(p,q) | Por(p,q) | Pxor(p,q) | Pimplies(p,q) | Piff(p,q) -> 
      named_predicate p ; 
      named_predicate q
  | Pnot p -> 
      named_predicate p
  | Pif(t,p,q) -> 
      term Context.epsilon t ; 
      named_predicate p ; 
      named_predicate q
  | Plet(phi,p) ->
      logic_body phi.l_body ;
      named_predicate p
  | Pforall(_,p) | Pexists(_,p) -> 
      named_predicate p
  | Pat(p,_) -> 
      named_predicate p

(* -------------------------------------------------------------------------- *)
(* --- Logic Visitor                                                      --- *)
(* -------------------------------------------------------------------------- *)

and logic_body = function
  | LBnone -> ()
  | LBreads its -> List.iter (identified_term Context.epsilon) its
  | LBterm t -> term Context.epsilon t
  | LBpred p -> predicate p.content
  | LBinductive cases -> List.iter (fun (_,_,_,p) -> named_predicate p) cases

(* -------------------------------------------------------------------------- *)
(* --- CIL Visitor                                                        --- *)
(* -------------------------------------------------------------------------- *)

class visitor =
object

  inherit Visitor.frama_c_inplace

  initializer Context.in_spec := false

  method! vexpr e = expr Context.epsilon e ; SkipChildren
  method! vinst = function 
    | Call( result , e , es , _ ) -> 
	lval_option Context.assigned result ; 
	funcall e es ; 
	SkipChildren
    | Set( lv , e , _ ) ->
	lvalue Context.assigned lv ; 
	expr Context.epsilon e ; 
	SkipChildren
    | Code_annot _ -> DoChildren
    | Skip _ -> DoChildren
    | Asm _ -> DoChildren

  method! vterm t = term Context.epsilon t ; SkipChildren
  method! vpredicate p = predicate p ; SkipChildren
  method! vspec = Context.on_spec

end

let compute () = 
  WpMain.feedback "Collecting variable usage" ;
  Visitor.visitFramacFile (new visitor) (Ast.get()) ; 
  fixpoint ()

(* -------------------------------------------------------------------------- *)
(* --- External API                                                       --- *)
(* -------------------------------------------------------------------------- *)

let (compute,_) = 
  State_builder.apply_once "VarUsage.compute" 
    (* [JS 2012/02/08] looks to be redundant with the definition of module 
       [U]. *) 
    [ Ast.self; 
      (* [JS 2012/02/08] put all annotations state, but unsure that this
	 state actually depends on all of them. *)
      Annotations.code_annot_state;
      Annotations.funspec_state;
      Annotations.global_state
] 
    compute

let of_cvar x = (occur (Root.Cvar x)).Occur.value
let of_formal x = (occur (Root.Cvar x)).Occur.param
let of_lvar x = (occur (Root.Lvar x)).Occur.value
let validated_cvar x = (occur (Root.Cvar x)).Occur.valid
let validated_lvar x = (occur (Root.Lvar x)).Occur.valid

let dump_lvar fmt x = 
  Usage.pretty ~name:x.lv_name fmt (of_lvar x) ;
  if validated_lvar x then Format.pp_print_string fmt " (validated)"

let dump () =
  Log.print_on_output 
    begin fun fmt ->
      Format.fprintf fmt "-------------------------------------------------@\n" ;
      Format.fprintf fmt "--- Roots Usage@\n" ;
      Format.fprintf fmt "-------------------------------------------------@\n" ;
      Globals.Vars.iter
	(fun x _ ->
	   Format.fprintf fmt "Global %a@." 
	     (Usage.pretty ~name:x.vname) (of_cvar x)
	) ;
      Globals.Functions.iter
	(fun kf ->
	   let xs = Kernel_function.get_formals kf in
	   let ys = Kernel_function.get_locals kf in
	   Format.fprintf fmt "Function '%s':@\n" (Kernel_function.get_name kf) ;
	   List.iter
	     (fun x ->
		let occ = occur (Root.Cvar x) in
		let value = occ.Occur.value in
		let param = occ.Occur.param in
		if Usage.leq value param then
		  Format.fprintf fmt " - formal %a"
		    (Usage.pretty ~name:x.vname) value
		else
		  Format.fprintf fmt " - formal %a (called: %a)"
		    (Usage.pretty ~name:x.vname) value
		    (Usage.pretty ~name:x.vname) param ;
		if occ.Occur.valid then
		  Format.fprintf fmt " (validated)@\n"
		else
		  Format.fprintf fmt "@\n"
	     ) xs ;
	   List.iter
	     (fun y ->
		Format.fprintf fmt " - local %a@\n"
		  (Usage.pretty ~name:y.vname) (of_cvar y)
	     ) ys ;
	   Format.pp_print_flush fmt ()
	) ;
      Annotations.iter_global
	(fun _ logic ->
	   match logic with
	   | Dfun_or_pred(linfo,_) ->
	     let name = linfo.l_var_info.lv_name in
	     let kind = 
	       if linfo.l_type = None then "Predicate" else "Logic" 
	     in
	     if linfo.l_profile = [] then
	       Format.fprintf fmt "%s '%s': %a@\n" kind name 
		 dump_lvar linfo.l_var_info
	     else begin
	       Format.fprintf fmt "%s '%s':@\n" kind name ;
	       let xs = linfo.l_profile in
	       List.iter
		 (fun x ->
		   Format.fprintf fmt " - parameter %a@\n" dump_lvar x) 
		 xs
	     end
	   | _ -> ());
      Format.fprintf fmt "-------------------------------------------------@." ;
    end

type usage =
  | NotUsed
  | ByValue
  | ByAddress
  | ByReference
  | ByArray of int list
  | ByRefArray of int list

let usage = function
  | Usage.Bot -> NotUsed
  | Usage.Top -> ByAddress
  | Usage.Value -> ByValue
  | Usage.Array box -> ByArray (List.rev box)
  | Usage.RefValue -> ByReference
  | Usage.RefArray box -> ByRefArray (List.rev box)

let of_cvar x = compute () ; usage (of_cvar x)
let of_formal x = compute () ; usage (of_formal x)
let of_lvar x = compute () ; usage (of_lvar x)
let validated_cvar x = compute () ; validated_cvar x
let validated_lvar x = compute () ; validated_lvar x

let pretty ~name fmt = function
  | NotUsed -> Format.fprintf fmt "%s not used" name
  | ByAddress -> Format.fprintf fmt "&%s" name
  | ByValue -> Format.fprintf fmt "%s" name
  | ByReference -> Format.fprintf fmt "*%s" name
  | ByArray dim -> Format.fprintf fmt "%s[]%a" name pp_dim dim
  | ByRefArray dim -> Format.fprintf fmt "(%s[])%a" name pp_dim dim
