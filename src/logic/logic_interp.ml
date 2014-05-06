(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil
open Cil_types
open Cil_datatype

exception Error of Cil_types.location * string
exception Unbound of string

let find_var kf x =
  let vi =
    try Globals.Vars.find_from_astinfo x (VLocal kf)
    with Not_found ->
      try
        Globals.Vars.find_from_astinfo x (VFormal kf)
      with Not_found ->
        try
          Globals.Vars.find_from_astinfo x VGlobal
        with Not_found ->
          raise (Unbound ("Unbound variable " ^ x))
  in
  cvar_to_lvar vi

(** Create a logic typer, the interpretation being done for the given
    kernel_function and stmt (the stmt is used check that loop invariants
    are allowed). *)
(* It is theoretically possible to use a first-class module instead, but the
   required signatures are not exported in Logic_typing. *)
module DefaultLT (X:
sig
  val kf: Kernel_function.t
  val stmt: stmt
end) =
    Logic_typing.Make
      (struct
         let anonCompFieldName = Cabs2cil.anonCompFieldName
         let conditionalConversion = Cabs2cil.logicConditionalConversion

         let is_loop () = Kernel_function.stmt_in_loop X.kf X.stmt

         let find_macro _ = raise Not_found

         let find_var x =
           try find_var X.kf x
           with Unbound s -> raise (Error (Stmt.loc X.stmt, s))

         let find_enum_tag _ = assert false (*TODO*)

         let find_comp_type ~kind:_ _s = assert false (*TODO*)

         let find_comp_field info s =
           let field = Cil.getCompField info s in
           Field(field,NoOffset)

         let find_type _s = assert false (*TODO*)

         let find_label s = Kernel_function.find_label X.kf s
         include Logic_env

         let add_logic_function =
           add_logic_function_gen Logic_utils.is_same_logic_profile

         let integral_cast ty t =
           raise
             (Failure
                (Pretty_utils.sfprintf
                   "term %a has type %a, but %a is expected."
                   Printer.pp_term t Printer.pp_logic_type Linteger Printer.pp_typ ty))

       end)

let wrap f stmt =
  try f ()
  with Unbound s -> raise (Error (Stmt.loc stmt, s))

let code_annot kf stmt s =
  let module LT = DefaultLT(struct
    let kf = kf
    let stmt = stmt
  end) in
  let loc = snd (Cabshelper.currentLoc ()) in
  let pa = match snd (Logic_lexer.annot (loc, s)) with
    | Logic_ptree.Acode_annot (_,a) -> a
    | _ ->
        raise (Error (Stmt.loc stmt,
                      "Syntax error (expecting a code annotation)"))
  in
  let parse () =
    LT.code_annot
      (Stmt.loc stmt)
      (Logic_utils.get_behavior_names (Annotations.funspec kf))
      (Ctype (Kernel_function.get_return_type kf)) pa
  in
  wrap parse stmt

let expr kf stmt s =
  let module LT = DefaultLT(struct
    let kf = kf
    let stmt = stmt
  end) in
  let (_,pa_expr) = Logic_lexer.lexpr (Lexing.dummy_pos, s) in
  let parse () =
    LT.term
      (Logic_typing.append_here_label (Logic_typing.Lenv.empty()))
      pa_expr
  in
  wrap parse stmt

let lval kf stmt s =
  match (expr kf stmt s).term_node with
  | TLval lv -> lv
  | _ -> raise (Error (Stmt.loc stmt, "Syntax error (expecting an lvalue)"))

(* may raise [Invalid_argument "not an lvalue"] *)
let error_lval () = invalid_arg "not an lvalue"

let rec logic_type_to_typ = function
  | Ctype typ -> typ
  | Linteger -> TInt(ILongLong,[]) (*TODO: to have an unlimited integer type
                                    in the logic interpretation*)
  | Lreal -> TFloat(FLongDouble,[]) (* TODO: handle reals, not floats... *)
  | Ltype({lt_name = name},[]) when name = Utf8_logic.boolean  ->
      TInt(ILongLong,[])
  | Ltype({lt_name = "set"},[t]) -> logic_type_to_typ t
  | Ltype _ | Lvar _ | Larrow _ -> error_lval ()


(* Expect conversion to be possible on all sub-terms, otherwise raise an error. *)

let logic_var_to_var { lv_origin = lv } =
  match lv with
    | None -> error_lval ()
    | Some lv -> lv

let create_const_list loc kind low high =
  let rec aux acc i =
    if Integer.lt i low then acc
    else
      aux (new_exp ~loc (Const (CInt64 (i,kind,None)))::acc) (Integer.pred i)
  in aux [] high

let range low high =
  let loc = fst low.eloc, snd high.eloc in
  match (Cil.constFold true low).enode, (Cil.constFold true high).enode with
      Const(CInt64(low,kind,_)), Const(CInt64(high,_,_)) ->
        create_const_list loc kind low high
    | _ -> error_lval()

let singleton f loc =
  match f loc with
      [ l ] -> l
    | _ -> error_lval()

let rec loc_lval_to_lval ~result (lh, lo) =
  Extlib.product
    (fun x y -> (x,y))
    (loc_lhost_to_lhost ~result lh)
    (loc_offset_to_offset ~result lo)

and loc_lhost_to_lhost ~result = function
  | TVar lvar -> [Var (logic_var_to_var lvar)]
  | TMem lterm -> List.map (fun x -> Mem x) (loc_to_exp ~result lterm)
  | TResult _ ->
      ( match result with
        None -> error_lval()
      | Some v -> [Var v])

and loc_offset_to_offset ~result = function
  | TNoOffset -> [NoOffset]
  | TModel _ -> error_lval ()
  | TField (fi, lo) ->
      List.map (fun x -> Field (fi,x)) (loc_offset_to_offset ~result lo)
  | TIndex (lexp, lo) ->
      Extlib.product
	(fun x y -> Index(x,y))
        (loc_to_exp ~result lexp) 
	(loc_offset_to_offset ~result lo)

and loc_to_exp ~result {term_node = lnode ; term_type = ltype; term_loc = loc} =
  match lnode with
  | TLval lv ->
      List.map (fun x -> new_exp ~loc (Lval x)) (loc_lval_to_lval ~result lv)
  | TAddrOf lv ->
      List.map (fun x -> new_exp ~loc (AddrOf x)) (loc_lval_to_lval ~result lv)
  | TStartOf lv ->
      List.map (fun x -> new_exp ~loc (StartOf x)) (loc_lval_to_lval ~result lv)
  | TSizeOfE lexp ->
      List.map (fun x -> new_exp ~loc (SizeOfE x)) (loc_to_exp ~result lexp)
  | TAlignOfE lexp ->
      List.map (fun x -> new_exp ~loc (AlignOfE x)) (loc_to_exp ~result lexp)
  | TUnOp (unop, lexp) ->
      List.map
        (fun x -> new_exp ~loc (UnOp (unop, x, logic_type_to_typ ltype)))
        (loc_to_exp ~result lexp)
  | TBinOp (binop, lexp1, lexp2) ->
      Extlib.product
        (fun x y -> new_exp ~loc (BinOp (binop, x,y, logic_type_to_typ ltype)))
        (loc_to_exp ~result lexp1) 
	(loc_to_exp ~result lexp2)
  | TSizeOfStr string -> [new_exp ~loc (SizeOfStr string)]
  | TConst constant -> 
    (* TODO: Very likely to fail on large integer and incorrect on reals not
       representable as floats *)
    [new_exp ~loc (Const (Logic_utils.lconstant_to_constant constant))]
  | TCastE (typ, lexp) ->
      List.map
        (fun x -> new_exp ~loc (CastE (typ, x))) (loc_to_exp ~result lexp)
  | TAlignOf typ -> [new_exp ~loc (AlignOf typ)]
  | TSizeOf typ -> [new_exp ~loc (SizeOf typ)]
  | Trange (Some low, Some high) ->
      let low = singleton (loc_to_exp ~result) low in
      let high = singleton (loc_to_exp ~result) high in
      range low high
  | Tunion l -> List.concat (List.map (loc_to_exp ~result) l)
  | Tempty_set -> []
  | Tinter _ | Tcomprehension _ -> error_lval()
  | Tat ({term_node = TAddrOf (TVar _, TNoOffset)} as taddroflval, _) ->
      loc_to_exp ~result taddroflval
  | TLogic_coerce(Linteger, t) when Logic_typing.is_integral_type t.term_type ->
    loc_to_exp ~result t
  | TLogic_coerce(Lreal, t) when Logic_typing.is_integral_type t.term_type ->
    List.map
      (fun x -> new_exp ~loc (CastE (logic_type_to_typ Lreal, x)))
      (loc_to_exp ~result t)
  | TLogic_coerce(Lreal, t) when Logic_typing.is_arithmetic_type t.term_type ->
    loc_to_exp ~result t
  | TLogic_coerce (set, t)
      when
        Logic_const.is_set_type set &&
          Logic_utils.is_same_type
          (Logic_typing.type_of_set_elem set) t.term_type ->
    loc_to_exp ~result t

 (* additional constructs *)
  | Tapp _ | Tlambda _ | Trange _   | Tlet _
  | TDataCons _
  | Tif _
  | Tat _
  | Tbase_addr _
  | Toffset _
  | Tblock_length _
  | Tnull
  | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
  | TLogic_coerce _
    -> error_lval ()

let rec loc_to_lval ~result t =
  match t.term_node with
  | TLval lv -> loc_lval_to_lval ~result lv
  | TAddrOf lv -> loc_lval_to_lval ~result lv
  | TStartOf lv -> loc_lval_to_lval ~result lv
  | Tunion l1 -> List.concat (List.map (loc_to_lval ~result) l1)
  | Tempty_set -> []
  (* coercions to arithmetic types cannot be lval. We only have to consider
     a coercion to set here.
   *) 
  | TLogic_coerce(set, t) when
      Logic_typing.is_set_type set &&
        Logic_utils.is_same_type
        (Logic_typing.type_of_set_elem set) t.term_type ->
    loc_to_lval ~result t
  | Tinter _ -> error_lval() (* TODO *)
  | Tcomprehension _ -> error_lval()
  | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TSizeOfStr _
  | TConst _ | TCastE _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
  | Tat _ | Toffset _ | Tbase_addr _ | Tblock_length _ | Tnull | Trange _
  | TCoerce _ | TCoerceE _ | TDataCons _ | TUpdate _ | Tlambda _
  | Ttypeof _ | Ttype _ | Tlet _ | TLogic_coerce _ ->
      error_lval ()

let loc_to_offset ~result loc =
  let rec aux h =
    function
        TLval(h',o) | TStartOf (h',o) ->
          (match h with None -> Some h', loc_offset_to_offset ~result o
             | Some h when Logic_utils.is_same_lhost h h' ->
                 Some h, loc_offset_to_offset ~result o
             | Some _ -> error_lval()
          )
      | Tat ({ term_node = TLval(TResult _,_)} as lv,LogicLabel (_,"Post")) ->
          aux h lv.term_node
      | Tunion locs -> List.fold_left
            (fun (b,l) x ->
               let (b,l') = aux b x.term_node in b, l @ l') (h,[]) locs
      | Tempty_set -> h,[]
      | Trange _ | TAddrOf _
      | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TSizeOfStr _
      | TConst _ | TCastE _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
      | Tat _ | Toffset _ | Tbase_addr _ | Tblock_length _ | Tnull
      | TCoerce _ | TCoerceE _ | TDataCons _ | TUpdate _ | Tlambda _
      | Ttypeof _ | Ttype _ | Tcomprehension _ | Tinter _ | Tlet _
      | TLogic_coerce _ 
          -> error_lval ()
  in snd (aux None loc.term_node)

let term_lval_to_lval ~result = singleton (loc_lval_to_lval ~result)

let term_to_lval ~result = singleton (loc_to_lval ~result)

let term_to_exp ~result = singleton (loc_to_exp ~result)

let term_offset_to_offset ~result = singleton (loc_offset_to_offset ~result)



(** Utilities to identify [Locations.Zone.t] involved into
    [code_annotation]. *)
module To_zone : sig

  type ctx = Db.Properties.Interp.To_zone.t_ctx =
      {state_opt:bool option;
       ki_opt:(stmt * bool) option;
       kf:Kernel_function.t}

  val mk_ctx_func_contrat: kernel_function -> state_opt:bool option -> ctx
  (** [mk_ctx_func_contrat] to define an interpretation context related to
      [kernel_function] contracts. 
      The control point of the interpretation is defined as follow:
      - pre-state if  [state_opt=Some true]
      - post-state if [state_opt=Some false]
      - pre-state with possible reference to the post-state if
      [state_opt=None]. *)

  val mk_ctx_stmt_contrat: 
    kernel_function -> stmt -> state_opt:bool option -> ctx
  (** [mk_ctx_stmt_contrat] to define an interpretation context related to
      [stmt] contracts. 
      The control point of the interpretation is defined as follow:
      - pre-state if  [state_opt=Some true]
      - post-state if [state_opt=Some false]
      - pre-state with possible reference to the post-state if
      [state_opt=None]. *)

  val mk_ctx_stmt_annot: kernel_function -> stmt -> ctx
  (** [mk_ctx_stmt_annot] to define an interpretation context related to an
      annotation attached before the [stmt]. *)

  type zone_info = Db.Properties.Interp.To_zone.t_zone_info
  type decl = Db.Properties.Interp.To_zone.t_decl
  type pragmas = Db.Properties.Interp.To_zone.t_pragmas
  val not_yet_implemented : string ref
  exception NYI of string

  val from_term: term -> ctx -> (zone_info * decl)
    (** Entry point to get zones
        needed to evaluate the [term] relative to the [ctx] of
	interpretation. *) 

  val from_terms: term list -> ctx -> (zone_info * decl)
    (** Entry point to get zones
        needed to evaluate the list of [terms] relative to the [ctx] of
	interpretation. *) 

  val from_pred: predicate named -> ctx -> (zone_info * decl)
    (** Entry point to get zones
        needed to evaluate the [predicate] relative to the [ctx] of
	interpretation. *) 

  val from_preds: predicate named list -> ctx -> (zone_info * decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of
	interpretation. *) 

  val from_stmt_annot: 
    code_annotation -> (stmt * kernel_function) -> 
    (zone_info * decl) * pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [stmt]. *)

  val from_stmt_annots:
    (code_annotation -> bool) option ->
    (stmt * kernel_function) -> (zone_info * decl) * pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [stmt]. *) 

  val from_func_annots:
    ((stmt -> unit) -> kernel_function -> unit) ->
    (code_annotation  -> bool) option ->
    kernel_function -> (zone_info * decl) * pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [kf]. *) 

  val code_annot_filter:
    code_annotation ->
    threat:bool -> user_assert:bool -> slicing_pragma:bool ->
    loop_inv:bool -> loop_var:bool -> others:bool -> bool
    (** To quickly build a annotation filter *)
  end
  = 
struct

  exception NYI of string
  (* Reimport here the type definitions of Db.Properties.Interp. See
     documentation there. *)
  type ctx = Db.Properties.Interp.To_zone.t_ctx =
          {state_opt:bool option;
           ki_opt:(stmt * bool) option;
           kf:Kernel_function.t}
  
  type pragmas = Db.Properties.Interp.To_zone.t_pragmas =
      {ctrl: Stmt.Set.t ; stmt: Stmt.Set.t} 

  type t = Db.Properties.Interp.To_zone.t 
      = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
  type zone_info = Db.Properties.Interp.To_zone.t_zone_info

  type decl = Db.Properties.Interp.To_zone.t_decl =
      {var: Varinfo.Set.t ; lbl: Logic_label.Set.t}

  let mk_ctx_func_contrat kf ~state_opt =
    { state_opt = state_opt;
      ki_opt = None;
      kf = kf }

  let mk_ctx_stmt_contrat kf ki ~state_opt =
    { state_opt=state_opt;
      ki_opt= Some(ki, false);
      kf = kf }

  let mk_ctx_stmt_annot kf ki =
    { state_opt = Some true;
      ki_opt = Some(ki, true);
      kf = kf }


  let empty_pragmas =
    { ctrl = Stmt.Set.empty;
      stmt = Stmt.Set.empty }

  let other_zones = Stmt.Hashtbl.create 7
  let locals = ref Varinfo.Set.empty
  let labels = ref Logic_label.Set.empty
  let pragmas = ref empty_pragmas

  let zone_result = ref (Some other_zones)
  let not_yet_implemented = ref ""

  let add_top_zone not_yet_implemented_msg = match !zone_result with
    | None -> (* top zone *) ()
    | Some other_zones ->
      Stmt.Hashtbl.clear other_zones;
      not_yet_implemented := not_yet_implemented_msg;
      zone_result := None

  let add_result ~before ki zone = match !zone_result with
    | None -> (* top zone *) ()
    | Some other_zones ->
      let zone_true, zone_false =
        try Stmt.Hashtbl.find other_zones ki
        with Not_found -> Locations.Zone.bottom, Locations.Zone.bottom
      in
      Stmt.Hashtbl.replace other_zones
        ki
        (if before then Locations.Zone.join zone_true zone, zone_false
         else zone_true, Locations.Zone.join zone_false zone)

  let get_result_aux () =
    let result =
      let zones = match !zone_result with
        | None ->
          (* clear references for the next time when giving the result.
             Note that other_zones has been cleared in [add_top_zone]. *)
          zone_result := Some other_zones;
          None
        | Some other_zones ->
          let z =
            Stmt.Hashtbl.fold
              (fun ki (zone_true, zone_false) other_zones ->
                let add before zone others =
                  if Locations.Zone.equal Locations.Zone.bottom zone then
                    others
                  else
                    { before = before; ki = ki; zone = zone} :: others
                in
                add true zone_true (add false zone_false other_zones))
              other_zones
              []
          in
          (* clear table for the next time when giving the result *)
          Stmt.Hashtbl.clear other_zones;
          Some z
      in zones, {var = !locals; lbl = !labels}
    in
      let res_pragmas = !pragmas in
      (* clear references for the next time when giving the result *)
      (* TODO: this is hideous and error-prone as some functions are
         recursive. See VP comment about a more functional setting *)
      locals := Varinfo.Set.empty ;
      labels := Logic_label.Set.empty ;
      pragmas := empty_pragmas;
      result, res_pragmas

  let get_result () = fst (get_result_aux ())

  let get_annot_result () =
    get_result_aux ()

  (** Logic_var utility: *)
  let extract_locals logicvars =
    Logic_var.Set.fold
      (fun lv cvars -> match lv.lv_origin with
      | None -> cvars
      | Some cvar ->
        if cvar.Cil_types.vglob then cvars
        else Varinfo.Set.add cvar cvars)
      logicvars
      Varinfo.Set.empty

  (** Term utility:
      Extract C local variables occuring into a [term]. *)
  let extract_locals_from_term term =
    extract_locals (extract_free_logicvars_from_term term)

  (** Predicate utility:
      Extract C local variables occuring into a [term]. *)
  let extract_locals_from_pred pred =
    extract_locals (extract_free_logicvars_from_predicate pred)

  type abs_label = | AbsLabel_here
                   | AbsLabel_pre
                   | AbsLabel_post
                   | AbsLabel_stmt of stmt

  let is_same_label absl l =
    match absl, l with
      | AbsLabel_stmt s1, StmtLabel s2 -> Cil_datatype.Stmt.equal s1 !s2
      | AbsLabel_here, LogicLabel (_, "Here") -> true
      | AbsLabel_pre, LogicLabel (_, "Pre") -> true
      | AbsLabel_post, LogicLabel (_, "Post") -> true
      | _ -> false


  class populate_zone before_opt ki_opt kf =
    (* interpretation from the
       - pre-state if  [before_opt=Some true]
       - post-state if [before_opt=Some false]
       - pre-state with possible reference to the post-state if
       [before_opt=None] of a property relative to
       - the contract of function [kf] when [ki_opt=None]
       otherwise [ki_opt=Some(ki, code_annot)],
       - the contract of the statement [ki] when [code_annot=false]
       - the annotation of the statement [ki] when [code_annot=true] *)
    object(self)
      inherit Visitor.frama_c_inplace
      val mutable current_label = AbsLabel_here

      method private get_ctrl_point () =
        let get_fct_entry_point () =
          (* TODO: to replace by true, None *)
          true, 
	  (try Some (Kernel_function.find_first_stmt kf)
           with Kernel_function.No_Statement -> 
	     (* raised when [kf] has no code. *)
	     None)  
        in
        let get_ctrl_point dft =
          let before = Extlib.opt_conv dft before_opt in
          match ki_opt with
          | None -> (* function contract *)

              if before then get_fct_entry_point ()
              else before, None
                (* statement contract *)
          | Some (ki,_) ->  (* statement contract and code annotation *)
              before, Some ki
        in
        let result = match current_label with
          | AbsLabel_stmt stmt -> true, Some stmt
          | AbsLabel_pre -> get_fct_entry_point ()
          | AbsLabel_here -> get_ctrl_point true
          | AbsLabel_post -> get_ctrl_point false
        in (* TODO: the method should be able to return result directly *)
        match result with
        | current_before, Some current_stmt -> current_before, current_stmt
        | _ -> raise (NYI
                        "[logic_interp] clause related to a function contract")

      method private change_label: 'a.abs_label -> 'a -> 'a visitAction =
        fun label x ->
          let old_label = current_label in
          current_label <- label;
          ChangeDoChildrenPost
            (x,fun x -> current_label <- old_label; x)

      method private change_label_to_here: 'a.'a -> 'a visitAction =
        fun x ->
          self#change_label AbsLabel_here x

      method private change_label_to_old: 'a.'a -> 'a visitAction =
        fun x ->
          match ki_opt,before_opt with
            (* function contract *)
          | None,Some true -> 
	    failwith "The use of the label Old is forbiden inside clauses \
        related the pre-state of function contracts." 
          | None,None
          | None,Some false -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label AbsLabel_pre x 
          (* statement contract *)
          | Some (_ki,false),Some true  -> 
	    failwith "The use of the label Old is forbiden inside clauses \
related the pre-state of statement contracts."
          | Some (ki,false),None
          | Some (ki,false),Some false  -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label (AbsLabel_stmt ki) x 
          (* code annotation *)
          | Some (_ki,true),None
          | Some (_ki,true),Some _ -> 
	    (* refers to the pre-state of the function contract. *)
	    self#change_label AbsLabel_pre x 

      method private change_label_to_post: 'a.'a -> 'a visitAction =
        fun x -> 
	  (* allowed when [before_opt=None] for function/statement contracts *)
          match ki_opt,before_opt with
            (* function contract *)
          | None,Some _ -> 
	    failwith "Function contract where the use of the label Post is \
 forbiden."
          | None,None -> 
	    (* refers to the post-state of the contract. *)
	    self#change_label AbsLabel_post x 
          (* statement contract *)
          | Some (_ki,false),Some _  -> 
	    failwith "Statement contract where the use of the label Post is \
forbiden."
          | Some (_ki,false),None -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label AbsLabel_post x 
          (* code annotation *)
          | Some (_ki,true), _ -> 
	    failwith "The use of the label Post is forbiden inside code \
annotations."

      method private change_label_to_pre: 'a.'a -> 'a visitAction =
        fun x ->
          match ki_opt with
            (* function contract *)
          | None -> 
	    failwith "The use of the label Pre is forbiden inside function \
contracts."
          (* statement contract *)
          (* code annotation *)
          | Some _ -> 
	    (* refers to the pre-state of the function contract. *)
	    self#change_label AbsLabel_pre x 

      method private change_label_to_stmt: 'a.stmt -> 'a -> 'a visitAction =
        fun stmt x ->
          match ki_opt with
            (* function contract *)
          | None -> 
	    failwith "the use of C labels is forbiden inside clauses related \
function contracts."
          (* statement contract *)
          (* code annotation *)
          | Some _ -> 
	    (* refers to the state at the C label of the statement [stmt]. *)
	    self#change_label (AbsLabel_stmt stmt) x


      method! vpredicate p =
      let fail () =
        raise (NYI (Pretty_utils.sfprintf
                      "[logic_interp] %a" Printer.pp_predicate p))
      in
      match p with
      | Pat (_, LogicLabel (_,"Old")) -> self#change_label_to_old p
      | Pat (_, LogicLabel (_,"Here")) -> self#change_label_to_here p
      | Pat (_, LogicLabel (_,"Pre")) -> self#change_label_to_pre p
      | Pat (_, LogicLabel (_,"Post")) -> self#change_label_to_post p
      | Pat (_, StmtLabel st) -> self#change_label_to_stmt !st p
      | Pat (_, LogicLabel (_,s)) ->
          failwith ("unknown logic label" ^ s)

      | Pfalse | Ptrue | Prel _ | Pand _ | Por _ | Pxor _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Papp (_, [], _) (* No label, thus cannot access memory *)
      | Pseparated _ (* need only to preserve the values of each pointer *)
        -> DoChildren

      | Pinitialized (lbl, t) ->
          if is_same_label current_label lbl then (
            let typ = Logic_typing.type_of_pointed t.term_type in
            let tlv = Cil.mkTermMem t TNoOffset in
            let tlv' = Logic_const.term (TLval tlv) typ in
            self#do_term_lval tlv';
            DoChildren
          )
          else fail ()

      | Pvalid_read (_lbl, _) | Pvalid (_lbl, _) ->
          (* Does not take dynamic allocation into account, but then
             Value does not either. [lbl] can be ignored because they are
             taken into account by the functions [from_...] below *)
          DoChildren

      | Papp _ | Pallocable _ | Pfreeable _ | Pfresh _ | Psubtype _
        -> fail ()

      method private do_term_lval t =
        let current_before, current_stmt = self#get_ctrl_point () in
        let state = Db.Value.get_stmt_state current_stmt in
        try
          let deps = !Db.From.find_deps_term_no_transitivity_state state t in
          (* TODO: what we should we do with other program points? *)
          let z = Logic_label.Map.find (LogicLabel (None,"Here")) deps in
          let z =
            Locations.Zone.filter_base
              (function Base.CLogic_Var _ -> false | _ -> true)
              z
          in
          add_result current_before current_stmt z
        with Invalid_argument "not an lvalue" ->
          raise (NYI "[logic_interp] dependencies of a term lval")

      method! vterm t =
        match t.term_node with
          | TAddrOf _ | TLval (TMem _,_)
          | TLval(TVar {lv_origin = Some _},_) | TStartOf _  ->
              self#do_term_lval t;
              SkipChildren
          | Tat (_, LogicLabel (_,"Old")) -> self#change_label_to_old t
          | Tat (_, LogicLabel (_,"Here")) -> self#change_label_to_here t
          | Tat (_, LogicLabel (_,"Pre")) -> self#change_label_to_pre t
          | Tat (_, LogicLabel (_,"Post")) -> self#change_label_to_post t
          | Tat (_, StmtLabel st) -> self#change_label_to_stmt !st t
          | Tat (_, LogicLabel (_,s)) ->
            failwith ("unknown logic label" ^ s)
          | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
            (* These are static constructors, there are no dependencies here *)
            SkipChildren
          | _ -> DoChildren
    end

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [term]
        relative to the [ctx] of interpretation. *)
    let from_term term ctx =
      (* [VP 2011-01-28] TODO: factorize from_terms and from_term, and use
	 a more functional setting. *)
      (try
         ignore(Visitor.visitFramacTerm
		  (new populate_zone ctx.state_opt ctx.ki_opt ctx.kf) term)
       with NYI msg -> 
	 add_top_zone msg) ;
      locals := Varinfo.Set.union (extract_locals_from_term term) !locals;
      labels := Logic_label.Set.union (extract_labels_from_term term) !labels;
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [terms]
        relative to the [ctx] of interpretation. *)
    let from_terms terms ctx =
      let f x =
        (try
           ignore(Visitor.visitFramacTerm
		    (new populate_zone ctx.state_opt ctx.ki_opt ctx.kf) x)
         with NYI msg -> 
	   add_top_zone msg) ;
        locals := Varinfo.Set.union (extract_locals_from_term x) !locals;
	labels := Logic_label.Set.union (extract_labels_from_term x) !labels
      in
        List.iter f terms;
        get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [pred]
        relative to the [ctx] of interpretation. *)
    let from_pred pred ctx =
        (try
           ignore(Visitor.visitFramacPredicateNamed
                    (new populate_zone ctx.state_opt ctx.ki_opt ctx.kf) pred)
         with NYI msg -> 
	   add_top_zone msg) ;
      locals := Varinfo.Set.union (extract_locals_from_pred pred) !locals;
      labels := Logic_label.Set.union (extract_labels_from_pred pred) !labels;
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [preds]
        relative to the [ctx] of interpretation. *)
    let from_preds preds ctx =
      let f pred =
        (try
           ignore(Visitor.visitFramacPredicateNamed
                    (new populate_zone ctx.state_opt ctx.ki_opt ctx.kf) pred)
         with NYI msg -> 
	   add_top_zone msg) ;
        locals := Varinfo.Set.union (extract_locals_from_pred pred) !locals;
	labels := Logic_label.Set.union (extract_labels_from_pred pred) !labels
      in
        List.iter f preds;
        get_result ()

   (** Used by annotations entry points. *)
    let get_zone_from_annot a (ki,kf) loop_body_opt =
      assert (!pragmas = empty_pragmas);
      (* check before modification. Anne.*)
      let get_zone_from_term k x =
        (try
           ignore
             (Visitor.visitFramacTerm
                (new populate_zone (Some true) (Some (k, true)) kf) x)
         with NYI msg -> 
	   add_top_zone msg) ;
        (* to select the declaration of the variables *)
        locals := Varinfo.Set.union (extract_locals_from_term x) !locals;
        (* to select the labels of the annotation *)
	labels := Logic_label.Set.union (extract_labels_from_term x) !labels
      and get_zone_from_pred k x =
        (try
           ignore
             (Visitor.visitFramacPredicateNamed
                (new populate_zone (Some true) (Some (k,true)) kf) x)
         with NYI msg -> 
	   add_top_zone msg) ;
        (* to select the declaration of the variables *)
        locals := Varinfo.Set.union (extract_locals_from_pred x) !locals;
        (* to select the labels of the annotation *)
	labels := Logic_label.Set.union (extract_labels_from_pred x) !labels
      in
      match a.annot_content with
      | APragma (Slice_pragma (SPexpr term) | Impact_pragma (IPexpr term)) ->
        (* to preserve the interpretation of the pragma *)
        get_zone_from_term ki term;
        (* to select the reachability of the pragma *)
        pragmas :=
          { !pragmas with ctrl = Stmt.Set.add ki !pragmas.ctrl }
      | APragma (Slice_pragma SPctrl) ->
        (* to select the reachability of the pragma *)
        pragmas :=
          { !pragmas with ctrl = Stmt.Set.add ki !pragmas.ctrl }
      | APragma (Slice_pragma SPstmt | Impact_pragma IPstmt) ->
        (* to preserve the effect of the statement *)
        pragmas :=
          { !pragmas with stmt = Stmt.Set.add ki !pragmas.stmt}
      | AAssert (_behav,pred) ->
        (* to preserve the interpretation of the assertion *)
        get_zone_from_pred ki pred;
      | AInvariant (_behav,true,pred) -> (* loop invariant *)
        (* WARNING this is obsolete *)
        (* [JS 2010/09/02] TODO: so what is the right way to do? *)
        (* to preserve the interpretation of the loop invariant *)
        get_zone_from_pred (Extlib.the loop_body_opt) pred;
      | AInvariant (_behav,false,pred) -> (* code invariant *)
        (* to preserve the interpretation of the code invariant *)
        get_zone_from_pred ki pred;
      | AVariant (term,_) ->
        (* to preserve the interpretation of the variant *)
        get_zone_from_term (Extlib.the loop_body_opt) term;
      | APragma (Loop_pragma (Unroll_specs terms))
      | APragma (Loop_pragma (Widen_hints terms))
      | APragma (Loop_pragma (Widen_variables terms)) ->
        (* to select the declaration of the variables *)
        List.iter
          (fun term ->
             locals := Varinfo.Set.union (extract_locals_from_term term) !locals;
 	     labels := Logic_label.Set.union (extract_labels_from_term term) !labels)
         terms
      | AAllocation (_,FreeAllocAny) -> ();
      | AAllocation (_,FreeAlloc(f,a)) -> 
        let get_zone x =
          get_zone_from_term (Extlib.the loop_body_opt) x.it_content
        in
          List.iter get_zone f ;
          List.iter get_zone a 
      | AAssigns (_, WritesAny) -> ()
      | AAssigns (_, Writes l) -> (* loop assigns *)
        let get_zone x =
          get_zone_from_term (Extlib.the loop_body_opt) x.it_content
        in
        List.iter
          (fun (zone,deps) ->
            get_zone zone;
            match deps with
                FromAny -> ()
              | From l -> List.iter get_zone l)
          l
      | AStmtSpec _ -> (* TODO *)
        raise (NYI "[logic_interp] statement contract")

    (** Used by annotations entry points. *)
    let get_from_stmt_annots code_annot_filter ((ki, _kf) as stmt) =
      Extlib.may
        (fun caf ->
           let loop_body_opt = match ki.skind with
             | Loop(_, { bstmts = body :: _ }, _, _, _) -> Some body
             | _ -> None
           in
           Annotations.iter_code_annot
             (fun _ a ->
                if caf a then get_zone_from_annot a stmt loop_body_opt)
             ki)
        code_annot_filter

    (** Used by annotations entry points. *)
    let from_ki_annot annot ((ki, _kf) as stmt) =
      let real_ki = match ki.skind with
          Loop(_,{bstmts = loop_entry::_},_,_,_) -> Some loop_entry
        | _ -> None
      in
      get_zone_from_annot annot stmt real_ki

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [stmt]. *)
    let from_stmt_annot annot stmt =
      from_ki_annot annot stmt;
      get_annot_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [stmt]. *)
    let from_stmt_annots code_annot_filter stmt =
      get_from_stmt_annots code_annot_filter stmt ;
      get_annot_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [kf]. *)
    let from_func_annots iter_on_kf_stmt code_annot_filter kf =
      let from_stmt_annots ki =
        get_from_stmt_annots code_annot_filter (ki, kf)
      in iter_on_kf_stmt from_stmt_annots kf;
        get_annot_result ()

    (** To quickly build a annotation filter *)
    let code_annot_filter annot ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var ~others =
      match annot.annot_content with
        | APragma (Slice_pragma _) -> slicing_pragma
        | AAssert _ ->
            (match Alarms.find annot with
               | None -> user_assert 
               | Some _a -> threat)
        | AVariant _ -> loop_var
        | AInvariant(_behav,true,_pred) -> loop_inv
        | AInvariant(_,false,_) -> others
        | AAllocation _ -> others
        | AAssigns _ -> others
        | APragma (Loop_pragma _)| APragma (Impact_pragma _) -> others
        | AStmtSpec _  (* TODO: statement contract *) -> false
  end

exception Prune

let to_result_from_pred p =
  let visitor = object (_self)
    inherit Visitor.frama_c_inplace

      method! vterm_lhost t =
        match t with
          | TResult _ -> raise Prune
          | _ -> DoChildren

  end
  in
  (try
     ignore(Visitor.visitFramacPredicateNamed visitor p);
     false
   with Prune -> 
     true)


let () =
  Db.Properties.Interp.code_annot := code_annot;
  Db.Properties.Interp.lval := lval;
  Db.Properties.Interp.expr := expr;
  Db.Properties.Interp.term_lval_to_lval := term_lval_to_lval;
  Db.Properties.Interp.term_to_exp := term_to_exp;

  Db.Properties.Interp.term_to_lval := term_to_lval;
  Db.Properties.Interp.term_offset_to_offset := term_offset_to_offset;

  Db.Properties.Interp.loc_to_lval := loc_to_lval;
  Db.Properties.Interp.loc_to_offset := loc_to_offset;
  Db.Properties.Interp.loc_to_exp := loc_to_exp;

  Db.Properties.Interp.To_zone.code_annot_filter := To_zone.code_annot_filter;
  Db.Properties.Interp.To_zone.mk_ctx_func_contrat := 
    To_zone.mk_ctx_func_contrat;
  Db.Properties.Interp.To_zone.mk_ctx_stmt_contrat := 
    To_zone.mk_ctx_stmt_contrat;
  Db.Properties.Interp.To_zone.mk_ctx_stmt_annot := To_zone.mk_ctx_stmt_annot;

  Db.Properties.Interp.To_zone.from_term := To_zone.from_term;
  Db.Properties.Interp.To_zone.from_terms := To_zone.from_terms;
  Db.Properties.Interp.To_zone.from_pred := To_zone.from_pred;
  Db.Properties.Interp.To_zone.from_preds := To_zone.from_preds;
  Db.Properties.Interp.To_zone.from_stmt_annot := To_zone.from_stmt_annot;
  Db.Properties.Interp.To_zone.from_stmt_annots := To_zone.from_stmt_annots;
  Db.Properties.Interp.To_zone.from_func_annots := To_zone.from_func_annots;

  Db.Properties.Interp.to_result_from_pred := to_result_from_pred;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
