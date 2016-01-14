(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

let dkey = Kernel.register_category "exn_flow"

(* all exceptions that can be raised somewhere in the AST. 
   Used to handle function pointers without exn specification
 *)
module All_exn =
  State_builder.Option_ref(Cil_datatype.Typ.Set)
    (struct let name = "Exn_flow.All_exn" let dependencies = [Ast.self] end)
  
module Exns =
  State_builder.Hashtbl(Kernel_function.Hashtbl)(Cil_datatype.Typ.Set)
    (struct
        let name = "Exn_flow.Exns"
        let dependencies = [Ast.self; All_exn.self]
        let size = 47
     end)

module ExnsStmt =
  State_builder.Hashtbl(Cil_datatype.Stmt.Hashtbl)(Cil_datatype.Typ.Set)
    (struct
      let name = "Exn_flow.ExnsStmt"
      let dependencies = [Ast.self; All_exn.self]
      let size = 53
    end)

let self_fun = Exns.self

let self_stmt = ExnsStmt.self

let purify t =
  let t = Cil.unrollTypeDeep t in
  Cil.type_remove_qualifier_attributes_deep t

class all_exn =
  object
    inherit Visitor.frama_c_inplace
    val mutable all_exn = Cil_datatype.Typ.Set.empty
    method get_exn = all_exn
    method! vstmt_aux s =
      match s.skind with
        | Throw (Some (_,t),_) ->
          all_exn <- Cil_datatype.Typ.Set.add (purify t) all_exn;
          SkipChildren
        | _ -> DoChildren
  end

let compute_all_exn () =
  let vis = new all_exn in
  Visitor.visitFramacFileSameGlobals (vis:>Visitor.frama_c_visitor) (Ast.get());
  vis#get_exn

let all_exn () = All_exn.memo compute_all_exn

let add_exn_var exns v =
  let t = Cil.unrollTypeDeep v.vtype in
  let t = Cil.type_remove_qualifier_attributes t in
  Cil_datatype.Typ.Set.add t exns

let add_exn_clause exns (v,_) = add_exn_var exns v

(* We're not really interested by intra-procedural Dataflow here: all the
   interesting stuff happens at inter-procedural level (except for Throw 
   encapsulated directly in a TryCatch, but even then it is easily captured
   at syntactical level). Therefore, we can as well use a syntactic pass 
   at intra-procedural level
 *)
class exn_visit =
object (self)
  inherit Visitor.frama_c_inplace
  val stack = Stack.create ()
  val possible_exn = Stack.create ()
  (* current set of exn included in a catch-all clause. Used to
     handle Throw None;
   *)
  val current_exn = Stack.create ()

  method private recursive_call kf =
    try
      Stack.iter
        (fun (kf',_) -> if Kernel_function.equal kf kf' then raise Exit) stack;
      false
    with Exit -> true

  method private add_exn t =
    let current_uncaught = Stack.top possible_exn in
    current_uncaught:= Cil_datatype.Typ.Set.add t !current_uncaught

  method private union_exn s =
    let current_uncaught = Stack.top possible_exn in
    current_uncaught := Cil_datatype.Typ.Set.union s !current_uncaught

  method! vstmt_aux s =
    match s.skind with
      | Throw (None,_) ->
          let my_exn = Stack.top current_exn in
          self#union_exn my_exn; ExnsStmt.replace s my_exn; SkipChildren
      | Throw(Some (_,t),_) ->
        let t = Cil.unrollTypeDeep t in
        let t = Cil.type_remove_qualifier_attributes t in
        self#add_exn t;
        ExnsStmt.replace s (Cil_datatype.Typ.Set.singleton t);
        SkipChildren
      | TryCatch (t,c,_) ->
        let catch, catch_all =
          List.fold_left
            (fun (catch, catch_all) ->
              function 
                | (Catch_all,_) -> catch, true
                | (Catch_exn(v,[]),_) ->
                    let catch = add_exn_var catch v in
                    catch, catch_all
                | (Catch_exn(_,aux), _) ->
                    let catch = List.fold_left add_exn_clause catch aux in
                    catch, catch_all)
            (Cil_datatype.Typ.Set.empty,false) c
        in
        Stack.push (ref Cil_datatype.Typ.Set.empty) possible_exn;
        ignore (Visitor.visitFramacBlock (self:>Visitor.frama_c_inplace) t);
        let my_exn = Stack.pop possible_exn in
        let uncaught = Cil_datatype.Typ.Set.diff !my_exn catch in
        (* uncaught exceptions are lift to previous set of exn, but
           only if there's no catch-all clause. *)
        Stack.push (ref Cil_datatype.Typ.Set.empty) possible_exn;
        if not catch_all then self#union_exn uncaught;
        List.iter
          (fun (v,b) ->
            let catch_all =
              match v with 
                  Catch_all -> true
                | Catch_exn (v,[]) ->
                    let catch = add_exn_var Cil_datatype.Typ.Set.empty v in
                    Stack.push catch current_exn; false
                | Catch_exn (_,aux) ->
                    let catch =
                      List.fold_left
                        add_exn_clause Cil_datatype.Typ.Set.empty aux
                    in
                    Stack.push catch current_exn; false
            in
            ignore 
              (Visitor.visitFramacBlock (self:>Visitor.frama_c_inplace) b);
            if not catch_all then ignore (Stack.pop current_exn))
          c;
        let my_exn = !(Stack.pop possible_exn) in
        ExnsStmt.replace s my_exn;
        self#union_exn my_exn;
        SkipChildren
      | If _ | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
      | TryFinally _ | TryExcept _
      | Instr _ -> (* must take into account exceptions thrown by a fun call*)
          Stack.push (ref Cil_datatype.Typ.Set.empty) possible_exn;
          DoChildrenPost
            (fun s ->
             let my_exn = !(Stack.pop possible_exn) in
             ExnsStmt.replace s my_exn;
             self#union_exn my_exn;
             s)
      (* No exception can be thrown here. *)
      | Return _ | Goto _ | Break _ | Continue _ ->
         ExnsStmt.replace s Cil_datatype.Typ.Set.empty;
         SkipChildren

  method! vinst =
    function
      | Call(_,{ enode = Lval(Var f,NoOffset) },_,_) ->
        let kf = Globals.Functions.get f in
        if self#recursive_call kf then begin
          let module Found =
              struct
                exception F of Cil_datatype.Typ.Set.t
              end
          in
          let computed_exn =
            try
              Stack.iter
                (fun (kf', exns) ->
                  if Kernel_function.equal kf kf' then raise (Found.F !exns))
                stack;
              Kernel.fatal "No cycle found!"
            with Found.F exns -> exns
          in
          let known_exn =
            try Exns.find kf with Not_found -> Cil_datatype.Typ.Set.empty
          in
          if Cil_datatype.Typ.Set.subset computed_exn known_exn then begin
            (* Fixpoint found, no need to recurse. *)
            self#union_exn known_exn
          end else begin
            (* add known exns in table and recurse. Termination is ensured
               by the fact that only a finite number of exceptions
               can be thrown. *)
            let kf_exn = Cil_datatype.Typ.Set.union computed_exn known_exn in
            Exns.replace kf kf_exn;
            ignore
              (Visitor.visitFramacFunction 
                 (self:>Visitor.frama_c_visitor)
                 (Kernel_function.get_definition kf));
            let callee_exn = Exns.find kf in
            self#union_exn callee_exn
          end
        end else if Exns.mem kf then begin
          self#union_exn (Exns.find kf)
        end else if Kernel_function.is_definition kf then begin
          let def = Kernel_function.get_definition kf in
          ignore
            (Visitor.visitFramacFunction (self:>Visitor.frama_c_visitor) def);
          let callee_exn = Exns.find kf in
          self#union_exn callee_exn
        end else begin (* TODO: introduce extension to declare
                          exceptions that can be thrown by prototypes. *)
            Kernel.warning
              "Assuming declared function %a can't throw any exception"
              Kernel_function.pretty kf
        end;
        SkipChildren
      | Call _ ->
        (* Function pointer: we consider that it can throw any possible
           exception. *)
        self#union_exn (all_exn()); SkipChildren
      | _ -> SkipChildren

  method! vfunc f =
    let my_exns = ref Cil_datatype.Typ.Set.empty in
    let kf = Globals.Functions.get f.svar in
    Stack.push (kf,my_exns) stack;
    Stack.push my_exns possible_exn;
    let after_visit f =
      let callee_exn = Stack.pop possible_exn in
      Exns.add kf !callee_exn;
      ignore (Stack.pop stack); f
    in
    DoChildrenPost after_visit

end

let compute_kf kf =
  if Kernel_function.is_definition kf then
    ignore
      (Visitor.visitFramacFunction (new exn_visit)
         (Kernel_function.get_definition kf))
  (* just ignore prototypes. *)

let compute () = Globals.Functions.iter compute_kf

let get_type_tag t =
  let rec aux t =
    match t with
      | TVoid _ -> "v"
      | TInt (IBool,_) -> "B"
      | TInt (IChar,_) -> "c"
      | TInt (ISChar,_) -> "sc"
      | TInt (IUChar,_) -> "uc"
      | TInt (IInt,_) -> "i"
      | TInt (IUInt,_) -> "ui"
      | TInt (IShort,_) -> "s"
      | TInt (IUShort,_) -> "us"
      | TInt (ILong,_) -> "l"
      | TInt (IULong,_) -> "ul"
      | TInt (ILongLong,_) -> "ll"
      | TInt (IULongLong,_) -> "ull"
      | TFloat(FFloat,_) -> "f"
      | TFloat(FDouble,_) -> "d"
      | TFloat (FLongDouble,_) -> "ld"
      | TPtr(t,_) -> "p" ^ aux t
      | TArray(t,_,_,_) -> "a" ^ aux t
      | TFun(rt,l,_,_) ->
        let base = "fun" ^ aux rt in
        (match l with
          | None -> base
          | Some l ->
            List.fold_left (fun acc (_,t,_) -> acc ^ aux t) base l)
      | TNamed _ -> Kernel.fatal "named type not correctly unrolled"
      | TComp (s,_,_) -> (if s.cstruct then "S" else "U") ^ s.cname
      | TEnum (e,_) -> "E" ^ e.ename
      | TBuiltin_va_list _ -> "va"
  in "__fc_" ^ aux t

let get_type_enum t = "__fc_exn_kind_" ^ (get_type_tag t)

let get_kf_exn kf =
  if not (Exns.is_computed()) then compute();
  Exns.find kf

let exn_uncaught_name = "exn_uncaught"
let exn_kind_name = "exn_kind"
let exn_obj_name = "exn_obj"

(* enumeration for all possible exceptions *)
let generate_exn_enum exns =
  let loc = Cil_datatype.Location.unknown in
  let v = ref 0 in
  let info =
    { eorig_name = "__fc_exn_enum";
      ename = "__fc_exn_enum";
      eitems = [];
      eattr = [];
      ereferenced = true; (* not generated if no exn can be thrown *)
      ekind = IInt; (* Take into account -enum option? *)
    }
  in
  let create_enum_item t acc =
    let ve = Cil.kinteger ~loc IInt !v in
    let name = get_type_enum t in
    incr v;
    { eiorig_name = name;
      einame = name;
      eival = ve;
      eihost = info;
      eiloc = loc;
    } :: acc
  in
  let enums = Cil_datatype.Typ.Set.fold create_enum_item exns [] in
  info.eitems <- enums;
  info

(* discriminated union (i.e. struct + union) for all possible exceptions. *)
let generate_exn_union e exns =
  let loc = Cil_datatype.Location.unknown in
  let create_union_fields _ =
    let add_one_field t acc = (get_type_tag t, t, None, [], loc) :: acc in
    Cil_datatype.Typ.Set.fold add_one_field exns []
  in
  let union_name = "__fc_exn_union" in
  let exn_kind_union =
    Cil.mkCompInfo false union_name ~norig:union_name create_union_fields []
  in
  let create_struct_fields _ =
    let uncaught = (exn_uncaught_name, Cil.intType, None, [], loc) in
    let kind = (exn_kind_name, TEnum (e,[]), None, [], loc) in
    let obj =
      (exn_obj_name,
       TComp(exn_kind_union, { scache = Not_Computed } , []), None, [], loc)
    in
    [uncaught; kind; obj]
  in
  let struct_name = "__fc_exn_struct" in
  let exn_struct =
    Cil.mkCompInfo true struct_name ~norig:struct_name create_struct_fields []
  in
  exn_kind_union, exn_struct

let add_types_and_globals typs globs f =
  let iter_globs (acc,added) g =
    match g with
      | GVarDecl _ | GVar _  | GFun _ as g when not added ->
        (g :: List.rev_append globs (List.rev_append typs acc), true)
      | _ -> g :: acc, added
  in
  let globs, added = List.fold_left iter_globs ([],false) f.globals in
  let globs =
    if added then List.rev globs
    else List.rev_append globs (List.rev_append typs globs)
  in
  f.globals <- globs;
  f

let make_init_assign loc v init =
  let rec aux lv acc = function
    | SingleInit e -> Cil.mkStmtOneInstr (Set(lv,e,loc)) :: acc
    | CompoundInit(_,l) ->
      let treat_one_offset acc (o,i) = aux (Cil.addOffsetLval o lv) acc i in
      List.fold_left treat_one_offset acc l
  in
  List.rev (aux (Var v, NoOffset) [] init)

let find_exns e =
  match e.enode with
    | Lval(Var v, NoOffset) ->
      (try Exns.find (Globals.Functions.get v)
       with Not_found -> Cil_datatype.Typ.Set.empty)
    | _ -> all_exn ()

class erase_exn =
object(self)
  inherit Visitor.frama_c_inplace
  (* reverse before filling. *)
  val mutable new_types = []

  val exn_enum = Cil_datatype.Typ.Hashtbl.create 7

  val exn_union = Cil_datatype.Typ.Hashtbl.create 7

  val mutable modified_funcs = Cil_datatype.Fundec.Set.empty

  val mutable exn_struct = None

  val mutable exn_var = None

  val mutable can_throw = false

  val mutable catched_var = None

  val mutable label_counter = 0

  val exn_labels = Cil_datatype.Typ.Hashtbl.create 7
  val catch_all_label = Stack.create ()

  method modified_funcs = modified_funcs

  method private update_enum_bindings enum exns =
    let update_one_binding t =
      let s = get_type_enum t in
      let ei = List.find (fun ei -> ei.einame = s) enum.eitems in
      Cil_datatype.Typ.Hashtbl.add exn_enum t ei
    in
    Cil_datatype.Typ.Set.iter update_one_binding exns

  method private update_union_bindings union exns =
    let update_one_binding t =
      let s = get_type_tag t in
      Kernel.debug2 ~dkey
        "Registering %a as possible exn type" Cil_datatype.Typ.pretty t;
      let fi = List.find (fun fi -> fi.fname = s) union.cfields in
      Cil_datatype.Typ.Hashtbl.add exn_union t fi
    in
    Cil_datatype.Typ.Set.iter update_one_binding exns

  method private exn_kind t = Cil_datatype.Typ.Hashtbl.find exn_enum t

  method private exn_field_off name =
    List.find (fun fi -> fi.fname = name) (Extlib.the exn_struct).cfields

  method private exn_field name =
    Var (Extlib.the exn_var), Field(self#exn_field_off name, NoOffset)

  method private exn_field_term name =
    TVar(Cil.cvar_to_lvar (Extlib.the exn_var)),
    TField(self#exn_field_off name, TNoOffset)

  method private exn_obj_field = self#exn_field exn_obj_name

  method private exn_obj_field_term = self#exn_field_term exn_obj_name

  method private exn_kind_field = self#exn_field exn_kind_name

  method private exn_kind_field_term = self#exn_field_term exn_kind_name

  method private uncaught_flag_field = self#exn_field exn_uncaught_name

  method private uncaught_flag_field_term =
    self#exn_field_term exn_uncaught_name

  method private exn_obj_kind_field t =
    Kernel.debug2 ~dkey
      "Searching for %a as possible exn type" Cil_datatype.Typ.pretty t; 
    Cil_datatype.Typ.Hashtbl.find exn_union t

  method private test_uncaught_flag loc b =
    let e1 = Cil.new_exp ~loc (Lval self#uncaught_flag_field) in
    let e2 = if b then Cil.one ~loc else Cil.zero ~loc in
    Cil.new_exp ~loc (BinOp(Eq,e1,e2,Cil.intType))

  method private pred_uncaught_flag loc b =
    let e1 =
      Logic_const.term
        ~loc (TLval self#uncaught_flag_field_term) Linteger
    in
    let e2 =
      if b then Logic_const.tinteger ~loc 1
      else Logic_const.tinteger ~loc 0
    in
    Logic_const.prel ~loc (Req,e1,e2)

  method private set_uncaught_flag loc b =
    let e = if b then Cil.one ~loc else Cil.zero ~loc in
    Cil.mkStmtOneInstr (Set(self#uncaught_flag_field,e,loc))

  method private set_exn_kind loc t =
    let e = self#exn_kind (purify t) in
    let e = Cil.new_exp ~loc (Const (CEnum e)) in
    Cil.mkStmtOneInstr(Set(self#exn_kind_field,e,loc))

  method private set_exn_value loc t e =
    let lv = self#exn_obj_field in
    let union_field = self#exn_obj_kind_field (purify t) in
    let lv = Cil.addOffsetLval (Field (union_field, NoOffset)) lv in
    Cil.mkStmtOneInstr (Set(lv,e,loc))

  method private jumps_to_default_handler loc =
    if Stack.is_empty catch_all_label then begin
      (* no catch-all clause in the function: just go up in the stack. *)
      let kf = Extlib.the self#current_kf in
      let ret = Kernel_function.find_return kf in
      let rtyp = Kernel_function.get_return_type kf in
      if ret.labels = [] then
        ret.labels <- [Label("__ret_label",Cil_datatype.Stmt.loc ret,false)];
      let goto = mkStmt (Goto (ref ret,loc)) in
      match ret.skind with
        | Return (None,_) -> [goto]
        (* rt is void: do not need to create a dummy return value *)
        | Return (Some { enode = Lval(Var rv, NoOffset) },_) ->
          let init = Cil.makeZeroInit ~loc rtyp in
          make_init_assign loc rv init @ [goto]
        | Return _ ->
          Kernel.fatal "exception removal should be used after oneRet"
        | _ ->
          Kernel.fatal "find_return did not give a Return statement"
    end else begin
      let stmt = Stack.top catch_all_label in
      [mkStmt (Goto (ref stmt, loc))]
    end

  method private jumps_to_handler loc t =
    let t = purify t in
    try
      let stmt = Cil_datatype.Typ.Hashtbl.find exn_labels t in
      [mkStmt (Goto (ref stmt, loc))]
    with
      | Not_found -> self#jumps_to_default_handler loc

  method! vfile f =
    let exns = all_exn () in
    if not (Cil_datatype.Typ.Set.is_empty exns) then begin
      let loc = Cil_datatype.Location.unknown in
      let e = generate_exn_enum exns in
      let u,s = generate_exn_union e exns in
      let exn =
        Cil.makeGlobalVar "__fc_exn" (TComp (s,{scache = Not_Computed},[]))
      in
      self#update_enum_bindings e exns;
      self#update_union_bindings u exns;
      exn_struct <- Some s;
      can_throw <- true;
      new_types <-
        GCompTag (s,loc) ::
        GCompTag (u,loc) ::
        GEnumTag (e,loc) :: new_types;
      exn_var <- Some exn;
      let exn_init = Cil.makeZeroInit ~loc (TComp(s,{scache=Not_Computed},[]))
      in
      let gexn_var = GVar(exn, { init = Some exn_init }, loc) in
      ChangeDoChildrenPost(
        f,add_types_and_globals (List.rev new_types) [gexn_var])
    end else (* nothing can be thrown in the first place, but we still have
                to get rid of (useless) try/catch blocks if any. *)
        DoChildren

  method private visit_catch_clause loc (v,b) =
    let loc =
      match b.bstmts with
        | [] -> loc
        | [x] -> Cil_datatype.Stmt.loc x
        | x::tl ->
          fst (Cil_datatype.Stmt.loc x),
          snd (Cil_datatype.Stmt.loc (Extlib.last tl))
    in
    let add_unreachable_block b =
      Cil.mkStmt (If(Cil.zero ~loc, b, Cil.mkBlock [], loc))
    in
    let assign_catched_obj v b =
      let exn_obj = self#exn_obj_field in
      let kind_field = self#exn_obj_kind_field (purify v.vtype) in
      let lv = Cil.addOffsetLval (Field (kind_field,NoOffset)) exn_obj in
      let s =
        Cil.mkStmtOneInstr
          (Set ((Var v, NoOffset), Cil.new_exp ~loc (Lval lv), loc))
      in
      b.bstmts <- s :: b.bstmts
    in
    let f = Extlib.the self#current_func in
    let update_locals v b =
      if not (List.memq v b.blocals) then b.blocals <- v::b.blocals;
      if not (List.memq v f.slocals) then f.slocals <- v::f.slocals
    in
    let b =
      (match v with
        | Catch_all -> b
        | Catch_exn (v,[]) ->
          v.vtype <- purify v.vtype; update_locals v b;assign_catched_obj v b; b
        | Catch_exn(v,aux) ->
          let add_one_aux stmts (v,b) =
            v.vtype <- purify v.vtype; update_locals v b;
            assign_catched_obj v b;
            add_unreachable_block b :: stmts
          in
          b.blocals <- List.filter (fun v' -> v!=v') b.blocals;
          let aux_blocks =
            List.fold_left add_one_aux [Cil.mkStmt (Block b)] aux
          in
          let main_block = Cil.mkBlock aux_blocks in
          v.vtype <- purify v.vtype;
          update_locals v main_block;
          main_block)
    in
    ignore (Visitor.visitFramacBlock (self :> Visitor.frama_c_visitor) b);
    add_unreachable_block b

  method! vfunc _ = label_counter <- 0; DoChildren

  method private modify_current () =
    modified_funcs <-
      Cil_datatype.Fundec.Set.add (Extlib.the self#current_func) modified_funcs;

  method private aux_handler_goto target (v,b) =
    let loc = v.vdecl in
    let goto_main_handler = Cil.mkStmt (Goto (ref target,loc)) in
    let suf =
      if label_counter = 0 then "" else "_" ^ (string_of_int label_counter)
    in
    let lab = (get_type_tag (purify v.vtype)) ^ suf in
    label_counter <- label_counter + 1;
    b.bstmts <- b.bstmts @ [goto_main_handler];
    (* we have at least the goto statement in the block *)
    let s = List.hd b.bstmts in
    s.labels <- (Label(lab,loc,false)::s.labels);
    Cil_datatype.Typ.Hashtbl.add exn_labels (purify v.vtype) s

  method private guard_post_cond (kind,pred as orig) =
    match kind with
        (* If we exit explicitely with exit,
           we haven't seen an uncaught exception anyway. *)
      | Exits | Breaks | Continues -> orig
      | Returns | Normal ->
          let loc = pred.ip_loc in
          let p = self#pred_uncaught_flag loc false in
          let pred' = Logic_const.pred_of_id_pred pred in
          (kind,
           (Logic_const.new_predicate
              (Logic_const.pimplies ~loc (p,pred'))))

  method! vbehavior b =
    match self#current_kf, self#current_stmt with
      | None, None -> SkipChildren
        (* Prototype is assumed to not throw any exception. *)
      | None, Some _ ->
          Kernel.fatal
            "Inconsistent visitor state: visiting a statement \
             outside of any function."
      | Some f, None when not (Kernel_function.is_definition f) -> 
          (* By hypothesis, prototypes do not throw anything. *)
          SkipChildren
      | Some f, None -> (* function contract *)
          let exns = Exns.find f in
          if Cil_datatype.Typ.Set.is_empty exns then SkipChildren
          else begin 
            b.b_post_cond <- List.map self#guard_post_cond b.b_post_cond;
            ChangeTo b (* need to register the new clauses. *)
          end
      | Some _, Some s -> (* statement contract *)
          let exns = ExnsStmt.find s in
          if Cil_datatype.Typ.Set.is_empty exns then SkipChildren
          else begin
            b.b_post_cond <- List.map self#guard_post_cond b.b_post_cond;
            ChangeTo b
          end

  method! vstmt_aux s =
    match s.skind with
      | Instr (Call (_,f,_,loc) as instr) ->
        let my_exns = find_exns f in
        if Cil_datatype.Typ.Set.is_empty my_exns then SkipChildren
        else begin
          self#modify_current ();
          let make_jump t (stmts, uncaught) =
            let t = purify t in
            if Cil_datatype.Typ.Hashtbl.mem exn_labels t then begin
              let e = self#exn_kind t in
              let e = Cil.new_exp ~loc (Const (CEnum e)) in
              let b = self#jumps_to_handler loc t in
              let s = Cil.mkStmt (Block (Cil.mkBlock b)) in
              s.labels <- [Case (e,loc)];
              s::stmts, uncaught
            end else stmts, true
          in
          let stmts, uncaught =
            Cil_datatype.Typ.Set.fold make_jump my_exns ([],false)
          in
          let stmts =
            if uncaught then begin
              let default = 
                Cil.mkStmt (
                  Block (Cil.mkBlock (self#jumps_to_default_handler loc)))
              in
              default.labels <- [Default loc];
              List.rev_append stmts [default]
            end else List.rev stmts
          in
          let test = self#test_uncaught_flag loc true in
          let cases = Cil.new_exp ~loc (Lval self#exn_kind_field) in
          let switch = Cil.mkStmt (Switch(cases,Cil.mkBlock stmts,stmts,loc)) in
          let handler =
            Cil.mkStmt (If(test,Cil.mkBlock [switch],Cil.mkBlock [],loc))
          in
          let instr =
            Visitor.visitFramacInstr (self:>Visitor.frama_c_visitor) instr
          in
          let call = Cil.mkStmtOneInstr (List.hd instr) in
          s.skind <- Block (Cil.mkBlock [call;handler]);
          SkipChildren
        end
      | Throw _ when not can_throw ->
        Kernel.fatal "Unexpected Throw statement"
      | Throw(Some(e,t),loc) ->
        self#modify_current();
        let s1 = self#set_uncaught_flag loc true in
        let s2 = self#set_exn_kind loc t in
        let s3 = self#set_exn_value loc t e in
        let rv = self#jumps_to_handler loc t in
        let b = mkBlock (s1 :: s2 :: s3 :: rv) in
        s.skind <- Block b;
        SkipChildren
      | Throw (None,loc) ->
        self#modify_current ();
        let s1 = self#set_uncaught_flag loc true in
        let t = purify (Extlib.the exn_var).vtype in
        let rv = self#jumps_to_handler loc t in
        let b = mkBlock (s1 :: rv) in
        s.skind <- Block b;
        SkipChildren
      | TryCatch (t,_,_) when not can_throw ->
        self#modify_current();
        (* no exception can be thrown:
           we can simply remove the catch clauses. *)
        s.skind <- (Block t);
        DoChildren (* visit the block for nested try catch. *)
      | TryCatch (t,c,loc) ->
        self#modify_current();
        (* Visit the catch clauses first, as they are in the same catch scope
           than the current block. As we are adding statements in the
           auxiliary blocks, we need to do that before adding labels to the
           entry points of these blocks.
         *)
        let stmts = List.map (self#visit_catch_clause loc) c in
        let suf =
          if label_counter = 0 then "" else "_" ^ (string_of_int label_counter)
        in
        label_counter <- label_counter + 1;
        (* now generate the labels for jumping to the appropriate block when
           catching an exception. *)
        List.iter
          (function
            | (Catch_exn (v,aux),b) ->
              (* first thing that we do is to flag the exn as caught *)
              let stmt = self#set_uncaught_flag v.vdecl false in
              let label = (get_type_tag (purify v.vtype)) ^ suf in
              stmt.labels <- [Label (label,v.vdecl,false)];
              b.bstmts <- stmt :: b.bstmts;
              (match aux with
                | [] ->
                  Cil_datatype.Typ.Hashtbl.add exn_labels (purify v.vtype) stmt
                | _ :: _ ->
                  List.iter (self#aux_handler_goto stmt) aux)
            | (Catch_all, b) ->
              let loc =
                match b.bstmts with [] -> loc | s::_ -> Cil_datatype.Stmt.loc s
              in
              let stmt = self#set_uncaught_flag loc false in
              stmt.labels <- [Label ("catch_all" ^ suf,loc,false)];
              b.bstmts <- stmt :: b.bstmts;
              Stack.push stmt catch_all_label)
          (* We generate the bindings in reverse order, as if two clauses
             match the same type, the first one (which is the one that has
             to be taken), will be visited last, hiding the binding of the
             second in the Hashtbl. *)
          (List.rev c);
        ignore (Visitor.visitFramacBlock (self:>Visitor.frama_c_visitor) t);
        List.iter
          (function
            | (Catch_exn (v,[]), _) ->
              Cil_datatype.Typ.Hashtbl.remove exn_labels (purify v.vtype)
            | Catch_exn(_,l), _ ->
              List.iter
                (fun (v,_) ->
                  Cil_datatype.Typ.Hashtbl.remove exn_labels (purify v.vtype))
                l
            | Catch_all,_ -> ignore (Stack.pop catch_all_label))
          c; (* we remove bindings in the reverse order as we added them,
                though order does not really matter here. *)
        t.bstmts <- t.bstmts @ stmts;
        s.skind <- Block t;
        SkipChildren
      | _ -> DoChildren
        
end

let prepare_file f =
  if Kernel.SimplifyCfg.get () then begin
    Cfg.prepareCFG ~keepSwitch:false f;
  end;
  File.must_recompute_cfg f

let remove_exn f =
  if Kernel.RemoveExn.get() then begin
    Visitor.visitFramacFileSameGlobals (new exn_visit) f;
    let vis = new erase_exn in
    Visitor.visitFramacFile (vis :> Visitor.frama_c_visitor) f;
    Cil_datatype.Fundec.Set.iter prepare_file vis#modified_funcs
  end

let transform_category = File.register_code_transformation_category "remove_exn"

let () =
  let deps = [ (module Kernel.RemoveExn: Parameter_sig.S) ] in
  File.add_code_transformation_after_cleanup ~deps transform_category remove_exn
