(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Cil_types
open Cil_datatype

let dkey = Widen_hints_ext.dkey

(* Note concerning all visitors and hints related to statements:
   currently, [stmt] is always [None]. Because our dataflow does not
   stabilize inner loop before the outer ones, we sometimes end up widening
   an inner variable inside an outer loop. Hence, we need to have the inner
   widening hints in the outer loops. To do so, the simplest is to avoid
   specifying statements altogether. This may be inefficient for codes that
   reuse loop indexes...
 *)

class pragma_widen_visitor init_widen_hints init_enclosing_loops = object(self)
  inherit Visitor.frama_c_inplace

  val widen_hints = init_widen_hints
  val enclosing_loops = init_enclosing_loops

  method private add_thresholds ?base thresholds =
    widen_hints := Widen_type.join (Widen_type.num_hints None(*see note*) base thresholds) !widen_hints

  method private add_var_hints ~stmt hints =
    widen_hints := Widen_type.join (Widen_type.var_hints stmt hints) !widen_hints

  method private process_loop_pragma stmt p =
    match p with
  | Widen_variables l -> begin
    let f (lv, lt) t = match t with
      | { term_node= TLval (TVar {lv_origin = Some vi}, _)} ->
        (Base.Set.add (Base.of_varinfo vi) lv, lt)
      | _ -> (lv, t::lt)
    in
    match List.fold_left f (Base.Set.empty, []) l with
    | (var_hints, []) ->
      (* the annotation is empty or contains only variables *)
      self#add_var_hints ~stmt var_hints
    | (_lv, _lt) ->
      Value_parameters.warning ~once:true
        "could not interpret loop pragma relative to widening variables"
  end
  | Widen_hints l -> begin
    let f (lv, lnum, lt) t = match t with
      | { term_node= TLval (TVar { lv_origin = Some vi}, _)} ->
        (Base.of_varinfo vi :: lv, lnum, lt)
      | { term_node= TConst (Integer(v,_))} ->
        (lv, Ival.Widen_Hints.add v lnum, lt)
      | _ -> (lv, lnum, t::lt)
    in
    match List.fold_left f ([], Ival.Widen_Hints.empty, []) l with
    | (vars, thresholds, []) ->
      (* the annotation is empty or contains only variables *)
      if vars = [] then
        self#add_thresholds thresholds
      else
        List.iter (fun base -> self#add_thresholds ~base thresholds) vars
    | _ ->
      Value_parameters.warning ~once:true
        "could not interpret loop pragma relative to widening hint"
  end
  | _ -> ()

  method! vstmt (s:stmt) =
    match s.skind with
    | Loop (_, bl, _, _, _) -> begin
      (* ZZZ: this code does not handle loops that are created using gotos. We
         could improve this by finding the relevant statements using a
         traversal of the CFG. *)
      let annot = Annotations.code_annot s in
      let pragmas = Logic_utils.extract_loop_pragma annot in
      List.iter (self#process_loop_pragma s) pragmas;
      let new_loop_info = s :: enclosing_loops in
      let visitor = new pragma_widen_visitor widen_hints new_loop_info in
      ignore (Visitor.visitFramacBlock visitor bl);
      Cil.SkipChildren (* Otherwise the inner statements are visited multiple
                          times needlessly *)
    end
    | If (exp, bl_then, bl_else, _) -> begin
      (* Look for if-goto and if-break statements. The variables of the
         condition are added to the early widening variable set for this loop.*)
      let aux_loop loop =
        let loop_stmts = Stmts_graph.get_stmt_stmts loop in
        let rec aux_block_loop bl =
          match bl with
          | {bstmts = []} -> ()
          | {bstmts = [{skind = Block bl}]} -> aux_block_loop bl
          | {bstmts = ({skind = Break _; succs = [stmt]}|
                       {skind = Goto ({contents=stmt},_)})
            ::_} when not (Stmt.Set.mem stmt loop_stmts) ->
            (* This block goes out of [loop]. The variables of [exp] are hints*)
            let varinfos = Cil.extract_varinfos_from_exp exp in
            let var_hints =
              Varinfo.Set.fold
                (fun vi set -> Base.Set.add (Base.of_varinfo vi) set)
                varinfos Base.Set.empty
            in
            self#add_var_hints ~stmt:loop var_hints
          | _ -> ()
        in
        aux_block_loop bl_then;
        aux_block_loop bl_else
      in
      List.iter aux_loop enclosing_loops;
      Cil.DoChildren
    end
    | _ -> Cil.DoChildren

  method! vexpr (e:exp) = begin
    let with_succ v = [v ; Integer.succ v]
    and with_pred v = [Integer.pred v ; v ]
    and with_s_p_ v = [Integer.pred v; v; Integer.succ v]
    and default_visit _e = Cil.DoChildren
    and unop_visit e =
      match e with
      | {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
               | Lval (Var varinfo, _))} ->
        let thresholds = Ival.Widen_Hints.singleton Integer.zero in
        let base = Base.of_varinfo varinfo in
        self#add_thresholds ~base thresholds;
        Cil.DoChildren
      | _ -> Cil.DoChildren
    and comparison_visit add1 add2 e1 e2 =
      let add base set =
        let thresholds =
          List.fold_right Ival.Widen_Hints.add set Ival.Widen_Hints.empty
        in
        self#add_thresholds ~base thresholds
      in
      let i1, i2 = Cil.constFoldToInt e1, Cil.constFoldToInt e2 in begin
      match i1, i2, e1, e2 with
      | Some int64, _, _, {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
                                 | Lval (Var varinfo, _))}->
        add (Base.of_varinfo varinfo) (add1 int64)
      | _, Some int64, {enode=(CastE(_, { enode=Lval (Var varinfo, _)})
                              | Lval (Var varinfo, _))}, _ ->
        add (Base.of_varinfo varinfo) (add2 int64)
      | _ -> ()
      end;
      Cil.DoChildren
    in
    match e.enode with
    | BinOp (Lt, e1, e2, _)
    | BinOp (Gt, e2, e1, _)
    | BinOp (Le, e2, e1, _)
    | BinOp (Ge, e1, e2, _) ->
        comparison_visit with_succ with_pred e1 e2
    | BinOp (Eq, e1, e2, _)
    | BinOp (Ne, e1, e2, _) ->
        comparison_visit with_s_p_ with_s_p_ e1 e2
    | UnOp (Neg, e, _) ->
        unop_visit e
    | Lval _ ->
        unop_visit e
    | _ -> default_visit e
  end

  (* [idx] is an expression that serves as index in an access to an array
     of size [size]. When possible, add hints for the variables in [idx] *)
  method private add_index_hints size idx =
    (* add the bounds [size-shift, size-shift-1] to the hints for [vidx] *)
    let add_hint vidx size shift =
      let bound1 = Integer.sub size shift in
      let bound2 = Integer.(sub bound1 one) in
      let thresholds = Ival.Widen_Hints.of_list [bound1; bound2] in
      self#add_thresholds ~base:(Base.of_varinfo vidx) thresholds
    in
    (* Find inside [idx] a variable on which we will add hints. [shift] is an
       integer that indicates that we access to [idx+shift], instead of to
       [idx] directly *)
    let rec aux_idx idx shift =
      match idx.enode with
      | Lval (Var vidx, _) -> add_hint vidx size shift
      | CastE (typ, e') when Cil.isIntegralType typ ->
        (* It is safe to ignore casts: hints do not need to be sound. *)
        aux_idx e' shift
      | BinOp ((PlusA | MinusA as op), e1, e2, _) -> begin
        (* See if either [e1] or [e2] is constant. If so, find a variable in
           the other expression and add a hint for this variable, shifted. *)
        let shift' s =
          if op = PlusA then Integer.add shift s else Integer.sub shift s
        in
        match Cil.constFoldToInt e1 with
        | Some shift1 -> aux_idx e2 (shift' shift1)
        | None -> begin
          match Cil.constFoldToInt e2 with
          | None -> ()
          | Some shift2 -> aux_idx e1 (shift' shift2)
        end
      end
      | _ -> ()
    in
    aux_idx idx Integer.zero

  (* Find an array access and infer hints for the variables involved. We visit
     the l-value ourselves. This way, we catch all accesses, including in
     sub-structures. *)
  method private find_array_accesses (host, off) =
    let rec aux_offset typ offs =
      match offs with
      | NoOffset -> ()
      | Field (fi, off) -> aux_offset fi.ftype off
      | Index (idx, off) -> begin
        match Cil.unrollType typ with 
        | TArray (typ_e, size, _, _) -> begin
          aux_offset typ_e off;
          try
            let size = Cil.lenOfArray64 size in
            if Integer.(gt size zero) then
              self#add_index_hints size idx
          with Cil.LenOfArray -> ()
        end
        | _ -> ()
      end
    in
    aux_offset (Cil.typeOfLhost host) off

  method! vlval lv =
    self#find_array_accesses lv;
    Cil.DoChildren
end

(* returns the (static) bases associated to [hvars], which
   must not be [HintMem]. *)
let base_of_static_hvars hvars =
  match hvars with
  | Widen_hints_ext.HintAllVars -> None
  | Widen_hints_ext.HintVar vi -> Some (Base.of_varinfo vi)
  | Widen_hints_ext.HintMem (e, offset) ->
    (* syntactic constraints prevent this from happening *)
    Value_parameters.fatal "unsupported lhost: %a" Printer.pp_lval (Mem e, offset)

let threshold_of_threshold_term ht =
  let global_find_init vi =
    try (Globals.Vars.find vi).init with Not_found -> None
  in
  let ht = Cil.visitCilTerm
      (new Logic_utils.simplify_const_lval global_find_init) ht
  in
  match Logic_utils.constFoldTermToInt ht with
  | None -> Value_parameters.abort ~source:(fst ht.term_loc)
              "could not parse widening hint: %a@ \
               If it contains variables, they must be global const integers."
              Printer.pp_term ht
  | Some i -> i

let thresholds_of_threshold_terms hts =
  List.fold_left (fun acc' ht ->
      Ival.Widen_Hints.add (threshold_of_threshold_term ht) acc'
    ) Ival.Widen_Hints.empty hts

class hints_visitor init_widen_hints global = object(self)
  inherit Visitor.frama_c_inplace

  val widen_hints = init_widen_hints

  method private iter_static_hints ~global hints =
    let static_hints =
      List.filter
        (fun h -> not (Widen_hints_ext.is_dynamic h)) hints
    in
    List.iter
      (fun ({Widen_hints_ext.vars; loc}, wh_terms) ->
         let base = base_of_static_hvars vars in
         let thresholds = thresholds_of_threshold_terms wh_terms in
         Value_parameters.feedback ~source:(fst loc) ~dkey
           "adding%s hint from annotation: %a, %a (for all statements)"
           (if global then " global" else "")
           (Pretty_utils.pp_opt ~none:(format_of_string "for all variables")
              Base.pretty) base
           Ival.Widen_Hints.pretty thresholds;
         let new_hints =
           Widen_type.num_hints None (* see note above *) base thresholds
         in
         widen_hints := Widen_type.join new_hints !widen_hints
      ) static_hints

  method! vstmt s =
    let all_hints = Widen_hints_ext.get_stmt_widen_hint_terms s in
    let global_hints =
      List.filter (fun ht -> Widen_hints_ext.is_global ht = global) all_hints
    in
    self#iter_static_hints ~global global_hints;
    Cil.DoChildren
end

module Global_Static_Hints =
  State_builder.Ref
    (Widen_type)
    (struct
      let dependencies = [ Ast.self ]
      let name = "Widen.Global_Static_Hints"
      let default = Widen_type.default
    end)
let () = Ast.add_monotonic_state Global_Static_Hints.self

(* Global widen hints, used for all functions *)
let global_widen_hints () =
  if (not (Global_Static_Hints.is_computed ())) then
    begin
      Value_parameters.debug ~dkey "computing global widen hints";
      let global_widen_hints = ref (Widen_type.default ()) in
      Globals.Functions.iter_on_fundecs (fun fd ->
          let visitor = new hints_visitor global_widen_hints true in
          ignore (Visitor.visitFramacFunction visitor fd)
        );
      Global_Static_Hints.set !global_widen_hints;
      Global_Static_Hints.mark_as_computed ();
      !global_widen_hints
    end
  else
    Global_Static_Hints.get ()

let per_function_static_hints fdec =
  let widen_hints = ref (global_widen_hints ()) in
  let visitor_pragma = new pragma_widen_visitor widen_hints [] in
  ignore (Visitor.visitFramacFunction visitor_pragma fdec);
  let visitor_local = new hints_visitor widen_hints false in
  ignore (Visitor.visitFramacFunction visitor_local fdec);
  !widen_hints  
  
module Per_Function_Static_Hints =
  State_builder.Hashtbl
    (Cil_datatype.Fundec.Hashtbl)
    (Widen_type)
    (struct
      let name = "Widen.Per_Function_Static_Hints"
      let size = 97
      let dependencies = [ Ast.self ]
    end)
let () = Ast.add_monotonic_state Per_Function_Static_Hints.self

(* parse and precompute global and local static hints *)
let precompute_widen_hints () =
  Globals.Functions.iter_on_fundecs
    (fun fd ->
       Per_Function_Static_Hints.replace fd (per_function_static_hints fd))

type dynamic_hint = {
  mutable bases : Base.Hptset.t
      (* dynamic, used to detect when a new base needs to be added to the global
         widening hints *);
  lv : exp * offset; (* static, parsed once from the AST *)
  thresholds : Ival.Widen_Hints.t; (* static, computed only once *)
}

module ExpOffset = Datatype.Pair(Exp)(Offset)

module DynamicHintDatatype = Datatype.Make(struct
    include Datatype.Serializable_undefined
    type t = dynamic_hint
    let name = "Widen.DynamicHintDatatype"
    let structural_descr =
      Structural_descr.t_tuple
        [| Base.Hptset.packed_descr;
           ExpOffset.packed_descr;
           Ival.Widen_Hints.packed_descr |]
    let reprs =
    List.map
      (fun wh -> { bases = Base.Hptset.empty;
                   lv = (Exp.dummy, NoOffset);
                   thresholds = wh })
      Ival.Widen_Hints.reprs
  let mem_project = Datatype.never_any_project
  end)

(* use a list of hints instead of multiple entries in a hashtable
   because we need to replace one entry (e.g. one hint for which
   a base was added) but not all, so Hashtbl.replace will not work. *)
module StmtDynamicHint = Datatype.List(DynamicHintDatatype)

(** Stores a mapping from statements to parsed dynamic hint terms.
    Only stores mappings for statements with annotations, to avoid
    wasting memory.
    The dataflow iteration consults this table each time it reaches
    a statement with an annotation. It must quickly evaluate the
    bases related to the annotations, to see if there are new bases
    that should be added to the global widening hints. Therefore,
    we store, for each annotation, the set of bases computed so far,
    plus the thresholds (to avoid recomputing them).
 *)
module Parsed_Dynamic_Hints =
  State_builder.Hashtbl
    (Stmt.Hashtbl)
    (StmtDynamicHint)
    (struct
      let name = "Widen.Parsed_Dynamic_Hints"
      let size = 7
      let dependencies = [ Ast.self; Db.Value.self ]
    end)

let dynamic_bases_of_lval states e offset =
  let lv = (Mem e, offset) in
  List.fold_left (fun acc' state ->
      let location = !Db.Value.lval_to_loc_state state lv in
      Locations.Location_Bits.fold_bases
        (fun base acc'' -> Base.Hptset.add base acc'')
        location.Locations.loc acc'
    ) Base.Hptset.empty states

(* Find syntactically the dynamic hints on [stmt]. *)
let extract_dynamic_hints stmt =
  let source = fst (Stmt.loc stmt) in
  Value_parameters.debug ~source ~dkey
    "computing dynamic hints for statement %d" stmt.sid;
  let wh = Widen_hints_ext.get_stmt_widen_hint_terms stmt in
  let aux l (hlv, threshold_terms) =
    let open Widen_hints_ext in
    match hlv.vars with
    | HintMem (e, offset) ->
      let thresholds = thresholds_of_threshold_terms threshold_terms in
      { bases = Base.Hptset.empty; lv = (e, offset); thresholds; } :: l
    | _-> l
  in
  List.fold_left aux [] wh

let parsed_dynamic_hints = Parsed_Dynamic_Hints.memo extract_dynamic_hints

module Dynamic_Hints =
  State_builder.Ref
    (Widen_type)
    (struct
      let dependencies = [ Ast.self; Db.Value.self ]
      let name = "Widen.Dynamic_Hints"
      let default = Widen_type.default
    end)
let () = Ast.add_monotonic_state Global_Static_Hints.self

(* The contents of this table should always be the join Dynamic_hints
   and Per_Function_Static_Hints, for the functions that have been computed.
   It must be cleared when Dynamic_Hints is changed. *)
module Per_Function_Hints =
  State_builder.Hashtbl
    (Cil_datatype.Fundec.Hashtbl)
    (Widen_type)
    (struct
      let name = "Widen.Per_Function_Hints"
      let size = 97
      let dependencies = [ Ast.self; Dynamic_Hints.self ]
    end)
let () = Ast.add_monotonic_state Per_Function_Hints.self

let extract_per_function_hints fdec =
  let for_fdec =
    try Per_Function_Static_Hints.find fdec
    with Not_found -> assert false
  in
  let dynamic = Dynamic_Hints.get () in
  Widen_type.join for_fdec dynamic

let per_function_hints = Per_Function_Hints.memo extract_per_function_hints

let dynamic_widen_hints_hook (stmt, _callstack, states) =
  if Annotations.has_code_annot stmt then
    let hs = parsed_dynamic_hints stmt in
    if hs <> [] then
      let source = fst (Stmt.loc stmt) in
      let modified, new_hints =
        List.fold_right (fun dhint (_acc_modified, acc_hints as acc) ->
            let old_bases = dhint.bases in
            let exp, offset = dhint.lv in
            let bases = dynamic_bases_of_lval states exp offset in
            let new_bases = Base.Hptset.diff bases old_bases in
            if Base.Hptset.is_empty new_bases then
              acc
            else
              let new_hints =
                Base.Hptset.fold (fun base acc ->
                    Value_parameters.debug ~source ~dkey
                      "adding new base due to dynamic widen hint: %a, %a"
                      Base.pretty base
                      Ival.Widen_Hints.pretty dhint.thresholds;
                    let hint_for_base =
                      Widen_type.num_hints None (Some base) dhint.thresholds
                    in
                    Widen_type.join acc hint_for_base
                  ) new_bases acc_hints
              in
              dhint.bases <- Base.Hptset.union dhint.bases new_bases;
              true, new_hints
          ) hs (false, Widen_type.empty)
      in
      if modified then begin
        Per_Function_Hints.clear ();
        let hints = Widen_type.join (Dynamic_Hints.get ()) new_hints in
        Dynamic_Hints.set hints;
      end

let () =
  Db.Value.Compute_Statement_Callbacks.extend_once dynamic_widen_hints_hook

let getWidenHints (kf:kernel_function) (stmt:stmt) =
  let hints =
    match kf.fundec with
    | Declaration _ -> Widen_type.empty
    | Definition (fdec, _) -> per_function_hints fdec
  in
  Widen_type.hints_from_keys stmt hints

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
