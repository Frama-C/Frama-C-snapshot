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

open Cil_types
open Cil_datatype


class widen_visitor kf init_widen_hints init_enclosing_loops = object(self)
  inherit Visitor.frama_c_inplace

  val widen_hints = init_widen_hints
  val enclosing_loops = init_enclosing_loops

  (* Caution: currently, [stmt] is always [None]. Because our dataflow does not
     stabilize inner loop before the outer ones, we sometimes end up widening
     an inner variable inside an outer loop. Hence, we need to have the inner
     widening hints in the outer loops. To do so, the simplest is to avoid
     specifying statements altogether. This may be inefficient for codes that
     reuse loop indexes... *)
  method private add_num_hints ?stmt ?base hints =
    widen_hints := Widen_type.add_num_hints stmt base hints !widen_hints

  method private add_var_hints ~stmt hints = 
    widen_hints := Widen_type.add_var_hints stmt hints !widen_hints

  method private process_loop_pragma stmt p = match p with
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
      Kernel.warning ~once:true ~current:true
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
    | (vars, hints, []) ->
      (* the annotation is empty or contains only variables *)
      if vars = [] then
        self#add_num_hints ?stmt:None hints
      else
        List.iter (fun base -> self#add_num_hints ?stmt:None ~base hints) vars
    | _ ->
      Kernel.warning ~once:true ~current:true
        "could not interpret loop pragma relative to widening hint"
  end
  | _ -> ()

  method! vstmt (s:stmt) =
    match s.skind with
    | Loop (_, bl, _, _, _) -> begin
      (* ZZZ: this code does not handle loops that are created using gotos. We
         could improve this by finding the relevants statements using a
         traversal of the CFG. *)
      let annot = Annotations.code_annot s in
      let pragmas = Logic_utils.extract_loop_pragma annot in
      List.iter (self#process_loop_pragma s) pragmas;
      let new_loop_info = s :: enclosing_loops in
      let visitor = new widen_visitor kf widen_hints new_loop_info in
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
        let hints = Ival.Widen_Hints.singleton Integer.zero in
        let base = Base.of_varinfo varinfo in
        self#add_num_hints ~base hints;
        Cil.DoChildren
      | _ -> Cil.DoChildren
    and comparison_visit add1 add2 e1 e2 =
      let add base set =
        let hints =
          List.fold_right Ival.Widen_Hints.add set Ival.Widen_Hints.empty
        in
        self#add_num_hints ~base hints
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
      let hints = Ival.Widen_Hints.of_list [bound1; bound2] in
      self#add_num_hints ~base:(Base.of_varinfo vidx) hints
    in
    (* Find insided [idx] a variable on which we will add hints. [shift] is an
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

let compute_widen_hints kf default_widen_hints =
  let widen_hints =
    begin
      match kf.fundec with
        | Declaration _ -> default_widen_hints
        | Definition (fd,_) ->
            begin
              let widen_hints = ref default_widen_hints in
	      let visitor = new widen_visitor kf widen_hints [] in
	      ignore (Visitor.visitFramacFunction visitor fd);
              !widen_hints
            end
    end
  in widen_hints

module Hints =
  Kernel_function.Make_Table
    (Widen_type)
    (struct
       let name = "Widen.Hints"
       let size = 97
       let dependencies = [ Ast.self ]
     end)
let () = Ast.add_monotonic_state Hints.self

let getWidenHints (kf:kernel_function) (s:stmt) =
  let widen_hints_map =
    Hints.memo (fun kf -> compute_widen_hints kf (Widen_type.default ())) kf
  in
  Widen_type.hints_from_keys s widen_hints_map

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
