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

let dkey = Options.register_category "value"

module Loop_Max_Iteration =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.Int)
    (struct
      let size = 97
      let name = "Loop_Max_Iteration"
      let dependencies = [Ast.self]
    end)

let add_loop_bound stmt n =
  Options.debug "Adding loop bound of %d to %a" n Cil_datatype.Stmt.pretty stmt;
  try let u = Loop_Max_Iteration.find stmt in
    Loop_Max_Iteration.replace stmt (max u n)
  with Not_found -> Loop_Max_Iteration.replace stmt n

module type BINARY_SEMILATTICE = sig
  include  Dataflows.JOIN_SEMILATTICE
end

let pretty_int = Integer.pretty ~hexa:false

module Binary(* :BINARY_SEMILATTICE *) = struct

  (* Affine expression whose coefficients are the loop counters. The
     last element is a simple integer with no coefficient. *)
  open Cil_types

  type binary =
    | ConstantInt of Integer.t
    | ConstantVar of varinfo
    (* The contents of a variable at the beginning of the loop and a
       fixed offset. *)
    | AffineRef of varinfo * Integer.t
    | Boolean of conds
    | Unknown

  and cond =
    | UnknownCond
    | Lt of bool * binary * binary (* bool if signed. *)
    | Le of bool * binary * binary (* bool if signed. *)
    | Eq of binary * binary
    | Ne of binary * binary

  (* Conjunction of conditions; empty means true. *)
  and conds = cond list

  let rec binary_compare a b = match (a,b) with
    | ConstantInt i1, ConstantInt i2 -> Integer.compare i1 i2
    | ConstantVar v1, ConstantVar v2 -> Cil_datatype.Varinfo.compare v1 v2
    | AffineRef (v1,i1), AffineRef(v2,i2) ->
      let res = Cil_datatype.Varinfo.compare v1 v2 in
      if res == 0 then Integer.compare i1 i2 else res
    | Boolean _c1, Boolean _c2 -> assert false
    | Unknown, Unknown -> 0
    | Unknown, _ -> 1 | _, Unknown -> -1
    | Boolean _, _ -> 1 | _, Boolean _ -> -1
    | AffineRef _, _ -> 1 | _, AffineRef _ -> -1
    | ConstantVar _, _ -> 1 | _, ConstantVar _ -> -1
  (* | ConstantInt _, _ -> 1 | _, ConstantInt _ -> -1 *)

  and cond_compare a b =
    let comp_bin_pair b1 b2 b3 b4 =
      let res1 = binary_compare b1 b3 in
      if res1 == 0 then binary_compare b2 b4 else res1
    in
    match (a,b) with
    | UnknownCond, UnknownCond -> 0
    | Lt(_,b1,b2), Lt(_,b3,b4) -> comp_bin_pair b1 b2 b3 b4
    | Le(_,b1,b2), Le(_,b3,b4) -> comp_bin_pair b1 b2 b3 b4
    | Ne(b1,b2), Ne(b3,b4) -> comp_bin_pair b1 b2 b3 b4
    | Eq(b1,b2), Eq(b3,b4) -> comp_bin_pair b1 b2 b3 b4
    | UnknownCond, _ -> 1 | _, UnknownCond -> -1
    | Lt _, _ -> 1 | _, Lt _ -> -1
    | Le _, _ -> 1 | _, Le _ -> -1
    | Ne _, _ -> 1 | _, Ne _ -> -1
  (* | Eq _, _ -> 1 | _, Eq _ -> -1*)

  module CondSet = Set.Make(struct
      type t = cond
      let compare = cond_compare
    end)


  type t = binary

  let add b1 b2 = match b1, b2 with
    | Unknown, _ | _, Unknown -> Unknown
    | ConstantInt(i1), ConstantInt(i2) -> ConstantInt(Integer.add i1 i2)
    | ConstantInt(i1), AffineRef(v,i2) | AffineRef(v,i2), ConstantInt(i1) ->
      AffineRef(v,Integer.add i1 i2)
    | _ -> Unknown

  let neg = function
    | Unknown -> Unknown
    | ConstantInt(i) -> ConstantInt(Integer.neg i)
    | _ -> Unknown


  let pretty fmt = function
    | ConstantInt(i) -> Format.fprintf fmt "%a" pretty_int i
    | AffineRef(v,i) -> Format.fprintf fmt "ref<%a>+%a"
                          Cil_datatype.Varinfo.pretty v
                          pretty_int i
    | Unknown -> Format.fprintf fmt "unknown"
    | ConstantVar(v) -> Format.fprintf fmt "%a" Cil_datatype.Varinfo.pretty v
    | Boolean _ -> Format.fprintf fmt "bools"


  let pretty_cond fmt = function
    | UnknownCond -> Format.fprintf fmt "<?>"
    | Lt(_,b1,b2) -> Format.fprintf fmt "%a < %a" pretty b1 pretty b2
    | Le(_,b1,b2) -> Format.fprintf fmt "%a <= %a" pretty b1 pretty b2
    | Eq(b1,b2) -> Format.fprintf fmt "%a == %a" pretty b1 pretty b2
    | Ne(b1,b2) -> Format.fprintf fmt "%a != %a" pretty b1 pretty b2
  ;;

  let pretty_conds fmt conds = List.iter (pretty_cond fmt) conds

  let rec transfer_lval lval _load = match lval with
    | (Var vi, NoOffset) -> ConstantVar vi
    | _ -> Unknown

  and transfer_exp exp load = match exp.enode with
    | Const(CInt64(i,_,_)) -> ConstantInt i
    | AddrOf lval -> transfer_lval lval load
    | Lval lval ->
      (match load lval with
       | None -> Unknown
       | Some(v) -> v)
    | BinOp(PlusA,e1,e2,_) -> add (transfer_exp e1 load) (transfer_exp e2 load)
    | BinOp(MinusA,e1,e2,_) ->
      add (transfer_exp e1 load) (neg (transfer_exp e2 load))
    | CastE(_,e) -> transfer_exp e load
    (* | BinOp((PlusPI|IndexPI|MinusA),_,_,_) -> assert false *)
    (* | BinOp(_,_,_,_) -> Unknown *)
    | _ ->
      (match Cil.constFoldToInt ~machdep:true exp with
       | None -> Unknown
       | Some(i) -> ConstantInt i)
  ;;

  let transfer_exp exp load =
    let res = transfer_exp exp load in
    Options.debug "transfer exp %a: %a" Cil_datatype.Exp.pretty exp pretty res;
    res
  ;;



  let not_cond = function
    | UnknownCond -> UnknownCond
    | Lt(signed,b1,b2) -> Lt(signed,b2,b1)
    | Le(signed,b1,b2) -> Le(signed,b2,b1)
    | Eq(b1,b2) -> Ne(b1,b2)
    | Ne(b1,b2) -> Eq(b1,b2)

  let transfer_cond exp load =
    match exp.enode with
    | Cil_types.BinOp(binop,e1,e2,_) ->
      let b1 = transfer_exp e1 load in
      let b2 = transfer_exp e2 load in
      (match binop with
       | Cil_types.Lt -> Lt(true,b1,b2)
       | Cil_types.Le -> Le(true,b1,b2)
       | Cil_types.Gt -> Lt(true,b2,b1)
       | Cil_types.Ge -> Le(true,b2,b1)
       | Cil_types.Eq -> Eq(b2,b1)
       | Cil_types.Ne -> Ne(b2,b1)
       | _ -> UnknownCond
      )
    | _ -> UnknownCond
  ;;

  let transfer_cond exp load =
    let res = transfer_cond exp load in
    Options.debug "transfer cond: %a" pretty_cond res;
    res
  ;;


  let join_conds conds1 conds2 =
    let cond_set1 = List.fold_right CondSet.add conds1 CondSet.empty in
    let cond_set2 = List.fold_right CondSet.add conds2 CondSet.empty in
    let inter = CondSet.inter cond_set2 cond_set1 in
    let conds = UnknownCond::(CondSet.elements inter) in
    conds
  ;;



  let join a b = match (a,b) with
    | ConstantInt(ia), ConstantInt(ib) when Integer.equal ia ib  -> a
    | ConstantVar(va), ConstantVar(vb)
      when Cil_datatype.Varinfo.equal va vb  -> a
    | AffineRef(va,ia), AffineRef(vb,ib)
      when Cil_datatype.Varinfo.equal va vb && Integer.equal ia ib -> a
    | Boolean(condsa), Boolean(condsb) -> Boolean(join_conds condsa condsb)
    | Unknown, Unknown -> Unknown
    | _,_ -> Unknown

end




module Store(* (B:sig *)
  (*   type t *)
  (*   open Cil_types *)
  (*   val bottom: t *)
  (*   val pretty: Format.formatter -> t -> unit *)
  (*   val transfer_exp : exp -> (lval -> t option) -> t *)
  (*   val transfer_lval : lval -> (lval -> t option) -> t *)
  (*   (\* include BINARY_SEMILATTICE *\) *)
  (*   (\* type address = Cil_types.varinfo *\) *)
  (*   (\* val get_address: t -> address *\) *)
  (* end *)
  (* ) *) = struct

  module B = Binary


  (* Note: We could do an unsound, heuristical analysis by using a map
     from lvalues instead, and completely ignoring aliasing. *)

  (* A map of local variables whose address is never taken. *)
  module Varinfo = Cil_datatype.Varinfo;;

  (* The map, and the condition that leads to this point, and the
     destination of edges. *)
  type t = (B.t Varinfo.Map.t * B.conds * Cil_types.stmt)

  let pretty fmt (m,conds) =
    Format.fprintf fmt "[@[mem={ @[";
    Varinfo.Map.iter (fun k v ->
        Format.fprintf fmt "%a -> %a@ " Varinfo.pretty k B.pretty v) m;
    Format.fprintf fmt "@]}@ conds=@ (%a)@]]" B.pretty_conds conds
  ;;

  let bottom = (Varinfo.Map.empty,[],Cil.dummyStmt)
  let init stmt = (Varinfo.Map.empty,[],stmt)

  let load map = let open Cil_types in function
      | (Var(vi),NoOffset) when not vi.vaddrof ->
        Some(
          try Varinfo.Map.find vi map
          with Not_found -> B.AffineRef(vi,Integer.zero))
      | _ -> None

  let join2_stmts stmt1 stmt2 =
    (* Cil.dummyStmt is bottom for statements. *)
    if Cil_datatype.Stmt.equal stmt1 stmt2
    then stmt1
    else if Cil_datatype.Stmt.equal stmt1 Cil.dummyStmt
    then stmt2
    else if Cil_datatype.Stmt.equal stmt2 Cil.dummyStmt
    then stmt1
    else assert false
  ;;

  let pretty_increment fmt increment =
    if Integer.(equal increment one) then Format.fprintf fmt "++"
    else if Integer.(equal increment minus_one) then Format.fprintf fmt "--"
    else if Integer.(gt increment zero) then
      Format.fprintf fmt " += %a" (Integer.pretty ~hexa:false) increment
    else if Integer.(lt increment zero) then
      Format.fprintf fmt " -= %a" (Integer.pretty ~hexa:false)
        (Integer.neg increment)
    else assert false (* should never happen *)

  let do_instr instr (value,conds) =
    let open Cil_types in
    match instr with
    | Set((Var(vi),NoOffset),exp,_) when not vi.vaddrof
      -> (Varinfo.Map.add vi (B.transfer_exp exp (load value)) value, conds)
    | Set _ -> (value,conds)
    (* | Set((Var(vi),NoOffset),exp,_) -> assert false *)
    | Local_init (vi, AssignInit (SingleInit e), _) ->
      (Varinfo.Map.add vi (B.transfer_exp e (load value)) value, conds)
    | Local_init (_, AssignInit (CompoundInit _), _) -> (value,conds)
    | Local_init (_,ConsInit _,_) -> (value,conds)
    | Call _ -> (value,conds)
    | Asm _ ->  (value,conds)
    | Code_annot _ -> (value,conds)
    | Skip _ -> (value,conds)

  let do_instr instr value =
    let output = do_instr instr value in
    Options.debug "Input %a output %a" pretty value pretty output;
    output
  ;;

  let do_guard _stmt exp (mem,conds) =
    let cond = B.transfer_cond exp (load mem) in
    let not_cond = B.not_cond cond in
    ((mem,cond::conds),
     (mem,not_cond::conds));;

  let compile_node stmt (mem,conds,stmt2) =
    let stmt = join2_stmts stmt stmt2 in
    let value = (mem,conds) in
    let open Cil_types in
    let map_on_all_succs (mem,conds) =
      List.map (fun x -> (Region_analysis.Edge(stmt,x),(mem,conds,x))) stmt.succs in
    match stmt.skind with
    | Instr(i) -> map_on_all_succs (do_instr i (mem,conds))
    | Return _ ->
      [Region_analysis.Exit stmt, (mem,conds,Cil.dummyStmt)]
    | Loop _ | Goto _ | Break _ | Continue _ | Block _ | UnspecifiedSequence _ ->
      map_on_all_succs value
    | If _ ->
      let result = Dataflows.transfer_if_from_guard do_guard stmt value in
      List.map (fun (succ,(mem,cond)) ->
          (Region_analysis.Edge(stmt,succ),(mem,cond,succ))) result
    | Switch _ ->
      let result = Dataflows.transfer_switch_from_guard do_guard stmt value in
      List.map (fun (succ,(mem,cond)) ->
          (Region_analysis.Edge(stmt,succ),(mem,cond,succ))) result
    | Throw _ | TryCatch _ | TryExcept _ | TryFinally _ ->
      Options.abort "unsupported exception-related statement: %a"
        Printer.pp_stmt stmt

  let value_min_max stmt vi =
    if (Db.Value.is_computed ()) then
      begin
        Options.feedback ~dkey ~once:true
          "value analysis computed, trying results";
        if Db.Value.is_reachable_stmt stmt then
          let state = Db.Value.get_stmt_state stmt in
          try
            let loc = Locations.loc_of_varinfo vi in
            let v = Db.Value.find state loc in
            let ival = Cvalue.V.project_ival v in
            let omin, omax = Ival.min_and_max ival in
            omin, omax
          with
          | Not_found ->
            Options.feedback ~dkey "value_min_max: not found: %a@.\
                                    function: %a, stmt: %a"
              Printer.pp_varinfo vi Kernel_function.pretty
              (Kernel_function.find_englobing_kf stmt) Printer.pp_stmt stmt;
            None, None
          | Cvalue.V.Not_based_on_null ->
            Options.feedback ~dkey "value_min_max: not based on null: %a@.\
                                    function: %a, stmt: %a"
              Printer.pp_varinfo vi Kernel_function.pretty
              (Kernel_function.find_englobing_kf stmt) Printer.pp_stmt stmt;
            None, None
        else
          begin
            Options.feedback ~dkey "skipping unreachable stmt (function: %a)"
              Kernel_function.pretty (Kernel_function.find_englobing_kf stmt);
            None, None
          end
      end
    else
      begin
        Options.feedback ~dkey ~once:true
          "value analysis NOT computed, loop analysis will not use it";
        None, None
      end

  let mu (f:(t -> t)) (value,conds,stmt) =
    Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
    let (result,final_conds,_) = f (init stmt) in

    (* Induction variables is a map from each Varinfo to its increment. *)
    let induction_variables = Varinfo.Map.fold  (fun key bin acc -> match bin with
        | B.AffineRef(vi,offset)
          when not (Integer.is_zero offset) && Varinfo.equal vi key ->
          Varinfo.Map.add key offset acc
        | _ -> acc) result Varinfo.Map.empty in

    (* The result after the loop: replace everything that changed by
       unknown (i.e. keep variables that did not change).  TODO: When
       we know the number of iterations, replace by the exact
       value. *)
    let new_ = Varinfo.Map.fold (fun key bin acc -> match bin with
        | B.AffineRef(vi,offset)
          when Integer.is_zero offset && Varinfo.equal vi key -> acc
        | _ -> Varinfo.Map.add key B.Unknown acc
      ) result value in

    let success = ref false in

    (* Now fill Loop_Max_Iteration for the kernel function. *)
    (* smaller = true => the test is "vi < bound";
       smaller = false => the test is "vi > bound". *)
    let maybe_insert vi smaller bound offset binop =
      try
        let initial =
          match Varinfo.Map.find vi value with
          | B.ConstantInt i -> i
          | B.AffineRef(vi,offset) ->
            begin
              match value_min_max stmt vi with
              | Some imin, _ when smaller -> Integer.add imin offset
              | _, Some imax when not smaller -> Integer.add imax offset
              | _, _ -> raise Not_found
            end
          | _ -> raise Not_found (* TODO: handle comparison between pointers *)
        in
        let increment = Varinfo.Map.find vi induction_variables in
        Options.debug "maybe_insert: function %a, found var %a, smaller: %b, \
                       initial %a, increment %a, bound %a, offset %a, binop '%a'"
          Kernel_function.pretty (Kernel_function.find_englobing_kf stmt)
          Printer.pp_varinfo vi smaller pretty_int initial pretty_int increment
          pretty_int bound pretty_int offset Printer.pp_binop binop;
        let bound = Integer.sub bound initial in
        let bound_offset =
          if smaller then Integer.sub bound offset
          else Integer.add bound offset
        in
        (* remainder is used for two purposes:
           1. in the case of '!=' loops, to warn if the termination condition
              may be missed;
           2. in '<=' and '>=' loops, to adjust for the last iteration *)
        let divident = Integer.sub bound offset in
        let remainder = Integer.rem divident increment in
        (* check if induction variable may miss termination condition *)
        if binop = Cil_types.Ne && not Integer.(equal remainder zero) then
          Options.warning ~current:true
            "termination condition may not be reached (infinite loop?)@;\
             loop amounts to: for (%a = 0; %a != %a; %a%a)"
            Printer.pp_varinfo vi
            Printer.pp_varinfo vi pretty_int divident
            Printer.pp_varinfo vi pretty_increment increment
        else
          try
            let value = (Integer.to_int (Integer.c_div bound_offset increment)) in
            let adjusted_value =
              if (binop = Cil_types.Le && Integer.(equal remainder zero))
              || (not Integer.(equal remainder zero))
              then value + 1
              else value
            in
            if adjusted_value >= 0 then
              begin
                success := true;
                add_loop_bound stmt adjusted_value
              end
          with Failure _ -> (* overflow in Integer.to_int *)
            ()
      (* TODO: check if this is useful and does not cause false alarms
         else
         if Kernel.UnsignedOverflow.get() then
          Options.warning ~current:true
            "possibly infinite loop, or loop which relies on unsigned overflow"
      *)
      with Not_found -> ()
    in
    List.iter (function
        | B.Lt(_,B.AffineRef(vi,offset),B.ConstantInt bound) ->
          maybe_insert vi true bound offset Cil_types.Lt
        | B.Le(_,B.AffineRef(vi,offset),B.ConstantInt bound) ->
          maybe_insert vi true bound offset Cil_types.Le
        | B.Lt(_,B.ConstantInt bound,B.AffineRef(vi,offset)) ->
          maybe_insert vi false bound offset Cil_types.Lt
        | B.Le(_,B.ConstantInt bound,B.AffineRef(vi,offset)) ->
          maybe_insert vi false bound offset Cil_types.Le
        | B.Ne(B.ConstantInt bound,B.AffineRef(vi, offset))
        | B.Ne(B.AffineRef(vi, offset),B.ConstantInt bound) ->
          begin
            try
              let increment = Varinfo.Map.find vi induction_variables in
              assert (not (Integer.equal increment Integer.zero));
              if Integer.gt increment Integer.zero then
                maybe_insert vi true bound offset Cil_types.Ne
              else
                maybe_insert vi false bound offset Cil_types.Ne
            with Not_found -> ()
          end
        | c ->
          if (Db.Value.is_computed ()) then
            begin
              let min_max_int = value_min_max stmt in
              match c with
              | B.Lt(_,B.AffineRef(vi,offset),B.AffineRef(vi', offset')) ->
                begin
                  match min_max_int vi, min_max_int vi' with
                  | (Some min_bound, _), (_, Some max_bound') ->
                    let b1 = Integer.add min_bound offset in
                    maybe_insert vi' false b1 offset' Cil_types.Lt;
                    let b2 = Integer.add max_bound' offset' in
                    maybe_insert vi true b2 offset Cil_types.Lt
                  | _, _ -> ()
                end
              | B.Le(_,B.AffineRef(vi,offset),B.AffineRef(vi', offset')) ->
                begin
                  match min_max_int vi, min_max_int vi' with
                  | (Some min_bound, _), (_, Some max_bound') ->
                    let b1 = Integer.add min_bound offset in
                    maybe_insert vi' false b1 offset' Cil_types.Le;
                    let b2 = Integer.add max_bound' offset' in
                    maybe_insert vi true b2 offset Cil_types.Le
                  | a, b ->
                    Options.debug "failed to get min/max bounds?@.\
                                   -   get_min_max_int_for_vi(%a)=%a@.\
                                   -   get_min_max_int_for_vi(%a)=%a"
                      Printer.pp_varinfo vi
                      (Pretty_utils.pp_pair
                         (Pretty_utils.pp_opt pretty_int)
                         (Pretty_utils.pp_opt pretty_int)) a
                      Printer.pp_varinfo vi'
                      (Pretty_utils.pp_pair
                         (Pretty_utils.pp_opt pretty_int)
                         (Pretty_utils.pp_opt pretty_int)) b
                end
              | B.Ne(B.AffineRef(vi, offset),B.AffineRef(vi', offset')) ->
                begin
                  try
                    let increment = Varinfo.Map.find vi induction_variables in
                    assert (not (Integer.equal increment Integer.zero));
                    if Integer.gt increment Integer.zero then
                      match min_max_int vi' with
                      | (_, Some max_bound') ->
                        let b = Integer.add max_bound' offset' in
                        maybe_insert vi true b offset Cil_types.Lt
                      | _ -> ()
                    else
                      match min_max_int vi' with
                      | (Some min_bound', _) ->
                        let b = Integer.add min_bound' offset in
                        maybe_insert vi false b offset' Cil_types.Lt
                      | _ -> ()
                  with Not_found -> (* try other variable as increment *)
                  try
                    let increment = Varinfo.Map.find vi' induction_variables in
                    assert (not (Integer.equal increment Integer.zero));
                    if Integer.gt increment Integer.zero then
                      match min_max_int vi with
                      | (_, Some max_bound) ->
                        let b = Integer.add max_bound offset in
                        maybe_insert vi' true b offset' Cil_types.Lt;
                      | _ -> ()
                    else
                      match min_max_int vi with
                      | (Some min_bound, _) ->
                        let b = Integer.add min_bound offset' in
                        maybe_insert vi' false b offset Cil_types.Lt
                      | _ -> ()
                  with Not_found -> ()
                end
              | _ -> Options.debug
                       "cannot use value, pattern not matched, c: %a@."
                       B.pretty_cond c
            end
          else
            () (* no value => cannot infer anything *)
      ) final_conds;

    (* TODO: Use this table in a second pass, for the slevel analysis. *)
    if not !success then
      Options.debug "no success %a init %a body %a result %a"
        Cil_datatype.Stmt.pretty stmt pretty (value,conds)
        pretty (result,final_conds) pretty (new_,conds)
    else
      Options.debug "success %a init %a body %a result %a"
        Cil_datatype.Stmt.pretty stmt pretty (value,conds)
        pretty (result,final_conds) pretty (new_,conds);
    (new_,conds,stmt)
  ;;

  let join2_mem m1 m2 =
    Varinfo.Map.merge (fun vi b1 b2 -> match (b1,b2) with
        | Some b1, Some b2 -> Some(B.join b1 b2)
        | Some b, None | None, Some b ->
          Some(B.join (B.AffineRef(vi,Integer.zero)) b)
        | None,None -> assert false) m1 m2
  ;;


  let join2 (mem1,conds1,stmt1) (mem2,conds2,stmt2) =
    let stmt = join2_stmts stmt1 stmt2 in
    let conds = B.join_conds conds1 conds2 in
    let mem = join2_mem mem1 mem2 in
    (* TODO: If a condition is in both lists, retrieve it. To that
       end: use a set of conditions?  And we do not need unknown; we
       have a safe approximation of the sufficient conditions to exit
       the loop. *)
    (mem,conds,stmt)
  ;;


  let join = function
    | [] -> bottom
    | [x]  -> x
    | a::b -> List.fold_left join2 a b




  type abstract_value = t
end


module Generic = struct
  include Store
end;;


let analyze kf =
  Options.debug "loop analysis of function %a" Kernel_function.pretty kf;
  let module Specific = struct
    let kf = kf
    include Generic
  end in
  let module Node = Region_analysis_stmt.MakeNode(Specific) in
  let module Result = Region_analysis.Make(Node) in
  let after = Result.after in
  let _dict = after (Generic.init (Kernel_function.find_first_stmt kf)) in
  ()
;;

let get_bounds stmt =
  try
    Some (Loop_Max_Iteration.find stmt)
  with Not_found -> None

let fold_bounds f acc =
  Loop_Max_Iteration.fold_sorted ~cmp:Cil_datatype.Stmt.compare f acc
