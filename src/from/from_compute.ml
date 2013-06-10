(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
open Cil
open Cil_datatype
open Db
open Abstract_interp
open Locations

exception Call_did_not_take_place

module type Froms_To_Use_Sig = sig
  val get : kernel_function -> kinstr -> Function_Froms.t
end

module type Values_To_Use_Sig = sig
  val lval_to_loc_with_deps :
    stmt ->
    deps:Locations.Zone.t ->
    Cil_types.lval ->
    Locations.Zone.t * Locations.location

  val expr_to_kernel_function :
    stmt ->
    deps:Locations.Zone.t option ->
    Cil_types.exp -> Locations.Zone.t * Kernel_function.Hptset.t

  val get_stmt_state : stmt -> Db.Value.state
  val access_expr : Cil_types.stmt -> Cil_types.exp -> Db.Value.t
end

module type Recording_Sig = sig
  val accept_base_in_lmap : kernel_function -> Base.t -> bool
  val final_cleanup: kernel_function -> Function_Froms.t -> Function_Froms.t
  val record_kf : kernel_function -> Function_Froms.t -> unit
    (* function to call at the end of the treatment of a function *)
end

let rec find_deps_no_transitivity state expr =
  (* The value of the expression [expr], just before executing the statement
     [instr], is a function of the values of the returned zones. *)
  match expr.enode with
    | Info (e, _) -> find_deps_no_transitivity state e
    | AlignOfE _| AlignOf _| SizeOfStr _ |SizeOfE _| SizeOf _ | Const _
        -> Zone.bottom
    | AddrOf lv  | StartOf lv ->
        let deps, _ = !Db.Value.lval_to_loc_with_deps_state
          state
          ~deps:Zone.bottom
          lv
        in deps
    | CastE (_, e)|UnOp (_, e, _) ->
        find_deps_no_transitivity state e
    | BinOp (_, e1, e2, _) ->
        Zone.join
          (find_deps_no_transitivity state e1)
          (find_deps_no_transitivity state e2)
    | Lval v ->
        find_deps_lval_no_transitivity state v

and find_deps_offset_no_transitivity state o =
  match o with
    | NoOffset -> Zone.bottom
    | Field (_,o) -> find_deps_offset_no_transitivity state o
    | Index (e,o) ->
        Zone.join
          (find_deps_no_transitivity state e)
          (find_deps_offset_no_transitivity state o)

and find_deps_lval_no_transitivity state lv =
  let deps, loc =
    !Db.Value.lval_to_loc_with_deps_state state ~deps:Zone.bottom lv
  in
  let direct_deps = enumerate_valid_bits ~for_writing:false loc in
  let result = Zone.join deps direct_deps in
  From_parameters.debug "find_deps_lval_no_trs:@\n deps:%a@\n direct_deps:%a"
    Zone.pretty deps Zone.pretty direct_deps;
  result



let compute_using_prototype_for_state state kf =
  let varinfo = Kernel_function.get_vi kf in
  let behaviors = !Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let return_deps,deps =
    match assigns with
      | WritesAny ->
          Lmap_bitwise.From_Model.LOffset.degenerate Zone.top,
          Lmap_bitwise.From_Model.top
      | Writes assigns ->
          let (rt_typ,_,_,_) = splitFunctionTypeVI varinfo in
          let input_zone out ins =
            (* Technically out is unused, but there is a signature problem *)
            !Db.Value.assigns_inputs_to_zone state (Writes [out, ins])
          in
          let treat_assign acc (out, ins) =
            try
              let output_locs, _deps =
                !Properties.Interp.loc_to_locs ~result:None state out.it_content
              in
              let input_zone = input_zone out ins in
              let treat_one_output acc out_loc =
		let exact = Location_Bits.cardinal_zero_or_one out_loc.loc in
                let output_zone =
                  Locations.enumerate_valid_bits ~for_writing:true out_loc
                in
                Lmap_bitwise.From_Model.add_binding ~exact
                  acc output_zone input_zone
              in
              List.fold_left treat_one_output acc output_locs
            with Invalid_argument "not an lvalue" ->
              From_parameters.result
                ~once:true ~current:true "Unable to extract assigns in %a"
                Kernel_function.pretty kf;
              acc
          in
          let treat_ret_assign acc (out,ins) =
            try
              let coffs =
                !Properties.Interp.loc_to_offset ~result:None out.it_content
              in
              List.fold_left
                (fun acc coff ->
                  let (base,width) = bitsOffset rt_typ coff in
                  Lmap_bitwise.From_Model.LOffset.add_iset
                    ~exact:true
                    (Lattice_Interval_Set.Int_Intervals.from_ival_size
                       (Ival.of_int base)
                       (Int_Base.inject (Int.of_int width)))
                    (input_zone out ins) acc)
                acc coffs
            with Invalid_argument "not an lvalue" | SizeOfError _ ->
              From_parameters.result  ~once:true ~current:true
                "Unable to extract a proper offset. \
                 Using FROM for the whole \\result";
              Lmap_bitwise.From_Model.LOffset.add_iset ~exact:false
                (Lattice_Interval_Set.Int_Intervals.from_ival_size
                   (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                (input_zone out ins) acc
          in
          let return_assigns, other_assigns =
            List.fold_left
              (fun (ra,oa) (loc,_ as a) ->
                if Logic_utils.is_result loc.it_content
                then a::ra,oa else ra,a::oa)
              ([],[]) assigns
          in
          let return_assigns =
            match return_assigns with
              | [] when Cil.isVoidType rt_typ ->
                  Lmap_bitwise.From_Model.LOffset.empty
              | [] -> (* \from unspecified. *)
                  Lmap_bitwise.From_Model.LOffset.add_iset ~exact:true
                    (Lattice_Interval_Set.Int_Intervals.from_ival_size
                       (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                    Zone.top
                    Lmap_bitwise.From_Model.LOffset.empty
              | _ ->
                  List.fold_left treat_ret_assign
                    Lmap_bitwise.From_Model.LOffset.empty return_assigns
          in
          return_assigns,
          List.fold_left
            treat_assign Lmap_bitwise.From_Model.empty other_assigns
  in
  { deps_return = return_deps; Function_Froms.deps_table = deps }

module Make
  (Values_To_Use:Values_To_Use_Sig)
  (Froms_To_Use: Froms_To_Use_Sig)
  (Recording_To_Do: Recording_Sig) =
struct
  type t' =
      { additional_deps_table : Zone.t Stmt.Map.t;
        (** Additional dependencies to add to all modified variables.
            Example: variables in the condition of an IF. *)
        additional_deps : Zone.t;
        (** Union of the sets in StmtMap.t *)
        deps_table : Lmap_bitwise.From_Model.t
          (** dependency table *)
      }

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  let find_deps stmt deps_tbl expr =
    let state = Values_To_Use.get_stmt_state stmt in
    let deps_no_trans = find_deps_no_transitivity state expr in
    !Db.From.access deps_no_trans deps_tbl

  module Computer(REACH:sig
                    val stmt_can_reach : stmt -> stmt -> bool
                    val blocks_closed_by_edge: stmt -> stmt -> block list
                  end) =
  struct

    let empty_from =
      { additional_deps_table = Stmt.Map.empty;
        additional_deps = Zone.bottom;
        deps_table = Lmap_bitwise.From_Model.empty }

    let bottom_from =
      { additional_deps_table = Stmt.Map.empty;
        additional_deps = Zone.bottom;
        deps_table = Lmap_bitwise.From_Model.bottom }

    let name = "from"

    let debug = ref false

    let stmt_can_reach = REACH.stmt_can_reach

    type t = t'

    module StmtStartData =
      Dataflow.StartData(struct type t = t' let size = 107 end)

    let callwise_states_with_formals = Stmt.Hashtbl.create 7

    type substit = 
        Froms of Zone.t
      (* VP: Unused constructor *)
      (* | Lvalue of Lmap_bitwise.From_Model.LOffset.t *)

    let cached_substitute call_site_froms extra_loc =
      let f k intervs =
        Lmap_bitwise.From_Model.find
          call_site_froms
          (Zone.inject k intervs)
      in
      let joiner = Zone.join in
      let projection base =
        match Base.validity base with
          | Base.Invalid -> Lattice_Interval_Set.Int_Intervals.bottom
          | Base.Periodic (min_valid, max_valid, _)
          | Base.Known (min_valid,max_valid)
          | Base.Unknown (min_valid,_,max_valid)->
              Lattice_Interval_Set.Int_Intervals.inject_bounds
                min_valid max_valid
      in
      let zone_substitution =
        Zone.cached_fold ~cache:("from substitution", 331) ~temporary:true
          ~f ~joiner ~empty:Zone.bottom ~projection
      in
      let zone_substitution x =
        try zone_substitution x
        with Zone.Error_Top -> Zone.top
      in
      fun z -> Zone.join extra_loc (zone_substitution z)


    let display_one_from fmt v =
      Lmap_bitwise.From_Model.pretty fmt v.deps_table;
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        (let module M = Stmt.Map.Make(Zone) in M.pretty)
        v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Zone.pretty
        v.additional_deps

    let copy (d: t) = d

    let pretty fmt (v: t) =
      display_one_from fmt v

    let eliminate_additional table s =
      let current_function = Stack.top call_stack in
      (* Eliminate additional variables originating
         from a branch closing at this statement. *)
      Stmt.Map.fold
        (fun k v (acc_set,acc_map,nb) ->
          if !Postdominators.is_postdominator
            current_function
            ~opening:k
            ~closing:s
          then acc_set,acc_map,nb
          else
            (Zone.join v acc_set),
            (Stmt.Map.add k v acc_map),nb+1
        )
        table
        (Zone.bottom, Stmt.Map.empty, 0)

    let computeFirstPredecessor (s: stmt) data =
      let new_additional_deps, new_additional_deps_table, _ =
        eliminate_additional data.additional_deps_table s
      in
      let data =
        {data with
          additional_deps = new_additional_deps;
          additional_deps_table = new_additional_deps_table}
      in
      match s.skind with
        | Switch (exp,_,_,_)
        | If (exp,_,_,_) ->
          let additional_vars = find_deps s data.deps_table exp in
          {data with
            additional_deps_table =
              Stmt.Map.add
                s
                additional_vars
                data.additional_deps_table;
            additional_deps =
              Zone.join
                additional_vars
                data.additional_deps }
        | _ -> data

    let combinePredecessors (s: stmt)
        ~old:({deps_table = old_table} as old)
             ({deps_table = new_table } as new_) =
      let new_ = computeFirstPredecessor s new_ in
      let changed = ref false in
      let merged =
        Stmt.Map.fold
          (fun k v acc ->
            try
              let current_val = Stmt.Map.find k acc.additional_deps_table in
              if Zone.is_included v current_val then acc
              else begin
                changed := true;
                {acc with
                  additional_deps_table =
                    Stmt.Map.add
                      k
                      (Zone.join current_val v)
                      acc.additional_deps_table;
                  additional_deps = Zone.join v acc.additional_deps}
              end
            with Not_found ->
              changed := true;
              {acc with
                additional_deps_table =
                  Stmt.Map.add
                    k
                    v
                    acc.additional_deps_table;
                additional_deps = Zone.join v acc.additional_deps
              }
          )
          new_.additional_deps_table
          old
      in
      let result = Lmap_bitwise.From_Model.join old_table new_table in
      if (not !changed) && Lmap_bitwise.From_Model.is_included result old_table
      then None
      else Some ({merged with deps_table = result })

    let resolv_func_vinfo ?deps stmt funcexp =
      Values_To_Use.expr_to_kernel_function ?deps stmt funcexp

    let doInstr stmt (i: instr) (d: t) =
      !Db.progress ();
      let add_with_additional_var lv v d =
        let deps, target =
          (* The modified location is [target],
             whose address is computed from [deps]. *)
          Values_To_Use.lval_to_loc_with_deps
            ~deps:Zone.bottom
            stmt
            lv
        in
        let deps = Zone.join
          v
          (Lmap_bitwise.From_Model.find d.deps_table deps)
        in
        let r = !Db.From.update
          target
          (Zone.join
             d.additional_deps
             deps)
          d.deps_table
        in
        {d with deps_table=r; }
      in
      match i with
        | Set (lv, exp, _) ->
          Dataflow.Post
            (fun state ->
              let comp_vars = find_deps stmt state.deps_table exp in
              let result = add_with_additional_var lv comp_vars state in
              result
            )
        | Call (lvaloption,funcexp,argl,_) ->
          Dataflow.Post
            (fun state ->
              !Db.progress ();
              let funcexp_deps, called_vinfos =
                resolv_func_vinfo
                  ~deps:Zone.bottom
                  stmt
                  funcexp
              in
              let funcexp_deps =
                (* dependencies for the evaluation of [funcexp] *)
                !Db.From.access funcexp_deps state.deps_table in
              let additional_deps =
                Zone.join d.additional_deps funcexp_deps
              in
              let args_froms =
                List.map
                  (fun arg ->
                    match arg with
                     (* TODO : optimize the dependencies on subfields
                      | Lval lv ->
                         Lvalue
                          (From_Model.LBase.find
                           (Interp_loc.lval_to_loc_with_deps kinstr lv)) *)
                      | _ ->
                        Froms (find_deps stmt d.deps_table arg))
                  argl
              in
              let states_with_formals = ref [] in
              let do_on kernel_function =
                let called_vinfo = Kernel_function.get_vi kernel_function in
                if Ast_info.is_cea_function called_vinfo.vname then
                  state
                else
                  let { Function_Froms.deps_return = return_from;
                        deps_table = called_func_froms } =
                    Froms_To_Use.get kernel_function (Kstmt stmt)
                  in
                  if Lmap_bitwise.From_Model.is_bottom called_func_froms then
                    bottom_from
                  else
                  let formal_args =
                    Kernel_function.get_formals kernel_function
                  in
                  let state_with_formals = ref state.deps_table in
                  begin try
                   List.iter2
                     (fun vi from ->
                       match from with
                         | Froms from ->
                             let zvi = Locations.zone_of_varinfo vi in
                             state_with_formals :=
                               Lmap_bitwise.From_Model.add_binding
                               ~exact:true
                               !state_with_formals
                               zvi
                               from
                         (*| Lvalue _ -> assert false *))
                     formal_args
                     args_froms;
                    with Invalid_argument "List.iter2" ->
                      From_parameters.warning ~once:true ~current:true
                        "variadic call detected. Using only %d argument(s)."
                        (min
                           (List.length formal_args)
                           (List.length args_froms))
                  end;

                  if not (Db.From.Record_From_Callbacks.is_empty ())
                  then
                    states_with_formals :=
                      (kernel_function, !state_with_formals) ::
                      !states_with_formals;
                  let substitute =
                    cached_substitute
                      !state_with_formals
                      additional_deps
                  in
                  let new_state =
                    (* From state just after the call,
                       but before the result assigment *)
                    {state with
                      deps_table =
                        Lmap_bitwise.From_Model.map_and_merge substitute
                          called_func_froms
                          state.deps_table}
                  in
                  (* Treatement for the possible assignement
                     of the call result *)
                  (match lvaloption with
                  | None -> new_state
                  | Some lv ->
                    let first = ref true in
                    (try
                       Lmap_bitwise.From_Model.LOffset.fold
                         (fun _itv (_,x) acc ->
                           if not !first
                           then (*treatment below only compatible with imprecise
                                  handling of Return elsewhere in this file *)
                             raise Not_found;
                           first := false;
                           let res = substitute x in
                           let deps, loc =
                             Values_To_Use.lval_to_loc_with_deps
                               ~deps:Zone.bottom
                               stmt
                               lv
                           in
                           let deps =
                             Lmap_bitwise.From_Model.find acc.deps_table deps in
                           let deps = Zone.join res deps in
                           let deps = Zone.join deps acc.additional_deps in
                           { acc with deps_table =
                               !Db.From.update loc deps acc.deps_table}
                         )
                         return_from
                         new_state
                     with Not_found -> (* from find_lonely_binding *)
                       let vars =
                         Lmap_bitwise.From_Model.LOffset.map
                           (fun (b,x) -> (b,substitute x))
                           return_from
                       in
                       add_with_additional_var
                         lv
                         (Lmap_bitwise.From_Model.LOffset.collapse vars)
                         new_state
                    ))
              in
              let f f acc =
                let p = do_on f in
                match acc with
                  | None -> Some p
                  | Some acc_memory ->
                    Some
                      {state with
                        deps_table = Lmap_bitwise.From_Model.join
                          p.deps_table
                          acc_memory.deps_table}
              in
              let result =
                try
                  (match Kernel_function.Hptset.fold f called_vinfos None with
                    | None -> state
                    | Some s -> s);
                with Call_did_not_take_place -> state
              in
              if not (Db.From.Record_From_Callbacks.is_empty ())
              then
                Stmt.Hashtbl.replace
                  callwise_states_with_formals
                  stmt
                  !states_with_formals;
              result
            )
        | _ -> Dataflow.Default

    let doStmt s d =
      if Db.Value.is_reachable (Values_To_Use.get_stmt_state s) &&
        not (Lmap_bitwise.From_Model.is_bottom d.deps_table)
      then Dataflow.SDefault
      else Dataflow.SDone

    let filterStmt stmt =
      Db.Value.is_reachable (Values_To_Use.get_stmt_state stmt)

    (* Remove all local variables and formals from table *)
    let externalize return kf state =
      let deps_return =
        (match return.skind with
          | Return (Some ({enode = Lval v}),_) ->
            let deps, target =
              Values_To_Use.lval_to_loc_with_deps
                ~deps:Zone.bottom
                return
                v
            in
            Lmap_bitwise.From_Model.LOffset.join
              (Lmap_bitwise.From_Model.find_base
                 state.deps_table deps)
              (Lmap_bitwise.From_Model.find_base
                 state.deps_table
                 (enumerate_valid_bits ~for_writing:false target))
          | Return (None,_) ->
            Lmap_bitwise.From_Model.LOffset.empty
          | _ -> assert false)
      in
      let deps_table =
        Lmap_bitwise.From_Model.filter_base
          (Recording_To_Do.accept_base_in_lmap kf)
          state.deps_table
      in
      { deps_return = deps_return;
        Function_Froms.deps_table = deps_table }

    let doGuard s e _t =
      let interpreted_e = Values_To_Use.access_expr s e in
      let t1 = unrollType (typeOf e) in
      let do_then, do_else =
        if isIntegralType t1 || isPointerType t1
        then Cvalue.V.contains_non_zero interpreted_e,
          Cvalue.V.contains_zero interpreted_e
        else true, true (* TODO: a float condition is true iff != 0.0 *)
      in
      (if do_then
       then Dataflow.GDefault
       else Dataflow.GUnreachable),
      (if do_else
       then Dataflow.GDefault
       else Dataflow.GUnreachable)

    let doEdge s succ d =
      match REACH.blocks_closed_by_edge s succ with
        | [] -> d
        | closed_blocks ->
          let deps_table =
            Lmap_bitwise.From_Model.uninitialize
              (List.fold_left (fun x y -> y.blocals @ x) [] closed_blocks)
              d.deps_table
          in { d with deps_table = deps_table }
  end

  let compute_using_cfg kf =
    match kf.fundec with
      | Declaration _ -> assert false
      | Definition (f,_) ->
        try
          let module Computer = Computer
             (struct
               let stmt_can_reach = Stmts_graph.stmt_can_reach kf
               let blocks_closed_by_edge = Kernel_function.blocks_closed_by_edge
              end)
          in
          let module Compute = Dataflow.Forwards(Computer) in
          Stack.iter
            (fun g ->
              if kf == g then begin
                if Db.Value.ignored_recursive_call kf then
                  From_parameters.error
                    "during dependencies computations for %a, \
                       ignoring probable recursive"
                    Kernel_function.pretty kf;
                raise Exit
              end)
            call_stack;
          Stack.push kf call_stack;
          let state =
            { Computer.empty_from with
              deps_table =
                Lmap_bitwise.From_Model.uninitialize
                  f.slocals Computer.empty_from.deps_table }
          in
          match f.sbody.bstmts with
            | [] -> assert false
            | start :: _ ->
              let ret_id =
                try Kernel_function.find_return kf
                with Kernel_function.No_Statement -> assert false
              in
              (* We start with only the start block *)
              Computer.StmtStartData.add
                start
                (Computer.computeFirstPredecessor start state);
              Compute.compute [start];
              if not (Db.From.Record_From_Callbacks.is_empty ())
              then begin
                From_parameters.feedback "Now calling From callbacks";
                let states =
                  Stmt.Hashtbl.create (Computer.StmtStartData.length ())
                in
                Computer.StmtStartData.iter
                  (fun k record ->
                    Stmt.Hashtbl.add states k record.deps_table);
                Db.From.Record_From_Callbacks.apply
                  (call_stack, states, Computer.callwise_states_with_formals)
              end;
              let _poped = Stack.pop call_stack in
              let last_from =
                try
                  if Db.Value.is_reachable
                    (Values_To_Use.get_stmt_state ret_id)
                  then
                    Computer.externalize
                      ret_id
                      kf
                      (Computer.StmtStartData.find ret_id)
                  else
                    raise Not_found
                with Not_found -> begin
                  From_parameters.result
                    "Non-terminating function %a (no dependencies)"
                    Kernel_function.pretty kf;
                  { Function_Froms.deps_return =
                      Lmap_bitwise.From_Model.LOffset.empty;
                    deps_table = Lmap_bitwise.From_Model.bottom }
                end
              in
              last_from

        with Exit (* Recursive call *) ->
          { Function_Froms.deps_return = Lmap_bitwise.From_Model.LOffset.empty;
            deps_table = Lmap_bitwise.From_Model.empty }

  let compute_using_prototype kf =
    let state = Value.get_initial_state kf in
    compute_using_prototype_for_state state kf

  let compute_and_return kf =
    let call_site_loc = CurrentLoc.get () in
    From_parameters.feedback
      "Computing for function %a%s"
      Kernel_function.pretty kf
      (let s = ref "" in
       Stack.iter
         (fun kf ->
           s := !s^" <-"^(Pretty_utils.sfprintf "%a" Kernel_function.pretty kf))
         call_stack;
       !s);
    !Db.progress ();

    let result =
      if !Db.Value.use_spec_instead_of_definition kf
      then compute_using_prototype kf
      else compute_using_cfg kf
    in
    let result = Recording_To_Do.final_cleanup kf result in
    Recording_To_Do.record_kf kf result;
    From_parameters.feedback
      "Done for function %a" Kernel_function.pretty kf;
    !Db.progress ();
    CurrentLoc.set call_site_loc;
    result

  let compute kf =
    !Db.Value.compute ();
    ignore (compute_and_return kf)

end
