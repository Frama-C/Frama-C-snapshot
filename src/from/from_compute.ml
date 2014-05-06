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

open Cil_types
open Cil
open Cil_datatype
open Abstract_interp
open Locations

exception Call_did_not_take_place

module type Froms_To_Use_Sig = sig
  val get : kernel_function -> kinstr -> Function_Froms.t
end

module type Values_To_Use_Sig = sig
  val lval_to_zone_with_deps :
    stmt ->
    deps:Locations.Zone.t option ->
    for_writing:bool ->
    Cil_types.lval ->
    Locations.Zone.t * Locations.Zone.t * bool

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
        -> Function_Froms.Deps.bottom
    | AddrOf lv  | StartOf lv ->
        let deps, _ = !Db.Value.lval_to_loc_with_deps_state (* loc ignored *)
          state
          ~deps:Zone.bottom
          lv
        in
        Function_Froms.Deps.data_deps deps
    | CastE (_, e)|UnOp (_, e, _) ->
        find_deps_no_transitivity state e
    | BinOp (_, e1, e2, _) ->
        Function_Froms.Deps.join
          (find_deps_no_transitivity state e1)
          (find_deps_no_transitivity state e2)
    | Lval v ->
        find_deps_lval_no_transitivity state v

and find_deps_lval_no_transitivity state lv =
  let ind_deps, direct_deps, _exact =
    !Db.Value.lval_to_zone_with_deps_state
      state ~for_writing:false ~deps:(Some Zone.bottom) lv
  in
  From_parameters.debug "find_deps_lval_no_trs:@\n deps:%a@\n direct_deps:%a"
    Zone.pretty ind_deps Zone.pretty direct_deps;
  { Function_Froms.Deps.data = direct_deps; indirect = ind_deps }

let update z exact new_v memory =
  Function_Froms.Memory.add_binding exact memory z new_v


let compute_using_prototype_for_state state kf =
  let varinfo = Kernel_function.get_vi kf in
  let behaviors = !Db.Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let return_deps,deps =
    match assigns with
      | WritesAny ->
          Function_Froms.(Memory.LOffset.degenerate Deps.top, Memory.top)
      | Writes assigns ->
          let (rt_typ,_,_,_) = splitFunctionTypeVI varinfo in
          let input_zone out ins =
            (* Technically out is unused, but there is a signature problem *)
              !Db.Value.assigns_inputs_to_zone state (Writes [out, ins])
          in
          let treat_assign acc (out, ins) =
            try
              let output_locs, _deps =
                !Db.Properties.Interp.loc_to_locs
		  ~result:None state out.it_content
              in
              let input_zone = input_zone out ins in
              let treat_one_output acc out_loc =
		let exact = Location_Bits.cardinal_zero_or_one out_loc.loc in
                let output_zone =
                  Locations.enumerate_valid_bits ~for_writing:true out_loc
                in
                let overlap = Zone.intersects output_zone input_zone in
                let exact = exact && not overlap in
                (* assign clauses do not let us specify address
                   dependencies for now, so we assume it is all
                   data dependencies *)
                let input_deps = Function_Froms.Deps.data_deps input_zone in
                Function_Froms.Memory.add_binding ~exact
                  acc output_zone input_deps
              in
              List.fold_left treat_one_output acc output_locs
            with Invalid_argument "not an lvalue" ->
              From_parameters.result
                ~once:true ~current:true "Unable to extract assigns in %a"
                Kernel_function.pretty kf;
              acc
          in
          let treat_ret_assign acc (out, from) =
            let zone_from = input_zone out from in
            (* assign clauses do not let us specify address dependencies for
               now, so we assume it is all data dependencies *)
            let inputs_deps = Function_Froms.Deps.data_deps zone_from in
            try
              let coffs =
                !Db.Properties.Interp.loc_to_offset ~result:None out.it_content
              in
              List.fold_left
                (fun acc coff ->
                  let (base,width) = bitsOffset rt_typ coff in
                  Function_Froms.Memory.LOffset.add_iset
                    ~exact:true
                    (Lattice_Interval_Set.Int_Intervals.from_ival_size
                       (Ival.of_int base)
                       (Int_Base.inject (Int.of_int width)))
                    inputs_deps
                    acc)
                acc coffs
            with Invalid_argument "not an lvalue" | SizeOfError _ ->
              From_parameters.result  ~once:true ~current:true
                "Unable to extract a proper offset. \
                 Using FROM for the whole \\result";
              Function_Froms.Memory.LOffset.add_iset ~exact:false
                (Lattice_Interval_Set.Int_Intervals.from_ival_size
                   (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                inputs_deps acc
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
                  Function_Froms.Memory.LOffset.empty
              | [] -> (* \from unspecified. *)
                  Function_Froms.(
                    Memory.LOffset.add_iset ~exact:true
                      (Lattice_Interval_Set.Int_Intervals.from_ival_size
                         (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                      Deps.top
                      Memory.LOffset.empty)
              | _ ->
                  List.fold_left treat_ret_assign
                    Function_Froms.Memory.LOffset.empty return_assigns
          in
          return_assigns,
          List.fold_left
            treat_assign Function_Froms.Memory.empty other_assigns
  in
  { deps_return = return_deps; Function_Froms.deps_table = deps }

module ZoneStmtMap = struct
  include
    Hptmap.Make(Stmt_Id)(Zone)(Hptmap.Comp_unused)
    (struct let v = [[]] end)
    (struct let l = [Ast.self] end)

  let join =
    let decide_none _base z = z in
    let decide_some z1 z2 = Zone.join z1 z2 in
    symmetric_merge ~cache:("From_compute.ZoneStmtMap.join", ())
      ~decide_none ~decide_some
end

module Make
  (Values_To_Use:Values_To_Use_Sig)
  (Froms_To_Use: Froms_To_Use_Sig)
  (Recording_To_Do: Recording_Sig) =
struct
  type t' =
      { additional_deps_table : ZoneStmtMap.t;
        (** Additional control dependencies to add to all modified variables,
            coming from the control statements encountered so far (If, Switch).
            The statement information is used to remove the dependencies that
            are no longer useful, when we reach a statement that post-dominates
            the statement that gave rise to the dependency. *)
        additional_deps : Zone.t;
        (** Union of the sets in {!additional_deps_table} *)
        deps_table : Function_Froms.Memory.t
        (** dependency table *)
      }

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  (** Recreate the [additional_deps] field from [additional_deps_table] *)
  let rebuild_additional_deps map =
    ZoneStmtMap.fold (fun _ z accz -> Zone.join z accz) map Zone.bottom

  (** given a [Function_Froms.Deps.t], apply [f] on both components and merge
      the result:
        depending directly on an indirect dependency -> indirect,
        depending indirectly on a direct dependency  -> indirect *)
  let merge_deps f deps =
    let open Function_Froms.Deps in
    let ind = f deps.indirect in
    let data = f deps.data in
    let ind = Zone.join data.indirect (to_zone ind) in
    let data = data.data in
    { data = data; indirect = ind }


  let find stmt deps_tbl expr =
    let state = Values_To_Use.get_stmt_state stmt in
    let pre_trans = find_deps_no_transitivity state expr in
    merge_deps
      (fun d -> Function_Froms.Memory.find_precise deps_tbl d) pre_trans

  let empty_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.empty }

  let bottom_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.bottom }

  module Computer = struct

    type t = t'
    let bottom = bottom_from;;

    let callwise_states_with_formals = Stmt.Hashtbl.create 7

    type substit = 
        Froms of Function_Froms.Deps.t

    let cached_substitute call_site_froms extra_loc =
      let f k intervs =
          Function_Froms.Memory.find_precise
            call_site_froms
            (Zone.inject k intervs)
      in
      let joiner = Function_Froms.Deps.join in
      let projection base = Base.valid_range (Base.validity base) in
      let zone_substitution =
        Zone.cached_fold ~cache_name:"from substitution" ~temporary:true
          ~f ~joiner ~empty:Function_Froms.Deps.bottom ~projection
      in
      let zone_substitution x =
        try zone_substitution x
        with Zone.Error_Top -> Function_Froms.Deps.top
      in
      let open Function_Froms.Deps in
      fun { data; indirect } ->
	let dirdeps = zone_substitution data in
        let inddeps = zone_substitution indirect in
        (* depending directly on an indirect dependency -> indirect,
           depending indirectly on a direct dependency  -> indirect *)
	let ind =
          Zone.(join dirdeps.indirect (join (to_zone inddeps) extra_loc))
        in
        let dir = dirdeps.data in
        { data = dir; indirect = ind }

    let display_one_from fmt v =
      Function_Froms.Memory.pretty fmt v.deps_table;
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        ZoneStmtMap.pretty v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Zone.pretty
        v.additional_deps

    let pretty fmt (v: t) =
      display_one_from fmt v

    let transfer_conditional_exp s exp state = 
      let additional = find s state.deps_table exp in
      let additional = Function_Froms.Deps.to_zone additional in
      {state with
        additional_deps_table =
          ZoneStmtMap.add s additional state.additional_deps_table;
        additional_deps =
          Zone.join additional state.additional_deps }


    let join_and_is_included smaller larger =
      let old = larger and new_ = smaller in
      let additional_map, additional_zone, included =
        let mold = old.additional_deps_table in
        let mnew = new_.additional_deps_table in
        let zold = old.additional_deps in
        let m = ZoneStmtMap.join mnew mold in
        if ZoneStmtMap.equal m mold then
          mold, zold, true
        else
          let new_z = Zone.join old.additional_deps new_.additional_deps in
          m, new_z, false
      in
      let map = Function_Froms.Memory.join old.deps_table new_.deps_table in
      let included' = Function_Froms.Memory.equal map old.deps_table in
      { deps_table = map;
        additional_deps_table = additional_map;
        additional_deps = additional_zone; },
      included && included'

    let join old new_ = fst (join_and_is_included old new_)

    let resolv_func_vinfo ?deps stmt funcexp =
      Values_To_Use.expr_to_kernel_function ~deps stmt funcexp

    let transfer_instr stmt (i: instr) (state: t) =
      !Db.progress ();
      let add_set_with_additional_var lv v d =
        let deps, target, exact =
          (* The modified location is [target],
             whose address is computed from [deps]. *)
          Values_To_Use.lval_to_zone_with_deps
            stmt ~deps:(Some Zone.bottom) ~for_writing:true lv
        in
        let deps_of_deps = Function_Froms.Memory.find d.deps_table deps in
        let deps = 
	  Function_Froms.Deps.add_indirect_dep
	    (Function_Froms.Deps.add_indirect_dep v deps_of_deps)
	    d.additional_deps
	in
        let r = update target exact deps d.deps_table in
        {d with deps_table=r; }
      in
      match i with
        | Set (lv, exp, _) ->
              let comp_vars = find stmt state.deps_table exp in
              add_set_with_additional_var lv comp_vars state
        | Call (lvaloption,funcexp,argl,_) ->
              !Db.progress ();
              let funcexp_deps, called_vinfos =
                resolv_func_vinfo
                  ~deps:Zone.bottom
                  stmt
                  funcexp
              in
              (* dependencies for the evaluation of [funcexp] *)
              let funcexp_deps =
                Function_Froms.Memory.find state.deps_table funcexp_deps
              in
              let additional_deps =
		Zone.join
		  state.additional_deps
		  funcexp_deps
              in
              let args_froms =
                List.map
                  (fun arg ->
                    (* TODO : optimize the dependencies on subfields for structs
                    *)
                    Froms (find stmt state.deps_table arg))
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
                  if Function_Froms.Memory.is_bottom called_func_froms then
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
                               Function_Froms.Memory.add_binding
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
                        Function_Froms.Memory.map_and_merge substitute
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
                       Function_Froms.Memory.LOffset.fold
                         (fun _itv (_,x) acc ->
                           if not !first
                           then (*treatment below only compatible with imprecise
                                  handling of Return elsewhere in this file *)
                             raise Not_found;
                           first := false;
                           let res = substitute x in
                           let deps, loczone, exact =
                             Values_To_Use.lval_to_zone_with_deps
                               stmt
                               ~deps:(Some Zone.bottom)
                               ~for_writing:true
                               lv
                           in
                           let deps =
                             Function_Froms.Memory.find_precise
                               acc.deps_table deps 
			   in
                           let deps = Function_Froms.Deps.join res deps in
                           let deps = 
			     Function_Froms.Deps.add_indirect_dep  
			       deps 
			       acc.additional_deps
			   in
                           { acc with deps_table =
                               update loczone exact deps acc.deps_table}
                         )
                         return_from
                         new_state
                     with Not_found -> (* from find_lonely_binding *)
                       let vars =
                         Function_Froms.Memory.LOffset.map
                           (fun (b,x) -> (b,substitute x))
                           return_from
                       in
                       add_set_with_additional_var
                         lv
                         (Function_Froms.Memory.LOffset.collapse vars)
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
                        deps_table = Function_Froms.Memory.join
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
        | _ -> state


    let transfer_guard s e d =
      let interpreted_e = Values_To_Use.access_expr s e in
      let t1 = unrollType (typeOf e) in
      let do_then, do_else =
        if isIntegralType t1 || isPointerType t1
        then Cvalue.V.contains_non_zero interpreted_e,
          Cvalue.V.contains_zero interpreted_e
        else true, true (* TODO: a float condition is true iff != 0.0 *)
      in
      (if do_then then d else bottom),
      (if do_else then d else bottom)
    ;;

    (* Eliminate additional variables originating from a control-flow branching
       statement closing at [s]. *)
    let eliminate_additional s data =
      let kf = Stack.top call_stack in
      let map = data.additional_deps_table in
      let map' =
        ZoneStmtMap.fold
          (fun k _v acc_map ->
            if !Db.Postdominators.is_postdominator kf ~opening:k ~closing:s
            then ZoneStmtMap.remove k acc_map
            else acc_map
          ) map map
      in
      if not (map == map') then
        { data with
          additional_deps_table = map';
          additional_deps = rebuild_additional_deps map';
        }
      else data

    let transfer_stmt s data =
      let data = eliminate_additional s data in
      let map_on_all_succs new_data = List.map (fun x -> (x,new_data)) s.succs in
      match s.skind with
      | Instr i -> map_on_all_succs (transfer_instr s i data)

      | If(exp,_,_,_) ->
      	let data = transfer_conditional_exp s exp data in
      	Dataflows.transfer_if_from_guard transfer_guard s data
      | Switch(exp,_,_,_) ->
      	let data = transfer_conditional_exp s exp data in
      	Dataflows.transfer_switch_from_guard transfer_guard s data

      | Return _ -> []

      | UnspecifiedSequence _ | Loop _ | Block _
      | Goto _ | Break _ | Continue _
      | TryExcept _ | TryFinally _
	-> map_on_all_succs data
    ;;

    (* Filter out unreachable values. *)
    let transfer_stmt s d = 
      if Db.Value.is_reachable (Values_To_Use.get_stmt_state s) &&
        not (Function_Froms.Memory.is_bottom d.deps_table)
      then transfer_stmt s d
      else []

    let doEdge s succ d =
      if Db.Value.is_reachable (Values_To_Use.get_stmt_state succ) 
      then
	let d = match Kernel_function.blocks_closed_by_edge s succ with
          | [] -> d
          | closed_blocks ->
            let deps_table =
              Function_Froms.Memory.uninitialize
		(List.fold_left (fun x y -> y.blocals @ x) [] closed_blocks)
		d.deps_table
            in { d with deps_table = deps_table }
	in d
      else 
	bottom_from

    (* Filter the outgoing data using doEdge. *)
    let transfer_stmt s d = 
      let ds = transfer_stmt s d in 
      List.map (fun (succ, d) -> (succ, doEdge s succ d)) ds
    ;;

  end


  (* Remove all local variables and formals from table *)
  let externalize return kf state =
    let deps_return =
      (match return.skind with
      | Return (Some ({enode = Lval v}),_) ->
        let deps, target, _exact =
          Values_To_Use.lval_to_zone_with_deps
            ~deps:(Some Zone.bottom)
            ~for_writing:false
            return
            v
        in
        Function_Froms.Memory.LOffset.join
          (Function_Froms.Memory.find_base state.deps_table deps)
          (Function_Froms.Memory.find_base state.deps_table target)
      | Return (None,_) ->
        Function_Froms.Memory.LOffset.empty
      | _ -> assert false)
    in
    let deps_table =
      Function_Froms.Memory.filter_base
        (Recording_To_Do.accept_base_in_lmap kf)
        state.deps_table
    in
    { deps_return = deps_return;
      Function_Froms.deps_table = deps_table }

  let compute_using_cfg kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
      if !Db.Value.no_results f then Function_Froms.top
      else
        try
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
            { empty_from with
              deps_table =
                Function_Froms.Memory.uninitialize
                  f.slocals empty_from.deps_table }
          in
	  let module Fenv =
                (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
          in
	  let module Dataflow_arg = struct
            include Computer
            let init = [(Kernel_function.find_first_stmt kf, state)]
          end
          in
          let module Compute = Dataflows.Simple_forward(Fenv)(Dataflow_arg) in
          let ret_id = Kernel_function.find_return kf in
          if not (Db.From.Record_From_Callbacks.is_empty ())
          then begin
            From_parameters.feedback "Now calling From callbacks";
            let states =
              Stmt.Hashtbl.create Fenv.nb_stmts
            in
	    Compute.iter_on_result (fun k record ->
              Stmt.Hashtbl.add states k record.deps_table);
            Db.From.Record_From_Callbacks.apply
              (call_stack, states, Dataflow_arg.callwise_states_with_formals)
          end;
          let _poped = Stack.pop call_stack in
          let last_from =
            try
              if Db.Value.is_reachable
                (Values_To_Use.get_stmt_state ret_id)
              then
                externalize
                  ret_id
                  kf
		  Compute.before.(Fenv.to_ordered ret_id)
              else
                raise Not_found
            with Not_found -> begin
              From_parameters.result
                "Non-terminating function %a (no dependencies)"
                Kernel_function.pretty kf;
              { Function_Froms.deps_return =
                  Function_Froms.Memory.LOffset.empty;
                deps_table = Function_Froms.Memory.bottom }
            end
          in
          last_from

        with Exit (* Recursive call *) ->
          { Function_Froms.deps_return = Function_Froms.Memory.LOffset.empty;
            deps_table = Function_Froms.Memory.empty }

  let compute_using_prototype kf =
    let state = Db.Value.get_initial_state kf in
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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
