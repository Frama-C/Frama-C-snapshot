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
open Cil
open Cil_datatype
open Abstract_interp
open Locations

exception Call_did_not_take_place

module type To_Use =
sig
  val get_from_call : kernel_function -> stmt -> Function_Froms.t
  val get_value_state : stmt -> Db.Value.state
  val keep_base : kernel_function -> Base.t -> bool
  val cleanup_and_save : kernel_function -> Function_Froms.t -> Function_Froms.t
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
        Function_Froms.Deps.from_data_deps deps
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

let compute_using_prototype_for_state state kf =
  let varinfo = Kernel_function.get_vi kf in
  let behaviors = !Db.Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let return_deps,deps =
    match assigns with
      | WritesAny ->
          From_parameters.warning "no assigns clauses@ for function %a.@ \
                                     Results@ will be@ imprecise."
                                  Kernel_function.pretty kf;
          Function_Froms.Memory.(top_return, top)
      | Writes assigns ->
          let (rt_typ,_,_,_) = splitFunctionTypeVI varinfo in
          let input_zone out ins =
            (* Technically out is unused, but there is a signature problem *)
              !Db.Value.assigns_inputs_to_zone state (Writes [out, ins])
          in
          let treat_assign acc (out, ins) =
            try
	      let (output_loc_under, output_loc_over, _deps) =
		!Db.Properties.Interp.loc_to_loc_under_over
                  ~result:None state out.it_content
              in
              let input_zone = input_zone out ins in
              (* assign clauses do not let us specify address
                 dependencies for now, so we assume it is all data
                 dependencies *)
              let input_deps =
                Function_Froms.Deps.from_data_deps input_zone
              in
              (* Weak update of the over-approximation of the zones assigned *)
              let acc = Function_Froms.Memory.add_binding_loc ~exact:false
                acc output_loc_over input_deps in
	      let output_loc_under_zone = Locations.enumerate_valid_bits_under
		~for_writing:true output_loc_under in
	      (* Now, perform a strong update on the zones that are guaranteed
                 to be assigned (under-approximation) AND that do not depend
                 on themselves.
                 Note: here we remove an overapproximation from an
		 underapproximation to get an underapproximation, which is not
		 the usual direction. It works here because diff on non-top
                 zones is an exact operation. *)
	      let sure_out_zone =
                Zone.(if equal top input_zone then bottom
                      else diff output_loc_under_zone input_zone)
              in
	      let acc = Function_Froms.Memory.add_binding ~exact:true
		acc sure_out_zone input_deps in
	      acc
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
            let inputs_deps = Function_Froms.Deps.from_data_deps zone_from in
            try
              let coffs =
                !Db.Properties.Interp.loc_to_offset ~result:None out.it_content
              in
              List.fold_left
                (fun acc coff ->
                  let (base,width) = bitsOffset rt_typ coff in
                  let size = Int_Base.inject (Int.of_int width) in
                  Function_Froms.Memory.(add_to_return
                                           ~start:base ~size ~m:acc inputs_deps)
                )
                acc coffs
            with Invalid_argument "not an lvalue" | SizeOfError _ ->
              From_parameters.result  ~once:true ~current:true
                "Unable to extract a proper offset. \
                 Using FROM for the whole \\result";
              let size = Bit_utils.sizeof rt_typ in
              Function_Froms.(Memory.add_to_return ~size ~m:acc inputs_deps)
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
                  Function_Froms.Memory.default_return
              | [] -> (* \from unspecified. *)
                let size = Bit_utils.sizeof rt_typ in
                Function_Froms.Memory.top_return_size size
              | _ ->
                  List.fold_left treat_ret_assign
                    Function_Froms.Memory.default_return return_assigns
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
    let decide _k z1 z2 = Zone.join z1 z2 in
    join ~cache:(Hptmap_sig.PersistentCache "From_compute.ZoneStmtMap.join")
      ~symmetric:true ~idempotent:true ~decide
end

module Make (To_Use: To_Use) =
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


  (** Bind all the variables of [b] to [Assigned \from \nothing]. This function
      is always called on local variables. We do *not* want to bind a local
      variable [v] to Unassigned, as otherwise we could get some dependencies
      that refer to [v] (when [v] is not guaranteed to be always assigned, or
      for padding in local structs), and that would need to be removed when v
      goes out of scope. Moreover, semantically, [v] *is* assigned (albeit to
      "uninitalized",  which represents an indefinite part of the stack). We
      do not attemps to track this "uninitalized" information in From, as this
      is redundant with the work done by Value -- hence the use of [\nothing].*)
  let bind_locals m b =
    let aux_local acc vi =
      Cil.CurrentLoc.set vi.vdecl;
      (* Consider that local are initialized to a constant value *)
      Function_Froms.Memory.bind_var vi Function_Froms.Deps.bottom acc
    in
    let loc = Cil.CurrentLoc.get () in

    let r = List.fold_left aux_local m b.blocals in
    Cil.CurrentLoc.set loc;
    r

  let unbind_locals m b =
    let aux_local acc vi =
      Function_Froms.Memory.unbind_var vi acc
    in
    List.fold_left aux_local m b.blocals


  let find stmt deps_tbl expr =
    let state = To_Use.get_value_state stmt in
    let pre_trans = find_deps_no_transitivity state expr in
    merge_deps
      (fun d -> Function_Froms.Memory.find_precise deps_tbl d) pre_trans

  let lval_to_zone_with_deps stmt ~for_writing lv =
    let state = To_Use.get_value_state stmt in
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Zone.bottom) ~for_writing lv

  let lval_to_precise_loc_with_deps stmt ~for_writing lv =
    let state = To_Use.get_value_state stmt in
    let deps, loc =
      !Db.Value.lval_to_precise_loc_with_deps_state
        state ~deps:(Some Zone.bottom) lv
    in
    let exact = Precise_locs.valid_cardinal_zero_or_one ~for_writing loc in
    deps, loc, exact

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

    let substitute call_site_froms extra_loc deps =
      let subst_deps = Function_Froms.Memory.substitute call_site_froms deps in
      Function_Froms.Deps.add_indirect_dep subst_deps extra_loc

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


    let join_and_is_included new_ old =
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
      let map, included' =
        Function_Froms.Memory.join_and_is_included
          new_.deps_table old.deps_table
      in
      { deps_table = map;
        additional_deps_table = additional_map;
        additional_deps = additional_zone; },
      included && included'

    let join old new_ = fst (join_and_is_included old new_)
    let is_included old new_ = snd (join_and_is_included old new_)

    (** Handle an assignement [lv = ...], the dependencies of the right-hand
        side being stored in [deps_right]. *)
    let transfer_assign stmt lv deps_right state =
      (* The assigned location is [loc], whose address is computed from
         [deps]. *)
      let deps, loc, exact =
        lval_to_precise_loc_with_deps stmt ~for_writing:true lv
      in
      let deps_of_deps = Function_Froms.Memory.find state.deps_table deps in
      let all_indirect = Zone.join state.additional_deps deps_of_deps in
      let deps = Function_Froms.Deps.add_indirect_dep deps_right all_indirect in
      { state with deps_table =
          Function_Froms.Memory.add_binding_precise_loc
            ~exact state.deps_table loc deps }

    let transfer_instr stmt (i: instr) (state: t) =
      !Db.progress ();
      match i with
        | Set (lv, exp, _) ->
              let comp_vars = find stmt state.deps_table exp in
              transfer_assign stmt lv comp_vars state
        | Call (lvaloption,funcexp,argl,_) ->
              !Db.progress ();
              let value_state = To_Use.get_value_state stmt in
              let funcexp_deps, called_vinfos =
                !Db.Value.expr_to_kernel_function_state
                  value_state ~deps:(Some Zone.bottom) funcexp
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
                    (* TODO : dependencies on subfields for structs *)
                    find stmt state.deps_table arg)
                  argl
              in
              let states_with_formals = ref [] in
              let do_on kf =
                let called_vinfo = Kernel_function.get_vi kf in
                if Ast_info.is_cea_function called_vinfo.vname then
                  state
                else
                  let froms_call = To_Use.get_from_call kf stmt in
                  let froms_call_table = froms_call.Function_Froms.deps_table in
                  if Function_Froms.Memory.is_bottom froms_call_table then
                    bottom_from
                  else
                  let formal_args = Kernel_function.get_formals kf in
                  let state_with_formals = ref state.deps_table in
                  begin try
                   List.iter2
                     (fun vi from ->
                       state_with_formals :=
                         Function_Froms.Memory.bind_var
                         vi from !state_with_formals;
                     ) formal_args args_froms;
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
                      (kf, !state_with_formals) :: !states_with_formals;
                  let subst_before_call =
                    substitute !state_with_formals additional_deps
                  in
                  (* From state just after the call,
                     but before the result assigment *)
                  let deps_after_call =
                    let before_call = state.deps_table in
                    let open Function_Froms in
                    let subst d = DepsOrUnassigned.subst subst_before_call d in
                    let call_substituted = Memory.map subst froms_call_table in
                    Memory.compose call_substituted before_call
                  in
                  let state = {state with deps_table = deps_after_call } in
                  (* Treatement for the possible assignement
                     of the call result *)
                  match lvaloption with
                  | None -> state
                  | Some lv ->
                    let return_from = froms_call.Function_Froms.deps_return in
                    let deps_ret = subst_before_call return_from in
                    transfer_assign stmt lv deps_ret state
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
      let value_state = To_Use.get_value_state s in
      let interpreted_e =
        !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode value_state e
      in
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

      | Return _ | Throw _ -> []

      | UnspecifiedSequence _ | Loop _ | Block _
      | Goto _ | Break _ | Continue _
      | TryExcept _ | TryFinally _ | TryCatch _
	-> map_on_all_succs data
    ;;

    (* Filter out unreachable values. *)
    let transfer_stmt s d = 
      if Db.Value.is_reachable (To_Use.get_value_state s) &&
        not (Function_Froms.Memory.is_bottom d.deps_table)
      then transfer_stmt s d
      else []

    let doEdge s succ d =
      if Db.Value.is_reachable (To_Use.get_value_state succ)
      then
        let dt = d.deps_table in
        let opened = Kernel_function.blocks_opened_by_edge s succ in
        let closed = Kernel_function.blocks_closed_by_edge s succ in
        let dt = List.fold_left bind_locals dt opened in
        let dt = List.fold_left unbind_locals dt closed in
        { d with deps_table = dt }
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
          lval_to_zone_with_deps ~for_writing:false return v
        in
        let z = Zone.join target deps in
        let deps = Function_Froms.Memory.find_precise state.deps_table z in
        let size = Bit_utils.sizeof (Cil.typeOfLval v) in
        Function_Froms.(Memory.add_to_return ~size deps)
      | Return (None,_) ->
        Function_Froms.Memory.default_return
      | _ -> assert false)
    in
    let accept = To_Use.keep_base kf in
    let deps_table =
      Function_Froms.Memory.filter_base accept state.deps_table
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
                deps_table = bind_locals empty_from.deps_table f.sbody }
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
              if Db.Value.is_reachable (To_Use.get_value_state ret_id)
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
                  Function_Froms.Memory.default_return;
                deps_table = Function_Froms.Memory.bottom }
            end
          in
          last_from

        with Exit (* Recursive call *) ->
          { Function_Froms.deps_return = Function_Froms.Memory.default_return;
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
    let result = To_Use.cleanup_and_save kf result in
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
compile-command: "make -C ../../.."
End:
*)
