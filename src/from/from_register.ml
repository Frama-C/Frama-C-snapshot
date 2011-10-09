(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Locations
open Abstract_interp
open Abstract_value


exception Call_did_not_take_place

module Functionwise_Dependencies =
  Kernel_function.Make_Table
    (Function_Froms)
    (struct
       let name = "Functionwise dependencies"
       let size = 17
       let dependencies = [ Value.self ]
       let kind = `Correctness
     end)

let () =
  Db.From.self := Functionwise_Dependencies.self;
  Db.From.is_computed := Functionwise_Dependencies.mem

module Callwise_Dependencies =
  Cil_state_builder.Kinstr_hashtbl
    (Function_Froms)
    (struct
       let name = "Callwise dependencies"
       let size = 17
       let dependencies = [ Value.self ]
       let kind = `Correctness
     end)

module type Froms_To_Use_Sig = sig
  val get : kernel_function -> kinstr -> Function_Froms.t
end

module type Values_To_Use_Sig = sig
  val lval_to_loc_with_deps :
    (stmt ->
      with_alarms:CilE.warn_mode ->
      deps:Locations.Zone.t ->
      Cil_types.lval -> Locations.Zone.t * Locations.location) ref
  val expr_to_kernel_function :
    (stmt ->
      with_alarms:CilE.warn_mode ->
      deps:Locations.Zone.t option ->
      Cil_types.exp -> Locations.Zone.t * Kernel_function.Hptset.t) ref

  val get_stmt_state : stmt -> Db.Value.state
  val access_expr : (Cil_types.stmt -> Cil_types.exp -> Db.Value.t) ref
end

module type Recording_Sig = sig
  val accept_base_in_lmap : kernel_function -> Base.t -> bool
  val final_cleanup: kernel_function -> Function_Froms.t -> Function_Froms.t
  val record_kf : kernel_function -> Function_Froms.t -> unit
    (* function to call at the end of the treatment of a function *)
end

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

  let rec find_deps_no_transitivity stmt expr =
    (* The value of the expression [expr], just before executing the statement
       [instr], is a function of the values of the returned zones. *)
    match (stripInfo expr).enode with
    | Info _ -> assert false
    |AlignOfE _| AlignOf _| SizeOfStr _
    |SizeOfE _| SizeOf _ | Const _
        -> Zone.bottom
    | AddrOf lv  | StartOf lv ->
        let deps, _ = !Values_To_Use.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~deps:Zone.bottom
          stmt
          lv
        in deps
    | CastE (_, e)|UnOp (_, e, _) ->
        find_deps_no_transitivity stmt e
    | BinOp (_, e1, e2, _) ->
        Zone.join
          (find_deps_no_transitivity stmt e1)
          (find_deps_no_transitivity stmt e2)
    | Lval v ->
        find_deps_lval_no_transitivity stmt v

  and find_deps_offset_no_transitivity instr o =
    match o with
    | NoOffset -> Zone.bottom
    | Field (_,o) -> find_deps_offset_no_transitivity instr o
    | Index (e,o) ->
        Zone.join
          (find_deps_no_transitivity instr e)
          (find_deps_offset_no_transitivity instr o)

  and find_deps_lval_no_transitivity stmt lv =
    let deps, loc =
      !Values_To_Use.lval_to_loc_with_deps
        ~with_alarms:CilE.warn_none_mode
        ~deps:Zone.bottom
        stmt
        lv
    in
    let direct_deps = valid_enumerate_bits ~for_writing:false loc in
    let result = Zone.join deps direct_deps in
    From_parameters.debug "find_deps_lval_no_trs:@\n deps:%a@\n direct_deps:%a"
      Zone.pretty deps
      Zone.pretty direct_deps;
    result

  let find_deps instr deps_tbl expr =
    let deps_no_trans = find_deps_no_transitivity instr expr in
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

  let name = "from"

  let debug = ref false

  let stmt_can_reach = REACH.stmt_can_reach

  type t = t'

  module StmtStartData =
    Dataflow.StartData(struct type t = t' let size = 107 end)

  let callwise_states_with_formals = Stmt.Hashtbl.create 7

  type substit = Froms of Zone.t | Lvalue of Lmap_bitwise.From_Model.LOffset.t

  let cached_substitute call_site_froms extra_loc =
    let f k intervs =
      Lmap_bitwise.From_Model.find
        call_site_froms
        (Zone.inject k intervs)
    in
    let joiner = Zone.join in
    let projection base =
      match Base.validity base with
      | Base.Periodic (min_valid, max_valid, _)
      | Base.Known (min_valid,max_valid) | Base.Unknown (min_valid,max_valid)->
          Int_Intervals.inject_bounds min_valid max_valid
      | Base.All -> assert false(*TODO*)
    in
    let zone_substitution =
        Zone.cached_fold ~cache:("from substitution", 331) ~temporary:true
          ~f ~joiner ~empty:Zone.bottom ~projection
    in
    let zone_substitution x =
      try
        zone_substitution x
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

  let display_one_from_stmt fmt k v =
    Format.fprintf fmt "Statement: %d@\n%a" k.sid display_one_from v

  let display_from fmt =
    Format.fprintf fmt "=========FROM START=======@\n";
    StmtStartData.iter (display_one_from_stmt fmt);
    Format.fprintf fmt "=========FROM END=======@\n"

  let copy (d: t) = d

  let pretty fmt (v: t) =
    display_one_from fmt v

  let eliminate_additional table s =
    let current_function = Stack.top call_stack in
    (* Eliminate additional variables originating
       from a branch closing at this statement. *)
    Stmt.Map.fold
      (fun k v (acc_set,acc_map,nb) ->
           (* [JS 2010/09/23] now better to let the kernel displays a (better?)
              backtrace. *)
(*       try*)
           if !Postdominators.is_postdominator
             current_function
             ~opening:k
             ~closing:s
           then acc_set,acc_map,nb
           else
             (Zone.join v acc_set),
           (Stmt.Map.add k v acc_map),nb+1
(*       with e ->
           From_parameters.fatal "internal error 356: (%s)Open:%d Close:%d"
             (Printexc.to_string e) k.sid s.sid*))
      table
      (Zone.bottom, Stmt.Map.empty,0)

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

  let combinePredecessors
      (s: stmt)
      ~old:({(*additional_deps_table = old_t;*)
             deps_table = old_table} as old)
      ({(*additional_deps_table = new_t;*)
        deps_table = new_table } as new_) =
    let new_ = computeFirstPredecessor s new_ in
    let changed = ref false in
    let merged =
      Stmt.Map.fold
        (fun k v acc ->
           try
             let current_val = Stmt.Map.find k acc.additional_deps_table in
             if Zone.is_included v current_val then
             acc
             else
               begin
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
    let result = Lmap_bitwise.From_Model.join old_table new_table
    in
    if (not !changed) && Lmap_bitwise.From_Model.is_included result old_table
    then None
    else
       Some ({merged with deps_table = result })

  let resolv_func_vinfo ?deps stmt funcexp =
    !Values_To_Use.expr_to_kernel_function ?deps stmt funcexp

  exception Ignore

  let doInstr stmt (i: instr) (d: t) =
    !Db.progress ();
    let add_with_additional_var lv v d =
      let deps, target =
        (* The modified location is [target],
           whose address is computed from [deps]. *)
        !Values_To_Use.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
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
                 ~with_alarms:CilE.warn_none_mode
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
                        (Interp_loc.lval_to_loc_with_deps kinstr lv))
                     *)
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
                 let formal_args =
                   Kernel_function.get_formals kernel_function
                 in
                 let state_with_formals = ref state.deps_table in
                 begin try
                   List.iter2
                       (fun vi from ->
                         match from with
                           Froms from ->
                             let zvi = Locations.zone_of_varinfo vi in
                             state_with_formals :=
                               Lmap_bitwise.From_Model.add_binding
                                 ~exact:true
                                 !state_with_formals
                                 zvi
                                 from
                         | Lvalue _ -> assert false)
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
                              then
                                (* treatment below only compatible with
                                   imprecise handling
                                   of Return elsewhere in this file *)
                                raise Not_found;
                              first := false;
                              let res = substitute x in
                              let deps, loc =
                                !Values_To_Use.lval_to_loc_with_deps
                                  ~with_alarms:CilE.warn_none_mode
                                  ~deps:Zone.bottom
                                  stmt
                                  lv
                              in
                              let deps =
                                Lmap_bitwise.From_Model.find
                                  acc.deps_table
                                  deps
                              in
                              let deps = Zone.join res deps in
                              let deps = Zone.join deps acc.additional_deps in
                              { acc with deps_table =
                                  !Db.From.update
                                    loc
                                    deps
                                    acc.deps_table})
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
                     None -> Some p
                   | Some acc_memory ->
                       Some
                         {state with
                            deps_table = Lmap_bitwise.From_Model.join
                             p.deps_table
                             acc_memory.deps_table}
             in
             let result =
             try
               ( match Kernel_function.Hptset.fold f called_vinfos None with
                   None -> state
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

  let doStmt (s: stmt) (_d: t) =
    if not (Db.Value.is_reachable (Values_To_Use.get_stmt_state s)) then
      Dataflow.SDone
    else
      Dataflow.SDefault

  let filterStmt stmt =
    Db.Value.is_reachable (Values_To_Use.get_stmt_state stmt)

  (* Remove all local variables and formals from table *)
  let externalize return kf state =
    let deps_return =
      (match return.skind with
      | Return (Some ({enode = Lval v}),_) ->
          let deps, target =
            !Values_To_Use.lval_to_loc_with_deps
              ~with_alarms:CilE.warn_none_mode
              ~deps:Zone.bottom
              return
              v
          in
          Lmap_bitwise.From_Model.LOffset.join
            (Lmap_bitwise.From_Model.find_base
                state.deps_table deps)
            (Lmap_bitwise.From_Model.find_base
                state.deps_table
                (valid_enumerate_bits ~for_writing:false target))
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
    let interpreted_e = !Values_To_Use.access_expr s e in
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
        [] -> d
      | closed_blocks ->
          let deps_table =
            Lmap_bitwise.From_Model.uninitialize_locals
              (List.fold_left (fun x y -> y.blocals @ x) [] closed_blocks)
              d.deps_table
          in { d with deps_table = deps_table }
end

let compute_using_cfg kf =
  match kf.fundec with
  | Declaration _ -> assert false
  | Definition (f,_) ->
      try
        let module Computer =
          Computer
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
                   "during dependencies computations for %a, ignoring probable recursive"
                 Kernel_function.pretty kf;
               raise Exit
             end)
          call_stack;
        Stack.push kf call_stack;
        let state =
          { Computer.empty_from with
              deps_table =
              Lmap_bitwise.From_Model.uninitialize_locals
                f.slocals Computer.empty_from.deps_table }
        in
        match f.sbody.bstmts with
          [] -> assert false
        | start :: _ ->
            let ret_id =
              try Kernel_function.find_return kf
              with Kernel_function.No_Statement -> assert false
            in
            (* We start with only the start block *)
            Computer.StmtStartData.add
              start
              (Computer.computeFirstPredecessor
                 start
                 state);
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
                From_parameters.result ~current:true "Non terminating function (no dependencies)";
                { Function_Froms.deps_return =
                    Lmap_bitwise.From_Model.LOffset.empty;
                  deps_table = Computer.empty_from.deps_table }
              end
            in
            last_from

      with Exit ->
          { Function_Froms.deps_return = Lmap_bitwise.From_Model.LOffset.empty;
            deps_table = Lmap_bitwise.From_Model.empty }

let compute_using_prototype_for_state state kf =
      let varinfo = Kernel_function.get_vi kf in
      let behaviors = !Value.valid_behaviors kf state in
      let assigns = Ast_info.merge_assigns behaviors in
      let return_deps,deps =
        match assigns with
          WritesAny ->
            (* [VP 2011-01-28] Shouldn't that be top? *)
            Lmap_bitwise.From_Model.LOffset.empty,
            Lmap_bitwise.From_Model.empty
        | Writes assigns ->
            let (rt_typ,_,_,_) = splitFunctionTypeVI varinfo in
            let input_zone ins =
              match ins with
                  FromAny -> Zone.top
                | From l ->
                  (try
                     List.fold_left
                       (fun acc loc ->
                         Zone.join acc
                           (Locations.valid_enumerate_bits
                               ~for_writing:false
                              (!Properties.Interp.loc_to_loc
                                  ~result:None
                                  state
                                  loc.it_content)))
                       Zone.bottom
                       l
                   with Invalid_argument "not an lvalue" ->
                     From_parameters.result  ~once:true ~current:true
                       "Unable to extract precise FROM in %a"
                       Kernel_function.pretty kf;
                     Zone.top)
            in
            let treat_assign acc (out, ins) =
              try
                let output_loc =
                  !Properties.Interp.loc_to_loc ~result:None state
                    out.it_content
                in
                let output_zone =
                  Locations.valid_enumerate_bits ~for_writing:true
                    output_loc
                in
                Lmap_bitwise.From_Model.add_binding ~exact:true
                  acc output_zone (input_zone ins)
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
                       (Abstract_value.Int_Intervals.from_ival_size
                          (Ival.of_int base)
                          (Int_Base.inject (Int.of_int width)))
                       (input_zone ins) acc)
                  acc coffs
              with Invalid_argument "not an lvalue" | SizeOfError _ ->
                From_parameters.result  ~once:true ~current:true
                  "Unable to extract a proper offset. \
                   Using FROM for the whole \\result";
                Lmap_bitwise.From_Model.LOffset.add_iset ~exact:false
                  (Abstract_value.Int_Intervals.from_ival_size
                     (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                  (input_zone ins) acc
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
                      (Abstract_value.Int_Intervals.from_ival_size
                         (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                      (input_zone FromAny)
                      Lmap_bitwise.From_Model.LOffset.empty
                | _ ->
                  List.fold_left treat_ret_assign
                    Lmap_bitwise.From_Model.LOffset.empty return_assigns
            in
            return_assigns,
            List.fold_left
              treat_assign
              Lmap_bitwise.From_Model.empty
              other_assigns
      in
      { deps_return = return_deps; Function_Froms.deps_table = deps }

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

(* Application-wide Froms *)
let force_compute = ref (fun _ -> assert false)

module Functionwise_From_to_use =
struct
  let memo kf =
    Functionwise_Dependencies.memo
      (fun kf ->
         !force_compute kf;
         try Functionwise_Dependencies.find kf
         with Not_found -> invalid_arg "could not compute dependencies")
      kf
  let get kf _ = memo kf
end

module Recording_To_Do =
struct
  let accept_base_in_lmap =
    Db.accept_base ~with_formals:false ~with_locals:false
  let final_cleanup kf froms =
    let f k intervs =
      if Db.accept_base ~with_formals:true ~with_locals:false kf k
      then Zone.inject k intervs
      else Zone.bottom
    in
    let joiner = Zone.join in
    let projection base =
      match Base.validity base with
      | Base.Periodic (min_valid, max_valid, _)
      | Base.Known (min_valid,max_valid)
      | Base.Unknown (min_valid,max_valid)->
          Int_Intervals.inject_bounds min_valid max_valid
      | Base.All -> assert false(*TODO*)
    in
    let zone_substitution =
      Zone.cached_fold ~cache:("from cleanup", 331) ~temporary:true
        ~f ~joiner ~empty:Zone.bottom ~projection
    in
    let zone_substitution x =
      try
        zone_substitution x
      with Zone.Error_Top -> Zone.top
    in
    { Function_Froms.deps_table =
        Lmap_bitwise.From_Model.map_and_merge
          zone_substitution
          froms.Function_Froms.deps_table
          Lmap_bitwise.From_Model.empty;
      deps_return =
        Lmap_bitwise.From_Model.LOffset.map
          (function b, d -> b, zone_substitution d)
          froms.Function_Froms.deps_return;
    }
  let record_kf kf last_from = Functionwise_Dependencies.add kf last_from
end

module Value_local = struct
  let get_stmt_state = Db.Value.get_stmt_state
  let access_expr  = ref (fun s exp -> !Db.Value.access_expr (Kstmt s) exp)
  let expr_to_kernel_function =
    ref (fun s ~with_alarms ~deps exp ->
      !Db.Value.expr_to_kernel_function (Kstmt s) ~with_alarms ~deps exp)
  let lval_to_loc_with_deps =
    ref (fun s ~with_alarms ~deps lval ->
      !Db.Value.lval_to_loc_with_deps (Kstmt s) ~with_alarms ~deps lval)
end

module From2 = Make(Value_local)(Functionwise_From_to_use)(Recording_To_Do)

let () =
  force_compute := From2.compute;
  Db.From.compute := (fun kf -> ignore (Functionwise_From_to_use.memo kf));
  Db.From.get := Functionwise_From_to_use.memo

let () = Db.From.pretty :=
  (fun fmt v ->
     let deps = Functionwise_From_to_use.memo v in
     Function_Froms.pretty_with_type (Kernel_function.get_type v) fmt deps)

let () = Db.From.find_deps_no_transitivity := From2.find_deps_no_transitivity

(* Call Froms *)

let merge_call_froms table callsite froms =
  try
    let current = Kinstr.Hashtbl.find table callsite in
    let new_froms = Function_Froms.join froms current in
    Kinstr.Hashtbl.replace table callsite new_froms
  with Not_found ->
    Kinstr.Hashtbl.add table callsite froms

let call_froms_stack = ref []

let record_callwise_dependencies_in_db call_site froms =
  try
    let previous = Callwise_Dependencies.find call_site
    in
    Callwise_Dependencies.replace
      call_site
      (Function_Froms.join previous froms)
  with Not_found ->
    Callwise_Dependencies.add call_site froms

let call_for_individual_froms (state, call_stack) =
  if From_parameters.ForceCallDeps.get () then begin
    let current_function, call_site = List.hd call_stack in
    if not (!Db.Value.use_spec_instead_of_definition current_function) then
      let table_for_current_function = Kinstr.Hashtbl.create 7 in
      call_froms_stack :=
        (current_function,table_for_current_function) :: !call_froms_stack
    else
      try
        let _above_function, table = List.hd !call_froms_stack in
        let froms =
          From2.compute_using_prototype_for_state state current_function
        in
        merge_call_froms table call_site froms;
        record_callwise_dependencies_in_db call_site froms;
      with Failure "hd" ->
        From_parameters.fatal "calldeps internal error 23 empty callfromsstack %a"
          Kernel_function.pretty current_function
  end

let record_for_individual_froms (call_stack, instrstates) =
  let instrstates = Lazy.force instrstates in
  if From_parameters.ForceCallDeps.get () then begin
    let module Froms_To_Use =
        struct
          let get _f callsite =
            let _current_function, table = List.hd !call_froms_stack in
(*          match f.fundec with
              Definition _ -> *)
                begin try
                    Kinstr.Hashtbl.find table callsite
                  with Not_found ->
                    raise Call_did_not_take_place

                end
(*          | Declaration _ ->
                Functionwise_From_to_use.get f callsite *)
        end
    in
    let module Values_To_Use =
        struct
          let get_stmt_state s =
            try Stmt.Hashtbl.find instrstates s
            with Not_found -> Cvalue.Model.bottom

      (* TODO: This should be better factored with Kinstr ! *)
          let lval_to_loc_with_deps kinstr ~with_alarms:_ ~deps lv =
            let state = get_stmt_state kinstr in
            !Db.Value.lval_to_loc_with_deps_state state
              ~deps lv

      let lval_to_loc_with_deps = ref lval_to_loc_with_deps

      let expr_to_kernel_function kinstr ~with_alarms:_ ~deps exp =
        let state = get_stmt_state kinstr in
        !Db.Value.expr_to_kernel_function_state state ~deps exp

      let expr_to_kernel_function = ref expr_to_kernel_function

      let access_expr kinstr expr =
        let state = get_stmt_state kinstr in
        !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state expr
      let access_expr = ref access_expr
    end
    in
    let module Recording_To_Do =
      struct
        let accept_base_in_lmap kf base =
          let fundec = Kernel_function.get_definition kf in
          not (Base.is_formal_or_local base fundec)
        let final_cleanup _kf froms = froms
        let record_kf _kf _last_froms = ()
      end
    in
    let module Callwise_Froms =
      Make(Values_To_Use)(Froms_To_Use)(Recording_To_Do)
    in
    let current_function, call_site = List.hd call_stack in
    let froms = Callwise_Froms.compute_and_return current_function in
    record_callwise_dependencies_in_db call_site froms;
    (* pop + record in top of stack the froms of function that just finished *)
    match !call_froms_stack with
      (current_function2, _) :: (((_caller, table) :: _) as tail) ->
            assert (
              if current_function2 != current_function then begin
                From_parameters.fatal "calldeps %a != %a@."
                  Kernel_function.pretty current_function (* g *)
                  Kernel_function.pretty current_function2; (* f *)
              end else
                true);
        call_froms_stack := tail;
        merge_call_froms table call_site froms

    | _ ->  (* the entry point, probably *)
        Callwise_Dependencies.mark_as_computed ()
  end

let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
       if From_parameters.ForceCallDeps.get() then begin
         Db.Value.Record_Value_Callbacks.extend record_for_individual_froms;
         Db.Value.Call_Value_Callbacks.extend call_for_individual_froms
       end)

let find_available kinstr =
  if From_parameters.ForceDeps.get () then begin
    try
      Callwise_Dependencies.find kinstr
    with Not_found ->
      From_parameters.fatal "internal error 457 : From.find_available called on non-Call statement, or called too early."
  end
  else begin
    match kinstr with
    | Kstmt ({skind = Instr(Call (_,funcexp,_,_))}) ->
        let _, called_functions =
          !Value.expr_to_kernel_function
            ~with_alarms:CilE.warn_none_mode
            kinstr ~deps:None funcexp
        in
        let treat_kf _kf acc =
          let kf_froms = (assert false)
          in
          match acc with
            None -> Some kf_froms
          | Some froms ->
              Some (Function_Froms.join kf_froms froms)
        in
        let froms =
          Kernel_function.Hptset.fold treat_kf called_functions None
        in
        begin
          match froms with
            None -> assert false (* TODO: do something *)
          | Some f -> f
        end
    | _ ->
        From_parameters.fatal "internal error 458 : From.find_available called on non-Call statement."
  end

let display_aux pp =
  !Db.Semantic_Callgraph.topologically_iter_on_functions
    (fun k ->
      if !Db.Value.is_called k then
        pp ("Function %a:@\n%a@." : (_, _, _, _, _, _) format6)
          Kernel_function.pretty k !Db.From.pretty k)

let display fmt =
  Format.fprintf fmt "@[<v>";
  display_aux (Format.fprintf fmt);
  Format.fprintf fmt "@]"

let force_compute_all () =
  !Db.Value.compute ();
  !Db.Semantic_Callgraph.topologically_iter_on_functions
    (fun kf ->
       if Kernel_function.is_definition kf && !Db.Value.is_called kf
       then !Db.From.compute kf)

let force_compute_all_calldeps ()=
  if Db.Value.is_computed () then
    Project.clear
      ~selection:(State_selection.Dynamic.with_dependencies Db.Value.self)
      ();
  !Db.Value.compute ()

module Functionwise_Pathdeps =
  Kernel_function.Make_Table
    (Zone)
    (struct
       let name = "Functionwise pathdeps"
       let size = 17
       let dependencies = [ Value.self ]
       let kind = `Correctness
     end)

class do_pathdepscheck froms callwise_states_with_formals =
object(self)
  inherit nopCilVisitor as super
  val mutable inputs = Zone.bottom

  method result = inputs

  method join new_ =
    inputs <- Zone.join new_ inputs;

  method vstmt s =
    if Value.is_reachable
      (Value.get_stmt_state (Cilutil.out_some self#current_stmt))
    then begin
      match s.skind with
      | UnspecifiedSequence seq ->
        List.iter
          (fun (stmt,_,_,_,_) ->
            ignore (visitCilStmt (self:>cilVisitor) stmt))
          seq;
        SkipChildren (* do not visit the additional lvals *)
      | If (_cond, _th, _el, _) ->
        DoChildren (* for _cond and for the statements in _th, _el *)
      | Loop _ | Block _ ->
        DoChildren (* for the statements *)
      | Instr _ ->
        DoChildren (* for Calls *)
      | Return _ | Goto _ | Break _ | Continue _ ->
        SkipChildren
      | Switch _ | TryExcept _ | TryFinally _ -> assert false
    end
    else SkipChildren

  method stmt_froms =
    let stmt = Cilutil.out_some (self#current_stmt) in
    Stmt.Hashtbl.find froms stmt

  method vlval lv =
    let deps,loc =
      !Value.lval_to_loc_with_deps
        ~with_alarms:CilE.warn_none_mode
        ~deps:Zone.bottom
        (Kstmt (Cilutil.out_some self#current_stmt))
        lv
    in
    let bits_loc = valid_enumerate_bits ~for_writing:false loc in
    let all = Zone.join bits_loc deps in
    let froms = self#stmt_froms in
    let all_f = Lmap_bitwise.From_Model.find froms all in
    self#join all_f;
    (*    Format.printf "lval: all %a all_f %a@."
          Zone.pretty all
          Zone.pretty all_f; *)
    SkipChildren

  method vinst i =
    let current_stmt = Cilutil.out_some self#current_stmt in
    if Value.is_reachable (Value.get_stmt_state current_stmt)
    then begin
      match i with
      | Call (_lv_opt,exp,_args,_) ->
        let current_stmt = Cilutil.out_some self#current_stmt in

        let deps_callees, _callees =
          !Value.expr_to_kernel_function
            ~with_alarms:CilE.warn_none_mode
            ~deps:(Some Zone.bottom)
            (Kstmt current_stmt) exp
        in

        let states_with_formals =
          try Stmt.Hashtbl.find callwise_states_with_formals current_stmt
          with Not_found -> assert false
        in
        let all_f =
          List.fold_left
            (fun acc (kf, state_with_formals) ->
              if not (!Db.Value.use_spec_instead_of_definition kf)
              then
                let deps =
                  try
                    Functionwise_Pathdeps.find kf
                  with Not_found ->
                    Format.printf "pathdeps dependencies not found for %a@."
                      Kernel_function.pretty kf;
                    assert false
                in
                let deps_f = Lmap_bitwise.From_Model.find
                  state_with_formals
                  deps
                in
                Zone.join acc deps_f
              else begin
                Format.printf "Assuming library function %a has no path dependencies@."
                  Kernel_function.pretty kf;
                acc
              end)
            deps_callees
            states_with_formals
        in
        self#join all_f;
        SkipChildren
      | _ -> SkipChildren
    end
    else SkipChildren

  method vexpr exp =
    match exp.enode with
    | AddrOf lv | StartOf lv ->
      let deps,_loc =
        !Value.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~deps:Zone.bottom
          (Kstmt (Cilutil.out_some self#current_stmt))
          lv
      in
      let froms = self#stmt_froms in
      let deps_f = Lmap_bitwise.From_Model.find froms deps in
      self#join deps_f;
        (*      Format.printf "AddrOf: deps %a deps_f %a@."
                Zone.pretty deps
                Zone.pretty deps_f; *)
      SkipChildren
    | _ -> DoChildren

end


let check_pathdeps (stack, froms, callwise_states_with_formals) =
  let kf = Stack.top stack in
  let name = Kernel_function.get_name kf in
  Format.printf "Computing path dependencies for function %s@." name;
  match kf.fundec with
    Definition (f, _) -> begin
      let computer = new do_pathdepscheck froms callwise_states_with_formals in
      ignore (visitCilFunction (computer:>cilVisitor) f);
      let result = computer#result in
      Format.printf "Path dependencies of %s: %a@."
        name
        Zone.pretty result;
      try
        ignore (Functionwise_Pathdeps.find kf);
        assert false
      with Not_found ->
        Functionwise_Pathdeps.add kf result
      end
  | Declaration _ ->
      assert false

let main () =
  if From_parameters.PathDeps.get ()
  then Db.From.Record_From_Callbacks.extend check_pathdeps;


  let not_quiet = From_parameters.verbose_atleast 1 in
  let forcedeps = From_parameters.ForceDeps.get () in
  let forcecalldeps = From_parameters.ForceCallDeps.get () in
  if forcedeps then begin
    !Db.From.compute_all ();
    From_parameters.feedback "====== DEPENDENCIES COMPUTED ======";
    display_aux (fun fm -> From_parameters.result fm);
    From_parameters.feedback "====== END OF DEPENDENCIES ======" 
  end;
  if forcecalldeps then !Db.From.compute_all_calldeps ();
  if not_quiet && forcecalldeps then begin
    From_parameters.feedback "====== DISPLAYING CALLWISE DEPENDENCIES ======";
    !Db.From.Callwise.iter
           (fun ki d ->
              let id,typ =
                match ki with
                | Cil_types.Kglobal ->
                    "entry point",
                    Kernel_function.get_type (fst (Globals.entry_point ()))
                | Cil_types.Kstmt s ->
                    let f =
                       try
                         Kernel_function.Hptset.min_elt
                           (Db.Value.call_to_kernel_function s)
                       with Not_found -> assert false
                    in
                    let id =
                      Pretty_utils.sfprintf "%a at %a (statement %d)"
                        Kernel_function.pretty f
                        pretty_loc_simply (Kstmt s)
                        s.Cil_types.sid
                    in
                    id,
                    Kernel_function.get_type f
              in
              From_parameters.result
                "@[call %s:@\n%a@\n@]@ "
                id (Function_Froms.pretty_with_type typ) d);
    From_parameters.feedback "====== END OF CALLWISE DEPENDENCIES ======";
  end

let () = Db.Main.extend main

let () =
  let _self =
    Db.register_compute "From.compute_all"
      [Functionwise_Dependencies.self]
      Db.From.compute_all
      force_compute_all
  in
    Db.register_guarded_compute
      "From.compute_all_calldeps"
      Callwise_Dependencies.is_computed
      Db.From.compute_all_calldeps
      force_compute_all_calldeps;

    Db.From.display := display;
    Db.From.Callwise.iter := Callwise_Dependencies.iter;
    Db.From.Callwise.find := Callwise_Dependencies.find

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
