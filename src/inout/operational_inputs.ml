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
open Db
open Locations
open Abstract_interp
open Abstract_value

(* Computation of over-approximed operational inputs:
   An acurate computation of these inputs needs the computation of
   under-approximed outputs.
*)

type compute_t =
    { over_inputs : Zone.t ;
      under_outputs : Zone.t }

(* Initial value for the computation *)
let empty =
  {
    over_inputs = Zone.bottom;
    under_outputs = Zone.bottom;
  }

let bottom =
  {
    over_inputs = Zone.bottom ;
    under_outputs = Zone.top
  }

let join c1 c2 =
  { over_inputs = Zone.join c1.over_inputs c2.over_inputs;
    under_outputs = Zone.meet c1.under_outputs c2.under_outputs;
  }

let is_included c1 c2 =
  Zone.is_included c1.over_inputs c2.over_inputs &&
    Zone.is_included c2.under_outputs c1.under_outputs

let catenate c1 c2 =
  { over_inputs =
      Zone.join
        c1.over_inputs
        (Zone.diff c2.over_inputs c1.under_outputs);
    under_outputs = Zone.link c1.under_outputs c2.under_outputs }

let pretty fmt x =
  Format.fprintf fmt
    "@[Over-approximated operational inputs: %a@]@\n\
       @[Under-approximated operational outputs: %a@]"
    Zone.pretty x.over_inputs
    Zone.pretty x.under_outputs


let call_stack : kernel_function Stack.t =
  Stack.create ()
    (* Stack of function being processed *)

module Computer (REACH:sig
                   val stmt_can_reach : stmt -> stmt -> bool
                 end) = struct
  let name = "InOut context"

  let debug = ref false

  let current_stmt = ref Kglobal

  let stmt_can_reach = REACH.stmt_can_reach

  let non_terminating_callees_inputs = ref Zone.bottom

  type t = compute_t

  let pretty = pretty

  module StmtStartData =
    Dataflow.StartData(struct type t = compute_t let size = 107 end)

  let display_one fmt k v =
    Format.fprintf fmt "Statement: %d@\n"
      k;
    Db.Operational_inputs.pretty fmt v

  let display fmt f =
    Format.fprintf fmt "=========INOUT CONTEXT START=======@\n";
    Inthash.iter
      (display_one fmt)
      f;
    Format.fprintf fmt "=========INOUT CONTEXT END=======@\n"

  let copy (d: t) = d

  let computeFirstPredecessor (s: stmt) data =
    match s.skind with
      | Switch (exp,_,_,_)
      | If (exp,_,_,_)
      | Return (Some exp, _) ->
          let inputs = !From.find_deps_no_transitivity s exp in
            {data with
               over_inputs =
                Zone.join data.over_inputs
                  (Zone.diff inputs data.under_outputs)}
      | _ -> data

  let combinePredecessors (s: stmt) ~old new_ =
    let new_c = computeFirstPredecessor s new_ in
    let result = join new_c old in
    if is_included result old
    then None
    else Some result

  let resolv_func_vinfo ?deps kinstr funcexp =
    !Value.expr_to_kernel_function ?deps kinstr funcexp

  let doInstr stmt (i: instr) (_d: t) =
    let kinstr = !current_stmt
    in
    let add_with_additional_var k j st =
      let deps, looking_for =
        !Value.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~deps:j
          kinstr
          k
      in
      let new_inputs =
        Zone.join st.over_inputs (Zone.diff deps st.under_outputs) in
      let new_outputs =
        if Locations.valid_cardinal_zero_or_one
          ~for_writing:true
          looking_for
        then
          (* There is only one modified zone. So, this is an exact output.
             Add it into the under-approximed outputs. *)
          Zone.link
            st.under_outputs
            (Locations.valid_enumerate_bits ~for_writing:true looking_for)
        else st.under_outputs
      in
      { over_inputs = new_inputs;
        under_outputs = new_outputs }
    in
    match i with
    | Set (lv, exp, _) ->
        Dataflow.Post
          (fun state ->
             let exp_inputs_deps =
               !From.find_deps_no_transitivity stmt exp
             in
             add_with_additional_var
               lv
               exp_inputs_deps
               state)
    | Call (lvaloption,funcexp,argl,_) ->
        Dataflow.Post
          (fun state ->
             let funcexp_inputs, called_vinfos =
               resolv_func_vinfo
                 ~with_alarms:CilE.warn_none_mode
                 ~deps:Zone.bottom
                 kinstr
                 funcexp
             in
             let acc_funcexp_arg_inputs =
               (* add the inputs of [argl] to the inputs of the
                  function expression *)
               List.fold_right
                 (fun arg inputs ->
                    let arg_inputs = !From.find_deps_no_transitivity stmt arg
                    in Zone.join inputs arg_inputs)
                 argl
                 funcexp_inputs
             in
             let state =
               catenate
                 state
                 { over_inputs = acc_funcexp_arg_inputs ;
                   under_outputs = Zone.bottom;}
             in
             let for_functions =
               Kernel_function.Hptset.fold
                 (fun called_vinfo acc  ->
                   let { Inout_type.over_inputs_if_termination = called_inputs_term;
                         under_outputs_if_termination = called_outputs ;
                         over_inputs = called_inputs} =
                     !Db.Operational_inputs.get_external called_vinfo
                   in
                   non_terminating_callees_inputs :=
                     Zone.join
                       !non_terminating_callees_inputs
                       (Zone.diff called_inputs state.under_outputs);
                   let for_function =
                     { over_inputs = called_inputs_term;
                       under_outputs = called_outputs }
                   in
                   join for_function acc)
                 called_vinfos
                 bottom
             in
(*           Format.printf "functions: %a@." pretty for_functions; *)
             let result = catenate state for_functions in
             let result =
               (* Treatment for the possible assignment of the call result *)
               (match lvaloption with
                | None -> result
                | Some lv ->
                    add_with_additional_var
                      lv
                      Zone.bottom
                      result)
             in result
          )
    | _ -> Dataflow.Default

  let doStmt (s: stmt) (_d: t) =
    current_stmt := Kstmt s;
    Dataflow.SDefault

  let filterStmt (s:stmt) =
    let state = Value.noassert_get_stmt_state s in
    Value.is_reachable state

  let doGuard s _e _t =
    current_stmt := Kstmt s;
    Dataflow.GDefault, Dataflow.GDefault

  let doEdge _ _ d = d

end

let get_using_prototype kf =
  let state = Value.get_initial_state kf in
  let behaviors = !Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let inputs =
    !Value.assigns_to_zone_inputs_state state assigns
  in
(*  Format.printf "proto inputs from assigns: %a@."
    Zone.pretty over_inputs_if_termination; *)
  { Inout_type.under_outputs_if_termination =
      Zone.bottom ;
    over_inputs_if_termination = inputs;
    over_inputs = inputs
  }

let compute_internal_using_prototype kf =
  match kf.fundec with
    | Definition _ -> assert false
    | Declaration _ -> get_using_prototype kf

let compute_internal_using_cfg kf =
  let compute_for_definition kf f =
    try
      let module Computer =
        Computer
          (struct let stmt_can_reach = Stmts_graph.stmt_can_reach kf end)
      in
      let module Compute = Dataflow.Forwards(Computer) in
      Stack.iter
        (fun g -> if kf == g then begin
           if Db.Value.ignored_recursive_call kf then
             Inout_parameters.warning ~current:true
               "During inout context analysis of %a: ignoring probable recursive call."
               Kernel_function.pretty kf;
           raise Exit
         end)
        call_stack;
      Stack.push kf call_stack;
      let res_if_termination =
        match f.sbody.bstmts with
          [] -> assert false
        | start :: _ ->
          try
            let ret_id = Kernel_function.find_return kf in
            Computer.StmtStartData.add
              start
              (Computer.computeFirstPredecessor
                 start
                 empty);
            Compute.compute [start];
            ignore (Stack.pop call_stack);
            try Computer.StmtStartData.find ret_id with Not_found -> bottom
          with Kernel_function.No_Statement->
            assert false
      in

      { Inout_type.over_inputs_if_termination = res_if_termination.over_inputs;
        under_outputs_if_termination = res_if_termination.under_outputs ;
        over_inputs =
          let acc = Computer.non_terminating_callees_inputs
          in
          Computer.StmtStartData.iter
            (fun _sid data -> acc := Zone.join data.over_inputs !acc);
          !acc}

    with Exit ->
      { Inout_type.over_inputs_if_termination = empty.over_inputs ;
        under_outputs_if_termination = empty.under_outputs ;
        over_inputs = empty.over_inputs
      }
  in
  match kf.fundec with
  | Declaration _ ->
      invalid_arg
        "compute_using_cfg cannot be called on library functions"
  | Definition (f, _) ->
      compute_for_definition kf f


module Internals =
  Kf_state.Context
    (struct
       let name = "Internal inouts"
       let dependencies = [ Value.self ]
       let kind = `Correctness
     end)

let get_internal =
  Internals.memo
    (fun kf ->
       !Value.compute ();
       Inout_parameters.feedback "computing for function %a%s"
         Kernel_function.pretty kf
         (let s = ref "" in
          Stack.iter
            (fun kf -> s := !s^" <-"^
               (Pretty_utils.sfprintf "%a" Kernel_function.pretty kf))
            call_stack;
          !s);
       let res =
         match kf.fundec with
         | Definition _ ->
             compute_internal_using_cfg kf
         | Declaration _ ->
             compute_internal_using_prototype kf
       in
       Inout_parameters.feedback "done for function %a"
         Kernel_function.pretty kf;
       res)

  let externalize ~with_formals kf =
    Zone.filter_base (Db.accept_base ~with_formals ~with_locals:false kf)

let raw_get_external ~with_formals kf =
  let internals = get_internal kf in
  let filter = externalize ~with_formals kf in

  { Inout_type.over_inputs_if_termination =
      (let r =
        filter internals.Inout_type.over_inputs_if_termination
      in
(*      Format.printf "filtered -> %a@." Zone.pretty r; *)
      r);
    under_outputs_if_termination =
      filter internals.Inout_type.under_outputs_if_termination;
    over_inputs = filter internals.Inout_type.over_inputs }

module Externals =
  Kf_state.Context
    (struct
       let name = "External inouts"
       let dependencies = [ Internals.self ]
       let kind = `Correctness
     end)
let get_external = Externals.memo (raw_get_external ~with_formals:false)
let compute_external kf = ignore (get_external kf)

module Externals_With_Formals =
  Kf_state.Context
    (struct
       let name = "External inouts with formals"
       let dependencies = [ Internals.self ]
       let kind = `Correctness
     end)
let get_external_with_formals =
  Externals_With_Formals.memo (raw_get_external ~with_formals:true)
let compute_external_with_formals kf = ignore (get_external_with_formals kf)


let pretty_internal fmt kf =
  Format.fprintf fmt "@[InOut (internal) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Db.Operational_inputs.pretty (get_internal kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[InOut for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Db.Operational_inputs.pretty (get_external kf)

let pretty_external_with_formals fmt kf =
  Format.fprintf fmt "@[InOut (with formals) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Db.Operational_inputs.pretty (get_external_with_formals kf)


let () =
  Db.Operational_inputs.self_internal := Internals.self;
  Db.Operational_inputs.self_external := Externals.self;
  Db.Operational_inputs.get_internal := get_internal;
  Db.Operational_inputs.get_external := get_external;
  Db.Operational_inputs.compute := compute_external;
  Db.Operational_inputs.display := pretty_internal

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
