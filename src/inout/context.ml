(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
open Db_types
open Locations
open Abstract_interp
open Abstract_value

(* Computation of over-approximed operational inputs:
   An acurate computation of these inputs needs the computation of
   under-approximed outputs.
   Computation principle for the treatement of a basic statement:
   *  I_new+ = I_old+ \/+ (Rd+(stmt) /+ O_old-)
   *  O_new- = O_old- \/-  Wr-(stmt)
*)

type compute_t =
    { over_inputs : Zone.t ;
      under_outputs : Zone.t }

(* Initial value for the computation *)
let empty =
  { (* initial value for the computation of over_inputs *)
    over_inputs = Zone.bottom;
    under_outputs = Zone.bottom;
  }

let non_terminating =
  {
    over_inputs = Zone.bottom ;
    under_outputs = Zone.top
  }

(*
let unknown =
  { over_inputs = Zone.top ;
  under_outputs = Zone.bottom }
*)

let call_stack : kernel_function Stack.t =
  Stack.create ()
    (* Stack of function being processed *)

let find_deps_no_transitivity = !From.find_deps_no_transitivity
  (* The value of the expression [expr], just before executing the statement [instr],
     is a function of the values of the returned zones. *)

module Computer (REACH:sig
                   val stmt_can_reach : stmt -> stmt -> bool
                 end) = struct
  let name = "InOut context"

  let debug = ref false

  let current_stmt = ref Kglobal

  let stmt_can_reach = REACH.stmt_can_reach

  let under_inputs_termination_no_depend = ref Zone.bottom

  type t = compute_t

  module StmtStartData =
    Dataflow.StmtStartData(struct type t = compute_t let size = 107 end)

  let display_one fmt k v =
    Format.fprintf fmt "Statement: %d@\n"
      k;
    InOutContext.pretty fmt v

  let pretty fmt x =
    Format.fprintf fmt "@[Over-approximated operational inputs: %a@]@\n@[Under-approximated operational outputs: %a@]"
      Zone.pretty x.over_inputs
      Zone.pretty x.under_outputs

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
      | If (exp,_,_,_) ->
          (* update [over_inputs] using the [exp] condition:
             I+ = I+ \/+ (D+(exp) /+ O-)
          *)
          let inputs = find_deps_no_transitivity (Kstmt s) exp in
            {data with
               over_inputs =
                Zone.join data.over_inputs
                  (Zone.diff inputs data.under_outputs)}
      | _ -> data

  let combinePredecessors
      (s: stmt)
      ~old:{under_outputs = old_outputs;
            over_inputs = old_inputs}
      new_ =
    let {under_outputs = new_outputs;
         over_inputs = new_inputs} = computeFirstPredecessor s new_
    in
    let result_inputs = Zone.join old_inputs new_inputs in
    (* over-approximation :
       I+ = I_old+ \/+ (I_new+  \/+ (D+(exp) /+ Onew-)
    *)
    let result_outputs = Zone.meet old_outputs new_outputs in
    (* under-approximation :
       O- = O_old- /\- O_new-
    *)
    if Zone.is_included result_inputs old_inputs
      (* test for an over-approximation *)
      && Zone.is_included old_outputs result_outputs
      (* test for an under-approximation *)
    then None
    else Some {under_outputs = result_outputs; over_inputs = result_inputs}

  let resolv_func_vinfo ?deps kinstr funcexp =
    !Value.expr_to_kernel_function ?deps kinstr funcexp

  let doInstr _stmt (i: instr) (_d: t) =
    let kinstr = !current_stmt
    in
    let add_with_additional_var k j st =
      let deps, looking_for =
        (* The modified tsets are [looking_for], those address are
           function of [deps]. *)
        !Value.lval_to_loc_with_deps
           ~with_alarms:CilE.warn_none_mode
          ~deps:j
          kinstr
          k
      in
      let new_inputs =
        Zone.join st.over_inputs (Zone.diff deps st.under_outputs) in
      let new_outputs =
        if Location_Bits.cardinal_zero_or_one looking_for.loc
        then
          (* There is only one modified zone. So, this is an exact output.
             Add it into the under-approximed outputs. *)
          Zone.link st.under_outputs (Locations.valid_enumerate_bits looking_for)
        else (* Impossible to add these outputs into the under-approximed outputs. *)
          st.under_outputs
      in
      { over_inputs = new_inputs;
        under_outputs = new_outputs }
    in
    match i with
    | Set (lv, exp, _) ->
        Dataflow.Post
          (fun state ->

             let exp_inputs_deps =
               find_deps_no_transitivity kinstr exp
             in
             add_with_additional_var
               lv
               exp_inputs_deps
               state)
    | Call (lvaloption,funcexp,argl,_) ->
        Dataflow.Post
          (fun state ->
               let funcexp_inputs, called_vinfos =
                 (* [funcexp_inputs]: inputs for the evaluation of [funcexp],
                    [called_vinfos]: list of called functions *)
                 resolv_func_vinfo
                   ~with_alarms:CilE.warn_none_mode
                   ~deps:Zone.bottom
                   kinstr
                   funcexp
               in
               let acc_funcexp_inputs =
                 (* inputs used by [funcexp] and inputs for the evaluation of [funcexp] *)
                 Zone.join funcexp_inputs state.over_inputs
               in
               let acc_funcexp_arg_inputs =
                 (* inputs used by [funcexp], inputs for the evaluation of [funcexp] and its [argl] *)
                 List.fold_right
		   (fun arg inputs ->
                      let arg_inputs = find_deps_no_transitivity kinstr arg
		      in Zone.join inputs arg_inputs)
		   argl
                   acc_funcexp_inputs
               in let result =
                   match called_vinfos with
                     | [] -> { over_inputs = acc_funcexp_arg_inputs ;
                               under_outputs = state.under_outputs }
                     | h::t ->
                         let do_on kernel_function =
                           let { over_inputs_if_termination = called_inputs;
                                 under_outputs_if_termination = called_outputs ;

                                 Inout_type.over_inputs = called_input_termination_no_depend} = !Db.InOutContext.get_external kernel_function

                           in
                           let _ = under_inputs_termination_no_depend := Zone.join
                             !under_inputs_termination_no_depend
                             (Zone.diff called_input_termination_no_depend state.under_outputs);
                           in { over_inputs = (* the real inputs of the call to those of the curent state *)
                                 Zone.diff called_inputs state.under_outputs;
                                under_outputs = called_outputs }
                         in
                         let acc = do_on h (* First call *)
                         in let done_on = List.fold_left (* Combine other calls *)
                             (fun acc_memory called_vinfo ->
                                let done_on = do_on called_vinfo
                                in {over_inputs = Zone.join done_on.over_inputs acc_memory.over_inputs; (* over-approximation *)
                                    under_outputs = Zone.meet done_on.under_outputs acc_memory.under_outputs (* under-approximation intersec*)
                                   })
                             acc
                             t
                         in (* state just after the call, but before the result asssigment *)
                           { over_inputs = Zone.join acc_funcexp_arg_inputs done_on.over_inputs ;
                             under_outputs = Zone.link state.under_outputs done_on.under_outputs (* under-approximed union *) }
               in let result =
                   (* Treatement for the eventual assignement of the call result *)
                   (match lvaloption with
                    | None -> result
                    | Some lv ->
                        add_with_additional_var
                          lv
                          Zone.bottom (* Inputs are already got using [!InOutContext.get_external kernel_function]. *)
                          result)
               in result
)
    | _ -> Dataflow.Default

  let doStmt (s: stmt) (_d: t) =
    current_stmt := Kstmt s;
    Dataflow.SDefault

  let filterStmt (s:stmt) =
    let state = Value.noassert_get_state (Kstmt s) in
    Value.is_reachable state
(*
  let clear_for_function f =
    StmtStartData.clear ();
    StmtStartData.iter (StmtStartData.add stmtStartData) f
*)
  let doGuard s _e _t =
    current_stmt := Kstmt s;
    Dataflow.GDefault

  let doEdge _ _ d = d

end

let get_using_prototype kf =
  let state = Value.get_initial_state kf in
  let behaviors = !Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let over_inputs_if_termination =
    !Value.assigns_to_zone_inputs_state state assigns
  in
  { Inout_type.under_outputs_if_termination =
      (* car les sorties sûre ne sont pas spécifiées ! *)
      Zone.bottom ;
    over_inputs_if_termination =
      (* [over_inputs_if_termination] = [Zone.top] ou [over_inputs_if_termination] ?
         La valeur [over_inputs_if_termination] est légèrement incorrect car les
	 le détail de l'implementation n'est pas précisé dans la spécification.
         La meilleure implementation peut se contenter [over_inputs_if_termination]
         comme entrées opérationelles. *)
      over_inputs_if_termination;
    over_inputs =
      (* [over_inputs] = [Zone.top] ou [over_inputs_if_termination] ?
         La valeur [over_inputs_if_termination] est légèrement incorrect car les
	 fonctions feuilles ne sont pas specifiées en cas de non terminaison. *)
      over_inputs_if_termination
  }

let compute_internal_using_prototype kf =
  match kf.fundec with
    | Definition _ -> assert false
    | Declaration _ -> get_using_prototype kf

let compute_internal_using_cfg kf =
  let compute_for_definition kf f =
    try
      let module Computer =
        Computer(struct let stmt_can_reach = Stmts_graph.stmt_can_reach kf end)
      in
      let module Compute = Dataflow.ForwardsDataFlow(Computer)
      in
      Stack.iter
        (fun g -> if kf == g then begin
           Inout_parameters.warning ~current:true
             "ignoring recursive call detected in function %s during [inout context] computation."
             (Kernel_function.get_name kf);
           raise Exit
         end)
        call_stack;
      Stack.push kf call_stack;
      let res_if_termination = (* result if termination *)
        match f.sbody.bstmts with
        [] -> assert false
      | start :: _ ->
          let ret_id = Kernel_function.find_return kf in
          (* We start with only the start block *)
          Computer.StmtStartData.add
            start.sid
            (Computer.computeFirstPredecessor
               start
               empty);
          Compute.compute [start];
          let _poped = Stack.pop call_stack in
          try
            Computer.StmtStartData.find ret_id.sid
          with Not_found ->
            non_terminating
      in

      { Inout_type.over_inputs_if_termination = res_if_termination.over_inputs ;
	under_outputs_if_termination = res_if_termination.under_outputs ;
	over_inputs = let acc = Computer.under_inputs_termination_no_depend
        in Computer.StmtStartData.iter (fun _sid data -> acc := Zone.join data.over_inputs !acc) ; !acc }

    with Exit ->
      { Inout_type.over_inputs_if_termination = empty.over_inputs ;
	under_outputs_if_termination = empty.under_outputs ;
	over_inputs = empty.over_inputs
      }
  in
  match kf.fundec with
  | Declaration _ ->
      invalid_arg
	"compute_using_cfg cannot be called on leaf functions"
  | Definition (f, _) ->
      compute_for_definition kf f


module Internals =
  Kf_state.Context
    (struct
       let name = "internal_inouts"
       let dependencies = [ Value.self ]
     end)

let get_internal =
  Internals.memo
    (fun kf ->
       !Value.compute ();
       Inout_parameters.feedback "computing for function %a%s"
	 Kernel_function.pretty_name kf
	 (let s = ref "" in
	  Stack.iter
	    (fun kf -> s := !s^" <-"^
	       (Pretty_utils.sfprintf "%a" Kernel_function.pretty_name kf))
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
	 Kernel_function.pretty_name kf;
       res)

let get_external_using_prototype = get_using_prototype

let externalize fundec =
  match fundec with
  | Definition (fundec,_) ->
      Zone.filter_base
        (fun v -> not (Base.is_formal_or_local v fundec))
  | Declaration (_,vd,_,_) ->
      Zone.filter_base
        (fun v -> not (Base.is_formal_of_prototype v vd))

module Externals =
  Kf_state.Context
    (struct
       let name = "external_inouts"
       let dependencies = [ Internals.self ]
     end)

let get_external =
  Externals.memo
    (fun kf ->
       let internals = get_internal kf in
       let filter = externalize kf.fundec in

	 { Inout_type.over_inputs_if_termination = filter internals.Inout_type.over_inputs_if_termination;
           under_outputs_if_termination = filter internals.Inout_type.under_outputs_if_termination;
	   over_inputs = filter internals.Inout_type.over_inputs })

let compute_external kf = ignore (get_external kf)

let pretty_internal fmt kf =
  Format.fprintf fmt "@[InOut (internal) for function %a:@\n%a@]@\n"
    Kernel_function.pretty_name kf
    InOutContext.pretty (get_internal kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[InOut for function %a:@\n%a@]@\n"
    Kernel_function.pretty_name kf
    InOutContext.pretty (get_external kf)

let () =
  (* Derefs.statement := statement; *)
  InOutContext.self_internal := Internals.self;
  InOutContext.self_external := Externals.self;
  InOutContext.get_internal := get_internal;
  InOutContext.get_external := get_external;
  InOutContext.compute := compute_external;
  InOutContext.display := pretty_internal

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
