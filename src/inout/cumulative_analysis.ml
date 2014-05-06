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
open Visitor

let specialize_state_on_call ?stmt kf =
  match stmt with
    | Some ({ skind = Instr (Call (_, _, l, _)) } as stmt) ->
        let at_stmt = Db.Value.get_stmt_state stmt in
        if Cvalue.Model.is_top at_stmt then
          Cvalue.Model.top (* can occur with -no-results-function option *)
        else !Db.Value.add_formals_to_state at_stmt kf l
    | _ -> Db.Value.get_initial_state kf


class virtual ['a] cumulative_visitor = object
  inherit frama_c_inplace as self

  method specialize_state_on_call kf =
    specialize_state_on_call ?stmt:self#current_stmt kf

  method virtual compute_kf: kernel_function -> 'a

end

class type virtual ['a] cumulative_class = object
  inherit ['a] cumulative_visitor

  method bottom: 'a

  method result: 'a
  method join: 'a -> unit

  method compute_funspec : kernel_function -> 'a

  method clean_kf_result: kernel_function -> 'a -> 'a
end


module Make (X:
  sig
    val analysis_name: string

    type t
    module T: Datatype.S with type t = t

    class virtual do_it: [t] cumulative_class
  end) =
struct

  module Memo =
    Kernel_function.Make_Table(X.T)
      (struct
         let name = "Memo " ^ X.analysis_name
         let dependencies = [ Db.Value.self ]
         let size = 97
       end)

  class do_it_cached call_stack = object(self)
    inherit X.do_it

    (* The cycle variable holds the list of functions that are
       involved in a cycle. As long as it is not empty, we known that
       the results we are computing are not complete, and we do not memorize
       them *)
    val mutable cycle = Kernel_function.Hptset.empty
    method private add_cycle s = cycle <- Kernel_function.Hptset.union s cycle
    method cycle = cycle

    (* Computation using the body of a kernel function. The result is
       automatically cached by the function if possible *)
    method private compute_kf_with_def kf =
      let f = Kernel_function.get_definition kf in
      if List.exists (Kernel_function.equal kf) call_stack then (
        if Db.Value.ignored_recursive_call kf then
          Inout_parameters.warning ~current:true ~once:true
            "During %s analysis of %a: ignoring probable recursive call."
            X.analysis_name Kernel_function.pretty kf;
        self#add_cycle (Kernel_function.Hptset.singleton kf);
        self#bottom
      )
      else
        let computer = new do_it_cached (kf :: call_stack) in
        ignore (visitFramacFunction (computer:>frama_c_visitor) f);
        (* Results on all the statements of the function *)
        let v = computer#result in
        let v = computer#clean_kf_result kf v in
        (* recursive calls detected during analysis of the statements*)
        let cycle_aux = Kernel_function.Hptset.remove kf computer#cycle in
        self#add_cycle cycle_aux;
        if Kernel_function.Hptset.is_empty cycle then (
          (* No recursive calls, our results are correct *)
          Inout_parameters.debug "Caching %s result for %a"
            X.analysis_name Kernel_function.pretty kf;
          Memo.add kf v;
        ) else
          Inout_parameters.debug
            "Not caching %s result for %a because of cycle"
            X.analysis_name Kernel_function.pretty kf;
        v

    (* Computation and caching for a kernel function, using its spec *)
    method private compute_kf_with_spec_generic kf =
      try Memo.find kf
      with Not_found ->
        let r_glob = self#compute_funspec kf in
        let r_glob = self#clean_kf_result kf r_glob in
        Memo.add kf r_glob;
        r_glob

    method compute_kf kf =
      if !Db.Value.use_spec_instead_of_definition kf then
        (* If only a declaration is available, or we are instructed to use
           the spec, do so. If a current stmt is available (most of the times),
           do not cache the results. Maybe [compute_funspec] will be able
           to deliver a more precise result on this given statement *)
        match self#current_stmt with
          | None -> self#compute_kf_with_spec_generic kf
          | Some _stmt -> self#compute_funspec kf
      else
        try Memo.find kf
        with Not_found -> self#compute_kf_with_def kf
  end

  let statement stmt =
    let computer = new do_it_cached [] in
    ignore (visitFramacStmt (computer:>frama_c_visitor) stmt);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

  let expr stmt e =
    let computer = new do_it_cached [] in
    computer#push_stmt stmt;
    ignore (visitFramacExpr (computer:>frama_c_visitor) e);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

  let kernel_function kf =
    let computer = new do_it_cached [] in
    computer#join (computer#compute_kf kf);
    assert (Kernel_function.Hptset.is_empty computer#cycle);
    computer#result

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
