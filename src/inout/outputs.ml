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
open Locations

class virtual do_it_ = object(self)
  inherit [Zone.t] Cumulative_analysis.cumulative_visitor as super
  val mutable outs = Zone.bottom

  method bottom = Zone.bottom

  method result = outs

  method! vstmt_aux s =
    match s.skind with
      | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore(visitFramacStmt (self:>frama_c_visitor) stmt))
            seq;
          Cil.SkipChildren (* do not visit the additional lvals *)
      | _ -> super#vstmt_aux s

  method join new_ =
    outs <- Zone.join new_ outs;

  method private do_assign lv =
    let state = Db.Value.get_state self#current_kinstr in
    let _deps, bits_loc, _exact =
      !Db.Value.lval_to_zone_with_deps_state state
	~deps:None ~for_writing:true lv
    in
    self#join bits_loc

  method! vinst i =
    if Db.Value.is_reachable (Db.Value.noassert_get_state self#current_kinstr) 
    then
      (* noassert needed for Eval.memoize. Not really satisfactory *)
    begin
      match i with
      | Set (lv,_,_) -> self#do_assign lv
      | Call (lv_opt,exp,_,_) ->
          (match lv_opt with None -> ()
             | Some lv -> self#do_assign lv);
          let state = Db.Value.get_state self#current_kinstr in
          if Cvalue.Model.is_top state then
            self#join Zone.top
          else
            let _, callees =
              !Db.Value.expr_to_kernel_function_state ~deps:None state exp in
            Kernel_function.Hptset.iter
              (fun kf ->
                let { Inout_type.over_outputs = z } =
                  Operational_inputs.get_external_aux
		    ?stmt:self#current_stmt kf 
		in
                self#join z
              ) callees
      | _ -> ()
    end;
    Cil.SkipChildren

  method clean_kf_result kf r =
    Zone.filter_base
      (!Db.Semantic_Callgraph.accept_base
         ~with_formals:true ~with_locals:true kf) r

  method compute_funspec kf =
    let state = self#specialize_state_on_call kf in
    let behaviors = !Db.Value.valid_behaviors kf state in
    let assigns = Ast_info.merge_assigns behaviors in
    !Db.Value.assigns_outputs_to_zone state ~result:None assigns
end

module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="outputs"

    type t = Locations.Zone.t
    module T = Locations.Zone

    class virtual do_it = do_it_
end)

let get_internal = Analysis.kernel_function

let externalize kf x =
  Zone.filter_base
    (!Db.Semantic_Callgraph.accept_base
       ~with_formals:false ~with_locals:false kf) x

module Externals =
  Kernel_function.Make_Table(Locations.Zone)
    (struct
       let name = "External outs"
       let dependencies = [ Analysis.Memo.self ]
       let size = 17
     end)

let get_external =
  Externals.memo (fun kf -> externalize kf (get_internal kf))

let pretty_internal fmt kf =
  try
    Format.fprintf fmt "@[Out (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
      Kernel_function.pretty kf
      Zone.pretty (get_internal kf)
  with Not_found ->
    ()

let pretty_external fmt kf =
  try
    Format.fprintf fmt "@[Out (external) for function %a:@\n@[<hov 2>  %a@]@]@\n"
      Kernel_function.pretty kf
      Zone.pretty (get_external kf)
  with Not_found ->
    ()

let () =
  Db.Outputs.self_internal := Analysis.Memo.self;
  Db.Outputs.self_external := Externals.self;
  Db.Outputs.get_internal := get_internal;
  Db.Outputs.get_external := get_external;
  Db.Outputs.compute := (fun kf -> ignore (get_internal kf));
  Db.Outputs.display := pretty_internal;
  Db.Outputs.display_external := pretty_external;
  Db.Outputs.statement := Analysis.statement

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
