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
open Locations
open Visitor

class virtual do_it_ = object(self)
  inherit [Zone.t] Cumulative_analysis.cumulative_visitor as super
  val mutable inputs = Zone.bottom

  method bottom = Zone.bottom

  method result = inputs

  method join new_ =
    inputs <- Zone.join new_ inputs;

  method! vstmt_aux s =
    match s.skind with
      | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore (visitFramacStmt (self:>frama_c_visitor) stmt))
            seq;
          Cil.SkipChildren (* do not visit the additional lvals *)
      | _ -> super#vstmt_aux s

  method! vlval lv =
    let state = Db.Value.get_state self#current_kinstr in
    let deps, bits_loc, _exact =
      !Db.Value.lval_to_zone_with_deps_state
        state ~deps:(Some Zone.bottom) ~for_writing:false lv
    in
    self#join deps;
    self#join bits_loc;
    Cil.SkipChildren

  method private do_assign lv =
    let deps,_loc =
      !Db.Value.lval_to_loc_with_deps (* loc ignored *)
        ~with_alarms:CilE.warn_none_mode
        ~deps:Zone.bottom
        self#current_kinstr
        lv
    in
    (*      Format.printf "do_assign deps:%a@."
            Zone.pretty deps; *)
    self#join deps;

  method! vinst i =
    if Db.Value.is_reachable (Db.Value.get_state self#current_kinstr) then begin
      match i with
      | Set (lv,exp,_) ->
          self#do_assign lv;
          ignore (visitFramacExpr (self:>frama_c_visitor) exp);
          Cil.SkipChildren

      | Call (lv_opt,exp,args,_) ->
        (match lv_opt with None -> ()
        | Some lv -> self#do_assign lv);
          let state = Db.Value.get_state self#current_kinstr in
          (if Cvalue.Model.is_top state then
              self#join Zone.top
           else
              let deps_callees, callees =
                !Db.Value.expr_to_kernel_function_state
                  ~deps:(Some Zone.bottom)
                  state exp
              in
              self#join deps_callees;
              Kernel_function.Hptset.iter
                (fun kf -> self#join (self#compute_kf kf)) callees;
          );
          List.iter
            (fun exp -> ignore (visitFramacExpr (self:>frama_c_visitor) exp))
            args;
          Cil.SkipChildren
      | _ -> Cil.DoChildren
    end
    else Cil.SkipChildren

  method! vexpr exp =
    match exp.enode with
    | AddrOf lv | StartOf lv ->
        let deps,_loc =
          !Db.Value.lval_to_loc_with_deps (* loc ignored *)
            ~with_alarms:CilE.warn_none_mode
            ~deps:Zone.bottom
            self#current_kinstr lv
        in
        self#join deps;
        Cil.SkipChildren
    | _ -> Cil.DoChildren

  method compute_funspec kf =
    let state = self#specialize_state_on_call kf in
    let behaviors = !Db.Value.valid_behaviors kf state in
    let assigns = Ast_info.merge_assigns behaviors in
    !Db.Value.assigns_inputs_to_zone state assigns

  method clean_kf_result (_ : kernel_function) (r: Locations.Zone.t) = r
end


module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="inputs"

    type t = Locations.Zone.t
    module T = Locations.Zone

    class virtual do_it = do_it_
end)

let get_internal = Analysis.kernel_function

module Externals =
  Kernel_function.Make_Table(Locations.Zone)
    (struct
       let name = "External inputs"
       let dependencies = [ Analysis.Memo.self ]
       let size = 17
     end)

let get_external =
  Externals.memo
    (fun kf ->
      Zone.filter_base
        (!Db.Semantic_Callgraph.accept_base
           ~with_formals:false ~with_locals:false kf)
        (get_internal kf))

let get_with_formals kf =
  Zone.filter_base
    (!Db.Semantic_Callgraph.accept_base
       ~with_formals:true ~with_locals:false kf)
    (get_internal kf)

let compute_external kf = ignore (get_external kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[Inputs for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Zone.pretty (get_external kf)

let pretty_with_formals fmt kf =
  Format.fprintf fmt "@[Inputs (with formals) for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Zone.pretty (get_with_formals kf)

let () =
  Db.Inputs.self_internal := Analysis.Memo.self;
  Db.Inputs.self_external := Externals.self;
  Db.Inputs.self_with_formals := Analysis.Memo.self;
  Db.Inputs.get_internal := get_internal;
  Db.Inputs.get_external := get_external;
  Db.Inputs.get_with_formals := get_with_formals;
  Db.Inputs.compute := compute_external;
  Db.Inputs.display := pretty_external;
  Db.Inputs.display_with_formals := pretty_with_formals;
  Db.Inputs.statement := Analysis.statement;
  Db.Inputs.expr := Analysis.expr

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
