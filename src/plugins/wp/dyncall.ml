(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Cil
open Cil_types
open Logic_typing
open Logic_ptree
open Cil_datatype

let dkey_calls = Wp_parameters.register_category "calls"

(* -------------------------------------------------------------------------- *)
(* --- Typing                                                             --- *)
(* -------------------------------------------------------------------------- *)

let find_call env loc f =
  try env.find_var f
  with Not_found ->
    env.error loc "Unknown function '%s'" f

let typecheck ~typing_context ~loc ps =
  ignore loc ;
  let fs =
    List.map
      (fun p ->
         let loc = p.lexpr_loc in
         match p.lexpr_node with
         | PLvar f ->
             let fv = find_call typing_context loc f in
             Logic_const.term ~loc (TLval(TVar fv,TNoOffset)) fv.lv_type
         | _ ->
             typing_context.error loc "Function name expected for calls"
      ) ps
  in
  Ext_terms fs

(* -------------------------------------------------------------------------- *)
(* --- Recover                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_call t = match t.term_node with
  | TLval (TVar { lv_origin = Some v } , TNoOffset ) -> Globals.Functions.get v
  | _ -> raise Not_found

let get_calls ecmd bhvs : (string * Kernel_function.t list) list =
  List.fold_right
    (fun bhv calls ->
       let fs = ref [] in
       List.iter
         (function
           | cmd, Ext_terms ts when cmd = ecmd ->
               fs := !fs @ List.map get_call ts
           | _ -> ())
         bhv.Cil_types.b_extended ;
       let fs = !fs in
       if fs <> [] then (bhv.Cil_types.b_name , fs) :: calls else calls
    ) bhvs []

let pp_calls fmt calls =
  List.iter
    (fun kf -> Format.fprintf fmt "@ %a" Kernel_function.pretty kf)
    calls

(* -------------------------------------------------------------------------- *)
(* --- Dynamic Calls                                                      --- *)
(* -------------------------------------------------------------------------- *)

module PInfo = struct let module_name = "Dyncall.Point" end
module Point = Datatype.Pair_with_collections(Datatype.String)(Stmt)(PInfo)
module Calls = Datatype.List(Kernel_function)
module CInfo =
struct
  let name = "Dyncall.CallPoints"
  let dependencies = [Ast.self]
  let size = 63
end
module CallPoints = State_builder.Hashtbl(Point.Hashtbl)(Calls)(CInfo)

let property ~kf ?bhv ~stmt ~calls =
  let fact = match bhv with
    | None -> Format.asprintf "@[<hov 2>calls%a@]" pp_calls calls
    | Some b -> Format.asprintf "@[<hov 2>for %s calls%a@]" b pp_calls calls
  in
  Property.ip_other fact (Some kf) (Kstmt stmt)

(* -------------------------------------------------------------------------- *)
(* --- Detection                                                          --- *)
(* -------------------------------------------------------------------------- *)

class dyncall =
  object(self)
    inherit Visitor.frama_c_inplace

    val mutable count = 0
    val mutable scope = []

    method count = count

    method private stmt =
      match self#current_stmt with None -> assert false | Some stmt -> stmt

    method! vfunc _ =
      scope <- [] ;
      DoChildren

    method! vspec spec =
      let calls = get_calls "calls" spec.Cil_types.spec_behavior in
      if calls <> [] && scope <> [] then
        List.iter
          (fun stmt ->
             count <- succ count ;
             List.iter
               (fun (bhv,kfs) ->
                  begin
                    if Wp_parameters.has_dkey dkey_calls then
                      let source = snd (Stmt.loc stmt) in
                      if Cil.default_behavior_name = bhv then
                        Wp_parameters.result ~source
                          "@[<hov 2>Calls%a@]" pp_calls kfs
                      else
                        Wp_parameters.result ~source
                          "@[<hov 2>Calls (for %s)%a@]" bhv pp_calls kfs
                  end ;
                  CallPoints.add (bhv,stmt) kfs
               ) calls
          ) scope ;
      scope <- [] ;
      let calls = get_calls "instanceof" spec.Cil_types.spec_behavior in
      if calls <> [] then
        begin
          match self#current_kf with None -> () | Some kf ->
            List.iter
              (fun (bhv,kfs) ->
                 Wp_parameters.result
                   "@[<hov 2>%a for %s instance of%a"
                   Kernel_function.pretty kf bhv pp_calls kfs)
              calls
        end ;
      DoChildren

    method! vinst = function
      | Call( _ , fct , _ , _ ) when Kernel_function.get_called fct = None ->
          scope <- self#stmt :: scope ;
          SkipChildren
      | _ -> SkipChildren

  end

let once = ref false

let compute () =
  if not !once && Wp_parameters.DynCall.get () then
    begin
      once := true ;
      Wp_parameters.feedback "Computing dynamic calls." ;
      let d = new dyncall in
      Visitor .visitFramacFile (d :> Visitor.frama_c_visitor) (Ast.get()) ;
      let n = d#count in
      if n > 0 then
        Wp_parameters.feedback "Dynamic call(s): %d." n
      else
        Wp_parameters.feedback "No dynamic call."
    end

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let get ?(bhv=Cil.default_behavior_name) stmt =
  compute () ;
  try CallPoints.find (bhv,stmt)
  with Not_found -> []

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let register () =
  if Wp_parameters.DynCall.get () then
    let register = Logic_typing.register_behavior_extension in
    begin
      register "calls" typecheck ;
      register "instanceof" typecheck ;
    end

let () = Cmdline.run_after_configuring_stage register
