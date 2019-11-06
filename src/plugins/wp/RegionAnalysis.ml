(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types
module Wp = Wp_parameters
module Kf = Kernel_function

(* ---------------------------------------------------------------------- *)
(* --- Compute Analysis                                               --- *)
(* ---------------------------------------------------------------------- *)

let compute kf =
  let map = Region.create () in
  if Kf.is_definition kf then
    begin
      Wp.feedback ~ontty:`Transient "[region] Analyzing %a" Kf.pretty kf ;
      let def = Kf.get_definition kf in
      RegionAccess.cc_fundec map def ;
      let spec = Annotations.funspec kf in
      RegionAccess.cc_spec map spec ;
      List.iter
        (fun bhv ->
           let region_specs = RegionAnnot.of_behavior bhv in
           if region_specs <> [] then
             if Cil.is_default_behavior bhv then
               List.iter (RegionAccess.cc_region map) region_specs
             else
               Wp.warning ~once:true
                 "Region specifications in non-default behaviours are skipped."
        ) spec.spec_behavior ;
      if Wp.Region_fixpoint.get () then Region.fixpoint map ;
    end ;
  map

(* ---------------------------------------------------------------------- *)
(* --- Projectified Analysis Result                                   --- *)
(* ---------------------------------------------------------------------- *)

module REGION = Datatype.Make
    (struct
      type t = Region.map
      include Datatype.Undefined
      let reprs = [Region.create ()]
      let name = "Wp.RegionAnalysis.region"
      let mem_project = Datatype.never_any_project
    end)

module GLOBAL = State_builder.Ref
    (REGION)
    (struct
      let name = "Wp.RegionAnalysis.ref"
      let dependencies = [Ast.self]
      let default = Region.create
    end)

module REGISTRY = State_builder.Hashtbl
    (Kernel_function.Hashtbl)
    (REGION)
    (struct
      let name = "Wp.RegionAnalysis.registry"
      let dependencies = [Ast.self]
      let size = 32
    end)

let get = function
  | None -> GLOBAL.get ()
  | Some kf ->
      try REGISTRY.find kf
      with Not_found ->
        let map = compute kf in
        REGISTRY.add kf map ; map

(* ---------------------------------------------------------------------- *)
(* --- Command Line Registry                                          --- *)
(* ---------------------------------------------------------------------- *)

let main () =
  if Wp.Region.get () then
    begin
      Ast.compute () ;
      let dir = Wp.get_output_dir "region" in
      Wp.iter_kf (fun kf ->
          let map = get (Some kf) in
          if not (Region.is_empty map) then
            RegionDump.dump ~dir kf map
        ) ;
    end

let () = Db.Main.extend main

(* ---------------------------------------------------------------------- *)
