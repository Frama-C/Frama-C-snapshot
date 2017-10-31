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

open Task

(* -------------------------------------------------------------------------- *)
(* --- Prover Why3 IDE Interface                                          --- *)
(* -------------------------------------------------------------------------- *)

class why3ide ~includes ~files ~session =
  object(why)

    inherit ProverTask.command "why3"

    method start () =
      why#add [ "ide" ] ;
      why#add ["--extra-config"; Wp_parameters.Share.file "why3/why3.conf"];
      why#add (Wp_parameters.WhyFlags.get ()) ;
      why#add_list ~name:"-L" includes;
      why#add ["-L";Wp_parameters.Share.file "why3"];
      why#add [session];
      why#add files;
      why#run ~echo:true ()

  end

let run ~includes ~files ~session =
  List.iter Wp_parameters.print_generated files;
  if Wp_parameters.Generate.get ()
  then Task.return false
  else
    let why = new why3ide ~includes ~files ~session in
    Task.todo why#start >>= fun s -> Task.return (s=0)

(* -------------------------------------------------------------------------- *)
(* --- Why-3 IDE Session                                                  --- *)
(* -------------------------------------------------------------------------- *)

module Files = Set.Make(String)
module Goals = Map.Make(ProverWhy3.Goal)
module Xml = Why3_xml
  
type env = {
  mutable files : Files.t ;
  mutable includes : Files.t ;
  mutable goals : Wpo.t Goals.t ;
  mutable provers : (string * VCS.prover) list ;
  session : string ; (* session directory *)
  callback : Wpo.t -> VCS.prover -> VCS.result -> unit ;
}

let add env wpo =
  match ProverWhy3.assemble_goal wpo with
  | None -> ()
  | Some (includes,goal) ->
      let open ProverWhy3 in
      begin
        env.includes <- List.fold_right Files.add includes env.includes ;
        env.files <- Files.add goal.file env.files ;
        env.goals <- Goals.add goal wpo env.goals ;
      end

let parse_prover env e =
  let open Xml in
  try
    let id = List.assoc "id" e.attributes in
    let name = List.assoc "name" e.attributes in
    let version = List.assoc "version" e.attributes in
    let prover = VCS.Why3 (Printf.sprintf "%s:%s" name version) in
    env.provers <- (id , prover) :: env.provers
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e

let parse_result env wpo vcs e =
  let open Xml in
  try
    match e.name with
    | "result" ->
        let time =
          try Some(float_of_string(List.assoc "time" e.attributes))
          with _ -> None in
        let steps =
          try Some(int_of_string(List.assoc "steps" e.attributes))
          with _ -> None in
        let verdict =
          match List.assoc "status" e.attributes with
          | "valid" -> VCS.Valid
          | "unknown" -> VCS.Unknown
          | "timeout" -> VCS.Timeout
          | _ -> VCS.Unknown in
        let result = VCS.result ?time ?steps verdict in
        env.callback wpo vcs result
    | _ -> ()
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e

let parse_goal env wpo e =
  let open Xml in
  try
    match e.name with
    | "proof" ->
        let pid = List.assoc "prover" e.attributes in
        let vcs = List.assoc pid env.provers in
        List.iter (parse_result env wpo vcs) e.elements
    | _ -> ()
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e

let parse_theory env file theory e =
  let open Xml in
  try
    match e.name with
    | "goal" ->
        let goal = List.assoc "name" e.attributes in
        let gid = { ProverWhy3.file ; theory ; goal } in
        let wpo = Goals.find gid env.goals in
        List.iter (parse_goal env wpo) e.elements
    | _ -> ()
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e

let parse_file env file e =
  let open Xml in
  try
    match e.name with
    | "theory" ->
        let thy = List.assoc "name" e.attributes in
        List.iter (parse_theory env file thy) e.elements
    | _ -> ()
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e


let parse_session env e =
  let open Xml in
  try
    match e.name with
    | "prover" -> parse_prover env e
    | "file" ->
        begin
          let file = List.assoc "name" e.attributes in
          let path = Filepath.normalize ~base_name:env.session file in
          let file = Filepath.relativize path in
          List.iter (parse_file env file) e.elements
        end
    | _ -> ()
  with Not_found ->
    Wp_parameters.warning "[why3] Skipped %a" Xml.pretty e

let parse env xml =
  let open Xml in
  let root = xml.content.name in
  if root <> "why3session" then
    Wp_parameters.warning
      "Don't find why3 session root element (found %S)" root ;
  List.iter (parse_session env) xml.content.elements

(* -------------------------------------------------------------------------- *)
(* --- Running Why-3 IDE on a selection of goals                          --- *)
(* -------------------------------------------------------------------------- *)

let register callback wpo vcs result =
  Wpo.set_result wpo vcs result ;
  match callback with
  | None -> ()
  | Some f -> f wpo vcs result

let prove ?callback ~iter =
  let output = Wp_parameters.get_output () in
  let session = output ^ "/project.session" in
  let env = {
    files = Files.empty ;
    includes = Files.empty ;
    goals = Goals.empty ;
    provers = [] ; session ;
    callback = register callback ;
  } in
  let () = iter (add env) in
  let includes = Files.elements env.includes in
  let files = Files.elements env.files in
  if files = [] then Task.nop else
    run ~includes ~files ~session >>=
    begin fun ok ->
      if ok then
        begin
          let file = session ^ "/why3session.xml" in
          if Sys.file_exists file then
            let xml = Why3_xml.from_file file in
            parse env xml
          else
            Wp_parameters.result "[why3] empty session"
        end ;
      Task.return ()
    end
