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

(* -------------------------------------------------------------------------- *)
(* --- Why3 Config & Provers                                              --- *)
(* -------------------------------------------------------------------------- *)

let cfg = lazy
  begin
    try
      Why3.Whyconf.read_config None
    with exn ->
      Wp_parameters.abort "%a" Why3.Exn_printer.exn_printer exn
  end

let version = Why3.Config.version
let config () = Lazy.force cfg

let set_procs = Why3.Controller_itp.set_session_max_tasks

let configure =
  let todo = ref true in
  begin fun () ->
    if !todo then
      begin
        let args = Array.of_list ("why3"::Wp_parameters.Why3Flags.get ()) in
        begin try
            Arg.parse_argv ~current:(ref 0) args
              (Why3.Debug.Args.[desc_debug;desc_debug_all;desc_debug_list])
              (fun _ -> raise (Arg.Help "Unknown why3 option"))
              "Why3 options"
          with Arg.Bad s -> Wp_parameters.abort "%s" s
        end;
        ignore (Why3.Debug.Args.option_list ());
        Why3.Debug.Args.set_flags_selected ();
        todo := false
      end
  end

type t = Why3.Whyconf.prover

let find_opt s =
  try
    let config = Lazy.force cfg in
    let filter = Why3.Whyconf.parse_filter_prover s in
    let filter = Why3.Whyconf.filter_prover_with_shortcut config filter in
    Some ((Why3.Whyconf.filter_one_prover config filter).Why3.Whyconf.prover)
  with
  | Why3.Whyconf.ProverNotFound _
  | Why3.Whyconf.ParseFilterProver _
  | Why3.Whyconf.ProverAmbiguity _  ->
      None

let find ?donotfail s =
  try
    try
      let config = Lazy.force cfg in
      let filter = Why3.Whyconf.parse_filter_prover s in
      let filter = Why3.Whyconf.filter_prover_with_shortcut config filter in
      (Why3.Whyconf.filter_one_prover config filter).Why3.Whyconf.prover
    with
    | Why3.Whyconf.ProverNotFound _ as exn when donotfail <> None ->
        Wp_parameters.warning ~once:true "%a" Why3.Exn_printer.exn_printer exn;
        (** from Why3.Whyconf.parse_filter_prover *)
        let sl = Why3.Strings.rev_split ',' s in
        (* reverse order *)
        let prover_name, prover_version, prover_altern =
          match sl with
          | [name] -> name,"",""
          | [version;name] -> name,version,""
          | [altern;version;name] -> name,version,altern
          | _ -> raise (Why3.Whyconf.ParseFilterProver s) in
        { Why3.Whyconf.prover_name; Why3.Whyconf.prover_version; Why3.Whyconf.prover_altern }
  with
  | ( Why3.Whyconf.ProverNotFound _
    | Why3.Whyconf.ParseFilterProver _
    | Why3.Whyconf.ProverAmbiguity _ ) as exn ->
      Wp_parameters.abort "%a" Why3.Exn_printer.exn_printer exn

let print = Why3.Whyconf.prover_parseable_format
let title p = Pretty_utils.sfprintf "%a" Why3.Whyconf.print_prover p
let compare = Why3.Whyconf.Prover.compare

let provers () =
  Why3.Whyconf.Mprover.keys (Why3.Whyconf.get_provers (config ()))

let provers_set () : Why3.Whyconf.Sprover.t =
  Why3.Whyconf.Mprover.domain (Why3.Whyconf.get_provers (config ()))

let is_available p =
  Why3.Whyconf.Mprover.mem p (Why3.Whyconf.get_provers (config ()))

let has_shortcut p s =
  match Why3.Wstdlib.Mstr.find_opt s
          (Why3.Whyconf.get_prover_shortcuts (config ())) with
  | None -> false
  | Some p' -> Why3.Whyconf.Prover.equal p p'
