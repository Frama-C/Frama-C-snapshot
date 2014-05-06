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

type acsl_stats =
  { mutable f_requires: int; (** number of requires in function contracts *)
    mutable s_requires: int; (** number of requires in statement contracts *)
    mutable f_ensures: int;
    mutable s_ensures: int;
    mutable f_behaviors: int;
    mutable s_behaviors: int;
    mutable f_assumes: int;
    mutable s_assumes: int;
    mutable f_assigns: int;
    mutable s_assigns: int; (** does not include loop assigns. *)
    mutable f_froms: int;
    mutable s_froms: int; (** does not include loop dependencies. *)
    mutable invariants: int;
    mutable loop_assigns: int;
    mutable loop_froms: int;
    mutable variants: int;
    mutable asserts: int;
  }

let empty_acsl_stat () =
  { f_requires = 0;
    s_requires = 0;
    f_ensures = 0;
    s_ensures = 0;
    f_behaviors = 0;
    s_behaviors = 0;
    f_assumes = 0;
    s_assumes = 0;
    f_assigns = 0;
    s_assigns = 0;
    f_froms = 0;
    s_froms = 0;
    invariants = 0;
    loop_assigns = 0;
    loop_froms = 0;
    variants = 0;
    asserts = 0;
  }

let incr_f_requires stat = stat.f_requires <- stat.f_requires + 1
let incr_s_requires stat = stat.s_requires <- stat.s_requires + 1
let incr_f_ensures stat = stat.f_ensures <- stat.f_ensures + 1
let incr_s_ensures stat = stat.s_ensures <- stat.s_ensures + 1
let incr_f_behaviors stat = stat.f_behaviors <- stat.f_behaviors + 1
let incr_s_behaviors stat = stat.s_behaviors <- stat.s_behaviors + 1
let incr_f_assumes stat = stat.f_assumes <- stat.f_assumes + 1
let incr_s_assumes stat = stat.s_assumes <- stat.s_assumes + 1
let incr_f_assigns stat = stat.f_assigns <- stat.f_assigns + 1
let incr_s_assigns stat = stat.s_assigns <- stat.s_assigns + 1
let incr_f_froms stat = stat.f_froms <- stat.f_froms + 1
let incr_s_froms stat = stat.s_froms <- stat.s_froms + 1
let incr_invariants stat = stat.invariants <- stat.invariants + 1
let incr_loop_assigns stat = stat.loop_assigns <- stat.loop_assigns + 1
let incr_loop_froms stat = stat.loop_froms <- stat.loop_froms + 1
let incr_variants stat = stat.variants <- stat.variants + 1
let incr_asserts stat = stat.asserts <- stat.asserts + 1

let pretty_acsl_stats fmt stat =
  Format.fprintf fmt
    "@[<v 0>requires: %d total, %d in function contracts,\
        %d in statement contracts@;\
     ensures: %d total, %d in function contracts, %d in statement contracts@;\
     behaviors: %d total, %d in function contracts, %d in statement contracts@;\
     assumes: %d total, %d in function contracts, %d in statement contracts@;\
     assigns: %d total, %d in function contracts, %d in statement contracts@;\
     froms: %d total, %d in function contracts, %d in statement contracts@;\
     invariants: %d@;loop assigns: %d@;loop froms: %d@;variants: %d@;\
     asserts: %d@;@]"
    (stat.f_requires + stat.s_requires)
    stat.f_requires stat.s_requires 
    (stat.f_ensures + stat.s_ensures)
    stat.f_ensures stat.s_ensures
    (stat.f_behaviors + stat.s_behaviors)
    stat.f_behaviors stat.s_behaviors
    (stat.f_assumes + stat.s_assumes)
    stat.f_assumes stat.s_assumes
    (stat.f_assigns + stat.s_assigns)
    stat.f_assigns stat.s_assigns
    (stat.f_froms + stat.s_froms)
    stat.f_froms stat.s_froms
    stat.invariants stat.loop_assigns stat.loop_froms stat.variants
    stat.asserts

let pretty_acsl_stats_html fmt stat =
  Format.fprintf fmt
    "@[<v 0>@{<h3>Contract elements@}@;@{<table>@;\
     @{<tr>@{<td>@}\
     @{<td>total@}@{<td>function contract@}@{<td>statement contract@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@;\
     @{<tr>@;@{<td class=\"entry\">requires@}@;\
     @{<td class=\"stat\">%d@}@;@{<td class=\"stat\">%d@}@;\
     @{<td class=\"stat\">%d@}@}@}@;\
     @{<h3>Simple code annotations@}@{<table>@;\
     @{<tr>@{<td class=\"entry\">invariants@}@{<td class=\"stat\">%d@}@}@;\
     @{<tr>@{<td class=\"entry\">loop assigns@}@{<td class=\"stat\">%d@}@}@;\
     @{<tr>@{<td class=\"entry\">loop froms@}@{<td class=\"stat\">%d@}@}@;\
     @{<tr>@{<td class=\"entry\">variants@}@{<td class=\"stat\">%d@}@}@;\
     @{<tr>@{<td class=\"entry\">asserts@}@{<td class=\"stat\">%d@}@}@;\
     @}@]"
    (stat.f_requires + stat.s_requires)
    stat.f_requires stat.s_requires 
    (stat.f_ensures + stat.s_ensures)
    stat.f_ensures stat.s_ensures
    (stat.f_behaviors + stat.s_behaviors)
    stat.f_behaviors stat.s_behaviors
    (stat.f_assumes + stat.s_assumes)
    stat.f_assumes stat.s_assumes
    (stat.f_assigns + stat.s_assigns)
    stat.f_assigns stat.s_assigns
    (stat.f_froms + stat.s_froms)
    stat.f_froms stat.s_froms
    stat.invariants stat.loop_assigns stat.loop_froms stat.variants
    stat.asserts

module Acsl_stats =
  Datatype.Make(
    struct
      type t = acsl_stats
      let reprs = [empty_acsl_stat ()]
      let name = "Metrics_acsl.acsl_stats"
      include Datatype.Serializable_undefined
      let pretty = pretty_acsl_stats
    end)

module Global_acsl_stats =
  State_builder.Ref(Acsl_stats)
    (struct
        let name = "Metrics_acsl.Global_acsl_stats"
        let dependencies =
          [ Ast.self; Annotations.code_annot_state; Annotations.funspec_state;
            Annotations.global_state
          ]
        let default = empty_acsl_stat
     end)

module Functions_acsl_stats =
  State_builder.Hashtbl
    (Kernel_function.Hashtbl)
    (Acsl_stats)
    (struct
      let name = "Metrics_acsl.Functions_acsl_stats"
      let dependencies =
        [Ast.self; Annotations.code_annot_state; Annotations.funspec_state]
      let size = 17
     end)

let get_kf_stats kf =
  try Functions_acsl_stats.find kf with Not_found -> empty_acsl_stat()

module Computed =
  State_builder.False_ref
    (struct
      let name = "Metrics_acsl.Computed"
      let dependencies = [ Global_acsl_stats.self; Functions_acsl_stats.self]
     end)

let treat_behavior local_stats ki b =
  let incr_behaviors =
    if ki = Kglobal then incr_f_behaviors else incr_s_behaviors
  in
  let incr_requires =
    if ki = Kglobal then incr_f_requires else incr_s_requires
  in
  let incr_ensures =
    if ki = Kglobal then incr_f_ensures else incr_s_ensures
  in
  let incr_assumes =
    if ki = Kglobal then incr_f_assumes else incr_s_assumes
  in
  let incr_assigns =
    if ki = Kglobal then incr_f_assigns else incr_s_assigns
  in
  let incr_froms =
    if ki = Kglobal then incr_f_froms else incr_s_froms
  in
  let incr_all f _ = f local_stats; f (Global_acsl_stats.get()) in
  incr_all incr_behaviors ();
  List.iter (incr_all incr_requires) b.b_requires;
  List.iter (incr_all incr_ensures) b.b_post_cond;
  List.iter (incr_all incr_assumes) b.b_assumes;
  (match b.b_assigns with
    | WritesAny -> ()
    | Writes l ->
      incr_all incr_assigns ();
      List.iter
        (function
          | (_,FromAny) -> ()
          | (_,From _) -> incr_all incr_froms ())
        l)
  (*TODO: allocation *)

let add_function_contract_stats kf =
  let local_stats = get_kf_stats kf in
  let treat_behavior _ b = treat_behavior local_stats Kglobal b in
  Annotations.iter_behaviors treat_behavior kf

let add_code_annot_stats stmt _ ca =
  let kf = Kernel_function.find_englobing_kf stmt in
  let local_stats = get_kf_stats kf in
  let incr_all f = f local_stats; f (Global_acsl_stats.get()) in
  match ca.annot_content with
    | AAssert _ -> incr_all incr_asserts
    | AStmtSpec (_,spec) ->
      List.iter (treat_behavior local_stats (Kstmt stmt)) spec.spec_behavior
    | AInvariant _ -> incr_all incr_invariants
    | AVariant _ -> incr_all incr_variants
    | AAssigns (_,WritesAny) -> ()
    | AAssigns (_,Writes l) ->
      incr_all incr_loop_assigns;
      List.iter
        (function (_,FromAny) -> () | (_,From _) -> incr_all incr_loop_froms) l
    | AAllocation _ -> () (* TODO *)
    | APragma _ -> ()

let compute () =
  if not (Computed.get()) then begin
    Ast.compute();
    Annotations.iter_all_code_annot add_code_annot_stats;
    Globals.Functions.iter add_function_contract_stats;
    Computed.set true;
  end

let get_global_stats () = compute (); Global_acsl_stats.get ()

let dump_html_global fmt = pretty_acsl_stats_html fmt (get_global_stats())

let dump_html_by_function fmt =
  compute ();
  Functions_acsl_stats.iter
    (fun kf stats -> 
      Format.fprintf fmt "@{<h2>Function %a@}@;%a"
        Kernel_function.pretty kf pretty_acsl_stats_html stats)

let dump_acsl_stats fmt =
  Metrics_base.mk_hdr 1 fmt "ACSL Statistics";
  Format.pp_print_newline fmt ();
  if Metrics_parameters.ByFunction.get () then begin
    compute ();
    Functions_acsl_stats.iter
      (fun kf stats ->
        let kf_name = Pretty_utils.sfprintf "%a" Kernel_function.pretty kf in
        Format.fprintf fmt "@[<v 2>%a@;%a@]@;"
          (Metrics_base.mk_hdr 2) kf_name pretty_acsl_stats stats)
  end else pretty_acsl_stats fmt (get_global_stats())
    

let dump_acsl_stats_html fmt =
  Format.pp_set_formatter_tag_functions fmt Metrics_base.html_tag_functions;
  Format.fprintf fmt
    "@[<v 0> <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\
          \"http://www.w3.org/TR/html4/strict.dtd\">@ \
      @{<html>@ \
      @{<head>@ \
       @{<title>%s@}@ \
       <meta content=\"text/html; charset=iso-8859-1\" \
        http-equiv=\"Content-Type\"/>@ \
        @{<style type=\"text/css\">%s@}@ \
      @}@ \
        @{<body>\
         @[<v 2>@ \
         @{<h1>%s@}@;\
         %t@]@}@}@]@?"
    "ACSL Metrics"
    Css_html.css
    (if Metrics_parameters.ByFunction.get ()
     then "Detailed ACSL statistics" else "Global ACSL statistics")
    (if Metrics_parameters.ByFunction.get ()
     then dump_html_global else dump_html_by_function)
    

let dump () =
  let out = Metrics_parameters.OutputFile.get () in
  if out <> "" then begin
    try
      let chan = open_out out in
      let fmt = Format.formatter_of_out_channel chan in
      (match Metrics_base.get_file_type out with
            | Metrics_base.Html -> dump_acsl_stats_html fmt
            | Metrics_base.Text -> dump_acsl_stats fmt);
      close_out chan
    with Sys_error s ->
      Metrics_parameters.abort "Cannot open file %s (%s)" out s
  end else Metrics_parameters.result "%t" dump_acsl_stats

