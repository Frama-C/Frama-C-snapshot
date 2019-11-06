(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

type coverage_stats =
  { syntactic_calls: int;
    indirect_calls: int;
    total_stmts: int;
    covered_stmts: int; }

let add_syntactic_call stats =
  { stats with syntactic_calls = stats.syntactic_calls + 1 }

let add_indirect_call stats =
  { stats with indirect_calls = stats.indirect_calls + 1 }

let empty_stats =
  { syntactic_calls = 0;
    indirect_calls = 0;
    total_stmts = 0;
    covered_stmts = 0 }

type call_kind = No_call | Only_indirect | Direct

type callee_info = { call: call_kind;
                     is_analyzed: bool;
                     visited: bool; }

let no_call =
  { call = No_call; is_analyzed = false; visited = false; }

let indirect_call = { no_call with call = Only_indirect }

let direct_call = { indirect_call with call = Direct }

let visit info = { info with visited = true; }

let is_analyzed_function vi =
  not (Cil.hasAttribute "fc_stdlib" vi.vattr) &&
  not (Cil.hasAttribute "fc_stdlib_generated" vi.vattr) &&
  Kernel_function.is_definition (Globals.Functions.get vi) &&
  not (List.exists
         (fun s ->
            List.exists
              (fun kf ->
                 Cil_datatype.Varinfo.equal
                   (Kernel_function.get_vi kf)
                   vi)
              (Globals.FileIndex.get_functions
                 (Filepath.Normalized.of_string s)))
         (Mdr_params.Stubs.get())) &&
  not (List.mem vi.vname
         (String.split_on_char ','
            (Dynamic.Parameter.String.get "-eva-use-spec" ()))) &&
  not (List.mem vi.vname
         (List.map
            (fun s -> List.hd (String.split_on_char ':' s))
            (String.split_on_char ','
               (Dynamic.Parameter.String.get "-eva-builtin" ()))))

let is_analyzed_info vi info = {info with is_analyzed=is_analyzed_function vi; }


class eva_coverage_vis ~from_entry_point = object(self)
  inherit Visitor.frama_c_inplace
  val mutable stats = empty_stats
  val calls = Cil_datatype.Varinfo.Hashtbl.create 17

  method private incr_total_stmts =
    stats <- { stats with total_stmts = stats.total_stmts + 1 }
  method private incr_covered_stmts =
    stats <- { stats with covered_stmts = stats.covered_stmts + 1 }

  method! vstmt_aux s =
    (* We only consider real statements: Blocks do not count. *)
    match s.skind with
    | Block _ | UnspecifiedSequence _ -> Cil.DoChildren
    | _ ->
      self#incr_total_stmts;
      if Db.Value.is_reachable_stmt s then self#incr_covered_stmts;
      Cil.DoChildren

  method! vinst i =
    match i with
    | Call(_, { enode = Lval (Var vi, NoOffset)},_,_)
    | Local_init(_,ConsInit (vi,_,_),_) ->
      if Cil_datatype.Varinfo.Hashtbl.mem calls vi then begin
        let info = Cil_datatype.Varinfo.Hashtbl.find calls vi in
        Cil_datatype.Varinfo.Hashtbl.replace
          calls vi { info with call = Direct }
      end else begin
        Cil_datatype.Varinfo.Hashtbl.add
          calls vi (is_analyzed_info vi direct_call)
      end;
      Cil.SkipChildren
    | Call(_,{ enode = Lval (Mem _,NoOffset)},_,_) ->
      let s = Extlib.the self#current_stmt in
      let kfs = Db.Value.call_to_kernel_function s in
      let handle_one kf =
        let vi = Kernel_function.get_vi kf in
        if not (Cil_datatype.Varinfo.Hashtbl.mem calls vi)
        then begin
          Cil_datatype.Varinfo.Hashtbl.add
            calls vi (is_analyzed_info vi indirect_call)
        end else begin
          let info = Cil_datatype.Varinfo.Hashtbl.find calls vi in
          if info.call = No_call then begin
            Cil_datatype.Varinfo.Hashtbl.replace
              calls vi { info with call = Only_indirect }
          end
        end
      in
      Kernel_function.Hptset.iter handle_one kfs;
      Cil.SkipChildren
    | _ -> Cil.SkipChildren (* No need to go further. *)

  method compute () =
    let treat_call vi info reached =
      let must_visit = not info.visited && info.is_analyzed in
      Cil_datatype.Varinfo.Hashtbl.replace calls vi (visit info);
      if must_visit then begin
        let kf = Globals.Functions.get vi in
        ignore (Visitor.visitFramacKf (self:>Visitor.frama_c_inplace) kf);
      end;
      reached && not must_visit
    in
    let check_fixpoint () =
      Cil_datatype.Varinfo.Hashtbl.fold treat_call calls true
    in
    if not from_entry_point then begin
      Globals.Functions.iter_on_fundecs
        (fun { svar } ->
           Cil_datatype.Varinfo.Hashtbl.add
             calls svar (is_analyzed_info svar no_call))
    end;
    let vi =
      Globals.Functions.get_vi
        (Globals.Functions.find_by_name (Kernel.MainFunction.get()))
    in
    (* main entry point might be a stub, but we still would like
       to collect non-stubs calls from it.
    *)
    let info = is_analyzed_info vi direct_call in
    Cil_datatype.Varinfo.Hashtbl.replace
      calls vi { info with is_analyzed = true };
    while not (check_fixpoint ()) do () done;
    Cil_datatype.Varinfo.Hashtbl.fold
      (fun _ info stats ->
         if info.is_analyzed then begin
           match info.call with
           | Direct -> add_syntactic_call stats
           | Only_indirect -> add_indirect_call stats
           | No_call -> stats
         end else stats)
      calls
      stats

end

let nb_fundefs () =
  Globals.Functions.fold
    (fun kf nb ->
       if Kernel_function.is_definition kf &&
          is_analyzed_function (Kernel_function.get_vi kf)
       then nb + 1 else nb) 0

let md_gen () =
  let main = Kernel.MainFunction.get () in
  !Db.Value.compute ();
  let vis = new eva_coverage_vis ~from_entry_point:false in
  let stats = vis#compute () in
  let summary_whole =
    Markdown.format
      "There are %d function definitions that are not stubbed. They represent \
       %d statements, of which %d are potentially reachable through EVA, \
       resulting in a **statement coverage of %.1f%%** with respect to the \
       entire application."
      (nb_fundefs())
      stats.total_stmts stats.covered_stmts
      (float_of_int stats.covered_stmts *. 100. /.
       float_of_int stats.total_stmts)
  in
  let vis = new eva_coverage_vis ~from_entry_point:true in
  let stats = vis#compute () in
  let summary =
    Markdown.format
      "There were potentially %d functions syntactically reachable from %s."
      stats.syntactic_calls main
  in
  let summary =
    if stats.indirect_calls = 0 then summary
    else
      summary @
      Markdown.format
        "In addition, %d were found potentially reachable through \
         indirect calls."
        stats.indirect_calls
  in
  let summary =
    summary @
    Markdown.format
      "These functions contain %d statements, \
       of which %d are potentially reachable according to EVA, resulting in \
       a **statement coverage of %.1f%%** with respect to the perimeter set \
       by this entry point."
      stats.total_stmts stats.covered_stmts
      (float_of_int stats.covered_stmts *. 100. /.
       float_of_int stats.total_stmts)
  in
  Markdown.([ Block [Text summary_whole]; Block [Text summary ]])
