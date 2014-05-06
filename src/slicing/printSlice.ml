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

(** Everything needed to print the result *)

(**/**)

open Cil_types

(**/**)

let find_sub_stmts st = match st.skind with
| If(_,bl1,bl2,_) | TryExcept (bl1, _, bl2, _)
| TryFinally (bl1, bl2, _) -> bl1.bstmts@bl2.bstmts
| Block bl | Loop (_,bl, _, _, _) | Switch (_, bl, _, _) ->  bl.bstmts
| UnspecifiedSequence seq -> List.map (fun (x,_,_,_,_) -> x) seq
| Continue _|Break _|Goto (_, _)|Return (_, _)|Instr _  -> []

let str_call_sig ff call fmt =
  try
    let _, ff_marks = ff.SlicingInternals.ff_marks in
    let called, sgn = PdgIndex.FctIndex.find_call ff_marks call in
    let print_called fmt = match called with
      | None
      | Some (None) -> Format.fprintf fmt "@[/* undetermined call */@]"
      | Some (Some (SlicingInternals.CallSlice ff)) ->
        Format.fprintf fmt "@[/* call to@ %a */@]"
          Fct_slice.print_ff_sig ff
      | Some (Some(SlicingInternals.CallSrc _)) ->
        Format.fprintf fmt "@[/* call to source function */@]"
    in
    Format.fprintf fmt "@[<v>@[/* sig call:@ %a */@]@ %t@]"
      SlicingMarks.pretty_sig sgn print_called
  with Not_found -> 
    Format.fprintf fmt "@[/* invisible call */@]"

class printerClass optional_ff = object(self)
  inherit Printer.extensible_printer () as super
  val opt_ff = optional_ff

  method! vdecl fmt var =
    match opt_ff with
    | None -> super#vdecl fmt var
    | Some ff ->
        if var.vglob  then
          Format.fprintf fmt "@[/**/%a@]" super#vdecl var
        else
          let str_m =
            try
              let m = Fct_slice.get_local_var_mark ff var in
                SlicingMarks.mark_to_string m
            with Not_found -> "[---]"
          in
          Format.fprintf fmt "@[<hv>/* %s */@ %a@]"
            str_m
            super#vdecl var

  method! stmtkind next fmt kind =
    let stmt_info fmt stmt = match opt_ff with
      | None -> Format.fprintf fmt "@[/* %d */@]" stmt.Cil_types.sid
      | Some ff ->
          let str_m = try
            let m = Fct_slice.get_stmt_mark ff stmt in
            SlicingMarks.mark_to_string m
          with Not_found -> "[---]"
          in
          if (SlicingMacros.is_call_stmt stmt)then 
	    Format.fprintf fmt "@[<hv>%t@ /* %s */@]" 
	      (str_call_sig ff stmt) str_m
          else
	    Format.fprintf fmt "@[/* %s */@]" str_m
    in
    let s = Extlib.the self#current_stmt in
    try
      Format.fprintf fmt "@[<v>%a@ %a@]"
        stmt_info s
        (fun fmt -> super#stmtkind next fmt) kind
    with Not_found -> 
      (* some sub statements may be visible *)
      let sub_stmts = find_sub_stmts s in
      List.iter (self#stmt fmt) sub_stmts

  method! label fmt l =
    let label_info = match opt_ff with
      | None -> "label"
      | Some ff ->
        let m = Fct_slice.get_label_mark ff (Extlib.the self#current_stmt) l in
        SlicingMarks.mark_to_string m
    in
    Format.fprintf fmt "@[<hv>/* %s */@ %a@]"
      label_info
      super#label l
end

let print_fct_from_pdg fmt ?ff pdg  =
  let kf = PdgTypes.Pdg.get_kf pdg in
  let fct = Kernel_function.get_definition kf in
  let loc = Lexing.dummy_pos,Lexing.dummy_pos in
  let glob = Cil_types.GFun (fct, loc) in (* TODO : make it cleaner *)
  let printer = new printerClass ff in
  printer#global fmt glob

let print_marked_ff fmt ff =
  let pdg = SlicingMacros.get_ff_pdg ff in 
  Format.fprintf fmt "@[<v>@[<hv>Print slice =@ %a@]@ @ %a@]" 
    Fct_slice.print_ff_sig ff
    (print_fct_from_pdg ~ff) pdg

let print_original_glob fmt glob =
  let printer = new printerClass None in
  printer#global fmt glob

(*----------------------------------------------------------------------------*)
module PrintProject = struct
  type t = string * SlicingInternals.project
  type node =
    | Src of SlicingInternals.fct_info
    | Slice of SlicingInternals.fct_slice
    | OptSlicingLevel of SlicingInternals.level_option
    | OptSliceCallers of bool
    | Action of (int * SlicingInternals.criterion)
  module V = struct
    type t = node
  end
  module E = struct
    type t = (node * node) * Cil_types.stmt option
    let src (e, _) = fst e
    let dst (e, _) = snd e
  end

  type tfi = Undef | PersistSelect | Other

  let fi_type fi = match fi.SlicingInternals.fi_def with
      | Some _f ->
          if SlicingMacros.fi_has_persistent_selection fi
          then PersistSelect
          else Other
      | None -> Undef

  let node_slice_callers () =
    (OptSliceCallers (SlicingParameters.Mode.Callers.get ()))
  let node_slice_calls () =
    (OptSlicingLevel (SlicingMacros.get_default_level_option true))

  let iter_vertex f (_, proj) =
    f (node_slice_calls ()); f (node_slice_callers ());
    let rec do_act n rq_list = match rq_list with
      | [] -> ()
      | rq :: rq_list -> f (Action (n, rq)) ; do_act (n+1) rq_list
    in do_act 1 proj.SlicingInternals.actions;
    let do_kf kf =
      let fi = SlicingMacros.get_kf_fi proj kf in
      let slices = SlicingMacros.fi_slices fi in
        List.iter (fun ff -> f (Slice ff)) slices;
        f (Src fi)
    in
      Globals.Functions.iter do_kf

  let iter_edges_slices f proj =
    let do_edge dest (ff_caller, call) =
      f ((Slice ff_caller, dest), Some call) in
    let do_f _f_var fi =
      List.iter (do_edge (Src fi)) fi.SlicingInternals.f_called_by;
      let do_ff ff = List.iter (do_edge (Slice ff)) ff.SlicingInternals.ff_called_by in
      List.iter do_ff (SlicingMacros.fi_slices fi)
    in
    Cil_datatype.Varinfo.Hashtbl.iter do_f proj.SlicingInternals.functions

  let iter_edges_actions f proj =
    let rec do_act_edge n rq_list = match rq_list with
      | [] -> ()
      | _ :: [] -> ()
      | rq1 :: rq2 :: rq_list ->
          f (((Action (n, rq1)), (Action (n+1, rq2))), None);
          do_act_edge (n+1) (rq2 :: rq_list)
    in do_act_edge 1 proj.SlicingInternals.actions

  let iter_edges_src_fun f proj =
    let do_kf_calls kf =
      let fi = SlicingMacros.get_kf_fi proj kf in
      let doit (kf_caller,_) =
        let fi_caller = SlicingMacros.get_kf_fi proj kf_caller in
          f ((Src fi_caller, Src fi), None)
      in List.iter doit (!Db.Value.callers kf)
    in
      Globals.Functions.iter do_kf_calls

  let iter_edges_e f (_, proj) =
    let _ = match proj.SlicingInternals.actions with [] -> ()
      | rq :: _ -> f ((node_slice_callers (), (Action (1, rq))), None) in
    let _ = iter_edges_slices f proj in
    let _ = iter_edges_actions f proj in
    let _ = iter_edges_src_fun f proj in
      ()

  let color_soft_green = (0x7FFFD4)
  let color_medium_green = (0x00E598)
  let _color_soft_blue = (0x7FAAFF)
  let color_soft_orange = (0xFFD57F)
  let color_medium_orange = (0xFFB57F)
  let _color_green_yellow = (0xAAFF7F)
  let color_soft_yellow = (0xFFFFC3)
  let color_medium_yellow = (0xFFFF5D)
  let _color_pale_orange = (0xFFE1C3)
  let color_soft_pink = (0xFACDEF)
  let color_medium_pink = (0xF070D1)
  let color_soft_purple = (0xE2CDFA)

  let graph_attributes (name, _) = [`Label name]

  let default_vertex_attributes _ = [`Style [`Filled]]

  let vertex_name v = match v with
    | Src fi -> SlicingMacros.fi_name fi
    | Slice ff -> SlicingMacros.ff_name ff
    | Action (n, _) -> ("rq_"^(string_of_int n))
    | OptSlicingLevel _ -> "slicing_level"
    | OptSliceCallers _ -> "slice_callers"

  let vertex_attributes v = match v with
    | Src fi    ->
        let color = match fi_type fi with
          | Undef -> (`Fillcolor color_soft_yellow)
          | PersistSelect -> (`Fillcolor color_soft_orange)
          | Other -> (`Fillcolor color_soft_green)
        in color::[`Shape `Plaintext]
    |  Slice ff ->
        let color =  match fi_type ff.SlicingInternals.ff_fct with
          | Undef -> assert false
          | PersistSelect -> (`Fillcolor color_soft_orange)
          | Other -> (`Fillcolor color_soft_green)
        in color ::[`Shape `Ellipse]
    |  Action (_, crit) ->
        let label = Pretty_utils.sfprintf "%a" SlicingActions.print_crit crit in
        let attrib = [] in
        let attrib = (`Label label)::attrib in
        let attrib = (`Fillcolor color_soft_pink)::attrib in
        let attrib = (`Shape `Box)::attrib in
          attrib
    | OptSlicingLevel mode ->
        let label = ("SliceCalls = "^(SlicingMacros.str_level_option mode)) in
        let attrib = [] in
        let attrib = (`Label label)::attrib in
        let attrib = (`Fillcolor color_soft_purple)::attrib in
        let attrib = (`Shape `Ellipse)::attrib in
        let attrib = (`Fontsize 10)::attrib in
          attrib
    | OptSliceCallers b ->
        let label = ("SliceCallers = "^(if b then "true" else "false")) in
        let attrib = [] in
        let attrib = (`Label label)::attrib in
        let attrib = (`Fillcolor color_soft_purple)::attrib in
        let attrib = (`Shape `Ellipse)::attrib in
        let attrib = (`Fontsize 10)::attrib in
          attrib

  let default_edge_attributes _ =
    let attrib = [] in
    let attrib = (`Fontsize 10)::attrib in
      attrib

  let edge_attributes (e, call) =
    let attrib = match e with
    | (Src _, Src _) -> [`Style [`Invis]]
    | (OptSliceCallers _, _) -> [`Style [`Invis]]
    | (_, OptSliceCallers _) -> [`Style [`Invis]]
    | _ -> []
    in match call with None -> attrib
      | Some call -> (`Label (string_of_int call.sid)):: attrib

  let get_subgraph v =
    let mk_subgraph name attrib =
      let attrib = (*(`Label name) ::*) (`Style [`Filled]) :: attrib in
          Some { Graph.Graphviz.DotAttributes.sg_name= name;
                 sg_parent = None;
                 sg_attributes = attrib }
    in
    let f_subgraph fi =
      let name = SlicingMacros.fi_name fi in
      let attrib = [`Label ""] in
      let color = match fi_type fi with
        | Undef -> (`Fillcolor color_medium_yellow)
        | PersistSelect -> (`Fillcolor color_medium_orange)
        | Other -> (`Fillcolor color_medium_green)
      in let attrib = color :: attrib in
        mk_subgraph name attrib
    in
    let rq_subgraph =
      let name = "Requests" in
      let attrib = [] in
      let attrib = (`Fillcolor color_medium_pink) :: attrib in
      let attrib = (`Label name) :: attrib in
        mk_subgraph name attrib
    in match v with
      | Src fi -> f_subgraph fi
      | Slice ff -> f_subgraph ff.SlicingInternals.ff_fct
      | Action _ -> rq_subgraph
      | OptSlicingLevel _ | OptSliceCallers _ -> rq_subgraph

end

module PrintProjGraph = Graph.Graphviz.Dot(PrintProject)

let build_dot_project filename title project =
  let file = open_out filename in
    PrintProjGraph.output_graph file (title, project);
    close_out file

let print_fct_stmts fmt (_proj, kf) =
  try
    let pdg = !Db.Pdg.get kf in
    print_fct_from_pdg fmt pdg;
    Format.pp_print_flush fmt ()
  with Not_found -> ()


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
