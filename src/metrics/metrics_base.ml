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

open Cil_types (* vname, vaddrof *)
open Cil_datatype
;;

(* Formatting html with Format.formatters *)
let html_tag_functions =
  let mark_open_tag t = Format.sprintf "<%s>" t
  and mark_close_tag t =
    try
      let index = String.index t ' ' in
      Format.sprintf "</%s>" (String.sub t 0 index)
    with
      | Not_found -> Format.sprintf "</%s>" t
  and print_open_tag _ = ()
  and print_close_tag _ = ()
  in
  { Format.mark_open_tag = mark_open_tag;
    Format.mark_close_tag = mark_close_tag;
    Format.print_open_tag = print_open_tag;
    Format.print_close_tag = print_close_tag;
  }
;;

(* Utility function to have underlines the same length as the title.
   Underlines follow reStructuredText header conventions.
*)
let mk_hdr level ppf hdr_strg =
  let c = match level with
    | 1 -> '='
    | 2 -> '-'
    | 3 -> '~'
    | _ -> assert false
  in
  let len = String.length hdr_strg in
  let underline = String.make len c in
  Format.fprintf ppf "@[<v 0>%s@ %s@]" hdr_strg underline ;
;;

(** Defining base metrics and operations on those *)
module BasicMetrics = struct
(** Record type to compute cyclomatic complexity *)

  type t = {
    cfile_name : string;
    cfunc_name : string;
    cslocs: int;
    cifs: int;
    cloops: int;
    ccalls: int;
    cgotos: int;
    cassigns: int;
    cexits: int;
    cfuncs: int;
    cptrs: int;
    cdecision_points: int;
    cglob_vars: int;
  }
  ;;

  let empty_metrics =
    { cfile_name = "";
      cfunc_name = "";
      cslocs = 0;
      cifs = 0;
      cloops = 0;
      ccalls = 0;
      cgotos = 0;
      cassigns = 0;
      cexits = 0;
      cfuncs = 0;
      cptrs = 0;
      cdecision_points = 0;
      cglob_vars = 0;
    }
  ;;


  let apply_then_set f metrics = metrics := f !metrics ;;

  let incr_slocs metrics = { metrics with cslocs = succ metrics.cslocs ;} ;;

  let incr_assigns metrics =
    { metrics with cassigns = succ metrics.cassigns ;}
  ;;

  let incr_calls metrics = { metrics with ccalls = succ metrics.ccalls ;} ;;

  let incr_exits metrics = { metrics with cexits = succ metrics.cexits ;} ;;

  let incr_funcs metrics = { metrics with cfuncs = succ metrics.cfuncs ;} ;;

  let incr_gotos metrics = { metrics with cgotos = succ metrics.cgotos ;} ;;

  let incr_ifs metrics = { metrics with cifs = succ metrics.cifs ;} ;;

  let incr_loops metrics = { metrics with cloops = succ metrics.cloops ;} ;;

  let incr_ptrs metrics = { metrics with cptrs = succ metrics.cptrs ;} ;;

  let incr_dpoints metrics =
    { metrics with cdecision_points = succ metrics.cdecision_points ;}
  ;;

  let incr_glob_vars metrics = { metrics with cglob_vars = succ metrics.cglob_vars ;} ;;



  (* Compute cyclomatic complexity of a given metrics record *)
  let cyclo metrics =
    metrics.cdecision_points - metrics.cexits + 2
  ;;

  let labels =
      [ "Sloc"; "Decision point"; "Global variables"; "If"; "Loop";  "Goto";
        "Assignment"; "Exit point"; "Function"; "Function call";
        "Pointer dereferencing";
        "Cyclomatic complexity";
      ]
  ;;

  let str_values metrics =
    List.map string_of_int
      [ metrics.cslocs; metrics.cdecision_points; metrics.cglob_vars; metrics.cifs;
        metrics.cloops; metrics.cgotos; metrics.cassigns;
        metrics.cexits; metrics.cfuncs; metrics.ccalls;
        metrics.cptrs; cyclo metrics;
      ]
  ;;

  let to_list metrics =
    List.map2 (fun x y -> [ x; y; ]) labels (str_values metrics)
  ;;

(* Pretty print metrics as text eg. in stdout *)
  let pp_base_metrics fmt metrics =
    let heading =
      if metrics.cfile_name = "" && metrics.cfunc_name = "" then
      (* It is a global metrics *)
        "Global metrics"
      else Format.sprintf "Stats for function <%s/%s>"
        (Filepath.pretty metrics.cfile_name) metrics.cfunc_name
    in
    Format.fprintf fmt "@[<v 0>%a @ %a@]"
      (mk_hdr 1) heading
      ((fun l1 ppf l2 ->
        List.iter2 (fun x y -> Format.fprintf ppf "%s = %s@ " x y)
          l1 l2) labels)
      (str_values metrics)
  ;;

(* Dummy utility functions for pretty printing simple types *)
  let pp_strg fmt s = Format.fprintf fmt "%s" s
  and pp_int fmt n = Format.fprintf fmt "%d" n
  ;;

  type cell_type =
    | Classic
    | Entry
    | Result
  ;;

  let cell_type_to_string = function
    | Entry -> "entry"
    | Result -> "result"
    | Classic -> "classic"
  ;;

  let pp_cell_type_html fmt cell_type =
    Format.fprintf fmt "class=\"%s\"" (cell_type_to_string cell_type)
  ;;

  (* Pretty print a HTML cell given a pretty printing function [pp_fun]
     and a value [pp_arg]
  *)
  let pp_cell cell_type pp_fun fmt pp_arg =
    Format.fprintf fmt "@{<td %a>%a@}"
      pp_cell_type_html cell_type
      pp_fun pp_arg
  ;;

  let pp_cell_default = pp_cell Classic;;

  let pp_base_metrics_as_html_row fmt metrics =
    Format.fprintf fmt "\
   @[<v 0>\
   @{<tr>@[<v 2>@ \
     @[<v 0>%a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ %a@ @]@]\
   @}@ @]"
      (pp_cell Entry pp_strg) metrics.cfunc_name
      (pp_cell_default pp_int) metrics.cifs
      (pp_cell_default pp_int) metrics.cassigns
      (pp_cell_default pp_int) metrics.cloops
      (pp_cell_default pp_int) metrics.ccalls
      (pp_cell_default pp_int) metrics.cgotos
      (pp_cell_default pp_int) metrics.cptrs
      (pp_cell_default pp_int) metrics.cexits
      (pp_cell Result pp_int) (cyclo metrics)
  ;;

end (* End of BasicMetrics *)

(** {3 Filename utilities} *)

exception No_suffix;;
let get_suffix filename =
  try
    let slen = String.length filename in
    let last_idx = pred slen in
    let last_dot_idx = String.rindex_from filename last_idx '.' in
    if last_dot_idx < last_idx then
      String.sub filename (succ last_dot_idx) (slen - last_dot_idx - 1)
    else ""
  with
    | Not_found -> raise No_suffix
;;

type output_type =
  | Html
  | Text
;;

let get_file_type filename =
  try
    match get_suffix filename with
      | "html" | "htm" -> Html
      | "txt" | "text" -> Text
      | s ->
        Metrics_parameters.fatal
          "Unknown file extension %s. Cannot produce output.@." s
  with
    | No_suffix ->
       Metrics_parameters.fatal
         "File %s has no suffix. Cannot produce output.@." filename

(** Map of varinfos sorted by name (and not by ids) *)
module VInfoMap = struct
  include FCMap.Make (
    struct
      let compare v1 v2 = Pervasives.compare v1.vname v2.vname;;
      type t = Cil_types.varinfo
    end
  )

  let to_varinfo_map vmap =
    fold (fun k v mapacc -> Varinfo.Map.add k v mapacc) vmap Varinfo.Map.empty
  ;;
end
;;


(** Other pretty-printing and formatting utilities *)
let pretty_set iter fmt s =
  Format.fprintf fmt "@[";
  iter
    (fun f n ->
      Format.fprintf fmt "%s %s(%d call%s);@ "
        f.Cil_types.vname
        (if f.vaddrof then "(address taken) " else "")
        n (if n > 1 then "s" else ""))
    s;
  Format.fprintf fmt "@]"

let is_entry_point vinfo times_called =
  times_called = 0 && not vinfo.vaddrof
;;

let number_entry_points fold fs =
  fold
    (fun fvinfo n acc -> if is_entry_point fvinfo n then succ acc else acc)
    fs 0
;;

let pretty_entry_points iter fmt fs =
  let print fmt =
    iter
      (fun fvinfo n  ->
        if is_entry_point fvinfo n
        then Format.fprintf fmt "%s;@ " fvinfo.vname)
  in
  Format.fprintf fmt "@[<hov 1>%a@]" print fs;
;;

let map_cardinal_varinfomap (map:'a Varinfo.Map.t) =
    Varinfo.Map.fold
      (fun _funcname _ cardinal -> succ cardinal)
      map 0
;;

(* Utilities for CIL ASTs *)

let file_of_vinfodef fvinfo =
  let kf = Globals.Functions.get fvinfo in
  let decl_loc1, _decl_loc2 =
  match kf.fundec with
    | Definition (_, loc) -> loc
    | Declaration (_, _, _, loc) -> loc
  in decl_loc1.Lexing.pos_fname
;;

let file_of_fundef (fun_dec: Cil_types.fundec) =
  file_of_vinfodef fun_dec.svar
;;

(* Utilities for Cabs ASTs *)

let extract_fundef_name sname =
  match sname with
    | _spec, (the_name, _, _, _) -> the_name
;;

let get_filename fdef =
  match fdef with
    | Cabs.FUNDEF(_, _, _, (loc1, _), _loc2) ->   loc1.Lexing.pos_fname
    | _ -> assert false
;;

let consider_function vinfo =
  not (!Db.Value.mem_builtin vinfo.vname
       || Ast_info.is_frama_c_builtin vinfo.vname
       || Cil.is_unused_builtin vinfo
  )

let float_to_string f =
  let s = Format.sprintf "%F" f in
  let len = String.length s in
  let plen = pred len in
  if s.[plen] = '.' then String.sub s 0 plen else Format.sprintf "%.2f" f

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
