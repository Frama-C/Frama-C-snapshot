(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
open Cil_datatype

let compare_vi_names v1 v2 = Extlib.compare_ignore_case v1.vname v2.vname

class coverageAuxVisitor ~libc = object(self)
  inherit Visitor.frama_c_inplace

  (* Visit the body and the spec of a function *)
  method private visit_function vi =
    if Metrics_base.consider_function ~libc vi then
      let kf = Globals.Functions.get vi in
      let self = (self :> Visitor.frama_c_visitor) in
      (* Visit the spec. There might be references to function pointers in
         the assigns *)
      let spec = Annotations.funspec ~populate:false kf in
      ignore (Visitor.visitFramacFunspec self spec);
      (try
         (* Visit the body if we have one *)
         let fundec = Kernel_function.get_definition kf in
         ignore (Visitor.visitFramacFunction self fundec);
       with Kernel_function.No_Definition -> ())

  (* Visit the initializer of the given var, if it exists, and returns it *)
  method private visit_non_function_var vi =
    if Metrics_base.consider_variable ~libc vi then
      try
        (* Visit the initializer if there is one *)
        let init = Globals.Vars.find vi in
        match init with
        | { init = None } -> None
        | { init = Some init } ->
          ignore (Visitor.visitFramacInit (self:>Visitor.frama_c_visitor)
                    vi NoOffset init);
          Some init
      with Not_found -> (* not a global *) None
    else None

end

(* Reachability metrics: from a given entry point
   compute a conservative estimation
   of the functions that can be transitively called *)
class callableFunctionsVisitor ~libc = object(self)
  inherit coverageAuxVisitor ~libc as super

  (* Functions reachable syntactically *)
  val mutable callable = Varinfo.Set.empty

  (* All globals initializers visited *)
  val mutable initializers = []
  method initializers = initializers

  (* All varinfos visited so far. Used to avoid looping *)
  val visited = Varinfo.Hashtbl.create 17
  (* Varinfos remaining to visit *)
  val todo = Stack.create ()

  method already_seen vi =
    Varinfo.Hashtbl.mem visited vi

  (* Each time we see a variable, mark it as to be visited. If it is a function,
     consider it is called *)
  method! vvrbl vi =
    if not (self#already_seen vi) then begin
      if Cil.isFunctionType vi.vtype &&
           Metrics_base.consider_function ~libc vi
      then
        callable <- Varinfo.Set.add vi callable;
      Stack.push vi todo;
    end;
    Cil.SkipChildren (* no children anyway *)

  method! visit_non_function_var vi =
    let r = super#visit_non_function_var vi in
    (match r with
       | None -> ()
       | Some init -> initializers <- (vi, init) :: initializers
    );
    r

  method compute vi =
    (* Initialisation *)
    Stack.clear todo;
    Stack.push vi todo;
    Varinfo.Hashtbl.clear visited;
    callable <- Varinfo.Set.singleton vi;
    (* Reach fixpoint *)
    while not (Stack.is_empty todo) do
      let vi = Stack.pop todo in
      if not (self#already_seen vi) then
        begin
          Metrics_parameters.debug "Coverage: visiting %s" vi.vname;
          Varinfo.Hashtbl.add visited vi ();
          if Cil.isFunctionType vi.vtype && Metrics_base.consider_function ~libc vi
          then self#visit_function vi
          else ignore (self#visit_non_function_var vi)
        end;
    done;
    callable

end

type coverage_metrics = {
  syntactic: Cil_datatype.Varinfo.Set.t;
  semantic: Cil_datatype.Varinfo.Set.t;
  initializers: (Cil_types.varinfo * Cil_types.init) list;
}

class deadCallsVisitor fmt ~libc cov_metrics =
  let unseen = Varinfo.Set.diff cov_metrics.syntactic cov_metrics.semantic in
object(self)
  inherit coverageAuxVisitor ~libc

  val mutable current_initializer = None

  (* When an unseen function is reachable by the body of a function reached,
     or inside an initializer, display the information *)
  method private reached_fun vi =
    if Metrics_base.consider_function ~libc vi && Varinfo.Set.mem vi unseen then
      match self#current_kf with
      | None ->
          (match current_initializer with
             | None -> assert false
             | Some vinit ->
                 Format.fprintf fmt
                   "@[<h>Initializer of %s references %s (at %t)@]@ "
                   vinit.vname vi.vname Cil.pp_thisloc
          )
      | Some f ->
        if Varinfo.Set.mem (Kernel_function.get_vi f) cov_metrics.semantic then
          let mess =
            match self#current_stmt with
              | Some
                  {skind =
                     Instr (
                       Call (_, {enode = Lval (Var v, NoOffset)}, _, _)
                     | Local_init (_, ConsInit(v, _, _),_))}
                  when Varinfo.equal v vi -> "calls"
              | _ -> "references"
          in
          Format.fprintf fmt
            "@[<h>Function %a %s %s (at %a)@]@ "
            Kernel_function.pretty f mess vi.vname
            Location.pretty (Cil.CurrentLoc.get ())

  method! vvrbl vi =
    if Cil.isFunctionType vi.vtype then self#reached_fun vi;
    Cil.SkipChildren (* no children anyway *)

  (* uses initializers *)
  method compute_and_print =
    if not (Varinfo.Set.is_empty unseen) || cov_metrics.initializers <> [] then begin
      Format.fprintf fmt "@[<v>%a@ "
        (Metrics_base.mk_hdr 2) "References to non-analyzed functions";
      let sorted_semantic =
        List.sort compare_vi_names (Varinfo.Set.elements cov_metrics.semantic)
      in
      List.iter self#visit_function sorted_semantic;
      let sorted_initializers =
        List.sort (fun (v1, _) (v2, _) -> compare_vi_names v1 v2) cov_metrics.initializers
      in
      List.iter (fun (vinit, init) ->
                   current_initializer <- Some vinit;
                   ignore (Visitor.visitFramacInit
                             (self:>Visitor.frama_c_visitor)
                             vinit NoOffset init);
                   current_initializer <- None;
                ) sorted_initializers;
      Format.fprintf fmt "@]"
    end

end

class coverageByFun = object
  inherit Visitor.frama_c_inplace

  val mutable total = 0
  val mutable value = 0

  method! vstmt s =
    total <- total + 1;
    if Db.Value.is_reachable_stmt s then value <- value + 1;
    Cil.DoChildren

  method result = (total, value)
end

module Kf_Coverage = Kernel_function.Make_Table
    (Datatype.Triple (Datatype.Int) (Datatype.Int) (Datatype.Float))
    (struct
      let name = "Metrics_coverage.Kf_coverage"
      let size = 7
      let dependencies = [ Db.Value.self; Metrics_parameters.Libc.self ]
    end)

let is_computed_by_fun () = Kf_Coverage.length () > 0

let get_coverage = Kf_Coverage.find

let compute_coverage_for kf =
  try
    let dec = Kernel_function.get_definition kf in
    let vis = new coverageByFun in
    ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) dec);
    let (total, value) = vis#result in
    let percent = (float_of_int value) /. (float_of_int total) *. 100. in
    Kf_Coverage.replace kf (total, value, percent)
  with Kernel_function.No_Definition -> ()

let compute_coverage_by_fun () =
  if Db.Value.is_computed () && not (is_computed_by_fun ())
  then
    let libc = Metrics_parameters.Libc.get () in
    Globals.Functions.iter
      (fun kf ->
         if !Db.Value.is_called kf &&
            Metrics_base.consider_function ~libc (Kernel_function.get_vi kf)
         then compute_coverage_for kf)

let clear_coverage_by_fun = Kf_Coverage.clear

let compute_syntactic ~libc kf =
  let vis = new callableFunctionsVisitor ~libc in
  let res = vis#compute (Kernel_function.get_vi kf) in
  res, vis#initializers
;;

let compute_semantic ~libc =
  assert (Db.Value.is_computed ());
  let res = ref Varinfo.Set.empty in
  (* Just iter on all the functions and consult the appropriate table *)
  Globals.Functions.iter
    (fun kf ->
       if !Db.Value.is_called kf &&
          Metrics_base.consider_function ~libc
            (Kernel_function.get_vi kf)
       then
         res := Varinfo.Set.add (Kernel_function.get_vi kf) !res
    );
  !res
;;

class syntactic_printer ~libc reachable = object(self)

  method private all_funs =
    Globals.Functions.fold
      (fun kf acc ->
         let vi = Kernel_function.get_vi kf in
         if Metrics_base.consider_function ~libc vi
         then Varinfo.Set.add vi acc
         else acc)
      Varinfo.Set.empty

  method private pp_fun_set_by_file fmt set =
    let add_binding map filename fvinfo =
      let set =
        try
          let x = Datatype.String.Map.find filename map in
          Varinfo.Set.add fvinfo x
        with Not_found -> Varinfo.Set.add fvinfo Varinfo.Set.empty
      in Datatype.String.Map.add filename set map
    in
    let map =
      Varinfo.Set.fold
        (fun fvinfo acc ->
           if Metrics_base.consider_function ~libc fvinfo then
             let fname = Metrics_base.file_of_vinfodef fvinfo in
             add_binding acc (Filepath.pretty fname) fvinfo
           else acc
        ) set Datatype.String.Map.empty
    in
    Format.fprintf fmt "@[<v 0>";
    Datatype.String.Map.iter
      (fun pretty_fname fvinfoset ->
         Format.fprintf fmt "@[<hov 2><%s>:@ %a@]@ "
           pretty_fname
           (fun fmt vinfoset ->
              let vars = Varinfo.Set.elements vinfoset in
              let sorted_vars = List.sort compare_vi_names vars in
              List.iter
                (fun vinfo -> Format.fprintf fmt "%a;@ " Printer.pp_varinfo vinfo)
                sorted_vars
           ) fvinfoset
      ) map;
    Format.fprintf fmt "@]"

  method pp_reached_from_function fmt kf =
    let card_syn = Varinfo.Set.cardinal reachable in
    let title_reach = Format.asprintf "%a: %d"
        Kernel_function.pretty kf card_syn
    in
    let all = self#all_funs in
    let card_all = Varinfo.Set.cardinal all in
    let title_unreach = Format.asprintf "%a: %d"
        Kernel_function.pretty kf (card_all - card_syn)
    in
    Format.fprintf fmt "@[<v 0>%a@ %a@ %a@ %a@]"
      (Metrics_base.mk_hdr 2)
      (Format.sprintf "Functions syntactically reachable from %s" title_reach)
      self#pp_fun_set_by_file reachable
      (Metrics_base.mk_hdr 2)
      (Format.sprintf "Functions syntactically unreachable from %s" title_unreach)
      self#pp_fun_set_by_file (Varinfo.Set.diff all reachable)

end


class semantic_printer ~libc (cov_metrics : coverage_metrics) = object(self)
  inherit syntactic_printer ~libc cov_metrics.syntactic

  (* uses semantic and initializers *)
  method pp_unreached_calls fmt =
    let v = new deadCallsVisitor ~libc fmt cov_metrics in
    v#compute_and_print

  (* uses semantic *)
  method pp_value_coverage fmt =
    assert (Db.Value.is_computed ());
    let all = self#all_funs in
    let syntactic = cov_metrics.syntactic
    and semantic = cov_metrics.semantic in
    let unseen = Varinfo.Set.diff syntactic semantic in
    let unseen_num = Varinfo.Set.cardinal unseen in
    let nall = Varinfo.Set.cardinal all in
    let nsyn = Varinfo.Set.cardinal syntactic
    and nsem = Varinfo.Set.cardinal semantic in
    let percent = (float_of_int nsem) *. 100.0 /. (float_of_int nsyn) in
    Format.fprintf fmt "@[<v 0>\
                        %a@ \
                        Syntactically reachable functions = %d (out of %d)@ \
                        Semantically reached functions = %d@ \
                        Coverage estimation = %.1f%% @ "
      (Metrics_base.mk_hdr 1) "Value coverage statistics"
      nsyn nall nsem percent;
    if unseen_num > 0 then
      Format.fprintf fmt "@ @[<v 2>Unreached functions (%d) =@ %a@]"
        unseen_num self#pp_fun_set_by_file unseen;
    Format.fprintf fmt "@]"

  (* uses semantic *)
  method pp_stmts_reached_by_function fmt =
    compute_coverage_by_fun ();
    let l =
      Kf_Coverage.fold
        (fun kf (total, value, percent) l -> (kf, total, value, percent) :: l)
        []
    in
    (* Sort by percentage (higher first),
       then sort by name (for same percentage) *)
    let l =
      List.sort (fun (kf1, _, _, p1) (kf2, _, _, p2) ->
          let c = compare p2 p1 in
          if c = 0 then compare kf1 kf2 else c
        ) l
    in
    let sum_total, sum_value = List.fold_left
        (fun (at, av) (_, t, v, _) -> at+t, av+v) (0, 0) l in
    let percent = 100. *. (float_of_int sum_value) /. (float_of_int sum_total) in
    Format.fprintf fmt "@[<v 0>%a@ \
                        %d stmts in analyzed functions, %d stmts analyzed (%.1f%%)@ "
      (Metrics_base.mk_hdr 2) "Statements analyzed by Value"
      sum_total sum_value percent;
    List.iter (fun (kf, total, visited, percent) ->
        Format.fprintf fmt "%a: %d stmts out of %d (%.1f%%)@ "
          Kernel_function.pretty kf visited total percent
      ) l;
    Format.fprintf fmt "@]"

end


let percent_coverage cov_metrics =
  let nsyn = Varinfo.Set.cardinal cov_metrics.syntactic
  and nsem = Varinfo.Set.cardinal cov_metrics.semantic in
  let percent = (float_of_int nsem) /. (float_of_int nsyn) *. 100.0 in
  percent
;;

let compute ~libc =
  assert (Db.Value.is_computed ());
  let semantic = compute_semantic ~libc in
  let main = fst (Globals.entry_point ()) in
  let syntactic, initializers = compute_syntactic ~libc main in
  { syntactic; semantic; initializers }
;;

(* Reexport a simpler function *)
let compute_syntactic ~libc kf =
  fst (compute_syntactic ~libc kf)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
