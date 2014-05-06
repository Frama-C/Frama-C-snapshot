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
open Cil_datatype

class coverageAuxVisitor = object(self)
  inherit Visitor.frama_c_inplace

  (* Visit the body and the spec of a function *)
  method private visit_function vi =
    if Metrics_base.consider_function vi then
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

end

(* Reachability metrics: from a given entry point
   compute a conservative estimation
   of the functions that can be transitively called *)
class callableFunctionsVisitor = object(self)
  inherit coverageAuxVisitor as super

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
      if Cil.isFunctionType vi.vtype then
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
          if Cil.isFunctionType vi.vtype
          then self#visit_function vi
          else ignore (self#visit_non_function_var vi)
        end;
    done;
    callable

end

class deadCallsVisitor fmt ~syntactic ~semantic initializers =
  let unseen = Varinfo.Set.diff syntactic semantic in
object(self)
  inherit coverageAuxVisitor

  val mutable current_initializer = None

  (* When an unseen function is reachable by the body of a function reached,
     or inside an initializer, display the information *)
  method private reached_vi vi =
    if Metrics_base.consider_function vi && Varinfo.Set.mem vi unseen then
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
        if Varinfo.Set.mem (Kernel_function.get_vi f) semantic then
          let mess =
            match self#current_stmt with
              | Some {skind = Instr (Call (_, {enode = Lval (Var v, _)}, _, _))}
                  when Varinfo.equal v vi -> "calls"
              | _ -> "references"
          in
          Format.fprintf fmt
            "@[<h>Function %a %s %s (at %a)@]@ "
            Kernel_function.pretty f mess vi.vname
            Location.pretty (Cil.CurrentLoc.get ())

  method! vvrbl vi =
    if Cil.isFunctionType vi.vtype then self#reached_vi vi;
    Cil.SkipChildren (* no children anyway *)


  method compute_and_print =
    if not (Varinfo.Set.is_empty unseen) || initializers <> [] then begin
      Format.fprintf fmt "@[<v>%a@ "
        (Metrics_base.mk_hdr 2) "References to non-analyzed functions";
      Varinfo.Set.iter self#visit_function semantic;
      List.iter (fun (vinit, init) ->
                   current_initializer <- Some vinit;
                   ignore (Visitor.visitFramacInit
                             (self:>Visitor.frama_c_visitor)
                             vinit NoOffset init);
                   current_initializer <- None;
                ) initializers;
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

let compute_coverage_by_fun semantic =
  let one_fun vi acc =
    try
      let kf = Globals.Functions.get vi in
      let dec = Kernel_function.get_definition kf in
      let vis = new coverageByFun in
      ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) dec);
      let (total, value) = vis#result in
      let percent = (float_of_int value) /. (float_of_int total) *. 100. in
      (kf, total, value, percent) :: acc
    with Kernel_function.No_Definition -> acc
  in
  let res = Varinfo.Set.fold one_fun semantic [] in
  List.sort (fun (_, _, _, p1) (_, _, _, p2) -> compare p2 p1) res


let pp_unreached_calls fmt ~syntactic ~semantic initializers =
  let v = new deadCallsVisitor fmt ~syntactic ~semantic initializers in
  v#compute_and_print
;;

let compute_syntactic kf =
  let vis = new callableFunctionsVisitor in
  let res = vis#compute (Kernel_function.get_vi kf) in
  res, vis#initializers
;;

let compute_semantic () =
  assert (Db.Value.is_computed ());
  let res = ref Varinfo.Set.empty in
  (* Just iter on all the functions and consult the appropriate table *)
  Globals.Functions.iter
    (fun kf ->
       if !Db.Value.is_called kf then
         res := Varinfo.Set.add (Kernel_function.get_vi kf) !res
    );
  !res
;;

let pp_fun_set_by_file fmt set =
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
         if Metrics_base.consider_function fvinfo then
           let fname = Metrics_base.file_of_vinfodef fvinfo in
           add_binding acc fname fvinfo
         else acc
      ) set Datatype.String.Map.empty
  in
  Format.fprintf fmt "@[<v 0>";
  Datatype.String.Map.iter
    (fun fname fvinfoset ->
      Format.fprintf fmt "@[<hov 2><%s>:@ %a@]@ "
        (Filepath.pretty fname)
        (fun fmt vinfoset ->
          Varinfo.Set.iter
            (fun vinfo -> Format.fprintf fmt "%a;@ " Printer.pp_varinfo vinfo)
            vinfoset)
        fvinfoset
    ) map;
    Format.fprintf fmt "@]"
;;

type reachable_functions = {
  syntactic : Varinfo.Set.t;
  semantic : Varinfo.Set.t;
}
;;

let percent_coverage rfun =
  let nsyn = Varinfo.Set.cardinal rfun.syntactic
  and nsem = Varinfo.Set.cardinal rfun.semantic in
  let percent = (float_of_int nsem) /. (float_of_int nsyn) *. 100.0 in
  percent
;;

let all_funs () =
  Globals.Functions.fold
    (fun kf acc ->
      let vi = Kernel_function.get_vi kf in
      if Metrics_base.consider_function vi
      then Varinfo.Set.add vi acc
      else acc)
    Varinfo.Set.empty

let compute () =
  !Db.Value.compute ();
  let semantic = compute_semantic () in
  let main = fst (Globals.entry_point ()) in
  let syntactic, initializers = compute_syntactic main in
  { syntactic = syntactic; semantic = semantic; },
  initializers
;;

let pp_value_coverage () =
  assert (Db.Value.is_computed ());
  let reachable, initializers = compute () in
  let all = all_funs () in
  let syntactic = reachable.syntactic
  and semantic = reachable.semantic in
  let unseen = Varinfo.Set.diff syntactic semantic in
  let unseen_num = Varinfo.Set.cardinal unseen in
  let nall = Varinfo.Set.cardinal all in
  let nsyn = Varinfo.Set.cardinal syntactic
  and nsem = Varinfo.Set.cardinal semantic in
  let percent = (float_of_int nsem) *. 100.0 /. (float_of_int nsyn) in
  (fun fmt ->
     Format.fprintf fmt "@[<v 0>\
       %a@ \
       Syntactically reachable functions = %d (out of %d)@ \
       Semantically reached functions = %d@ \
       Coverage estimation = %.1f%% @ "
       (Metrics_base.mk_hdr 1) "Value coverage statistics"
       nsyn nall nsem percent;
     if unseen_num > 0 then
       Format.fprintf fmt "@ @[<v 2>Unseen functions (%d) =@ %a@]"
         unseen_num pp_fun_set_by_file unseen;
     Format.fprintf fmt "@]"
  ),
  (fun fmt ->
     pp_unreached_calls fmt ~syntactic ~semantic initializers)
;;

let pp_reached_from_function fmt kf =
  let syntactic, _ = compute_syntactic kf in
  let all = all_funs () in
  let card_syn = Varinfo.Set.cardinal syntactic in
  let title_reach = Pretty_utils.sfprintf "%a: %d"
    Kernel_function.pretty kf card_syn
  in
  let card_all = Varinfo.Set.cardinal all in
  let title_unreach = Pretty_utils.sfprintf "%a: %d"
    Kernel_function.pretty kf (card_all - card_syn)
  in
  Format.fprintf fmt "@[<v 0>%a@ %a@ %a@ %a@]"
    (Metrics_base.mk_hdr 2)
    (Format.sprintf "Functions syntactically reachable from %s" title_reach)
    pp_fun_set_by_file syntactic
    (Metrics_base.mk_hdr 2)
    (Format.sprintf "Functions syntactically unreachable from %s" title_unreach)
    pp_fun_set_by_file (Varinfo.Set.diff all syntactic)


let pp_stmts_reached_by_function fmt =
  let semantic = compute_semantic () in
  let l = compute_coverage_by_fun semantic in
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


(* Reexport a simpler function *)
let compute_syntactic kf = fst (compute_syntactic kf)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
