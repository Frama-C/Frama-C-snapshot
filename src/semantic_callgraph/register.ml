(* $Id: register.ml,v 1.6 2008/06/19 14:42:45 uid568 Exp $ *)

open Db_types
open Db

module SGraph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (Kernel_function)
    (struct include Cilutil.StmtComparable let default = Cil.dummyStmt end)

module SCQueue =
  Computation.Queue
    (Kernel_datatype.KernelFunction)
    (struct
       let name = Project.Computation.Name.make "SCQueue"
       let dependencies = [ Value.self ]
     end)

(* Callgraph computation does not deal with Project because it is not used
   outside [topologically_iter_on_functions]. *)
let callgraph () =
  let g = SGraph.create () in
  !Value.compute ();
  Globals.Functions.iter
    (fun kf ->
       if !Value.is_called kf then SGraph.add_vertex g kf;
       List.iter
	 (fun (caller,call_sites) ->
	    List.iter
	      (fun call_site -> SGraph.add_edge_e g (kf,call_site,caller))
	      call_sites)
	 (!Value.callers kf));
  g

let topologically_iter_on_functions =
  let module T = Graph.Topological.Make(SGraph) in
  fun f ->
    (* compute on need *)
    if SCQueue.is_empty () then T.iter SCQueue.add (callgraph ());
    SCQueue.iter f

let () =
  Db.Semantic_Callgraph.topologically_iter_on_functions :=
    topologically_iter_on_functions

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
