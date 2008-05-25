(* $Id: register.ml,v 1.5 2007/12/10 15:32:39 uid568 Exp $ *)

open Db_types
open Db

module SGraph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (Kernel_function)
    (struct include Cilutil.StmtComparable let default = Cil.dummyStmt end)

module SCQueue =
  Computation.Ref
    (struct include Kernel_datatype.KF_Queue let default = Queue.create () end)
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
    let q = SCQueue.get () in
    (* compute on need *)
    if Queue.is_empty q then T.iter (fun kf -> Queue.add kf q) (callgraph ());
    Queue.iter f q

let () =
  Db.Semantic_Callgraph.topologically_iter_on_functions :=
    topologically_iter_on_functions

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
