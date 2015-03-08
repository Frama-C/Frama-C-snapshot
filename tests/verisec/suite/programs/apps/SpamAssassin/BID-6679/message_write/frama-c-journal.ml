(* Frama-C journal generated at 15:32 the 02/09/2008 *)

(* Running *)
let () = Journal.run ()
let () = Cmdline.ForceValues.set true
let () = Cmdline.Files.set ["loop_ok.c"; ]
let () = File.init_from_cmdline ()
let () = !Db.Syntactic_callgraph.dump ()
let () = !Db.Value.compute ()
(* Finished *)
let () = Journal.finished ()
