(* Frama-C journal generated at 16:10 the 26/08/2008 *)

(* Running *)
let () = Journal.run ()
let () = Cmdline.ForceValues.set true
let () = Cmdline.Files.set ["expands_vars_bad.c"; ]
let () = Cmdline.Files.set ["../bind.c"; "expands_vars_bad.c"; ]
let () = File.init_from_cmdline ()
let () = !Db.Syntactic_callgraph.dump ()
let () = !Db.Value.compute ()
(* Finished *)
let () = Journal.finished ()
