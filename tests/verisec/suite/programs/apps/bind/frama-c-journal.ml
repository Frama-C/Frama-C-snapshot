(* Frama-C journal generated at 16:31 the 05/09/2008 *)

(* Running *)
let () = Journal.run ()
let () = Cmdline.ForceValues.set true
let () = Cmdline.Files.set ["loop_ok.c"; ]
let () = File.init_from_cmdline ()
let () = !Db.Syntactic_callgraph.dump ()
(* exception raised on: *)
let __ : unit = !Db.Value.compute ()
(* Finished *)
let () = Journal.finished ()
