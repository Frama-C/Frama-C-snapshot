(* Frama-C journal generated at 16:32 the 05/09/2008 *)

(* Running *)
let () = Journal.run ()
let () = Cmdline.ForceValues.set true
let () = Cmdline.Files.set ["bind.c"; ]
let () = File.init_from_cmdline ()
let () = !Db.Syntactic_callgraph.dump ()
(* exception raised on: *)
let __ : unit = !Db.Value.compute ()
(* Finished *)
let () = Journal.finished ()
