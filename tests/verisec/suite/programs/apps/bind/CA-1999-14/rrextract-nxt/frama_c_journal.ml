(* Frama-C journal generated at 15:18 the 08/10/2008 *)

(* Running *)
let start () =
 let () = Journal.run () in
 let () = Cmdline.ForceValues.set true in
 let () = Cmdline.Files.add "simp_bad.c" in
 let () = Cmdline.Files.add "../bind.c" in
 let () = Cmdline.Files.add "../../../../../lib/stubs.c" in
 let () = File.init_from_cmdline () in
 let () = !Db.Value.compute () in
 (* Finished *)
 Journal.finished ()

let () =
 try start ()
 with e -> Format.eprintf "Journal raised an exception: %s" (Printexc.to_string e)
