(* Frama-C journal generated at 15:47 the 08/10/2008 *)

(* Running *)
let start () =
 let () = Journal.run () in
 let () = Cmdline.Files.add "stubs.c" in
 let () = File.init_from_cmdline () in
 (* Finished *)
 Journal.finished ()

let () =
 try start ()
 with e -> Format.eprintf "Journal raised an exception: %s" (Printexc.to_string e)
