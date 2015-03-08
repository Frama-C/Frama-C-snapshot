(* Frama-C journal generated at 17:37 the 24/04/2009 *)

(* Running *)
let run () =
  Parameters.ForceValues.set true;
  Parameters.SemanticUnrollingLevel.set 500;
  Parameters.Verbose.set 0;
  Parameters.Debug.set 0;
  Parameters.Files.set
    [ "../../lib/stubs.c"; "OpenSER/CVE-2006-6876/fetchsms/fetchsms2.c" ];
  File.init_from_cmdline ();
  !Db.Value.compute ();
  ()

(* Main *)
let main () =
  try run ()
  with e ->
    Format.eprintf "Journal raised an exception: %s@." (Printexc.to_string e)

(* Registering *)
let main : unit -> unit =
  Dynamic.register
    "Frama_c_journal.main"
    (Type.func Type.unit Type.unit)
    ~journalize:false
    main
