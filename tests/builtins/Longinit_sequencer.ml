(* Small script to test long_init*.c files, which require one test to run
   before the other. *)

open Kernel

include Plugin.Register
(struct
  let name = "long init testing module"
  let shortname = "test-long-init"
  let help = "utility script for tests"
 end)

module Res = String(struct
    let option_name = "-res-file"
    let help = ""
    let arg_name = "file"
    let default = "result"
  end)

let ok = ref false

let tmpfile () = Res.get () ^ "/Longinit_sequencer.sav"

let () =
  at_exit (fun () ->
    let tmpfile = tmpfile () in
    if Debug.get () >= 1 || not !ok then
      result "Keeping temp file %s" tmpfile
    else
      try Sys.remove tmpfile with Sys_error _ -> ())

let main () =
  let tmpfile = tmpfile () in
  let fmt = Format.std_formatter in
  let display_results state = Format.fprintf fmt "@[%a@]@\n" !Db.Value.display state in
  Dynamic.Parameter.String.set "" "tests/builtins/long_init.c";
  Dynamic.Parameter.String.set "-eva-save-fun-state" ("init_inner:" ^ tmpfile);
  Dynamic.Parameter.String.set "-eva-builtin" "malloc:Frama_C_malloc_fresh";
  Dynamic.Parameter.Bool.set "-eva-alloc-returns-null" false;
  Dynamic.Parameter.String.set "-eva-warn-key" "builtins:override=inactive";
  !Db.Value.compute ();
  Callgraph.Uses.iter_in_rev_order display_results;
  Files.clear ();
  Dynamic.Parameter.String.set "" "tests/builtins/long_init2.c";
  (* clear and set parameters to the same value to recompute
     kernel function IDs *)
  Dynamic.Parameter.String.clear "-eva-save-fun-state" ();
  Dynamic.Parameter.String.set "-eva-save-fun-state" ("init_outer:" ^ tmpfile);
  Dynamic.Parameter.String.set "-eva-load-fun-state" ("init_inner:" ^ tmpfile);
  (* set builtins in a different order to force kernel to recompute
     kernel function IDs *)
  Dynamic.Parameter.String.set "-eva-builtin" "malloc:Frama_C_malloc_fresh";
  !Db.Value.compute ();
  Callgraph.Uses.iter_in_rev_order display_results;
  Files.clear ();
  Dynamic.Parameter.String.set "" "tests/builtins/long_init3.c";
  Dynamic.Parameter.String.clear "-eva-save-fun-state" ();
  Dynamic.Parameter.String.clear "-eva-load-fun-state" ();
  Dynamic.Parameter.String.set "-eva-load-fun-state" ("init_outer:" ^ tmpfile);
  (* set builtins in a different order to force kernel to recompute
     kernel function IDs *)
  Dynamic.Parameter.String.set "-eva-builtin" "malloc:Frama_C_malloc_fresh";
  !Db.Value.compute ();
  Callgraph.Uses.iter_in_rev_order display_results;
  ok:=true (* no error, we can erase the file *)

let () = Db.Main.extend main
