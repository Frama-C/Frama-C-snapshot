(* dynamic plug-in to test rewriting of formals in ensures clauses *)

let rewrite () =
  Ast.compute ();
  Globals.Functions.iter (fun kf -> ignore (Logic_interp.formals_in_ensures kf))

include Plugin.Register
    (struct
      let name = "rewrite-ensures"
      let shortname = name
      let help = "test purposes only"
     end)

let () = Db.Main.extend rewrite
