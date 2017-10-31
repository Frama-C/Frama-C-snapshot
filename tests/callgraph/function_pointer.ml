let main () =
  Format.printf "number of calls = %d@." (Callgraph.Uses.nb_calls ())

let () = Db.Main.extend main
