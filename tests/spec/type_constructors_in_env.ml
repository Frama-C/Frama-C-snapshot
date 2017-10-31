let run () =
  Ast.compute ();
  (match Logic_env.find_all_logic_functions "f" with
  | [] -> Kernel.fatal "f should be in the environment"
  | _ -> ());
  (try
     ignore (Logic_env.find_logic_ctor "A")
   with Not_found -> Kernel.fatal "A should be in the environment");
  File.pretty_ast ()

let () = Db.Main.extend run
