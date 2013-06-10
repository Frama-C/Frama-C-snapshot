let main () =
  (* File.create_project_from_visitor calls File.cil_init but never calls
     Logic_env.Builtins.apply *)
  ignore
    (File.create_project_from_visitor "foo"
       (fun p -> new Visitor.generic_frama_c_visitor (Cil.copy_visit p)));
  let p = Project.create "bar" in
  (* Computing the AST first calls File.cil_init, than calls
     Logic_env.Builtins.apply. This second call raises an exception because
     logic builtins were registered twice by File.cil_init (even if
     File.cil_init was called on two different projects: the hook
     Logic_env.Builtins is not projectified) *)
  Project.on p Ast.compute ()

let () = Db.Main.extend main
