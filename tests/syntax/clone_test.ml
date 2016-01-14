let run () =
  Ast.compute ();
  let f = Globals.Functions.find_by_name "f" in
  let new_f = Clone.clone_defined_kernel_function f in
  File.pretty_ast();
  Visitor.visitFramacFileSameGlobals (new Filecheck.check "clone") (Ast.get())

let () = Db.Main.extend run
