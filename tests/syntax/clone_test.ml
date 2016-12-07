let run () =
  Ast.compute ();
  let f = Globals.Functions.find_by_name "f" in
  let _ = Clone.clone_defined_kernel_function f in
  File.pretty_ast();
  Filecheck.check_ast "clone"

let () = Db.Main.extend run
