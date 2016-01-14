let main () =
  !Db.Value.compute();
  let vis = new Filecheck.check "Check alarm" in
  Visitor.visitFramacFile vis (Ast.get());
  File.pretty_ast ()

let () = Db.Main.extend main
