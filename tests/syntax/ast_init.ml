open Cil_types

let apply _ =
  let f = Globals.Functions.find_by_name "f" in
  let s = Kernel_function.find_first_stmt f in
  let l = Kernel_function.find_all_enclosing_blocks s in
  List.iter
    (fun b -> b.bstmts <-
        Cil.mkStmtOneInstr (Skip (Cil_datatype.Stmt.loc s)) :: b.bstmts)
    l;
  Ast.mark_as_grown ()

let () = Ast.apply_after_computed apply

let run () =
  Ast.compute ();
  File.pretty_ast ()

let () = Db.Main.extend run
