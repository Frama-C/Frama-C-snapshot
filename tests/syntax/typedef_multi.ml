let run () = 
  File.reorder_ast ();
  File.pretty_ast ()

let () = Db.Main.extend run
