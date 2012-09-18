open Cil_types

let check_vi_exists vi _ =
  try
    ignore (Globals.Functions.get vi)
  with Not_found ->
    Kernel.fatal 
      "%s(%d) has an entry in FormalsDecl, but does not exist in AST"
      vi.vname vi.vid

let run () =
  let _ = Ast.get () in Cil.iterFormalsDecl check_vi_exists

let () = Db.Main.extend run
