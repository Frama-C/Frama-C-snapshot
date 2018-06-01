open Cil_types

let find_x f =
  let kf = Globals.Functions.find_by_name f in
  let stmt = Kernel_function.find_return kf in
  Extlib.the (Globals.Syntactic_search.find_in_scope "x" (Block_scope stmt))

let run () =
  Ast.compute ();
  let x_f = find_x "f" in
  let x_g = find_x "g" in
  let x_main = find_x "main" in
  let x_glob =
    Extlib.the (Globals.Syntactic_search.find_in_scope "x" Program)
  in
  if not (Cil_datatype.Varinfo.equal x_main x_glob) then
    Kernel.fatal "in main, global variable x should be in scope";
  if Cil_datatype.Varinfo.equal x_glob x_f then
    Kernel.fatal "in f, global variable x should not be in scope";
  if Cil_datatype.Varinfo.equal x_glob x_g then
    Kernel.fatal "in g, global variable x should not be in scope";
  if Cil_datatype.Varinfo.equal x_g x_f then
    Kernel.fatal "mixing local variables from f and g";
  File.pretty_ast ()

let () = Db.Main.extend run
