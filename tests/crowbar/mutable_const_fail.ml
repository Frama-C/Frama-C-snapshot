open Cil_types

let main () =
  Ast.compute ();
  let def =
    Kernel_function.get_definition
      (Globals.Functions.find_def_by_name "f")
  in
  let s = List.hd (def.sbody.bstmts) in
  match s.skind with
  | Instr (Set (_,{ enode = Lval (Var x,offset) },_)) ->
    Format.printf "Type of variable: %a@\nOffset: %a@."
      Printer.pp_typ x.vtype Printer.pp_offset offset;
    assert
      (Cil.typeHasAttribute "const" (Cil.typeOffset x.vtype offset))
  | _ -> assert false

let () = Db.Main.extend main
