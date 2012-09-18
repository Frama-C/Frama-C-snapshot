include Plugin.Register
  (struct
    let name = "test"
    let shortname = "test"
    let help = "unitary test of inserted cast hook"
   end)

let print_warning e ot nt =
  result "Inserting cast for expression %a of type %a to type %a@."
    !Ast_printer.d_exp e !Ast_printer.d_type ot !Ast_printer.d_type nt;
  nt
;;

Cabs2cil.typeForInsertedCast := print_warning
