include Plugin.Register
  (struct
    let name = "test"
    let shortname = "test"
    let help = "unitary test of inserted cast hook"
   end)

let print_warning e ot nt =
  result "Inserting cast for expression %a of type %a to type %a@."
    Printer.pp_exp e Printer.pp_typ ot Printer.pp_typ nt;
  nt
;;

Cabs2cil.typeForInsertedCast := print_warning
