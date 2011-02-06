let run () =
  let _ = Ast.get () in
  let kf = Globals.Functions.find_by_name "f" in
  let pretty_list fmt l =
    Pretty_utils.pp_list
      ~sep:Pretty_utils.space_sep Format.pp_print_string fmt l
  in
  Format.printf
    "@[external: %a@\ninternal: %a@\nall: %a@\nnew1: %s@\nnew2: %s@]@."
    pretty_list (Kernel_function.spec_function_behaviors kf)
    pretty_list (Kernel_function.internal_function_behaviors kf)
    pretty_list (Kernel_function.all_function_behaviors kf)
    (Kernel_function.fresh_behavior_name kf "foo")
    (Kernel_function.fresh_behavior_name kf "bla")

let () = Db.Main.extend run
