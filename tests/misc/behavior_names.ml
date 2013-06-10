let run () =
  let _ = Ast.get () in
  let kf = Globals.Functions.find_by_name "f" in
  let pretty_list fmt l =
    Pretty_utils.pp_list ~sep:"@ " Format.pp_print_string fmt l
  in
  Format.printf
    "@[stmt: %a@\nnew1: %s@\nnew2: %s@]@."
    pretty_list (Annotations.behavior_names_of_stmt_in_kf kf)
    (Annotations.fresh_behavior_name kf "foo")
    (Annotations.fresh_behavior_name kf "bla")

let () = Db.Main.extend run
