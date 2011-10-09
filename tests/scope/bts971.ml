
let find_pp kf_name =
    let kf = Globals.Functions.find_by_name kf_name in
    let stmt = Kernel_function.find_first_stmt kf in
      Format.printf "Current program point = first one in function '%s'@\n" 
        kf_name;
      stmt, kf
;;

let compute_and_print pp str_data =
  let stmt, kf = pp in
  let lval_term = !Db.Properties.Interp.lval kf stmt str_data in
  let lval = !Db.Properties.Interp.term_lval_to_lval ~result:None lval_term in
  let defs = !Db.Scope.get_defs kf stmt lval in
    Format.printf "* @[<v 2>Defs for (%s) at current program point=@[<v 2>@." 
      str_data;
  let _ = match defs with
      | None -> Format.printf "computation problem.@."
      | Some (defs, _undef) when Cil_datatype.Stmt.Set.is_empty defs ->
          Format.printf "no Defs found@."
      | Some (defs, _undef) ->
          Cil_datatype.Stmt.Set.iter 
            (fun s ->
               Format.printf "%a: %a@\n" Cil.d_loc (Cil_datatype.Stmt.loc s)
                 Cil_datatype.Stmt.pretty s) defs
  in Format.printf "@]@]@.@."
;;

let tests () =
  let pp = find_pp "f1" in compute_and_print pp "v";
  let stmt, kf as pp = find_pp "g1" in compute_and_print pp "v";
  let stmt = match stmt.Cil_types.succs with s::_ -> s | _ -> assert false in
    Format.printf "Current program point = 2d one in function '%s'@\n" "g1";
    compute_and_print (stmt, kf) "v";
  let pp = find_pp "f" in compute_and_print pp "v"

let main _ =
  Format.printf "=== Tests for Scope.Defs@.";
  Ast.compute ();
  Dynamic.Parameter.Bool.set "-val-show-progress" false ;
  Dynamic.Parameter.Int.set "-value-verbose" 0 ;
  Dynamic.Parameter.Int.set "-from-verbose" 0 ;
  Dynamic.Parameter.Int.set "-pdg-verbose" 0 ;

  Format.printf "--- Intraprocedural mode (-scope-no-defs-interproc)@.";
  Dynamic.Parameter.Bool.set "-scope-defs-interproc" false ;
  tests ();

  Format.printf "--- Interprocedural mode (-scope-defs-interproc)@.";
  Dynamic.Parameter.Bool.set "-scope-defs-interproc" true ;
  tests ()
;;

let _ = Db.Main.extend main

