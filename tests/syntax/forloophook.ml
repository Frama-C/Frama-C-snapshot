open Cabs

let () =
  Cabs2cil.register_for_loop_all_hook
    (fun _ _ _ _ -> Format.printf "Found a for loop@.")

let () =
  Cabs2cil.register_for_loop_init_hook
    (fun fc ->
      match fc with 
        | FC_EXP _ -> Format.printf "No declaration@."
        | FC_DECL _ -> Format.printf "Local declaration@.")

let () =
  Cabs2cil.register_for_loop_test_hook
    (fun e ->
      match e.expr_node with
        | NOTHING -> Format.printf "No test@."
        | _ -> Format.printf "Has a test@.")

let () =
  Cabs2cil.register_for_loop_incr_hook
    (fun e ->
      match e.expr_node with
        | NOTHING -> Format.printf "No increment@."
        | _ -> Format.printf "Has an increment@.")
 
let () =
  Cabs2cil.register_for_loop_body_hook
    (fun s ->
      match s.stmt_node with
        | NOP _ -> Format.printf "No body@."
        | _ -> Format.printf "Has a body@.")
