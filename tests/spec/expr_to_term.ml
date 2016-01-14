open Cil_types

let emitter = Emitter.(create "Test" [Funspec] ~correctness:[] ~tuning:[])

let check_expr_term check fct s e =
  let exp =
    match s.skind with
      | Instr (Set (lv,_,loc)) -> Cil.new_exp ~loc (Lval lv)
      | _ -> Kernel.fatal "Unexpected statement %a" Printer.pp_stmt s
  in
  let term =
    match e with
      | (_, { ip_content = Papp(_,_,[l;_]) }) -> l
      | _ -> Kernel.fatal "Unexpected ensures %a" Printer.pp_post_cond e
  in
  let term' = Logic_utils.expr_to_term ~cast:false exp in
  if check && not (Cil_datatype.Term.equal term term') then
    Kernel.fatal
      "translation of C expression %a inconsistent with logic term %a"
      Printer.pp_exp exp Printer.pp_term term;
  let p = List.hd (Logic_env.find_all_logic_functions "int_eq") in
  let app = Logic_const.papp (p,[],[term;term']) in
  let post = Logic_const.new_predicate app in
  Annotations.add_ensures emitter fct Cil.default_behavior_name [Normal,post]


let treat_fct check fct =
  let stmts = (Kernel_function.get_definition fct).sbody.bstmts in
  let stmts =
    List.filter
      (function 
        { skind = Instr (Set (lv,_,_)) } ->
          (match lv with (Var v,_) -> v.vglob | _ -> true)
        | _ -> false)
      stmts
  in
  let ensures = (List.hd (Annotations.funspec fct).spec_behavior).b_post_cond
  in
  (* A bit fragile, but should do the trick as long as the test itself does
     not get too complicated (regarding the C code at least). *)
  if not (List.length stmts = List.length ensures) then
    Kernel.fatal 
      "Stmts:@\n%a@\nPreds:@\n%a@\n"
      (Pretty_utils.pp_list ~sep:"@\n@\n" Printer.pp_stmt) stmts
      (Pretty_utils.pp_list ~sep:"@\n@\n" Printer.pp_post_cond) ensures;
  List.iter2 (check_expr_term check fct) stmts ensures;
  Visitor.visitFramacFileSameGlobals
    (new Filecheck.check "check_expr_to_term") (Ast.get())

let compute () =
  let main = Globals.Functions.find_by_name "main" in
  let f = Globals.Functions.find_by_name "f" in
  treat_fct true main;
  treat_fct false f


let () = Db.Main.extend compute
