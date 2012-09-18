open Cil_types

let emitter = 
  Emitter.create "test" [ Emitter.Global_annot ] ~correctness:[] ~tuning:[]

let add () =
  let li = Cil_const.make_logic_info "bla" in
  li.l_body <- LBpred Logic_const.ptrue;
  let glob = Dfun_or_pred (li,Cil_datatype.Location.unknown) in
  Logic_utils.add_logic_function li;
  Annotations.add_global emitter glob

let check () =  
  assert (Logic_env.find_all_logic_functions "foo" <> []);
  assert (Logic_env.find_all_logic_functions "bar" <> []);
  assert (Logic_env.find_all_logic_functions "bla" <> []);
  let x = List.hd (Logic_env.find_all_logic_functions "bar") in
  let lv = x.l_var_info in
  assert (x == Logic_env.find_logic_cons lv);
  Format.printf "Check OK@."

let run () =
  let _ = Ast.get () in
  add ();
  check ();
  let prj = File.create_project_from_visitor "foo"
    (fun p -> new Visitor.frama_c_copy p)
  in
  Project.on prj check ()

let () = Db.Main.extend run
