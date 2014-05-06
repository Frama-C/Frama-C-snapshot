let e = Emitter.create "foo" [ Emitter.Funspec ] ~correctness:[] ~tuning:[]
let emitter = e
let populate = false

let run () =
  let () = Ast.compute () in
  let main = Globals.Functions.find_by_name "main" in
  Annotations.add_requires e main Cil.default_behavior_name [];
  File.pretty_ast ();
  Annotations.add_assumes e main
    Cil.default_behavior_name [ Logic_const.new_predicate Logic_const.ptrue];
  File.pretty_ast();
  Annotations.remove_behavior
    e main (List.hd (Annotations.behaviors ~populate ~emitter main));
  File.pretty_ast();
  Annotations.add_ensures e main Cil.default_behavior_name [];
  File.pretty_ast();
  Annotations.add_complete e main ["foo"; "bar"];
  Annotations.add_disjoint e main ["foo"; "bar"];
  File.pretty_ast();
  Annotations.add_assumes e main "foo"
    [ Logic_const.new_predicate Logic_const.ptrue ];
  Annotations.add_complete e main ["foo"];
  Annotations.add_disjoint e main ["foo"];
  Annotations.remove_behavior_components e main
    (List.hd (Annotations.behaviors ~populate ~emitter main));
  File.pretty_ast()

let () = Db.Main.extend run
