let e = Emitter.create "foo" [ Emitter.Funspec ] ~correctness:[] ~tuning:[]
let emitter = e
let populate = false

let run () =
  let () = Ast.compute () in
  let main = Globals.Functions.find_by_name "main" in
  Annotations.add_requires e main [];
  File.pretty_ast ();
  Annotations.add_assumes
    e main [ Logic_const.new_predicate Logic_const.ptrue];
  File.pretty_ast();
  Annotations.remove_behavior
    e main (List.hd (Annotations.behaviors ~populate ~emitter main));
  File.pretty_ast();
  Annotations.add_ensures e main [];
  File.pretty_ast();
  (try
    Annotations.add_complete e main ["foo"; "bar"];
   with
     Log.AbortFatal s -> Kernel.warning "Caught fatal error: %s" s);
  (try
     Annotations.add_disjoint e main ["foo"; "bar"];
   with
     Log.AbortFatal s -> Kernel.warning "Caught fatal error: %s" s);
  File.pretty_ast();
  let behavior = "foo" in
  Annotations.add_assumes e main ~behavior
    [ Logic_const.new_predicate Logic_const.ptrue ];
  Annotations.add_complete e main [behavior];
  Annotations.add_disjoint e main [behavior];
  Annotations.remove_behavior_components e main
    (List.hd (Annotations.behaviors ~populate ~emitter main));
  File.pretty_ast()

let () = Db.Main.extend run
