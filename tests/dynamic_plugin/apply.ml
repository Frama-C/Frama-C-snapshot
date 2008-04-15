module L = List
open Type
module List = L

module Test = 
  Cmdline.Dynamic.Register.False(struct let name = "Apply.Test" end)

let main _fmt =
  if Cmdline.Dynamic.Apply.Bool.get "Apply.Test" then begin 
    ignore (Dynamic.apply "Register_mod2.g_test" (func int int) 41);
    try Dynamic.apply "Register_mod2.g_test"
      (func int (func (list char) (func (couple string float) unit))) 
      42 ['a'] ("r",6.8)
    with FunTbl.Incompatible_Type s -> 
      Format.eprintf "%s@." s;
      try Dynamic.apply "Register_mod2.unknow" (func unit unit) ()
      with FunTbl.Not_Registered s -> Format.eprintf "Not_Registered %s@." s;
  end

let options =
    [ "-dynamic-test",
    Arg.Unit Test.on,
    ": print dynamic test" ]

let () =
  Options.add_plugin
    ~name:"dynamic-test"
    ~descr:"Test dynamic command line"
    options;
  Db.Main.extend main
