module L = List
open Type
module List = L

module Param =
  Plugin.Register
    (struct
       let name = "apply"
       let shortname = "apply"
       let descr = "testing purpose"
       let module_name = "Apply.Param"
       let is_dynamic = true
     end)

module Test =
  Param.False
    (struct
       let option_name = "-dynamic-test"
       let module_name = "Apply.Test"
       let descr = "print dynamic test"
     end)

let main _fmt =
  if Parameters.Dynamic.Bool.get "-dynamic-test" then begin
    ignore (Dynamic.get "Register_mod2.g_test" (func int int) 41);
    try Dynamic.get "Register_mod2.g_test"
      (func int (func (list char) (func (couple string float) unit)))
      42 ['a'] ("r",6.8)
    with Type.StringTbl.Incompatible_type s ->
      Param.error "%s@." s;
      try
	Dynamic.get "Register_mod2.unknown" (func unit unit) ()
      with Type.StringTbl.Unbound_value s ->
	Param.error "Value %s not registered@." s;
  end

let () = Db.Main.extend main
