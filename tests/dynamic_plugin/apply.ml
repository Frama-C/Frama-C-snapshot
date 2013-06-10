open Datatype

module Param =
  Plugin.Register
    (struct
       let name = "apply"
       let shortname = "apply"
       let help = "testing purpose"
     end)

module Test =
  Param.False
    (struct
       let option_name = "-dynamic-test"
       let help = "print dynamic test"
     end)

let main () =
  if Dynamic.Parameter.Bool.get "-dynamic-test" () then begin
    ignore (Dynamic.get ~plugin:"Register_mod2" "g_test" (func int int) 41);
    try
      Dynamic.get ~plugin:"Register_mod2" "g_test"
	(func int (func (list char) (func (pair string float) unit)))
	42 ['a'] ("r",6.8)
    with Dynamic.Incompatible_type s ->
      Param.feedback "%s" s;
      try Dynamic.get ~plugin:"Register_mod2" "unknown" (func unit unit) ()
      with Dynamic.Unbound_value s ->
	Param.feedback "value %S not registered" s
  end

let () = Db.Main.extend main
