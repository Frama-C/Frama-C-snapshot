(*
  Test on save/load procedure
  
  What it does: it adds three states A, B, C with a dependency chain between 
  them. This configuration is saved and restore with a new configuration
  composed of one new state which depends on A and is a dependence of C.
  The aim is to verify that an unserialization does not override a previous
  setting to default.

  Date: 18/07/2008
  Author: Julien Peeters
*)

module StateA = 
  Computation.Ref
    (struct include Datatype.Int let default () = 0 end)
    (struct let name = "Project.Test.StateA" let dependencies = [] end)

module StateB =
  Computation.OptionRef
    (struct include Datatype.Bool let default () = false end)
    (struct 
       let name = "Project.Test.StateB"
       let dependencies = [ StateA.self ]
     end)

module StateD = 
  Computation.Ref
    (struct include Datatype.Int let default () = 0 end)
    (struct
       let name = "Project.Test.StateD"
       let dependencies = [ StateA.self ]
     end)

module StateC =
  Computation.OptionRef
    (struct include Datatype.Int let default () = false end)
    (struct
       let name = "Project.Test.StateC"
       let dependencies = [ StateB.self; StateD.self ]
     end)

let () = StateA.set 10
let () = StateB.set (StateA.get () = 10)
let () = StateD.set (if StateA.get () = 5 then 5 else 0)
let () = StateC.set (if StateB.get () && StateD.get () = 5 then 10 else 5)
