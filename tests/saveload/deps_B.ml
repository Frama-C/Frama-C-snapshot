(*
  Test on save/load procedure
  
  What it does: it add two states with a dependency relations between them.
  This configuration is saved and restore with a different parent
  state. This should test the propagation of the reset.
  A third test verifies a non propagation case.

  Date: 16/07/2008
  Author: Julien Peeters
*)


module StateABis = 
  Computation.OptionRef
    (struct include Datatype.Int let default = 0 end)
    (struct let name = "Project.Test.StateABis" let dependencies = [] end)

module StateB =
  Computation.OptionRef
    (struct include Datatype.Bool let default = false end)
    (struct 
       let name = "Project.Test.StateB" 
       let dependencies = [ StateABis.self ]
     end)

let () = StateABis.set 10
let () = StateB.set (if StateABis.get () = 10 then true else false)
