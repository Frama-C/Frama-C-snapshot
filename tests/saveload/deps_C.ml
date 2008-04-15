(*
  Test on save/load procedure
  
  What it does: it add two states with a dependency relations between them.
  This configuration is saved and restore with a different parent
  state. This should test the propagation of the reset.
  A thrid test verifies a non propagation case.

  Date: 16/07/2008
  Author: Julien Peeters
*)

module StateA = 
  Computation.Ref
    (struct include Datatype.Int let default () = 0 end)
    (struct let name = "Project.Test.StateA" let dependencies = [] end)

module StateABis =
  Computation.OptionRef
    (struct include Datatype.Int let default () = 0 end)
    (struct let name = "Project.Test.StateABis" let dependencies = [] end)

let () = StateA.set 5
let () = StateABis.set 10
