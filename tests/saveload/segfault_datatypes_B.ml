(*
  Test on save/load procedure
  
  What it does: it add a state with a certain datatype to see whether 
  the load with another datatype raises an exception like it should.

  Date: 15/07/2008
  Author: Julien Peeters
*)


module StateA = 
  Computation.OptionRef
    (struct include Datatype.Int let default = 0 end)
    (struct let name = "Project.Test.StateA" let dependencies = [] end)

let () = StateA.set 3
