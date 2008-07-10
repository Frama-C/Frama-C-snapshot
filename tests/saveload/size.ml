(*
  Test on save/load procedure
  
  What it does: it add some states through Project.Computation.Register
  in order to see whether the load with less states definitions works

  Date: 04/07/2008
  Author: Julien Peeters
*)


(* We just add one state at this time *)
module StateA = 
  Computation.Ref
    (struct include Datatype.Int let default = 0 end)
    (struct
       let name = Project.Computation.Name.make "Project.Test.StateA"
       let dependencies = []
     end)

let () = StateA.set 10
