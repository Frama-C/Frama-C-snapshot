(* This datatype tests the bug fix of BTS #1277 *)
module A = 
  Datatype.Pair
    (Datatype.List(Datatype.String))
    (Datatype.List(Datatype.String))

module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let default () = 0
     end)

let () = StateA.set 10
