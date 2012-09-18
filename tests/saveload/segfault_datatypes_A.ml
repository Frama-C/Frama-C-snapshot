module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let default () = 0
     end)

let () = StateA.set 10
