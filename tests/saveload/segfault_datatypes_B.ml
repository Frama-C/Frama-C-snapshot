module StateA =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
     end)

let () = StateA.set 3
