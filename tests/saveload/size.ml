module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let kind = `Internal
      let default () = 0
     end)

let () = StateA.set 10
