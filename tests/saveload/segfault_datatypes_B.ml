module StateA =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let kind = `Internal
      let default = 0
     end)

let () = StateA.set 3
