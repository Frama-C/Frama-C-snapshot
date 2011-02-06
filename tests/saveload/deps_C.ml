module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let kind = `Internal
      let default () = 0
     end)

module StateABis =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateABis"
      let dependencies = []
      let kind = `Internal
     end)

let () = StateA.set 5
let () = StateABis.set 10
