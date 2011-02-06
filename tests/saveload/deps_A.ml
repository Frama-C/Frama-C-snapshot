module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateA"
       let dependencies = []
       let kind = `Internal
       let default () = 0
     end)

module StateB =
  State_builder.Option_ref
    (Datatype.Bool)
    (struct
       let name = "Project.Test.StateB"
       let dependencies = [ StateA.self ]
       let kind = `Internal
       let default () = false
     end)

module StateC =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateC"
       let dependencies = [ StateB.self ]
       let kind = `Internal
     end)

let () = StateA.set 10
let () = StateB.set (if StateA.get () = 10 then true else false)
let () = assert (StateB.get ())
let () = StateC.set (if StateB.get () then 10 else 5)

