module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateA"
      let dependencies = []
      let default () = 0
     end)

module StateB =
  State_builder.Option_ref
    (Datatype.Bool)
    (struct
       let name = "Project.Test.StateB"
       let dependencies = [ StateA.self ]
     end)

module StateD =
  State_builder.Ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateD"
       let dependencies = [ StateA.self ]
       let default () = 0
     end)

module StateC =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateC"
       let dependencies = [ StateB.self; StateD.self ]
     end)

let () = StateA.set 10
let () = StateB.set (StateA.get () = 10)
let () = StateD.set (if StateA.get () = 5 then 5 else 0)
let () = StateC.set (if StateB.get () && StateD.get () = 5 then 10 else 5)
