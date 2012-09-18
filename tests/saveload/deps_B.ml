module StateABis =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateABis"
      let dependencies = []
     end)

module StateB =
  State_builder.Option_ref
    (Datatype.Bool)
    (struct
       let name = "Project.Test.StateB"
       let dependencies = [ StateABis.self ]
     end)

let () = StateABis.set 10
let () = StateB.set (if StateABis.get () = 10 then true else false)
