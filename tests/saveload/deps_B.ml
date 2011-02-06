module StateABis =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
      let name = "Project.Test.StateABis"
      let dependencies = []
      let kind = `Internal
     end)

module StateB =
  State_builder.Option_ref
    (Datatype.Bool)
    (struct
       let name = "Project.Test.StateB"
       let dependencies = [ StateABis.self ]
       let kind = `Internal
     end)

let () = StateABis.set 10
let () = StateB.set (if StateABis.get () = 10 then true else false)
