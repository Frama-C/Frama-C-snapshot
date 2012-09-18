(* test of incompatible state datatype *)

(* the same than deps_A.ml *)
module StateA =
  State_builder.Ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateA"
       let dependencies = []
       let default () = 0
     end)

(* same name but incompatible with deps_A.ml *)
module StateB =
  State_builder.Option_ref
    (Datatype.Float)
    (struct
      let name = "Project.Test.StateB"
      let dependencies = []
     end)

(* the unchanged dependency of StateB *)
module StateC =
  State_builder.Option_ref
    (Datatype.Int)
    (struct
       let name = "Project.Test.StateC"
       let dependencies = [ StateB.self ]
     end)

let () = StateA.set 5
let () = StateB.set 10.
let () = StateC.set 3

let main () =
  assert (StateA.get () = 10);
  assert (StateB.get_option () = None); (* reset to default *)
  assert (StateC.get_option () = None) (* reset because of dependency of B *)
    
let () = Db.Main.extend main
