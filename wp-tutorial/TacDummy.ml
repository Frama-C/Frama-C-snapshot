(* frama-c -load-script TacDummy.ml *)

module T = Wp.Tactical

class dummy =
  object
    inherit T.make
        ~id:"POPL.Dummy"
        ~title:"Dummy"
        ~descr:"Tactic BoilerPlate Code"
        ~params:[]

    method select feedback selection =
      match selection with
      | _ -> T.Not_applicable

  end

let () = T.register (new dummy)
