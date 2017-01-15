(* frama-c -load-script TacDummy.ml *)

module T = Wp.Tactical
module F = Wp.Lang.F
module A = Wp.Auto

class dummy_split =
  object
    inherit T.make
        ~id:"POPL.DummySplit"
        ~title:"Dummy Split"
        ~descr:"Simplified Split"
        ~params:[]

    method select feedback selection =
      let e = T.selected selection in
      if F.is_prop e then
        let p = F.p_bool e in
        T.Applicable(A.t_case p
                       (A.t_finally "Positive")
                       (A.t_finally "Negative"))
      else
        T.Not_applicable

  end

let () = T.register (new dummy_split)
