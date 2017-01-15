module F = Wp.Lang.F
module R = Wp.Repr
module T = Wp.Tactical
module S = Wp.Strategy
module A = Wp.Auto

let range a n = F.p_and (F.p_leq F.e_zero a) (F.p_lt a (F.e_int (1 lsl n)))

let bit_test x k = Wp.Cint.l_and x (F.e_int (1 lsl k))

let rec bitwise_eqs a b n =
  if n >= 0 then
    F.e_eq (bit_test a n) (bit_test b n) ::
    bitwise_eqs a b (n-1)
  else []

let bitwise_eq a b n = F.e_and (bitwise_eqs a b n)
let rewrite descr u v = T.rewrite [ descr , F.p_true , u , v ]

class bitcase =
  object
    inherit T.make
        ~id:"POPL.BitCase"
        ~title:"Bit Case"
        ~descr:"Resolve Bitwise Equality"
        ~params:[]

    method select feedback selection =
      let e = T.selected selection in
      match R.term e with
      | R.Eq(a,b) when F.is_int a && F.is_int b ->
          let inrange = F.p_and (range a 8) (range b 8) in
          let bitwise = bitwise_eq a b 8 in
          T.Applicable
            (A.t_cut ~by:"range" inrange
               (T.rewrite [ "bitwise" , F.p_true , e , bitwise ]))
      | _ -> T.Not_applicable

  end

let () = T.register (new bitcase)
