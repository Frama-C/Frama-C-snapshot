module F = Wp.Lang.F
module R = Wp.Repr
module T = Wp.Tactical
module S = Wp.Strategy
module A = Wp.Auto

let on_int f w e =
  match R.term e with
  | R.Int z -> (try f w (Integer.to_int z) with Integer.Too_big -> w)
  | _ -> w

let pos_range a n = F.p_and (F.p_leq F.e_zero a) (F.p_lt a (F.e_int n))

class bitrange =
  object(self)
    inherit T.make
        ~id:"POPL.BitRange"
        ~title:"Bit Range"
        ~descr:"Compute Range of Bitwise Expressions"
        ~params:[]
    method select feedback selection =
      let e = T.selected selection in
      match R.term e with
      | R.Call( f , es ) when R.lfun f = "land" ->
          let max_pos w k = if 0 <= k then max w k else w in
          let m = List.fold_left (on_int max_pos) (-1) es in
          if m < 0 then raise Not_found ;
          T.Applicable(T.insert ["range",pos_range e m])
      | R.Call( f , _ ) ->
          Format.eprintf "CALL %S@." (R.lfun f) ;
          T.Not_applicable
      | _ -> T.Not_applicable
  end

let () = T.register (new bitrange)
