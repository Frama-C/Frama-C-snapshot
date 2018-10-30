module L = Wp.Lang
module F = Wp.Lang.F

let init _ = F.e_const (L.t_addr()) F.e_zero

let update = function
  | [ m ; a ] -> F.e_set m a (F.e_add (F.e_get m a) (F.e_int 1))
  | _ -> assert false

let access = function
  | [ m ; a ] -> F.e_get m a
  | _ -> assert false

let () =
  begin
    Wp.LogicBuiltins.hack "INDEX_init" init ;
    Wp.LogicBuiltins.hack "INDEX_access" access ;
    Wp.LogicBuiltins.hack "INDEX_update" update ;
  end
