open Cil_datatype
open Cil

let loc = Location.unknown

let null () =
  let e = zero ~loc in
  mkCast ~force:true ~e ~newt:voidPtrType

let inull () =
  let e = zero ~loc in
  mkCast ~force:true ~e ~newt:intPtrType

let cone () =
  let e = one ~loc in
  mkCast ~force:true ~e ~newt:charPtrType

let ione () =
  let e = one ~loc in
  mkCast ~force:true ~e ~newt:intPtrType

let test =
  let n = ref 0 in
  fun e1 e2 ->
    incr n;
    let e = Cil.mkBinOp ~loc Cil_types.Eq (e1 ()) (e2 ()) in
    Format.printf "TEST %d: %a@." !n Exp.pretty e;
    let e = Cil.mkBinOp_safe_ptr_cmp ~loc Cil_types.Eq (e1()) (e2()) in
    Format.printf "TEST %d (safe ptr cmp): %a@." !n Exp.pretty e

let main () =
  test null null;
  test null inull;
  test inull null;

  test null cone;
  test cone null;
  test cone cone;

  test cone ione;
  test ione cone

let () = Db.Main.extend main
