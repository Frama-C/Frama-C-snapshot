open Cil_types
open Logic_const

let main () =
  let s, kf = Kernel_function.find_from_sid 2 in
  let add a = Annotations.add kf s [] (User (new_code_annotation a)) in
  add (AInvariant(["foo"], true, ptrue));
  add (AVariant(tinteger 0, None));
  add (AInvariant([], true, ptrue));
  add (AInvariant(["foo"], true, ptrue))

let () = Db.Main.extend main
