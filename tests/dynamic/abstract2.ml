
module AA : sig end = struct
  type t = string
  let ty =
    Type.register ~pp:(fun _ _ _ -> ()) ~name:"AA.t" ~value_name:None [ "" ]
  let mk =
    Dynamic.register ~plugin:"AA" ~journalize:false "mk"
      (Type.func Type.unit ty)
      (fun () -> "a")
end

module BB : sig end = struct
  type t = float
  let ty =
    Type.register ~pp:(fun _ _ _ -> ()) ~name:"BB.t" ~value_name:None [ 1.0 ]
  let print =
    Dynamic.register ~plugin:"BB" ~journalize:false "print"
      (Type.func ty Type.unit)
      print_float
end

let main () =
  let a = Type.get_dynamic "AA.t" in
  let b = Type.get_dynamic "BB.t" in
  let s = Dynamic.get ~plugin:"AA" "mk" (Type.func Type.unit a) () in
  try Dynamic.get ~plugin:"BB" "print" (Type.func b Type.unit) s; assert false
  with Type.StringTbl.Incompatible_type s -> print_endline s

let () = Db.Main.extend main
