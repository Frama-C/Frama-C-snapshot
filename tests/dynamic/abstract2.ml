
module AA : sig end = struct
  type t = string
  let ty =
    Type.register ~name:"AA.t" ~ml_name:None Structural_descr.Unknown [ "" ]
  let () = Type.is_dynamic_abstract ty
  let mk =
    Dynamic.register ~plugin:"AA" ~journalize:false "mk"
      (Datatype.func Datatype.unit ty)
      (fun () -> "a")
end

module BB : sig end = struct
  type t = float
  let ty =
    Type.register ~name:"BB.t" ~ml_name:None Structural_descr.Unknown [ 1.0 ]
  let () = Type.is_dynamic_abstract ty
  let print =
    Dynamic.register ~plugin:"BB" ~journalize:false "print"
      (Datatype.func ty Datatype.unit)
      print_float
end

let main () =
  let a = Type.get "AA.t" in
  let b = Type.get "BB.t" in
  let s = Dynamic.get ~plugin:"AA" "mk" (Datatype.func Datatype.unit a) () in
  try
    Dynamic.get ~plugin:"BB" "print" (Datatype.func b Datatype.unit) s;
    assert false
  with Dynamic.Incompatible_type s ->
    print_endline s

let () = Db.Main.extend main
