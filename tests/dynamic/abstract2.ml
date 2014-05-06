
module AA : sig end = struct
  type _t = string
  let ty =
    Type.register ~name:"AA.t" ~ml_name:None Structural_descr.t_unknown [ "" ]
  let _mk =
    Dynamic.register ~plugin:"AA" ~journalize:false "mk"
      (Datatype.func Datatype.unit ty)
      (fun () -> "a")
end

module BB : sig end = struct
  type _t = float
  let ty =
    Type.register ~name:"BB.t" ~ml_name:None Structural_descr.t_unknown [ 1.0 ]
  let _print =
    Dynamic.register ~plugin:"BB" ~journalize:false "print"
      (Datatype.func ty Datatype.unit)
      print_float
end

let main () =
  let module A = Type.Abstract(struct let name = "AA.t" end) in
  let a = A.ty in
  let module B = Type.Abstract(struct let name = "BB.t" end) in
  let _b =  B.ty in
  let _s = Dynamic.get ~plugin:"AA" "mk" (Datatype.func Datatype.unit a) () in
  (* is now statically checked and no more dynamically *) 
(*  Dynamic.get ~plugin:"BB" "print" (Datatype.func b Datatype.unit) s;*)
  ()
  

let () = Db.Main.extend main
