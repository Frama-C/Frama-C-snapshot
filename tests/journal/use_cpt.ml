
let main () =
  let module T = Type.Abstract(struct let name = "Abstract_cpt.t" end) in
  let c = 
    Dynamic.get
      ~plugin:"Abstract_cpt" "mk" (Datatype.func Datatype.unit T.ty) () 
  in
  let incr = 
    Dynamic.get
      ~plugin:"Abstract_cpt" "incr" (Datatype.func T.ty Datatype.int) 
  in
  let pretty = 
    Dynamic.get
      ~plugin:"Abstract_cpt" "pretty" (Datatype.func T.ty Datatype.unit) 
  in
  let incr_and_pretty c = ignore (incr c); pretty c in
  for i = 1 to 3 do incr_and_pretty c done

let () = Db.Main.extend main
  
