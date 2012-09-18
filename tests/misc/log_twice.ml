

(* Run the user commands *)
let run () =
  let p_default =
    Project.create_by_copy
      ~src:(Project.from_unique_name "default")
      "default"
  in
  !Db.Value.compute ();
  Project.set_current p_default;
  !Db.Value.compute ();
  ()

let () = Db.Main.extend run 
