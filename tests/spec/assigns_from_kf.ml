let run () =
    Globals.Functions.iter (fun kf -> ignore (Annotations.funspec kf))

let () = Db.Main.extend run