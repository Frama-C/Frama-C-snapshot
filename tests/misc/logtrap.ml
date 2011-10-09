let main () =
  begin
    Log.print_on_output
      (fun fmt -> 
	 Format.fprintf fmt "Start.@." ;
	 if true then assert false ;
	 Format.fprintf fmt "End.@." ;
      )
  end

let () = Db.Main.extend main
