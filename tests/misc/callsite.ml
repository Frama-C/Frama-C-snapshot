open Cil_types

let dump f =
  let kf = Globals.Functions.find_by_name f in
  let csites = Kernel_function.find_syntactic_callsites kf in
  Log.print_on_output
    (fun fmt ->
       Format.fprintf fmt "Call Sites for %s:@\n" f ;
       List.iter
	 (fun (ckf,stmt) ->
	    Format.fprintf fmt "  - From %s at #%03d@\n"
	      (Kernel_function.get_name ckf)  stmt.sid)
	 csites)

let main () =
  Ast.compute () ;
  List.iter dump ["f";"g";"h";"k"]

let () = Db.Main.extend main
