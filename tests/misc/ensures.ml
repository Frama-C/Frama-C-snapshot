open Cil_types

let run () =
  Dynamic.Parameter.Bool.set "-context-valid-pointers" true;
  !Db.Value.compute ();
  Globals.Functions.iter
    (fun kf ->
       let kf_name = Kernel_function.get_name kf in
       let spec = Annotations.funspec kf in
       let ip = Property.ip_of_spec kf Kglobal spec in
       List.iter
         (fun ip ->
            let bname = match Property.get_behavior ip with
              | None -> "?"
              | Some b -> b.b_name
            in
            let function_name = kf_name ^ ": behavior " ^ bname in
            let status = Property_status.get ip in
            Kernel.result "@[%s@ @[%a@]@]" 
	      function_name Property_status.pretty status)
         ip)

let () = Db.Main.extend run
