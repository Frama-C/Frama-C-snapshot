open Cil_types

let run () =
  Parameters.Dynamic.Bool.set "-context-valid-pointers" true;
  !Db.Value.compute ();
  Globals.Functions.iter
    (fun kf ->
       let kf_name = Kernel_function.get_name kf in
       let spec = Kernel_function.get_spec kf in
       let ip = Property.ip_of_spec kf Kglobal spec in
       List.iter
         (fun ip ->
            let bname =
              match Property.get_behavior ip with
                  Some b -> b.b_name
                | None -> "Ook"
            in
            let function_name = 
              kf_name ^ ": behavior " ^ bname 
            in
            let statuses = Properties_status.get_all ip in
            List.iter
              (fun status ->
                Kernel.result "%s %a"
                  function_name Cil.d_annotation_status status)
              statuses)
         ip)
;;

Db.Main.extend run
