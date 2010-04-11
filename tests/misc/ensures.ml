open Cil_types

let run () =
  Parameters.Dynamic.Bool.set "-context-valid-pointers" true;
  !Db.Value.compute ();
  Globals.Functions.iter
    (fun kf ->
       let kf_name = Kernel_function.get_name kf in
       let spec = Kernel_function.get_spec kf in
       List.iter
         (fun behavior ->
            let function_name = kf_name ^ ": behavior " ^ behavior.b_name in
            List.iter
	      (fun (_, post) ->
                 let statuses = 
		   Properties_status.Predicate.get_all_status post 
		 in
                 List.iter
                   (fun status ->
                      Kernel.result "%s %a"
                        function_name Cil.d_annotation_status status)
                   statuses)
              behavior.b_post_cond)
         spec.spec_behavior);;

Db.Main.extend run
