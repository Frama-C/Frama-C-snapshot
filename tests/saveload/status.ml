open Cil_types

let emitter = Emitter.create "Test" ~correctness:[] ~tuning:[]

let main () =
  Ast.compute ();
  Annotations.iter
    (fun s _ (ca, _) ->
      let kf = Kernel_function.find_englobing_kf s in
      let ps =
	Property.ip_of_code_annot kf s (Annotations.get_code_annotation ca)
      in
      List.iter
        (fun p ->
          Property_status.emit
	    emitter
	    p
	    ~hyps:[ Property.ip_other "Blob" None Kglobal ]
	    Property_status.Dont_know;
          Format.printf "%a@." Property_status.pretty (Property_status.get p))
        ps)

let () = Db.Main.extend main
