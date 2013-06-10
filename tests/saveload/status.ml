open Cil_types

let emitter = 
  Emitter.create "Test" [ Emitter.Property_status ] ~correctness:[] ~tuning:[]

let main () =
  Ast.compute ();
  let o = object
    inherit Visitor.frama_c_inplace
    method vstmt_aux stmt =
      Annotations.iter_code_annot
	(fun _ ca ->
	  let kf = Kernel_function.find_englobing_kf stmt in
	  let ps = Property.ip_of_code_annot kf stmt ca in
	  List.iter
            (fun p ->
              Property_status.emit
		emitter
		p
		~hyps:[ Property.ip_other "Blob" None Kglobal ]
		Property_status.Dont_know;
              Format.printf "%a@." 
		Property_status.pretty (Property_status.get p))
            ps)
	stmt;
      Cil.DoChildren
  end in
  Visitor.visitFramacFileSameGlobals o (Ast.get ())
    

let () = Db.Main.extend main
