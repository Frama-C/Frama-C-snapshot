open Cil_types

module Self =
  State_builder.False_ref
    (struct
      let name = "Test"
      let dependencies = []
      let kind = `Correctness
     end)

module Up =
  Properties_status.Make_updater
    (struct let name = "Test" let emitter = Self.self end)

module Blob =
  State_builder.False_ref
    (struct
      let name = "Blob"
      let dependencies = []
      let kind = `Correctness
     end)

let main () =
  Ast.compute ();
  Annotations.iter
    (fun s _ (ca, _) ->
      let s', kf = Kernel_function.find_from_sid s.Cil_types.sid in
      assert (Cil_datatype.Stmt.equal s s');
      let ps =
	Property.ip_of_code_annot kf s (Annotations.get_code_annotation ca)
      in
      List.iter
        (fun p ->
          Up.set
	    p
	    [ Property.ip_blob Blob.self ]
	    (Checked { emitter = "Test emitter"; valid = Maybe });
          Format.printf "%a@." Properties_status.pretty_all p)
        ps)

let () = Db.Main.extend main
