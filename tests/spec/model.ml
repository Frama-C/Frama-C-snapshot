open Cil_types

let find () = 
  let module M = struct exception Found of typeinfo end in
  try
    List.iter
      (function
        | GType (ty,_) ->
            if ty.tname = "T" then raise (M.Found ty)
        | _ -> ())
      (Ast.get ()).globals;
    Kernel.fatal "No typedef for T: test is broken"
  with M.Found ty -> ty

let print_models typ =
  let models = Annotations.model_fields typ in
  Format.printf "Model fields for type %a:@\n" Printer.pp_typ typ;
  List.iter (fun m -> Format.printf "%s, " m.mi_name) models;
  Format.printf "@\n"

let e = Emitter.create "test" [Emitter.Global_annot] ~correctness:[] ~tuning:[]

let add_model ty =
  let m = 
    { mi_name = "test_field";
      mi_field_type = Linteger;
      mi_base_type = ty;
      mi_decl = Cil_datatype.Location.unknown }
  in
  let annot = Dmodel_annot (m,Cil_datatype.Location.unknown) in
  Annotations.add_global e annot;
  annot

let remove_model annot = Annotations.remove_global e annot

let main () =
  let t = find () in
  let typ = TNamed(t,[]) in
  print_models typ;
  let m = add_model typ in
  Format.printf "After adding field@.";
  print_models typ;
  remove_model m;
  Format.printf "After removing field@.";
  print_models typ;
  Format.print_flush ()

let () = Db.Main.extend main
