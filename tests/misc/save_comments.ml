open Cil_types
open Cil

let find_comment () =
  let kf = Globals.Functions.find_by_name "f" in
  let loc1 = Kernel_function.get_location kf in
  let loc2 = Cil_datatype.Stmt.loc (Kernel_function.find_return kf) in
  let zone = (fst loc1, snd loc2) in
  Format.printf 
    "@[In project %s, searching for comments between %a and %a:@\n%a\
     @\nEnd of comments@."
    (Project.get_name (Project.current()))
    Printer.pp_location loc1
    Printer.pp_location loc2
    (Pretty_utils.pp_list ~sep:"@\n" Format.pp_print_string)
    (Cabshelper.Comments.get zone)

let run () =
  let ast = Ast.get () in
  let vis = object
      inherit Visitor.frama_c_inplace
      method vglob_aux g = match g with GText s -> Format.printf "got global comment %s@." s; SkipChildren | _ -> DoChildren
  end
  in
  ignore (Visitor.visitFramacFile vis ast);
  let fmt = Format.std_formatter in
  Format.printf "Printing default project first time:@.";
  File.pretty_ast ~fmt ();
  Format.printf "Printing default project second time:@.";
  File.pretty_ast ~fmt ();
  let file = Extlib.temp_file_cleanup_at_exit "save_comments_test" ".sav" in
  let name = "saved_project" in
  find_comment ();
  Project.save file;
  let prj = Project.load ~name file in
  Project.on prj find_comment ();
  Format.printf "Printing saved project:@.";
  File.pretty_ast ~prj ~fmt ()

let () = Db.Main.extend run
