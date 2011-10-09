open Cil_types
open Cil

class pathcrawlerVisitor prj =
object(self)
  inherit Visitor.frama_c_copy prj

  method vspec sp =
    Format.printf "Considering spec of function %s@."
      (Kernel_function.get_name (Extlib.the self#current_kf));
    (match self#current_func with
      | Some f ->
          if  f.svar.vname ="f" then (
	    Format.printf "Funspec of f is '%a' through visitor@."
              Cil.d_funspec sp;
	    Format.printf "It is '%a' through get_spec@."
              Cil.d_funspec
              (Kernel_function.get_spec (Globals.Functions.get f.svar));
          )
      | None -> 
        Format.printf "Function prototype; Funspec is '%a'@."
          Cil.d_funspec sp;
    );
    DoChildren
end

let startup () = 
    let cil_file = Ast.get () in
    Format.printf "Starting visit@.";
    let prj = File.create_project_from_visitor "pcanalyzer" 
      (fun prj -> new pathcrawlerVisitor prj)
    in
    Format.printf "End visit@.";
    Project.set_current prj;
;;

let () = Db.Main.extend startup
