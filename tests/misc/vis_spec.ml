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
	    Format.printf "@[Funspec of f is@ @['%a'@]@ through visitor@]@."
              Printer.pp_funspec sp;
	    Format.printf "@[It is@ @['%a'@]@ through get_spec@]@."
              Printer.pp_funspec
              (Annotations.funspec (Globals.Functions.get f.svar));
          )
      | None -> 
        Format.printf "@[Function prototype;@ Funspec is@ @['%a'@]@]@."
          Printer.pp_funspec sp;
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
