(** Counting for loops on the untyped AST.
*)


module Enabled = Cmdline.Dynamic.Register.False(
  struct 
    let name = "cea.untyped_metrics" 
  end)

open Cabs

class count_for = 
object inherit Cabsvisit.nopCabsVisitor as super
       val mutable counted_for = 0
       method counted_for = counted_for
       method vstmt s =
	 begin match s.stmt_node with
	   | FOR _ -> counted_for <- counted_for + 1
	   | _ -> ()
	 end;
	 super#vstmt s
end

let count_for (fname,_ as file) = 
  let counter = new count_for in
  ignore (Cabsvisit.visitCabsFile (counter:>Cabsvisit.cabsVisitor) file);
  fname,counter#counted_for 
  
let print_stat (fname,n) = Format.printf "%s: %d@." fname n

let startup () = 
  if Enabled.get () then begin
    let untyped_files = Cil_state.UntypedFiles.get () in
      
    let stats = List.map count_for untyped_files in
      List.iter print_stat stats
  end


let options = 
  [ "-count_for",
    Arg.Unit Enabled.on,
    ": pretty print the number of for loops per file" ]

let () = 
  Options.add_plugin
    ~name:"Untyped Metrics"
    ~descr:"pretty prints metrics on the untyped AST"
    options;
  Dynamic.Main.extend startup

