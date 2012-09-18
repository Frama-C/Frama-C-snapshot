open Cil_types
open Cil
open Format

class cF = object(self) inherit Visitor.frama_c_inplace

method vstmt s =
let fd = (Extlib.the self#current_func) in
match s.skind with
| Instr (Set (lv,e,loc)) ->
    let vi = makeLocalVar fd "varbidon" (typeOf e) in
    let sk = Instr (Set (var vi,new_exp ~loc (Lval lv),loc)) in
    let s0 = mkStmt ~valid_sid:true sk in
    ChangeTo (Cil.mkStmtCfgBlock [s0; s])
| _ -> SkipChildren
end

let run () =
Visitor.visitFramacFileSameGlobals (new cF) (Ast.get());
Cfg.clearFileCFG ~clear_id:false (Ast.get());
Cfg.computeFileCFG (Ast.get())

module Computed =
  State_builder.False_ref
    (struct let name = "Bidon"  let dependencies = [] end)

let main () =
  if not (Computed.get ()) then begin
    Computed.set true;
    if not (Ast.is_computed()) then Ast.compute();
     let prj =
       File.create_project_from_visitor
         "bidon" (fun prj -> new Visitor.frama_c_copy prj)
     in 
     Project.on prj run ();
  end

let () = Db.Main.extend main
