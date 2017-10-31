open Cil_types

let print_loc fmt (b,e) =
  let open Lexing in
  Format.fprintf fmt "Start line %d, char %d; End line %d, char %d"
    b.pos_lnum (b.pos_cnum - b.pos_bol) e.pos_lnum (e.pos_cnum - e.pos_bol)

class vis =
object
  inherit Visitor.frama_c_inplace
  method! vexpr e =
    (match e.enode with
     | Const (CStr _ | CWStr _ as c) ->
       Kernel.result "Constant %a location: %a"
         Printer.pp_constant c print_loc e.eloc
     | _ -> ());
    Cil.DoChildren
end

let do_it () = Visitor.visitFramacFileSameGlobals (new vis) (Ast.get())

let () = Db.Main.extend do_it
