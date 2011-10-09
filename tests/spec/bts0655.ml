include 
  Plugin.Register(
    struct
      let name = "bts0655"
      let shortname = "bts0655"
      let help = "inspects relevant AST parts of bts0655.i"
    end)


class check_float =
object
inherit Visitor.frama_c_inplace
  method vterm t =
    result "term %a has type %a" 
      !Ast_printer.d_term t !Ast_printer.d_logic_type t.Cil_types.term_type;
    Cil.DoChildren
end

let run () =
  let f = Ast.get () in
  Visitor.visitFramacFileSameGlobals (new check_float) f
;;

Db.Main.extend run
;;
