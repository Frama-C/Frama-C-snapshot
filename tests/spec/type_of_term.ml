open Cil_types

include Plugin.Register
  (struct
      let name = "type_of_term"
      let shortname = "type_of_term"
      let help = "checks typeOfTermLval over sets"
   end)

class visitor =
  object
    inherit Visitor.frama_c_inplace
    method vterm t =
      result "Term: %a, type is %a"
        !Ast_printer.d_term t !Ast_printer.d_logic_type t.Cil_types.term_type;
      Cil.DoChildren
    method vterm_lval (host,off as lv) =
      let ty = Cil.typeOfTermLval lv in
      let plain_lval = (host,TNoOffset) in
      let tyh = Cil.typeOfTermLval plain_lval in
      let tyoff = Cil.typeTermOffset tyh off in
      result "Host: %a, type is %a"
        !Ast_printer.d_term_lval plain_lval !Ast_printer.d_logic_type tyh;
      result "Offset: %a, type is %a"
        !Ast_printer.d_term_offset off !Ast_printer.d_logic_type tyoff;
      result "Lval: %a, type is %a"
        !Ast_printer.d_term_lval lv !Ast_printer.d_logic_type ty;
      Cil.DoChildren
  end

let run () =
  let ast = Ast.get () in
  Visitor.visitFramacFileSameGlobals (new visitor) ast
;;

Db.Main.extend run
