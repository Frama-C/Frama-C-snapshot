open Logic_const
open Cil
open Cil_types

class foo = object
  inherit
    Visitor.generic_frama_c_visitor (Project.current()) (Cil.inplace_visit())
  method vstmt_aux stmt =
    let x = Cil.make_logic_var "x" Linteger in
    let loc = Cil.CurrentLoc.get () in
    Annotations.add_assert stmt true
      (pforall ([x],prel(Req,
                         {term_name = [];
                          term_node = TLval (TVar x,TNoOffset);
                          term_type = Linteger;
                          term_loc = loc},
                         {term_name = [];
                          term_node = TLval (TVar x,TNoOffset);
                          term_type = Linteger;
                          term_loc = loc}
                        )));
    DoChildren

  method vglob_aux _ = DoChildren

end;;

File.pretty Format.std_formatter;;

let file = Cil_state.file ();;

ignore (Cil.visitCilFileSameGlobals (new foo:>cilVisitor) file);;

Format.printf "%a@." (Cil.d_file (new Printer.print ())) file
