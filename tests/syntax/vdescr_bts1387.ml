open Cil_types

class print_vdescr =
  object
    inherit Visitor.frama_c_inplace
    method vvdec vi =
      Kernel.feedback "Variable %a has vdescr '%a'"
        Printer.pp_varinfo vi
        (Pretty_utils.pp_opt Format.pp_print_string) vi.vdescr;
      Cil.SkipChildren
  end

let run () = Visitor.visitFramacFileSameGlobals (new print_vdescr) (Ast.get())

let () = Db.Main.extend run
