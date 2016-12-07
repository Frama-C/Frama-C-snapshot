open Cil_types
open Lexing

let print_pos fmt pos =
  Format.fprintf
    fmt "line %d, char %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

class print_term = object(self)
  inherit Visitor.frama_c_inplace

  method private should_print =
    let n = Kernel_function.get_name (Extlib.the self#current_kf) in
    n = "f" || n = "main"

  method! vterm v =
    if  not self#should_print then Cil.SkipChildren
    else begin
        Kernel.feedback
          "Term %a:@\nstart %a@\nend %a"
          Printer.pp_term v
          print_pos (fst v.term_loc)
          print_pos (snd v.term_loc);
        Cil.DoChildren
      end
  method! vpredicate p =
    if not self#should_print then Cil.SkipChildren
    else begin
        Kernel.feedback
          "Predicate %a:@\nstart %a@\nend %a"
          Printer.pp_predicate p
          print_pos (fst p.pred_loc)
          print_pos (snd p.pred_loc);
        Cil.DoChildren
      end
end

let main () =
  Visitor.visitFramacFileSameGlobals (new print_term) (Ast.get())

let () = Db.Main.extend main
