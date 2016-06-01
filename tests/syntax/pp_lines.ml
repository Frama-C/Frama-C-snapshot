(* This module can be used to force the printing of #lines inside
   oracles.  If you want to use it inside another test, please move
   the EXECNOW that forces its compilation inside test_config, to
   avoid race conditions. *)

open Printer_api
let () =
  Cil_printer.state.line_directive_style <- Some Line_comment_sparse;
