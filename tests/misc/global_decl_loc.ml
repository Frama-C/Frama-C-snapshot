open Cil_types

let run () =
  Globals.Vars.iter
    (fun vi _ ->
       Kernel.result "global variable %a declared at %a"
         Printer.pp_varinfo vi
         Printer.pp_location vi.vdecl
    )

let () = Db.Main.extend run
