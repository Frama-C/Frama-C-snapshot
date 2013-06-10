open Cil_types

let warn_cast =
  let typeForInsertedCast = !Cabs2cil.typeForInsertedCast in
  fun e t1 t2 ->
    Kernel.feedback ~source:(fst e.eloc) "Inserted implicit cast from %a to %a"
      Printer.pp_typ t1 Printer.pp_typ t2;
    typeForInsertedCast e t1 t2
  
let () = Cabs2cil.typeForInsertedCast := warn_cast

let run () =
  let f = Ast.get () in
  let output = function
    | GEnumTag(e,_) ->
        Kernel.feedback "Enum %s is represented by %a@."
          e.ename Printer.pp_ikind e.ekind
    | _ -> ()
  in 
  List.iter output f.globals
let () = Db.Main.extend run
