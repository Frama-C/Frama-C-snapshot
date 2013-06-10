open Cil_types

class visitor =
object
  inherit Visitor.frama_c_inplace
  method vexpr e =
    match e.enode with
      | Const(CInt64 (_,_,Some s)) ->
          Format.printf "Found representation %s@." s; Cil.SkipChildren
      | Const(CInt64(n,_,None)) ->
          Format.printf "No representation for %s@." (Integer.to_string n);
          Cil.SkipChildren
      | _ -> Cil.DoChildren
end

let run () =
  let file = Ast.get () in
  Visitor.visitFramacFile (new visitor) file

let () = Db.Main.extend run
