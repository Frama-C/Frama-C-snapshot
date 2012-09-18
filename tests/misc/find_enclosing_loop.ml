open Cil
open Cil_types

class check =
object(self)
  inherit Visitor.frama_c_inplace
  val current_loop = Stack.create ()
  method vstmt_aux s =
    let res =
      match s.skind with
        | Loop _ -> Stack.push s current_loop; 
            ChangeDoChildrenPost
              (s, 
               fun s -> ignore (Stack.pop current_loop); s)
        | _ -> DoChildren
    in
    let has_loop =
      try
        Some 
          (Kernel_function.find_enclosing_loop (Extlib.the self#current_kf) s)
      with Not_found -> None
    in
    (match has_loop with
      | Some s -> assert (s == Stack.top current_loop)
      | None -> assert (Stack.is_empty current_loop));
    res
end

let run () =
  Visitor.visitFramacFileSameGlobals (new check) (Ast.get())

let () = Db.Main.extend run
