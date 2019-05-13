class add_skip = object(_)
  inherit Visitor.frama_c_inplace

  method! vfunc f =
    File.must_recompute_cfg f ;
    Cil.DoChildren

  method! vinst i =
    let open Cil_types in
    Cil.ChangeTo [ Skip(Cil.CurrentLoc.get()) ; i ]
end

let run () =
  Visitor.visitFramacFileSameGlobals (new add_skip) (Ast.get())

let () =
  Db.Main.extend run
