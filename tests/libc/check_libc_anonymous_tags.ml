(* Checks that the Frama-C libc does not declare any anonymous
   enums/structs/unions *)

open Cil_types

class tags_visitor = object
  inherit Visitor.frama_c_inplace
  val in_stdlib = ref false

  method! vglob_aux g =
    if Cil.hasAttribute "fc_stdlib" (Cil.global_attributes g) then
      begin
        in_stdlib := true;
        begin
          match g with
          | GEnumTag (ei, loc) | GEnumTagDecl (ei, loc) ->
            if ei.eorig_name = "" && !in_stdlib then
              Kernel.warning ~source:(fst loc) ~once:true
                "anonymous enum in Frama-C stdlib";
          | GCompTag (ci, loc) | GCompTagDecl (ci, loc) ->
            if ci.corig_name = "" && !in_stdlib then
              Kernel.warning ~source:(fst loc) ~once:true
                "anonymous %s in Frama-C stdlib"
                (if ci.cstruct then "struct" else "union");
          | _ -> ()
        end;
        Cil.DoChildren
      end
    else begin
      in_stdlib := false;
      Cil.SkipChildren
    end

  method! vtype typ =
    begin
      match typ with
      | TEnum (ei, _) when ei.eorig_name = "" && !in_stdlib ->
        Kernel.warning ~current:true ~once:true
          "anonymous enum in Frama-C stdlib";
        ()
      | TComp (ci, _, _) when ci.corig_name = "" && !in_stdlib ->
        Kernel.warning ~current:true ~once:true
          "anonymous %s in Frama-C stdlib"
          (if ci.cstruct then "struct" else "union")
      | _ -> ()
    end;
    Cil.DoChildren
end

let run_once = ref false

let () =
  Db.Main.extend (fun () ->
      if not !run_once then begin
        run_once := true;
        Visitor.visitFramacFile (new tags_visitor) (Ast.get ())
      end)
