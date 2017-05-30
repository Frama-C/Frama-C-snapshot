open Cil_types

let emitter =
  Emitter.create "Fancy" [ Emitter.Global_annot ] ~correctness:[] ~tuning:[] 

class vis prj =
object(self)
  inherit Visitor.frama_c_copy prj

  method! vglob_aux g =
    match g with
    | GFun ({ svar = { vname = "main" }},_) ->
      let ax =
        Daxiomatic
          ("MyAxiomatic",
           [ Dlemma(
                 "myaxiom", true, [], [],
                 Logic_const.ptrue, [], Cil_datatype.Location.unknown)],
           [], Cil_datatype.Location.unknown)
      in
      Queue.add (fun () -> Annotations.add_global emitter ax)
        self#get_filling_actions;
      Cil.ChangeDoChildrenPost
        ([ GAnnot(ax, Cil_datatype.Location.unknown); g ], fun x -> x)
    | _ -> Cil.DoChildren

end

let transform () =
  Ast.compute ();
  let prj = File.create_project_from_visitor "prj" (fun prj -> new vis prj) in
  Project.on prj Filecheck.check_ast "prj";
  File.pretty_ast ~prj ()

let () = Db.Main.extend transform
