open Cil_types
open Cil

class vis prj = 
object(self)
  inherit Visitor.frama_c_refresh prj
  method! vglob_aux g =
    match g with
      | GFun (f,loc) ->
        let my_kf = Extlib.the self#current_kf in
        let f1 = Visitor.visitFramacFunction (self:>Visitor.frama_c_visitor) f
        in
        let v2 = Cil.copyVarinfo f.svar (f.svar.vname ^ "1") in
        let orig = Visitor_behavior.Get_orig.varinfo self#behavior f.svar in
        Visitor_behavior.Set.varinfo self#behavior orig v2;
        Visitor_behavior.Set_orig.varinfo self#behavior v2 orig;
        Visitor_behavior.Reset.fundec self#behavior;
        Visitor_behavior.Reset.stmt self#behavior;
        let f2 = Visitor.visitFramacFunction (self:>Visitor.frama_c_visitor) f
        in
        f2.svar <- v2;
        self#set_current_kf my_kf;
        ChangeTo ([GFun(f1,loc); GFun(f2,loc)])
      | _ -> DoChildren
end

let run () =
  let prj =
    File.create_project_from_visitor "prj" (fun prj -> new vis prj)
  in
  File.pretty_ast ~prj ()

let () = Db.Main.extend run
