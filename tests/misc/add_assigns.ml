open Cil_types

let emitter =
  Emitter.(
    create "assigns" [ Property_status; Funspec ] ~correctness:[] ~tuning:[])

let computed = ref false
        
let main () =
  if not !computed then begin
    computed := true;
    Ast.compute ();
    let kf = Globals.Functions.find_by_name "f" in
    let y = Globals.Vars.find_from_astinfo "y" (VFormal kf) in
    let mem =
      Logic_const.(
        new_identified_term
          (term (TLval (TMem (tvar (Cil.cvar_to_lvar y)), TNoOffset))
             (Ctype Cil.intType)))
    in
    let assigns = Writes [mem, FromAny] in
    let bhv = Cil.mk_behavior ~assigns () in
    Annotations.add_behaviors emitter kf [bhv];
    let bhv =
      List.find
        (fun b -> b.b_name = Cil.default_behavior_name)
        (Annotations.behaviors kf)
    in
    let ip = Extlib.the (Property.ip_assigns_of_behavior kf Kglobal [] bhv) in
    Property_status.(emit emitter ~hyps:[] ip True)
  end

let () = Db.Main.extend main
