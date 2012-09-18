open Cil
open Cil_types

let emitter = 
  Emitter.create "Property_test" [ Emitter.Funspec ] ~correctness:[] ~tuning:[]

class visit prj =
  object(self)
    inherit Visitor.frama_c_copy prj
    method vbehavior b =
      let kf = Extlib.the self#current_kf in
      if Kernel_function.get_name kf = "main" then begin
        let x = Globals.Vars.find_from_astinfo "X" VGlobal in
        let x = Cil.cvar_to_lvar x in
        let c = Globals.Vars.find_from_astinfo "c" (VFormal kf) in
        let c = Cil.cvar_to_lvar c in
        b.b_assigns <- 
          Writes 
          [ Logic_const.new_identified_term (Logic_const.tvar x),
            From [ Logic_const.new_identified_term (Logic_const.tvar x);
                   Logic_const.new_identified_term (Logic_const.tvar c)]
          ]
      end;
      ChangeTo b
  end

let show_properties () =
  Format.printf "In project %a:@." Project.pretty (Project.current());
  let strs =
    Property_status.fold
      (fun p acc ->
         let s = Pretty_utils.sfprintf "Status of %a: %a@."
           Property.pretty p Property_status.pretty (Property_status.get p)
         in
         Datatype.String.Set.add s acc
      ) Datatype.String.Set.empty
  in
  Datatype.String.Set.iter (Format.pp_print_string Format.std_formatter) strs

let run () =
  let prj = 
    File.create_project_from_visitor "property_test" (fun p -> new visit p)
  in
  show_properties ();
  Project.on prj show_properties ()

let () = Db.Main.extend run
