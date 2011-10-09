open Cil
open Cil_types

class visit prj =
  object(self)
    inherit Visitor.frama_c_copy prj
    method vbehavior b =
      let x = Globals.Vars.find_from_astinfo "X" VGlobal in
      let x = Cil.cvar_to_lvar x in
      let c = 
        Globals.Vars.find_from_astinfo 
          "c" (VFormal (Extlib.the self#current_kf))
      in
      let c = Cil.cvar_to_lvar c in
      b.b_assigns <- 
        Writes 
        [ Logic_const.new_identified_term (Logic_const.tvar x),
          From [ Logic_const.new_identified_term (Logic_const.tvar x);
                 Logic_const.new_identified_term (Logic_const.tvar c)]
        ];
      DoChildren
  end

let show_properties () =
  Format.printf "In project %a:@." Project.pretty (Project.current());
  Property_status.iter
    (fun p -> Format.printf "Status of %a: %a@."
      Property.pretty p Property_status.pretty (Property_status.get p))

let run () =
  let prj = 
    File.create_project_from_visitor "property_test" (fun p -> new visit p)
  in
  show_properties ();
  Project.on prj show_properties ()

let () = Db.Main.extend run
