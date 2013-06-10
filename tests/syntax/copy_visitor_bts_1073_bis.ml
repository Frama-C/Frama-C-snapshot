(*============================================================================*)
module P = Plugin.Register
             (struct
                let name = "Testing plugin"
                let shortname = "test"
                let help = "Just to test Filter..."
              end)
module Opt = P.False
               (struct
                  let option_name = "-test"
                  let help = "switch the plug-in on"
                end)
(*============================================================================*)
module Visi = struct
  exception EraseAssigns
  exception EraseAllocation

  type fct = unit
  type proj = unit

  let fct_name vf _fi = vf.Cil_types.vname
  let fct_info () _ = [ () ]
  let param_visible _ _ = true
  let body_visible _fi = true
  let loc_var_visible _ _ = true
  let inst_visible _ _ = true
  let label_visible _ _ _ = true
  let annotation_visible  _ _ _ = true
  let fun_precond_visible _ _ = true
  let fun_postcond_visible  _ _ = true
  let fun_variant_visible _ _ = true
  let fun_frees_visible _ _ = true
  let fun_allocates_visible _ _ = true
  let fun_assign_visible _ _ = true
  let fun_deps_visible _ _ = true
  let called_info _ _ = None
  let res_call_visible _ _ = true
  let result_visible _ _ = true
  let cond_edge_visible _ _ = true, true
end
(*============================================================================*)
let main () =
  if Opt.get () then
    begin
      let _ast = Ast.get () in
        P.feedback "start compute";
      let new_proj_name = "filtered" in
      let module Transform = Filter.F (Visi) in
      let new_prj = Transform.build_cil_file new_proj_name () in
        Project.on new_prj Opt.clear ();
        P.feedback "exported in new project : %s" new_proj_name
    end

let () = Db.Main.extend main
(*============================================================================*)

