open Cil
open Cil_types

let emitter = 
  Emitter.create "Property_test" [ Emitter.Funspec ] ~correctness:[] ~tuning:[]

class visit prj =
  object(self)
    inherit Visitor.frama_c_copy prj
    method! vbehavior b =
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
          ];
        let nkf = Visitor_behavior.Get.kernel_function self#behavior kf in
        let keep_empty = true in
        let post b =
          Queue.add
            (fun () ->
               Annotations.add_assigns ~keep_empty emitter nkf b.b_assigns)
          self#get_filling_actions;
          b
        in
        ChangeDoChildrenPost(b, post)
      end else DoChildren

    method! vstmt_aux stmt =
      match stmt.skind with
      | Return _ ->
        let kf = Extlib.the self#current_kf in
        let requires = [ Logic_const.new_predicate (Logic_const.ptrue) ] in
        let post_cond =
          [ Normal, Logic_const.new_predicate (Logic_const.pfalse) ]
        in
        let s1 = Cil.empty_funspec () in
        let b1 = Cil.mk_behavior ~requires () in
        s1.spec_behavior <- [ b1 ];
        let ca1 = Logic_const.new_code_annotation (AStmtSpec ([], s1)) in
        Annotations.add_code_annot emitter ~kf stmt ca1;
        let s2 = Cil.empty_funspec () in
        let b2 = Cil.mk_behavior ~post_cond () in
        s2.spec_behavior <- [ b2 ];
        let ca2 = Logic_const.new_code_annotation (AStmtSpec ([], s2)) in
        Annotations.add_code_annot emitter ~kf stmt ca2;
        Cil.DoChildren
      | _ -> Cil.DoChildren
  end

let show_properties () =
  Format.printf "In project %a:@." Project.pretty (Project.current());
  let strs =
    Property_status.fold
      (fun p acc ->
         let s = Format.asprintf "Status of %a: %a@."
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
