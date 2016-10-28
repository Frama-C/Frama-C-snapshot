open Cil_types
open Logic_const

let main () =
  let s, kf = Kernel_function.find_from_sid 2 in
  let s1 = Kernel_function.find_label kf "L1" in
  let s2 = Kernel_function.find_label kf "L2" in
  let s3 = Kernel_function.find_label kf "L3" in
  let add s a =
    Annotations.add_code_annot Emitter.end_user ~kf s (new_code_annotation a)
  in
  let add_behavior s spec_behavior =
    let contract =
      { spec_behavior; spec_variant = None; spec_terminates = None;
        spec_complete_behaviors = []; spec_disjoint_behaviors = [] }
    in
    add s (AStmtSpec ([],contract))
  in
  add s (AInvariant(["foo"], true, ptrue));
  add s (AVariant(tinteger 0, None));
  add s (AInvariant([], true, ptrue));
  add s (AInvariant(["foo"], true, ptrue));
  Filecheck.check_ast "after adding invariants";
  let requires = [Logic_const.new_predicate Logic_const.ptrue] in
  let bhv = [Cil.mk_behavior ~requires ()] in
  add_behavior !s1 bhv;
  Filecheck.check_ast "after adding contract";
  let post_cond = [Normal, Logic_const.new_predicate Logic_const.ptrue] in
  let bhv = [Cil.mk_behavior ~post_cond ()] in
  add_behavior !s1 bhv;
  Filecheck.check_ast "after merging contract";
  let requires = [Logic_const.new_predicate Logic_const.ptrue] in
  Annotations.add_requires Emitter.end_user kf ~stmt:!s2 requires;
  let post_cond = [Normal, Logic_const.new_predicate Logic_const.ptrue] in
  let bhv = [Cil.mk_behavior ~post_cond ()] in
  add_behavior !s2 bhv;
  Filecheck.check_ast "after merging requires and code_annot";
  let requires = [Logic_const.new_predicate Logic_const.ptrue] in
  let bhv = [Cil.mk_behavior ~requires ()] in
  add_behavior !s3 bhv;
  let post_cond = [Normal, Logic_const.new_predicate Logic_const.ptrue] in
  Annotations.add_ensures Emitter.end_user kf ~stmt:!s3 post_cond;
  Filecheck.check_ast "after merging code_annot and ensures"

let () = Db.Main.extend main
