(* Performs syntactic checks related to naming conventions in Frama-C's libc *)

open Cil_types

let warn_if_unnamed pred_type pred =
  if pred.pred_name = [] then
    Kernel.warning ~source:(fst pred.pred_loc) ~once:true
      "unnamed %s" pred_type

class special_pred_visitor p_cond p_name required_name outermost_pred_name = object
  inherit Visitor.frama_c_inplace

  method! vpredicate pred =
    if p_cond pred.pred_content &&
       not (List.mem required_name outermost_pred_name) then begin
      Kernel.warning ~source:(fst pred.pred_loc) ~once:true
        "clause with '%s' must contain name '%s'"
        p_name required_name;
      Cil.SkipChildren
    end
    else
      Cil.DoChildren
end

let contains_special_predicate p_cond p_name required_name pred =
  let outermost_pred_name = pred.pred_name in
  ignore
    (Visitor.visitFramacPredicate
       (new special_pred_visitor p_cond p_name required_name outermost_pred_name) pred)

let check_initialized =
  contains_special_predicate
    (fun p -> match p with | Pinitialized _ -> true | _ -> false)
    "\\initialized" "initialization"

let check_dangling =
  contains_special_predicate
    (fun p -> match p with | Pdangling _ -> true | _ -> false)
    "\\dangling" "danglingness"

let check_separated =
  contains_special_predicate
    (fun p -> match p with | Pseparated _ -> true | _ -> false)
    "\\separated" "separation"

let run_once = ref false

let () =
  Db.Main.extend (fun () ->
      if not !run_once then begin
        run_once := true;
        Globals.Functions.fold (fun kf () ->
            let fun_attrs =
              match kf.fundec with
              | Definition (fd, _) -> fd.svar.vattr
              | Declaration (_, vi, _, _) -> vi.vattr
            in
            if Cil.hasAttribute "fc_stdlib" fun_attrs then begin
              Annotations.iter_behaviors (fun _emitter bhv ->
                  List.iter (fun ip ->
                      let pred = ip.ip_content in
                      warn_if_unnamed "requires" pred;
                      check_initialized pred;
                      check_dangling pred;
                      check_separated pred;
                    ) bhv.b_requires;
                  List.iter (fun ip ->
                      let pred = ip.ip_content in
                      warn_if_unnamed "assumes" pred;
                      check_initialized pred;
                      check_dangling pred;
                      check_separated pred;
                    ) bhv.b_assumes;
                  List.iter (fun (_termination, ip) ->
                      let pred = ip.ip_content in
                      warn_if_unnamed "ensures" pred;
                      check_initialized pred;
                      check_dangling pred;
                      check_separated pred;
                    ) bhv.b_post_cond;
                ) kf;
            end
          ) ()
      end
    )
