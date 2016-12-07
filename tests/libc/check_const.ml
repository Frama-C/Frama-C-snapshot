open Cil_types

let warn_if_const string typ vi loc = 
  if Cil.typeHasQualifier "const" typ then
    Kernel.result ~source:(fst loc)
      "'requires \\valid%s' of a const variable %a. \
             You probably meant '\\valid_read%s' instead"
      string Printer.pp_varinfo vi string

let warn_if_not_const string typ vi loc = 
  if not (Cil.typeHasQualifier "const" typ) then
    Kernel.result ~source:(fst loc)
      "'requires \\valid_read%s' of a non-const variable %a. \
             You may have meant '\\valid%s'"
      string Printer.pp_varinfo vi string

let check_annot _ (a: identified_predicate) =
  let p = a.ip_content.pred_content in
  match p with
  | Pvalid (_, t) | Pvalid_read (_, t)
  | Papp ({l_var_info={lv_name=("valid_string"|"valid_read_string")}},
          _, [t]) ->
    begin
    let warn = match p with
      | Pvalid _ ->
        warn_if_const ""
      | Papp ({l_var_info={lv_name="valid_string"}},_,_) ->
        warn_if_const "_string"
      | Pvalid_read _ ->
        warn_if_not_const ""
      | Papp ({l_var_info={lv_name="valid_read_string"}},_,_) ->
        warn_if_not_const "_string"
      | _ -> assert false
    in
    match t.term_node with
    | TAddrOf (TVar lvi, _) -> begin
      match lvi.lv_origin with
      | Some ({vtype = typ} as vi) ->
        warn typ vi t.term_loc
      | _ -> ()
    end
    | TBinOp ((PlusPI | MinusPI | IndexPI),
              ({term_node = TLval (TVar lvi, _)} |
                  {term_node = TCastE (_, {term_node = TLval (TVar lvi, _)})}),
              _)
    | TLval (TVar lvi, _) -> begin
      match lvi.lv_origin with
      | Some vi ->
        warn (Cil.typeOf_pointed vi.vtype) vi t.term_loc
      | _ -> ()
    end
    | _ -> ()
  end
  | _ -> ()

let check () =
  let check_kf kf =
    let bhvs = Annotations.behaviors ~populate:false kf in
    List.iter (fun bhv ->
      Annotations.iter_requires check_annot kf bhv.b_name) bhvs
  in
  Globals.Functions.iter check_kf

let () = Db.Main.extend check
