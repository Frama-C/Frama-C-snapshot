module P =
  Plugin.Register(struct
    let name = "remove_status_hyps"
    let shortname = "rsh"
    let help = ""
  end)

let emitter =
  Emitter.(create "test" [ Property_status ] ~correctness:[] ~tuning:[])

let pretty_status fmt = function
  | Property_status.Best(s, [ e ]) ->
    Format.fprintf fmt "%a (hyps: %t)"
      Property_status.Emitted_status.pretty s
      (fun fmt ->
        Pretty_utils.pp_list Property.pretty fmt e.Property_status.properties)
  | Property_status.Never_tried ->
    Format.fprintf fmt "no try"
  | Property_status.Inconsistent _
  | Property_status.Best(_, ([] | _ :: _ :: _)) -> assert false

let report msg l =
  P.feedback msg;
  List.iter
    (fun (_, _, p) ->
      P.feedback "%a: %a"
        Property.pretty p
        pretty_status (Property_status.get p))
    l

let main () =
  let kf =
    try Globals.Functions.find_by_name "main"
    with Not_found -> assert false
  in
  (* for any annotation, emits valid depending on the previous annotations *)
  let _, l =
    Annotations.fold_all_code_annot
      ~sorted:true
      (fun stmt _ ca (even, acc) ->
        let ppt = Property.ip_of_code_annot_single kf stmt ca in
        (if even then Property_status.(emit emitter ~hyps:[] ppt Dont_know)
         else
            let hyps = List.map (fun (_, _, ppt) -> ppt) acc in
            Property_status.(emit emitter ~hyps ppt Dont_know));
        not even, (ca, stmt, ppt) :: acc)
      (true, [])
  in
  let l = List.rev l in
  report "initial statuses" l;
  (* exactly four annotations in the tested program *)
  match l with
  | (a1, s1, p1) :: ([ a2, s2, p2; a3, s3, p3; a4, s4, p4 ] as l') ->
    Property_status.(emit emitter ~hyps:[ p2 ] p4 True);
    report "P4 only depends on P2" l;
    Annotations.remove_code_annot emitter ~kf s1 a1;
    report "removing P1" l'
  | _ -> assert false

let () = Db.Main.extend main
