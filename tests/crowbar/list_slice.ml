open Crowbar

let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)

let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l

let python_list_re = Str.regexp "\\[\\(-?[0-9]+ ?,? ?\\)*\\]"
let python_elem_re = Str.regexp "\\(-?[0-9]+\\) ?,? ?"

let parse_python_list s =
  if Str.string_match python_list_re s 0 then begin
    let n = ref 1 in
    let ls = ref [] in
    while Str.string_match python_elem_re s !n do
      let number = Str.matched_group 1 s in
      let i = int_of_string number in
      ls := i :: !ls;
      let matched_len = String.length (Str.matched_string s) in
      n := !n + matched_len;
    done;
    Some (List.rev !ls)
  end else
    None

let pp_arg = Pretty_utils.pp_opt Format.pp_print_int

let python_slice list first last =
  let input = Format.asprintf "[%a][%a:%a]"
      (Pretty_utils.pp_list ~sep:", " Format.pp_print_int) list
      pp_arg first
      pp_arg last
  in
  let cmd = Format.asprintf "python -c 'print(%s)'" input in
  let res = cmd_to_list cmd in
  parse_python_list (List.hd res)

let test l first last =
  begin
    match python_slice l first last with
    | None ->
      Crowbar.fail ("could not parse python list slice")
    | Some oracle ->
      let result = Extlib.list_slice ?first ?last l in
      if oracle <> result then
        Crowbar.fail
          (Format.asprintf "oracle: [%a], result: [%a]"
             (Pretty_utils.pp_list ~sep:"; " Format.pp_print_int) oracle
             (Pretty_utils.pp_list ~sep:"; " Format.pp_print_int) result
          )
  end;
  true

let mk_arg =
  Crowbar.map [ Crowbar.option (Crowbar.range 20) ]
    (fun opt_x -> match opt_x with
       | None -> None
       | Some x -> Some (x - 10))

let gen_list = Crowbar.list (Crowbar.range 10000)

let () = Crowbar.add_test ~name:"Extlib.list_slice"
    [ gen_list; mk_arg; mk_arg ] @@
  (fun l first last -> Crowbar.check (test l first last))
