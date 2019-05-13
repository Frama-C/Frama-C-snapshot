open Crowbar

let reparse v s =
  let failure info =
    Crowbar.fail
      ("Pretty-printing '" ^ (Z.to_string v) ^ "' returns '" ^ s ^ "'" ^ info)
  in
  if String.length s <= 2 then failure "";
  let is_neg = s.[0] = '1' in
  let is_hex = s.[1] = 'x' in
  let s = String.(concat "" (split_on_char '_' s)) in
  let v' =
    if is_neg then begin
      let chr = if is_hex then 'F' else '1' in
      let module M = struct exception Found of int end in
      let check i c = if i > 1 && c <> chr then raise (M.Found i) in
      try String.iteri check s; Z.minus_one
      with M.Found idx ->
        let len, v' =
          if is_hex then begin
            let remains = String.sub s idx (String.length s - idx) in
            let v' = Z.of_string ("0x" ^ remains) in
            4 * (String.length remains), v'
          end else begin
           let remains = String.sub s idx (String.length s - idx) in
           let v' = Z.of_string ("0b" ^ remains) in
           String.length remains, v'
         end
        in
        let m = Z.(one lsl len) in
        let m = Z.pred m in
        let v' = Z.logxor m v' in
        Z.pred (Z.lognot (Z.pred v'))
    end else
      Z.of_string s
  in
  if not (Z.equal v v') then
    failure (" reparsed as '" ^ Z.format "%b" v' ^ "' (" ^ Z.to_string v' ^ ")")

let test z is_hex nbits has_sep =
  guard (nbits >= 0 && nbits <= 1024);
  let sep = if has_sep then Some "_" else None in
  let pp z = if is_hex then
      Integer.pp_hex ~nbits ?sep z
    else
      Integer.pp_bin ~nbits ?sep z
  in
  let s = Format.asprintf "%a" pp z in
  reparse z s

let zarith =
  let open Crowbar in
  fix (fun zarith ->
      choose
        [ map [int64] Z.of_int64;
          map [zarith; int64] (fun z i -> Z.((z lsl 64) + of_int64 i)) ])

let () = Crowbar.add_test ~name:"pp_bin_hex"
    [ zarith; Crowbar.bool; Crowbar.int; Crowbar.bool ] test
