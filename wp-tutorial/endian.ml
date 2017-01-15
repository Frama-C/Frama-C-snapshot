

let bit x k = x land (1 lsl k) <> 0

let reverse x =
  let u = ref 0 in
  for i = 0 to 7 do
    if bit x i then u := !u lor (1 lsl (7-i)) ;
  done ; !u

let () =
  begin
    Format.printf "@[<hov 0>@[<hov 2>const int endian[] = {" ;
    for i = 0 to 255 do
      if i>0 then Format.printf "," ;
      Format.printf "@ 0x%02X" (reverse i) ;
    done ;
    Format.printf "@]@ };@]@." ;
  end
