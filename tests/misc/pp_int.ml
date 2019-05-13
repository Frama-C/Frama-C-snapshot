(* -------------------------------------------------------------------------- *)
(* --- Test for Integer.pp_int and Integer.pp_hex                         --- *)
(* -------------------------------------------------------------------------- *)

let pp_bin_naive ~sep fmt x =
  Format.pp_print_string fmt (if 0 <= x then "0b" else "1b") ;
  for i = 31 downto 0 do
    let b = (x land (1 lsl i)) <> 0 in
    Format.pp_print_char fmt (if b then '1' else '0') ;
    if i > 0 && i mod 4 = 0 then Format.pp_print_string fmt sep ;
  done

let pp_hex_naive ~sep fmt x =
  let m = Printf.sprintf "%08X" x in
  let n = String.length m in
  let m = if n < 8 then m else String.sub m (n-8) 8 in
  let n = String.length m in
  Format.pp_print_string fmt (if 0 <= x then "0x" else "1x") ;
  for i = n - 1 downto 0 do
    Format.pp_print_char fmt m.[n-1-i] ;
    if i > 0 && i mod 4 = 0 then Format.pp_print_string fmt sep ;
  done

let pp_bar fmt c =
  Format.fprintf fmt "%s@\n" (String.make 70 c)

let testvalue ~nbits ~sep ~tbin ~thex fmt x =
  begin
    let v = Integer.of_int x in
    let v2 = Integer.(lognot v) in
    pp_bar fmt '-' ;
    Format.fprintf fmt "value   '%d' '%x'@\n" x x ;
    Format.fprintf fmt "refhex  '%a'@\n" (pp_hex_naive ~sep) x ;
    Format.fprintf fmt "pp_hex  '%s%a'@\n" thex (Integer.pp_hex ~nbits ~sep) v ;
    Format.fprintf fmt "refbin  '%a'@\n" (pp_bin_naive ~sep) x ;
    Format.fprintf fmt "pp_bin  '%s%a'@\n" tbin (Integer.pp_bin ~nbits ~sep) v ;
    Format.fprintf fmt "pp_neg  '%s%a'@\n" tbin (Integer.pp_bin ~nbits ~sep) v2 ;
  end

let testdata data fmt =
  begin
    List.iter
      (fun (nbits,sep,values) ->
         pp_bar fmt '=' ;
         Format.fprintf fmt "Nbits: %d   Sep: %S@\n" nbits sep ;
         List.iter
           (fun (nhex,nbin,values) ->
              let tbin = String.make nbin ' ' in
              let thex = String.make nhex ' ' in
              List.iter (testvalue ~nbits ~sep ~tbin ~thex fmt) values
           ) values ;
      ) data ;
    pp_bar fmt '-' ;
  end

let () =
  Format.printf "%t"
    begin testdata [
        0, "," , [
          0 , 15 , [ 65537;65536 ] ;
          5 , 20 , [ 65335;65534 ] ;
          5 , 30 , [ 127;128;129 ] ;
          5 , 35 , [ 2;1;0;-1;-2;-3 ] ;
          5 , 30 , [ -126;-127;-128;-129;-130 ] ;
          5 , 30 , [ -254;-255;-256 ] ;
          5 , 25 , [ -257;-258 ] ;
          5 , 20 , [ -65534;-65535;-65536 ] ;
          0 , 15 , [ -65537;-65538 ] ;
        ] ;
        8, "" , [
          4 , 16 , [ 65335;65534 ] ;
          4 , 24 , [ 2;1;0;-1;-2 ] ;
          4 , 20 , [ -254;-255;-256 ] ;
          4 , 16 , [ -65534;-65535;-65536 ] ;
        ] ;
        10, "" , [
          4 , 16 , [ 65335;65534 ] ;
          4 , 20 , [ 2;1;0;-1;-2 ] ;
          4 , 20 , [ -254;-255;-256 ] ;
          4 , 16 , [ -65534;-65535;-65536 ] ;
        ] ;
        10, "." , [
          5 , 20 , [ 65335;65534 ] ;
          5 , 25 , [ 2;1;0;-1;-2 ] ;
          5 , 25 , [ -254;-255;-256 ] ;
          5 , 20 , [ -65534;-65535;-65536 ] ;
        ] ;
      ]
    end
    
