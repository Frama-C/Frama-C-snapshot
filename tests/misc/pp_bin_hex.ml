let pp_dec fmt z = Integer.pretty ~hexa:false fmt z
let pp_hex fmt z = Integer.pp_hex ~nbits:16 ~sep:"_" fmt z
let pp_bin fmt z = Integer.pp_bin ~nbits:8  ~sep:"_" fmt z

let hrule () =
  Format.printf "--------------------------------------------------@."

let testcase z =
  begin
    hrule () ;
    Format.printf "Dec. %a@." pp_dec z ;
    Format.printf "Hex. %a@." pp_hex z ;
    Format.printf "Bin. %a@." pp_bin z ;
  end

let () =
  begin
    List.iter
      (fun z ->
         testcase z ;
         if not (Integer.equal z Integer.zero) then
           testcase (Integer.neg z)
      ) [
        Integer.of_string "0" ;
        Integer.of_string "1" ;
        Integer.of_string "2" ;
        Integer.of_string "5" ;
        Integer.of_string "9" ;
        Integer.of_string "16" ;
        Integer.of_string "127" ;
        Integer.of_string "128" ;
        Integer.of_string "0xFF" ;
        Integer.of_string "0xFF0F000F" ;
        Integer.of_string "0x17070007" ;
      ] ;
    hrule () ;
  end
