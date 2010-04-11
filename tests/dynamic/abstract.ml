
(* register functions using abstract types "t" and "u" *)
module A : sig end = struct
  type t = A of int | B of bool
  type u = float
  let mk () = 1.05
  let ppt p_caller fmt = function
    | A n ->
	Type.par p_caller Type.Call fmt
	  (fun fmt -> Format.fprintf fmt "A %d" n)
    | B b ->
	Type.par p_caller Type.Call fmt
	  (fun fmt -> Format.fprintf fmt "B %b" b)
  let ppu _ fmt f = Format.fprintf fmt "%f" f
  let f = function A n -> n | B false -> min_int | B true -> max_int
  let t : t Type.t = Type.register ~pp:ppt ~name:"A.t" ~value_name:None [ A 1 ]
  let u : u Type.t = Type.register ~value_name:None ~pp:ppu ~name:"A.u" [ 1.0 ]
  let mk =
    Dynamic.register ~plugin:"A" ~journalize:false "mk" (Type.func Type.unit u)
      mk
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "f" (Type.func t Type.int) f
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "g" (Type.func u Type.int)
      (fun x -> Format.printf "%f@." x; int_of_float x)
  let v1 = Dynamic.register ~plugin:"A" ~journalize:false "v1" t (A 1)
  let _ = Dynamic.register ~plugin:"A" ~journalize:false "v2" t (A 2)
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "h"
      (Type.func t (Type.func u Type.bool))
      (fun x y ->
	 match x with A x ->
	   Format.printf "params = %d %f@." x y;
	   x = int_of_float y | B _ -> false)
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "succ"
      (Type.func Type.int Type.int) succ
  let _ =
    Dynamic.register ~journalize:false "ho" ~plugin:"A"
      (Type.func (Type.func Type.int Type.int) (Type.func t u))
      (fun ff x -> float (ff (f x)))
  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "ppu" (Type.func u Type.unit)
      (fun f -> Format.printf "ppu %f@." f)
  let ho2 =
    Dynamic.register ~plugin:"A" "ho2" ~journalize:false
      (Type.func (Type.func t Type.int) (Type.func t u))
      (fun f x -> float (f x))

  let _ = ignore (Dynamic.get ~plugin:"A" "mk" (Type.func Type.unit u) ())

  let _ =
    (Dynamic.get ~plugin:"A" "mk"
       (Type.func Type.unit (Type.get_dynamic "A.u")) ())

  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "poly"
      (Type.list u) [ 1.; 2.; 3. ]

  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "poly2" (Type.list u)
      [ mk (); ho2 (function A n -> n | B _ -> min_int) v1; ho2 f v1 ]

end

(* use of the abstract functions *)
module B = struct
  let ty = Type.get_dynamic "A.t"
  let _ = Type.register ~value_name:None ~name:"B.t" [ 0.0 ]
  let ty' = Type.get_dynamic "A.u"
  let fut = Type.func Type.unit ty'
  let mk = Dynamic.get ~plugin:"A" "mk" fut
  let g = Dynamic.get ~plugin:"A" "g" (Type.func ty' Type.int)
  let f = Dynamic.get ~plugin:"A" "f" (Type.func ty Type.int)
  let h = Dynamic.get ~plugin:"A" "h" (Type.func ty (Type.func ty' Type.bool))
  let v1 = Dynamic.get ~plugin:"A" "v1" ty
  let v2 = Dynamic.get ~plugin:"A" "v2" ty
  let cinq = Dynamic.get ~plugin:"A" "succ" (Type.func Type.int Type.int) 4
  let () = Format.printf "succ=%d@." cinq
  let () = Format.printf "n=%d@." (g (mk ()))
  let () = Format.printf "v1=%d@." (f v2)
  let () = Format.printf "b1=%b@." (h v1 (mk ()))
  let () = Format.printf "b2=%b@." (h v2 (mk ()))
  let ho =
    Dynamic.get ~plugin:"A" "ho"
      (Type.func (Type.func Type.int Type.int) (Type.func ty ty'))
  let ppu = Dynamic.get ~plugin:"A" "ppu" (Type.func ty' Type.unit)
  let res = ho (Dynamic.get ~plugin:"A" "succ" (Type.func Type.int Type.int)) v2
  let () = Format.printf "print:@."; ppu res
  let ho_bug =
    try
      ignore (Dynamic.get ~plugin:"A" "ho"
		(Type.func (Type.func ty Type.int) (Type.func ty ty')) f v2);
      assert false
    with Type.StringTbl.Incompatible_type s ->
      print_endline s
  let () =
    try
      List.iter
	(Dynamic.get ~plugin:"A" "ppu" (Type.func ty' Type.unit))
	(Dynamic.get ~plugin:"A" "poly" (Type.list ty'));
      assert false
    with Type.StringTbl.Incompatible_type s ->
      print_endline s
  let () =
    List.iter
      (Dynamic.get ~plugin:"A" "ppu" (Type.func ty' Type.unit))
      (Dynamic.get ~plugin:"A" "poly2" (Type.list ty'))
end

