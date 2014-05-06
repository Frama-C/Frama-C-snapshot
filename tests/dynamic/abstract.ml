
(* register functions using abstract types "t" and "u" *)
module A : sig end = struct
  type t = A of int | B of bool
  type tt = t
  let mk () = 1.05
  let _ = B false
  let f = function A n -> n | B false -> min_int | B true -> max_int
  module T = 
    Datatype.Make(struct
      type t = tt
      let name = "A.t"
      let reprs = [ A 1 ]
      include Datatype.Undefined
    end)
  let t = T.ty
  module U = 
    Datatype.Make(struct
      type t = float
      let name = "A.u"
      let reprs = [ 1.0 ]
      include Datatype.Undefined
    end)
  let u = U.ty
  let mk =
    Dynamic.register ~plugin:"A" ~journalize:false "mk"
      (Datatype.func Datatype.unit u)
      mk
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "f"
      (Datatype.func t Datatype.int)
      f
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "g" 
      (Datatype.func u Datatype.int)
      (fun x -> Format.printf "%f@." x; int_of_float x)
  let v1 = Dynamic.register ~plugin:"A" ~journalize:false "v1" t (A 1)
  let _ = Dynamic.register ~plugin:"A" ~journalize:false "v2" t (A 2)
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "h"
      (Datatype.func t (Datatype.func u Datatype.bool))
      (fun x y ->
	 match x with A x ->
	   Format.printf "params = %d %f@." x y;
	   x = int_of_float y | B _ -> false)
  let _ =
    Dynamic.register ~plugin:"A" ~journalize:false "succ"
      (Datatype.func Datatype.int Datatype.int) succ
  let _ =
    Dynamic.register ~journalize:false "ho" ~plugin:"A"
      (Datatype.func (Datatype.func Datatype.int Datatype.int) (Datatype.func t u))
      (fun ff x -> float (ff (f x)))
  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "ppu" (Datatype.func u Datatype.unit)
      (fun f -> Format.printf "ppu %f@." f)
  let ho2 =
    Dynamic.register ~plugin:"A" "ho2" ~journalize:false
      (Datatype.func (Datatype.func t Datatype.int) (Datatype.func t u))
      (fun f x -> float (f x))

  let _ = 
    ignore (Dynamic.get ~plugin:"A" "mk" (Datatype.func Datatype.unit u) ())

  module UA = Type.Abstract(struct let name = "A.u" end)
  let __ : UA.t =
    Dynamic.get ~plugin:"A" "mk" (Datatype.func Datatype.unit UA.ty) ()

  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "poly"
      (Datatype.list u) [ 1.; 2.; 3. ]

  let _ =
    Dynamic.register ~journalize:false ~plugin:"A" "poly2" (Datatype.list u)
      [ mk (); ho2 (function A n -> n | B _ -> min_int) v1; ho2 f v1 ]

end

(* use of the abstract functions *)
module B = struct
  module T = Type.Abstract(struct let name = "A.t" end)
  let ty = T.ty
  let _ =
    Type.register ~ml_name:None ~name:"B.t" Structural_descr.t_unknown [ 0.0 ]
  module U = Type.Abstract(struct let name = "A.u" end)
  let ty' = U.ty
  let fut = Datatype.func Datatype.unit ty'
  let mk = Dynamic.get ~plugin:"A" "mk" fut
  let g = Dynamic.get ~plugin:"A" "g" (Datatype.func ty' Datatype.int)
  let f = Dynamic.get ~plugin:"A" "f" (Datatype.func ty Datatype.int)
  let h =
    Dynamic.get ~plugin:"A" "h"
      (Datatype.func ty (Datatype.func ty' Datatype.bool))
  let v1 = Dynamic.get ~plugin:"A" "v1" ty
  let v2 = Dynamic.get ~plugin:"A" "v2" ty
  let cinq =
    Dynamic.get ~plugin:"A" "succ" (Datatype.func Datatype.int Datatype.int) 4
  let () = Format.printf "succ=%d@." cinq
  let () = Format.printf "n=%d@." (g (mk ()))
  let () = Format.printf "v1=%d@." (f v2)
  let () = Format.printf "b1=%b@." (h v1 (mk ()))
  let () = Format.printf "b2=%b@." (h v2 (mk ()))
  let ho =
    Dynamic.get ~plugin:"A" "ho"
      (Datatype.func
	 (Datatype.func Datatype.int Datatype.int) (Datatype.func ty ty'))
  let ppu = Dynamic.get ~plugin:"A" "ppu" (Datatype.func ty' Datatype.unit)
  let res =
    ho (Dynamic.get ~plugin:"A" "succ"
	  (Datatype.func Datatype.int Datatype.int)) v2
  let () = Format.printf "print:@."; ppu res
  let ho_bug =
    try
      ignore
	(Dynamic.get ~plugin:"A" "ho"
	   (Datatype.func
	      (Datatype.func ty Datatype.int) (Datatype.func ty ty')) f v2);
      assert false
    with Dynamic.Incompatible_type s ->
      print_endline s
  (*  let () = (* is now statically checked and no more dynamically *)
    try
      List.iter
	(Dynamic.get ~plugin:"A" "ppu" (Datatype.func ty' Datatype.unit))
	(Dynamic.get ~plugin:"A" "poly" (Datatype.list ty'));
      assert false
    with Dynamic.Incompatible_type s ->
      print_endline s*)
  let () =
    List.iter
      (Dynamic.get ~plugin:"A" "ppu" (Datatype.func ty' Datatype.unit))
      (Dynamic.get ~plugin:"A" "poly2" (Datatype.list ty'))
end

