(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

let memory_footprint_var_name = "FRAMA_C_MEMORY_FOOTPRINT"

let memory_footprint =
  let error () =
    Kernel.error "@[Bad value for environment variable@ %s.@ Expected value: \
     integer between@ 1 and 10.@ Using@ default value@ of 2.@]"
      memory_footprint_var_name;
    2
  in
  try
    let i = int_of_string (Sys.getenv memory_footprint_var_name) in
    if i <= 0 || i > 10 then error ()
    else i
  with
  | Not_found -> 2
  | Failure "int_of_string" -> error ()

let cache_size = 1 lsl (8 + memory_footprint)

module type Cacheable =
sig
  type t
  val hash : t -> int
  val sentinel : t
  val equal : t -> t -> bool
end

module type Result =
sig
  type t
  val sentinel : t
end

module Array_2 =
struct
  type ('a, 'b) t

  let (clear : ('a, 'b) t -> 'a -> 'b -> unit)
      = fun t a b ->
        let t = Obj.repr t in
        let size2 = Obj.size t in
        let i = ref 0 in
        while (!i < size2)
        do
          let base = !i in
          Obj.set_field t (base)   (Obj.repr a);
          Obj.set_field t (base+1) (Obj.repr b);
          i := base + 2;
        done

  let (make : int -> 'a -> 'b -> ('a, 'b) t)
      = fun size a b ->
        let size2 = 2 * size in
        let t  = Obj.obj (Obj.new_block 0 size2) in
        clear t a b;
        t

  let (set : ('a, 'b) t -> int -> 'a -> 'b -> unit)
      = fun t i a b ->
        let t = Obj.repr t in
        let base = 2 * i in
        Obj.set_field t (base)   (Obj.repr a);
        Obj.set_field t (base+1) (Obj.repr b)

  let (get0 :
          ('a, 'b) t -> int -> 'a)
      = fun t i ->
        let t = Obj.repr t in
        let base = 2 * i in
        Obj.obj (Obj.field t (base))

  let (get1 : ('a, 'b) t -> int -> 'b)
      = fun t i ->
        let t = Obj.repr t in
        let base = 2 * i in
        Obj.obj (Obj.field t (base+1))
end

module Array_3 =
struct
  type ('a, 'b, 'c) t

  let (clear : ('a, 'b, 'c) t ->
        'a -> 'b -> 'c -> unit)
      = fun t a b c ->
        let t = Obj.repr t in
        let size3 = Obj.size t in
        let i = ref 0 in
        while (!i < size3)
        do
          let base = !i in
          Obj.set_field t (base)   (Obj.repr a);
          Obj.set_field t (base+1) (Obj.repr b);
          Obj.set_field t (base+2) (Obj.repr c);
          i := base + 3;
        done

  let (make : int -> 'a -> 'b -> 'c -> ('a, 'b, 'c) t)
      = fun size a b c ->
        let size3 = 3 * size in
        let t  = Obj.obj (Obj.new_block 0 size3) in
        clear t a b c;
        t

  let (set : ('a, 'b, 'c) t -> int -> 'a -> 'b -> 'c -> unit)
      = fun t i a b c ->
        let t = Obj.repr t in
        let base = 3 * i in
        Obj.set_field t (base)   (Obj.repr a);
        Obj.set_field t (base+1) (Obj.repr b);
        Obj.set_field t (base+2) (Obj.repr c)

  let (get0 :
          ('a, 'b, 'c) t -> int -> 'a)
      = fun t i ->
        let t = Obj.repr t in
        let base = 3 * i in
        Obj.obj (Obj.field t (base))

  let (get1 : ('a, 'b, 'c) t -> int -> 'b)
      = fun t i ->
        let t = Obj.repr t in
        let base = 3 * i in
        Obj.obj (Obj.field t (base+1))

  let (get2 :
          ('a, 'b, 'c) t -> int -> 'c)
      = fun t i ->
        let t = Obj.repr t in
        let base = 3 * i in
        Obj.obj (Obj.field t (base+2))
end

module Array_7 =
struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t

  let (clear : ('a , 'b , 'c , 'd , 'e , 'f , 'g) t ->
        'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> unit)
      = fun t a b c d e f g ->
        let t = Obj.repr t in
        let size7 = Obj.size t in
        let i = ref 0 in
        while (!i < size7)
        do
          let base = !i in
          Obj.set_field t (base)   (Obj.repr a);
          Obj.set_field t (base+1) (Obj.repr b);
          Obj.set_field t (base+2) (Obj.repr c);
          Obj.set_field t (base+3) (Obj.repr d);
          Obj.set_field t (base+4) (Obj.repr e);
          Obj.set_field t (base+5) (Obj.repr f);
          Obj.set_field t (base+6) (Obj.repr g);
          i := base + 7;
        done

  let (make : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g ->
        ('a , 'b , 'c , 'd , 'e , 'f , 'g) t)
      = fun size a b c d e f g ->
        let size7 = 7 * size in
        let t  = Obj.obj (Obj.new_block 0 size7) in
        clear t a b c d e f g;
        t

  let (set :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int ->
        'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> unit)
      = fun t i a b c d e f g ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.set_field t (base)   (Obj.repr a);
        Obj.set_field t (base+1) (Obj.repr b);
        Obj.set_field t (base+2) (Obj.repr c);
        Obj.set_field t (base+3) (Obj.repr d);
        Obj.set_field t (base+4) (Obj.repr e);
        Obj.set_field t (base+5) (Obj.repr f);
        Obj.set_field t (base+6) (Obj.repr g)

  let (get0 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'a)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base))

  let (get1 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'b)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+1))

  let (get2 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'c)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+2))

  let (get3 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'd)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+3))

  let (get4 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'e)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+4))

  let (get5 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'f)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+5))

  let (get6 :
          ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> int -> 'g)
      = fun t i ->
        let t = Obj.repr t in
        let base = 7 * i in
        Obj.obj (Obj.field t (base+6))

end

module Make_Symmetric (H: Cacheable) (R: Result) =
struct
  let size = cache_size
  let cache = Array_3.make size H.sentinel H.sentinel R.sentinel

  let mask = pred size

  let clear () =
    Array_3.clear cache H.sentinel H.sentinel R.sentinel

  let hash = H.hash

  let merge f a0 a1 =
    let a0, a1, h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in

    if H.equal (Array_3.get0 cache has) a0
      && H.equal (Array_3.get1 cache has) a1
    then begin
(*      Format.printf "Cache O@.";  *)
        Array_3.get2 cache has
      end
    else
      let result = f a0 a1 in
(*      Format.printf "Cache N@."; *)
      Array_3.set cache has a0 a1 result;
      result
end


module Make_Asymmetric (H: Cacheable) (R: Result) =
struct
  let size = cache_size
  let cache = Array_3.make size H.sentinel H.sentinel R.sentinel

  let mask = pred size

  let clear () =
    Array_3.clear cache H.sentinel H.sentinel R.sentinel

  let merge f a0 a1 =
    let h0 = H.hash a0 in
    let h1 = H.hash a1 in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in

    if H.equal (Array_3.get0 cache has) a0
      && H.equal (Array_3.get1 cache has) a1
    then begin
(*      Format.printf "Cache O@.";  *)
        Array_3.get2 cache has
      end
    else
      let result = f a0 a1 in
(*      Format.printf "Cache N@."; *)
      Array_3.set cache has a0 a1 result;
      result
end

module Array_Bit =
struct
  let make size =
    let size = (size + 7) lsr 3 in
    String.make size (char_of_int 0)

  let get s i =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    (Char.code s.[c]) land b <> 0

  let set s i v =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    let oldcontents = Char.code s.[c] in
    let newcontents = 
      if v 
      then b lor oldcontents 
      else 
	let mask = lnot b in
	oldcontents land mask 
    in
    s.[c] <- Char.chr newcontents

  let clear s =
    let zero = char_of_int 0 in
    String.fill s 0 (String.length s) zero
end

module Make_Binary (H0: Cacheable) (H1: Cacheable) =
struct
  let size = cache_size
  let cache = Array_2.make size H0.sentinel H1.sentinel
  let result = Array_Bit.make size
  let mask = pred size

  let clear () =
    Array_2.clear cache H0.sentinel H1.sentinel;
    Array_Bit.clear result

  let merge f a0 a1 =
    let has =
      let h0 = H0.hash a0 in
      let h1 = H1.hash a1 in
      599 * h0 + h1
    in
    let has = has land mask in

    if H0.equal (Array_2.get0 cache has) a0
      && H1.equal (Array_2.get1 cache has) a1
    then begin
(*      Format.printf "Cache O@.";  *)
        Array_Bit.get result has
      end
    else
      let r = f a0 a1 in
(*      Format.printf "Cache N@."; *)
      Array_2.set cache has a0 a1;
      Array_Bit.set result has r;
      r
end

module Make_Symmetric_Binary (H0: Cacheable) =
struct
  let size = cache_size
  let cache = Array_2.make size H0.sentinel H0.sentinel 
  let result = Array_Bit.make size
  let mask = pred size

  let clear () =
    Array_2.clear cache H0.sentinel H0.sentinel;
    Array_Bit.clear result

  let hash = H0.hash

  let merge f a0 a1 =
    let a0, a1, h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in

    if H0.equal (Array_2.get0 cache has) a0
      && H0.equal (Array_2.get1 cache has) a1
    then begin
(*      Format.printf "Cache O@.";  *)
        Array_Bit.get result has
      end
    else
      let r = f a0 a1 in
(*      Format.printf "Cache N@."; *)
      Array_2.set cache has a0 a1;
      Array_Bit.set result has r;
      r
end

module Make_Het1_1_4 (H0: Cacheable) (H1: Cacheable) (H2: Cacheable) (R: Result) =
struct
  let size = cache_size
  let cache =
    Array_7.make size
      H0.sentinel H1.sentinel
      H2.sentinel H2.sentinel H2.sentinel H2.sentinel
      R.sentinel

  let mask = pred size

  let clear () =
    Array_7.clear cache
      H0.sentinel H1.sentinel
      H2.sentinel H2.sentinel H2.sentinel H2.sentinel
      R.sentinel

  let merge f a0 a1 a2 a3 a4 a5 =
    let has = H0.hash a0 + 4909 * (H1.hash a1) +
      127 * (H2.hash a2) + 971 * (H2.hash a3) +
      31 * (H2.hash a4) + 7907 * (H2.hash a5)
    in
    let has = has land mask in
    if H0.equal (Array_7.get0 cache has) a0
      && H1.equal (Array_7.get1 cache has) a1
      && H2.equal (Array_7.get2 cache has) a2
      && H2.equal (Array_7.get3 cache has) a3
      && H2.equal (Array_7.get4 cache has) a4
      && H2.equal (Array_7.get5 cache has) a5
    then begin
(*      Format.printf "Cache O@."; *)
        Array_7.get6 cache has
      end
    else
      let result = f () in
(*      Format.printf "Cache N@."; *)
      Array_7.set cache has a0 a1 a2 a3 a4 a5 result;
      result
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
