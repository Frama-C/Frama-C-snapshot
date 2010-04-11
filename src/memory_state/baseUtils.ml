(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

module BaseSet = (* TODO: use patricia trees instead of Set *)
struct
  include Set.Make (Base)

  let singleton_null = singleton Base.null
  let pretty fmt s =
    Format.fprintf fmt "{";
    iter (Format.fprintf fmt "%a@ " Base.pretty) s;
    Format.fprintf fmt "}";
end

module BaseMap = struct
  include Map.Make (Base)
  let pretty pretty_v fmt m =
    Format.fprintf fmt  "@[{{ ";
    iter 
      (fun k v ->
	 Format.fprintf fmt "@[@[%a@] -> @[%a@]@];@ "
	   Base.pretty k
	   pretty_v v)
      m;
    Format.fprintf fmt  " }}@]"      
end

module BaseHashtbl = Hashtbl.Make(Base)
