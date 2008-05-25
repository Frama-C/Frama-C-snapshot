(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

open Locations

module Zone_with_empty_default =
 struct
  include Zone
   let default _base _b _e = bottom
   let defaultall _base = bottom
end

module Lmap_bitwise_with_empty_default = 
  struct
    include Lmap_bitwise.Make_bitwise (Zone_with_empty_default)
    let pretty fmt v = 
      let already_printed = ref [] in
      let counter = ref 0 in
      let folder _key (_b,value) () = 
        if not (List.memq value !already_printed) then 
          begin 
            Format.fprintf fmt 
              "@[Zone %d: @[{%a}@];@\n@]" 
              !counter 
              Zone_with_empty_default.pretty value;
            incr counter;
            already_printed := value::!already_printed;
          end
      in
      try 
        fold folder v ()
      with Cannot_fold -> 
        Format.fprintf fmt 
          "@[Zone 0: @[{ALL VARIABLES}@];@\n@]" 

  end
