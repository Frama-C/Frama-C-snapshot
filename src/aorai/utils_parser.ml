(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

let rec get_last_field  my_field my_offset =
  match my_offset with
    | Cil_types.NoOffset -> my_field
    | Cil_types.Field(fieldinfo,the_offset)  -> get_last_field fieldinfo the_offset
    | _ ->  Aorai_option.fatal "NOT YET IMPLEMENTED : struct with array access."


let rec add_offset father_offset new_offset =
  match father_offset with
    | Cil_types.NoOffset -> new_offset
    | Cil_types.Field(_,the_offset)  -> (Cil.addOffset father_offset (add_offset the_offset new_offset))
    | _ ->  Aorai_option.fatal "NOT YET IMPLEMENTED : struct with array access."



let rec get_field_info_from_name my_list name =
  if(List.length my_list <> 0) then begin
    let my_field = List.hd my_list in
    if(my_field.Cil_types.fname = name) then my_field
    else get_field_info_from_name (List.tl my_list) name
  end
  else  Aorai_option.fatal "no field found with name :%s" name



let get_new_offset my_host my_offset name=
  match my_host with
    | Cil_types.Var(var) ->
       let var_info = var in
       (* if my_offset is null no need to search the last field *)
       (* else we need to have the last *)

       let my_comp =
         if (my_offset = Cil_types.NoOffset) then
           match var_info.Cil_types.vtype with
             | Cil_types.TComp(mc,_,_) -> mc
             | _ -> assert false
          (*Cil_types.TComp(my_comp,_,_) = var_info.Cil_types.vtype in*)

         else begin
           let get_field_from_offset my_offset = begin
             match my_offset with
               | Cil_types.Field(fieldinfo,_)  -> fieldinfo
               | _ ->  Aorai_option.fatal "support only struct no array wtih struct"
             end in
             let field_info = get_field_from_offset my_offset in
             let last_field_offset = get_last_field field_info my_offset in
             (* last field in offset but not the field we want, for that we search in*)
             let mc = last_field_offset.Cil_types.fcomp in
             mc
           end
         in

         let field_info = get_field_info_from_name my_comp.Cil_types.cfields name in
         Cil_types.Field(field_info,Cil_types.NoOffset)

     | _ -> Aorai_option.fatal "NOT YET IMPLEMENTED : mem is not supported"
