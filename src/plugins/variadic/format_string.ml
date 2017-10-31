(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type t =
| String of string
| WString of int64 list

exception OutOfBounds
exception NotAscii of int64

let get_char (s : t) (i : int) : char =
  match s with
  | String s ->
      begin try
        String.get s i
      with
        Invalid_argument _ -> raise OutOfBounds
      end
  | WString s ->
      begin try 
        let c = List.nth s i in
        if (c >= Int64.zero && c<= (Int64.of_int 255)) then
          Char.chr (Int64.to_int c)
        else
          raise (NotAscii c)
      with
        Failure _ -> raise OutOfBounds
      end

let get_wchar (s : t) (i : int) : int64 =
  match s with
  | String s ->
      begin try
        Int64.of_int (Char.code (String.get s i))
      with
        Invalid_argument _ -> raise OutOfBounds
      end
  | WString s ->
      begin try 
        List.nth s i
      with
        Failure _ -> raise OutOfBounds
      end

let sub_string (s : t) (start : int) (len : int) : string =
  let init_char i =
    get_char s (start + i)
  in
  String.init len init_char
