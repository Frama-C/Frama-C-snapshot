(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2009-2012 INRIA                                         *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*    * Redistributions of source code must retain the above copyright    *)
(*      notice, this list of conditions and the following disclaimer.     *)
(*    * Redistributions in binary form must reproduce the above           *)
(*      copyright notice, this list of conditions and the following       *)
(*      disclaimer in the documentation and/or other materials provided   *)
(*      with the distribution.                                            *)
(*    * Neither the name of the <organization> nor the names of its       *)
(*      contributors may be used to endorse or promote products derived   *)
(*      from this software without specific prior written permission.     *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY <INRIA> ''AS IS'' AND ANY                *)
(*  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     *)
(*  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR    *)
(*  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE       *)
(*  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR   *)
(*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT     *)
(*  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR    *)
(*  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF            *)
(*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT             *)
(*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE     *)
(*  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH      *)
(*  DAMAGE.                                                               *)
(*                                                                        *)
(**************************************************************************)

(* caml_unmarshal by Ineffable Casters *)

(* Version 3.11.1.8 *)

(** Extends {!Unmarshal} to deal with the data types of the [Nums] library.

    You must make sure that this module is linked with your program,
    by using one of the values declared below.  If you don't need
    them in your program (for example because you are using
    [Unmarshal.Abstract] for all your BigNums), you should add the
    following line to one of your source files.

    -  [ignore Unmarshal_nums.t_num;;] *)

val t_nat : Unmarshal.t;;
val t_big_int : Unmarshal.t;;
val t_ratio : Unmarshal.t;;
val t_num : Unmarshal.t;;
