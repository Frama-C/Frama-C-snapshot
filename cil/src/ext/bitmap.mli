(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

                              (* Imperative bitmaps *)

type t
                                        (* Create a bitmap given the number 
                                         * of bits *)
val  make : int -> t
val  init : int -> (int -> bool) -> t   (* Also initialize it *)

val  size : t -> int                    (* How much space it is reserved *)

                                        (* The cardinality of a set *)
val  card  : t -> int

                                        (* Make a copy of a bitmap *)
val  clone : t -> t 

val  cloneEmpty : t -> t                (* An empty set with the same 
                                         * dimentions *)

val  set : t -> int -> bool -> unit
val  get : t -> int -> bool
                                        (* destructive union. The first 
                                         * element is updated. Returns true 
                                         * if any change was actually 
                                         * necessary  *)
val  union  : t -> t -> bool

                                        (* accLive livein liveout def. Does 
                                         * liveIn += (liveout - def) *)
val  accLive : t -> t -> t -> bool

                                        (* Copy the second argument onto the 
                                         * first *)
val  assign : t -> t -> unit


val  inters : t -> t -> unit
val  diff   : t -> t -> unit


val  empty  : t -> bool

val  equal  : t -> t -> bool

val  toList : t -> int list

val  iter   : (int -> unit) -> t -> unit
val  fold   : ('a -> int -> 'a) -> t -> 'a -> 'a 

