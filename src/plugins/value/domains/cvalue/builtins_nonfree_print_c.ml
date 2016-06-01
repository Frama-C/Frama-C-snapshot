(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp
open Cvalue

let substitute_space_by_underscore s =
  let s = String.copy s in
  for i=1 to String.length s - 1 do
    assert (s.[i] <> '*');
    if s.[i] = ' ' then s.[i] <- '_'; 
  done;
  s

let c_string_of_int n =
  if Int.equal n (Int.of_string "-2147483648")
  then "-2147483648LL"
  else if Int.equal n (Int.of_string "-9223372036854775808")
  then "(long long)-9223372036854775808ULL"
  else
    Int.to_string n

let pretty_assignment_expression_ival typname fmt v =
  match Ival.min_and_max v with
  | Some mn, Some mx ->
    let mn_repr = c_string_of_int mn in 
    if Int.equal mn mx
    then Format.fprintf fmt "%s" mn_repr
    else 
      let mx_repr = c_string_of_int mx in
      Format.fprintf fmt "Frama_C_%s_interval(%s, %s)" 
	(substitute_space_by_underscore typname) mn_repr mx_repr
  | _, _ -> assert false

 let pretty_assignment_expression typname fmt v =
    match v with
    | Locations.Location_Bytes.Top (Base.SetLattice.Top, _) ->
      Format.fprintf fmt "{{ ANYTHING }}"
    | Locations.Location_Bytes.Top (t, _) ->
      Format.fprintf fmt "{{ garbled mix of &%a }}"
        Base.SetLattice.pretty t
    | Locations.Location_Bytes.Map m ->
        let print_binding fmt k v =
          if Ival.equal Ival.zero v
          then Format.fprintf fmt "%a" Base.pretty_addr k
          else begin
	      Format.fprintf fmt "(char*)%a + %a"
		Base.pretty_addr k 
		(pretty_assignment_expression_ival typname) v
	  end
        in
        Pretty_utils.pp_iter
          ~pre:" " ~suf:" " ~sep:" ;@ "
          (fun pp map ->
            Locations.Location_Bytes.M.iter (fun k v -> pp (k, v)) map)
          (fun fmt (k, v) -> print_binding fmt k v)
          fmt m

let pretty_int_range fmt print_ampamp typname lv v =
  let v = V.project_ival v in
  match Ival.min_and_max v with
    | Some mn, Some mx ->
	let mn_repr = c_string_of_int mn in 
        if Int.equal mn mx
        then begin
          print_ampamp();
          Format.fprintf fmt "*(%s*)%s == %s" typname lv mn_repr
        end
        else begin
	  let mx_repr = c_string_of_int mx in
          print_ampamp();
          Format.fprintf fmt "%s <= *(%s*)%s && *(%s*)%s <= %s"
            mn_repr typname lv typname lv mx_repr
        end
    | _ -> ()

let pretty_int_assignment fmt typname lv v =
   Format.fprintf fmt "*(%s*)%s = %a;\n" 
     typname lv (pretty_assignment_expression_ival typname) (V.project_ival v)


let pretty_float_range fmt print_ampamp typname lv v =
  let use_hex = true in
  let pp_float = Fval.F.pretty_normal ~use_hex in
  let i = V.project_ival v in
  let mn, mx = Ival.min_and_max_float i in
  if Fval.F.equal mn mx
  then begin
    print_ampamp();
    Format.fprintf fmt "*(%s*)%s == %a"
      typname lv pp_float mn
  end
  else begin
    print_ampamp();
    Format.fprintf fmt "%a <= *(%s*)%s && *(%s*)%s <= %a"
      pp_float mn typname lv typname lv pp_float mx;
  end

let pretty_float_assignment fmt typname lv v =
  let use_hex = true in
  let pp_float = Fval.F.pretty_normal ~use_hex in
  let i = V.project_ival v in
  let mn, mx = Ival.min_and_max_float i in
  if Fval.F.equal mn mx then begin
    Format.fprintf fmt "*(%s*)%s = %a;\n"
      typname lv pp_float mn
  end
  else 
    begin
      Format.fprintf fmt "*(%s*)%s = Frama_C_%s_interval(%a, %a);\n" 
	typname lv 
	(substitute_space_by_underscore typname)
	pp_float mn  
	pp_float mx
    end

let pretty_pointer_assignment fmt typname lv v = 
  if V.cardinal_zero_or_one v then 
    Format.fprintf fmt  "*(void * *)%s = %a;\n"  lv 
      (pretty_assignment_expression typname) v 
  else assert false


let types = Hashtbl.create 7;;


let () =
  Hashtbl.add types 1
    [V.inject_ival (Ival.inject_range
                      (Some Int.zero) (Some (Int.of_int 255))),
     "unsigned char", pretty_int_range, pretty_int_assignment;
     V.inject_ival (Ival.inject_range
                      (Some (Int.of_int (-128))) (Some (Int.of_int 127))),
     "char", pretty_int_range, pretty_int_assignment];
  Hashtbl.add types 2
    [V.inject_ival (Ival.inject_range
                      (Some Int.zero) (Some (Int.of_int 65535))),
     "unsigned short", pretty_int_range, pretty_int_assignment;
     V.inject_ival (Ival.inject_range
                      (Some (Int.of_int (-32768))) (Some (Int.of_int 32767))),
     "short", pretty_int_range, pretty_int_assignment];
  Hashtbl.add types 4
    [ V.top_float,
      "float", pretty_float_range, pretty_float_assignment;

      V.inject_ival (Ival.inject_range
                       (Some Int.zero) (Some (Int.of_string "4294967295"))),
      "unsigned int", pretty_int_range, pretty_int_assignment;

      V.inject_ival (Ival.inject_range
                       (Some (Int.of_string "-2147483648"))
                       (Some (Int.of_string  "2147483647"))),
      "int", pretty_int_range, pretty_int_assignment;

      V.top, 
      "void *", 
      (fun _ _ _ _ _ -> ()) , 
      pretty_pointer_assignment

];
  Hashtbl.add types 8
    [ V.top_float,
      "double", pretty_float_range, pretty_float_assignment;
      V.inject_ival(Ival.inject_range
                       (Some (Int.of_string "0"))
                       (Some (Int.of_string  "18446744073709551615"))),
      "unsigned long long", pretty_int_range, pretty_int_assignment;
      V.inject_ival (Ival.inject_range
                       (Some (Int.of_string "-9223372036854775808"))
                       (Some (Int.of_string  "9223372036854775807"))),
      "long long", pretty_int_range, pretty_int_assignment]
;;


let value_pretty cas print_ampamp lv s_bytes fmt v =
  try
    let candidate_types = Hashtbl.find types s_bytes in
    let rec find_typ = function
      | [] -> ()
      | (range, _, _, _) :: t when not (V.is_included v range) ->
          find_typ t
      | (_range, typname, pr, _) :: _ ->
	pr fmt print_ampamp typname lv v
    in
    let rec find_typ_assignment = function
      | [] -> ()
      | (range, _, _, _) :: t when not (V.is_included v range) ->
          find_typ_assignment t
      | (_range, typname, _, pr) :: _ ->
	pr fmt typname lv v
    in
    if cas 
    then find_typ candidate_types 
    else find_typ_assignment candidate_types
  with V.Not_based_on_null -> ()


let value_uninit_pretty cas prampamp lv s fmt = function
  | V_Or_Uninitialized.C_init_noesc v ->
      value_pretty cas prampamp lv s fmt v
  | _ -> ()


let offsetmap_pretty cas name print_ampamp fmt offsm =
  let pretty_binding (bk,ek) (v, modu, offset) =
    let iso = V_Or_Uninitialized.is_isotropic v in
    if Integer.is_zero (Integer.rem bk Integer.eight)
      && (Rel.is_zero offset)
      && (iso || (Integer.is_zero (Integer.rem modu Integer.eight)))
    then
      let ek = Integer.succ ek in
      if Integer.is_zero (Integer.rem ek Integer.eight)
      then
        let step = if iso then 1 else (Integer.to_int modu) / 8 in
        let start = ref ((Integer.to_int bk) / 8) in
        let ek = Integer.to_int ek in
        let ek = ek / 8 in
        while !start + step <= ek do
          let lv =
            if !start = 0
            then
              Format.sprintf "&%s" name
            else
              Format.sprintf "((unsigned char*)&%s+%d)"
                name
                !start
          in
          value_uninit_pretty cas print_ampamp lv step fmt v;
          start := !start + step
        done;
      else ()
    else ()
  in
  Cvalue.V_Offsetmap.iter pretty_binding offsm

let state_pretty cas fmt m =
  Format.fprintf fmt "@[";
  (match m with
     | Model.Bottom -> Format.fprintf fmt "0"
     | Model.Map m ->
         let first = ref true in
         let print_ampamp () =
           if !first
           then first := false
           else Format.fprintf fmt "@\n&& ";
         in
         Model.iter
           (fun base offs ->
              match base with
                | Base.Allocated (v,_)
		| Base.Var(v,_) ->
                    let name = v.Cil_types.vname in
		    if name <> "crc32_tab" (* Specialized for Csmith *)
		    then offsetmap_pretty cas name print_ampamp fmt offs
            | _ -> ())
          m
     | Model.Top -> Format.fprintf fmt "1"
  );
  Format.fprintf fmt "@]"

let pretty_state_as_c_assert = state_pretty true

let print_declarations_for_malloc_bases fmt =
  let malloced_bases = Value_util.malloced_bases () in
  let pretty_declaration base = 
    match base with
    | Base.Var (var, validity) ->
      let name = var.Cil_types.vname in 
      let dim =  
	match validity with 
	| Base.Known (l,u) when (Int.is_zero l)->
	  Int.div (Int.succ u) Int.eight
	| _ -> assert false
      in 
      Format.fprintf fmt "char %s[%a];\n" name Int.pretty dim
    | _ -> assert false
      
  in
   Base.Hptset.iter 
     pretty_declaration
     malloced_bases

let pretty_state_as_c_assignments fmt state = 
  print_declarations_for_malloc_bases fmt ;
  Format.fprintf fmt "void Frama_C_dump_assignments(void)\n{%a}"
    (state_pretty false) state


let frama_c_dump_assert state _actuals =
  Value_parameters.result ~current:true "Frama_C_dump_assert_each called:@\n(%a)@\nEnd of Frama_C_dump_assert_each output"
    pretty_state_as_c_assert state;
  { Value_types.c_values = [None, state];
    c_clobbered = Base.SetLattice.bottom;
    c_from = None;
    c_cacheable = Value_types.NoCache;
  }

let () = Builtins.register_builtin "Frama_C_dump_assert_each" frama_c_dump_assert

let frama_c_dump_assignments state _actuals =
  Value_parameters.result ~current:true "Frama_C_dump_assignment_each called:@\n%a@\nEnd of Frama_C_dump_assignment_each output"
    pretty_state_as_c_assignments state;
  { Value_types.c_values = [None, state];
    c_clobbered = Base.SetLattice.bottom;
    c_from = None;
    c_cacheable = Value_types.NoCache;
  }

let () = 
  Builtins.register_builtin "Frama_C_dump_assignments_each" frama_c_dump_assignments


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
