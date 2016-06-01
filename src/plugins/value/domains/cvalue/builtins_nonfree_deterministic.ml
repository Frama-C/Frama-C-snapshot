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

(* Builtins for printing function. The name of the file is historical *)

open Abstract_interp
open Locations

exception Interpret_format_finished
exception Interpret_format_partial
exception Return_bottom

let bottom_result = 
  { Value_types.c_values = [ None, Cvalue.Model.bottom ] ;
    c_clobbered = Base.SetLattice.bottom;
    c_from = None;
    c_cacheable = Value_types.NoCache;
  }

let alarm_behavior_raise_problem = 
  {CilE.a_ignore with CilE.a_call = fun _ -> raise Interpret_format_partial}

let with_alarms = 
  { CilE.defined_logic = alarm_behavior_raise_problem; 
    unspecified        = alarm_behavior_raise_problem;
    others             = alarm_behavior_raise_problem;    
    imprecision_tracing = CilE.a_ignore}

type formatting_result =
  { string: string;
    partial: bool }

exception Copy_string_done

let copy_string ~source_char_size b state l = 
  if not (Location_Bytes.cardinal_zero_or_one l)
  then raise Interpret_format_partial;
  let l = ref (loc_bytes_to_loc_bits l) in
  let sizeofchar_size = Int_Base.inject source_char_size in
  let sizeofchar_ival = Ival.inject_singleton source_char_size in
  try
    while true do
      let loc = Locations.make_loc !l sizeofchar_size in
      let c = Eval_op.find ~with_alarms state loc in
      let c = Cvalue.V.project_ival c in
      let c = Ival.project_int c in       
      let c = Int.to_int c in
      assert (-128 <= c && c <= 255);
      let string_ends = c = 0 in
      if string_ends
      then raise Copy_string_done;
      let c =
	if c >= 0 
	then c
	else c + 256
      in
      let c = char_of_int c in
      Buffer.add_char b c;
      l := Location_Bits.shift sizeofchar_ival !l
    done;
  with 
    Ival.Not_Singleton_Int | Cvalue.V.Not_based_on_null ->
      raise Interpret_format_partial
  | Copy_string_done -> ()

let copy_char b c = 
  try
    let c = Cvalue.V.project_ival c in
    let c = Ival.project_int c in       
    let c = Int.pos_rem c (Int.of_int 256) in
    Buffer.add_char b (char_of_int (Int.to_int c));
  with 
    Ival.Not_Singleton_Int | Cvalue.V.Not_based_on_null ->
      raise Interpret_format_partial

let copy_int ~modifier ~hexa b arg = 
  ignore (modifier);
  let i = Cvalue.V.project_ival arg in
  let i = Ival.project_int i in       
  let i = Pretty_utils.to_string (Integer.pretty ~hexa) i in
  Buffer.add_string b i

let copy_pointer ~modifier b arg = 
  ignore (modifier);
  let p = Pretty_utils.to_string Cvalue.V.pretty arg in
  Buffer.add_string b p

let copy_float ~modifier b arg = 
  ignore (modifier);
  let i = Cvalue.V.project_ival arg in
  let f = Ival.project_float i in       
  let s = Pretty_utils.to_string Fval.pretty f in
  Buffer.add_string b s

let write_string_to_memory l state formatting_result =
  let exact = Location_Bytes.cardinal_zero_or_one l in
  if not exact
  then begin
    Value_parameters.warning ~current:true
      "Destination is not precise%t"
      Value_util.pp_callstack;
  end;
  let l = ref (loc_bytes_to_loc_bits l) in
  let state = ref state in
  let sizeofchar = Bit_utils.sizeofchar() in
  let sizeofchar_size = Int_Base.inject sizeofchar in
  let sizeofchar_ival = Ival.inject_singleton sizeofchar in
  let s = formatting_result.string in
  let length = String.length s in
  let problem = ref false in
  let set_problem =
    {CilE.a_ignore with CilE.a_call = fun _ -> problem := true}
  in
  let with_alarms =
    { CilE.defined_logic  = set_problem;
      unspecified         = set_problem;
      others              = set_problem;
      imprecision_tracing = CilE.a_ignore }
  in
  for i = 0 to pred length do
    let v = Cvalue.V.inject_int (Int.of_int (int_of_char s.[i])) in
    let loc = Locations.make_loc !l sizeofchar_size in
    state := Eval_op.add_binding ~exact ~with_alarms !state loc v;
    l := Location_Bits.shift sizeofchar_ival !l  
  done;
  if formatting_result.partial || !problem
  then Value_parameters.warning ~current:true
    "Destination possibly invalid. assert(match format and arguments)%t"
    Value_util.pp_callstack;
  let loc = Locations.make_loc !l sizeofchar_size in
  if formatting_result.partial
  then
    let loc = 
      Locations.make_loc
        (Location_Bits.shift (Ival.inject_range (Some Int.zero) None) !l) 
        sizeofchar_size
    in
    snd (Cvalue.Model.add_binding ~exact:false !state loc Cvalue.V.top_int)
  else
    Eval_op.add_binding ~exact ~with_alarms !state loc
      Cvalue.V.singleton_zero  

type seen_percent = 
  Not_seen
| Seen of string * Integer.t option * Integer.t option * string * bool
    (* flags, width, precision, length modifier, last_character_was_a_star *)

let interpret_format ~character_width state l args =
  if not (Location_Bytes.cardinal_zero_or_one l)
  then begin
    Value_parameters.error ~current:true
      "Format string could not be resolved%t"
      Value_util.pp_callstack;
    raise Db.Value.Aborted
  end;
  let alarm () = 
    Value_parameters.error ~current:true
      "assert(match format and arguments)%t"
      Value_util.pp_callstack;
  in
  let abort () =
    alarm ();
    raise Db.Value.Aborted
  in
  let do_bottom () =
    alarm ();
    raise Return_bottom
  in
  let l = ref (loc_bytes_to_loc_bits l) in
  let sizeofchar_size = Int_Base.inject character_width in
  let sizeofchar_ival = Ival.inject_singleton character_width in
  let result = Buffer.create 17 in
  let seen_percent = ref Not_seen in
  let args = ref args in
  try
    while true do
      let loc = Locations.make_loc !l sizeofchar_size in
      let c = Eval_op.find ~with_alarms state loc in
      let c = Cvalue.V.project_ival c in
      let c = Ival.project_int c in       
      let c = Int.to_int c in
      assert (-128 <= c && c <= 255);
      let format_ends = c = 0 in
      if format_ends
      then raise Interpret_format_finished;
      let code =
	if c >= 0
	then c
	else c + 256
      in
      let c = char_of_int code in
      let eat_arg_and_reset_seen_percent expected_typ allowable_typ = 
	match !args with
	  (arg_exp, arg_v, _) :: remaining_args ->
            let arg_typ = Cil.typeOf arg_exp in
            let compare_typ = Cabs2cil.compatibleTypesp arg_typ in
	    if not (List.exists compare_typ expected_typ)
            then begin  
              Value_parameters.warning ~current:true
                "argument %a has type %a but format indicates %s%a"
                Printer.pp_exp arg_exp
                Printer.pp_typ arg_typ
                (match expected_typ with
                | [_] -> "" 
                | [] -> assert false 
                | _ -> "one of ")
                (Pretty_utils.pp_list ~sep:", " Printer.pp_typ) expected_typ;
              if List.exists compare_typ allowable_typ
              then
                Value_parameters.warning
                  "Continuing analysis because this seems innocuous"
              else
              do_bottom()
            end;
	    args := remaining_args;
	    seen_percent := Not_seen;
	    arg_v
	| [] ->
	  Value_parameters.error ~current:true "Too few arguments for format";
          do_bottom()
      in
      ( match !seen_percent with
      | Seen(flags, width, precision, modifier, star) ->
	  let catc s = s ^ String.make 1 c in
          let abort_modifier () =
            Value_parameters.warning ~current:true 
              "modifier %s not supported (yet) for %%%c" modifier c;
            abort ()
          in
	  ( match c with
	  | '+' | '-' | '0' | '#' | ' ' 
		when width = None && precision = None && modifier = "" ->
	      seen_percent := 
		Seen(catc flags, None, None, "", false)
	  | '.' when modifier = "" && precision = None ->
	      seen_percent := 
		Seen(flags, width, Some Integer.zero, "", false)
	  | _digit when c >= '0' && c <= '9' && modifier = "" && not star ->
	      let catc i =
		Integer.add
		  (Integer.mul (Integer.of_int 10) i)
		  (Integer.of_int (code - (Char.code '0')))
	      in
	      let new_seen =
		( match precision with
		  Some p -> 
		    if Integer.is_zero p && c = '0'
		    then begin
		      (* The simplest way to reject "%.0*d" is to
			 reject when any leading 0 is found in precision. *)
		      Value_parameters.error ~current:true
			"Leading zero in precision. This seems wrong.%t"
			Value_util.pp_callstack;
		      raise Db.Value.Aborted
		    end;
		    Seen(flags, width, Some (catc p), "", false)
		| None ->
		    let width = 
		      match width with
			None -> Integer.zero
		      | Some l -> l
		    in
		    let width = catc width in
		    Seen(flags, Some width, None, "", false))
	      in
	      seen_percent := new_seen
	  | '*' when modifier = "" && (not star) &&
	      ((width = None && precision = None) ||
	      (precision <> None && Integer.is_zero (Extlib.the precision))) ->
	      let arg = eat_arg_and_reset_seen_percent [Cil.intType] [] in
              if not Cvalue.V.(is_included arg top_int)
              then begin
                Value_parameters.warning ~current:true 
                  "addresses appear to be passed for %%*";
                alarm();
                raise Interpret_format_partial
              end;
	      seen_percent := Seen (flags, width, precision, "", true)
	  | 's' ->
	      let types, source_char_size = 
		if modifier = "l"
		then 
		  let wchar = Cil.theMachine.Cil.wcharType in
		  [Cil_types.TPtr(wchar, [])], Int.of_int (Cil.bitsSizeOf wchar)
		else if modifier = ""
		then 
		  [Cil.charPtrType; Cil.ucharPtrType; Cil.scharPtrType;
		   Cil.charConstPtrType],
		  (Bit_utils.sizeofchar())
		else abort_modifier()
	      in
	      let arg = eat_arg_and_reset_seen_percent types [] in
	      copy_string ~source_char_size
		result state arg;
	  | 'p' ->
	      begin
		let typ = 
		  if modifier = "" then Cil.voidPtrType
		  else abort_modifier()
		in
		let arg = eat_arg_and_reset_seen_percent [typ] [] in
		copy_pointer ~modifier result arg
	      end
	  | 'd' | 'i' ->
	      begin
		let typ, allowable = 
                  if modifier = "h" || modifier = "hh" 
                  then [Cil.uintType; Cil.intType;
                        Cil.charType; Cil.ucharType; Cil.scharType;
                        Cil_types.(TInt(IShort, [])); 
                        Cil_types.(TInt(IUShort, []))
                       ], (* be lenient because hey, promotions *)
                    []
                  else if modifier = "" 
                  then begin
                    [Cil.intType], [Cil.uintType]
                  end
		  else if modifier = "l" then [Cil.longType], []
		  else if modifier = "ll" || modifier = "j" 
                  then [Cil.longLongType], []
		  else abort_modifier()
		in
		let arg = eat_arg_and_reset_seen_percent typ allowable in
		copy_int ~modifier ~hexa:false result arg
	      end
	  | 'u' | 'x' | 'X' ->
	      begin
		let typ, allowable = 
                  if modifier = "h" || modifier = "hh" 
                  then [Cil.uintType; Cil.intType;
                        Cil.charType; Cil.ucharType; Cil.scharType;
                        Cil_types.(TInt(IShort, [])); 
                        Cil_types.(TInt(IUShort, []))
                       ], (* be lenient because hey, promotions *)
                    []
		  else if modifier = "" then [Cil.uintType], [Cil.intType]
		  else if modifier = "l" then [Cil.ulongType], []
		  else if modifier = "ll" || modifier = "j" 
                  then [Cil.ulongLongType], []
                  else if modifier = "z" 
                  then [Cil.theMachine.Cil.typeOfSizeOf], []
		  else abort_modifier()
		in
		let arg = eat_arg_and_reset_seen_percent typ allowable in
		copy_int ~modifier ~hexa:(c='x') result arg
	      end
	  | 'f' | 'F' | 'g' | 'G' | 'e' | 'E' | 'a' | 'A' ->
	      begin
		let typ = 
		  if modifier = "" || modifier = "l" then Cil.doubleType
		  else if modifier = "L" then Cil.longDoubleType
                  else abort_modifier()
		in
		let arg = eat_arg_and_reset_seen_percent [typ] [] in
		copy_float ~modifier result arg
	      end
          | 'c' ->
            begin
		let typ = 
		  if modifier = "" then Cil.intType
		  else if modifier = "l" then Cil.theMachine.Cil.wcharType
                  else abort_modifier()
		in
		let arg = eat_arg_and_reset_seen_percent [typ] [] in
		copy_char result arg
            end
	  | '%' ->
	      begin
		if modifier <> "" then abort_modifier();
		Buffer.add_char result '%';
		seen_percent := Not_seen
	      end
	  | 'L' | 'l' | 'h' | 'j' | 'z' ->
	      begin
		seen_percent := 
		  Seen(flags, width, precision, catc modifier, false);
	      end
	  | _ ->     
            Value_parameters.warning ~current:true 
              "format %%%c not supported (yet)" c;
            abort ())
      | Not_seen ->
	if c = '%'
	then seen_percent := Seen("", None, None, "", false)
	else Buffer.add_char result c ) ;
      l := Location_Bits.shift sizeofchar_ival !l
    done;
    assert false (* ugly to have to write this *)
  with 
  | Ival.Not_Singleton_Int | Cvalue.V.Not_based_on_null 
  | Interpret_format_partial ->
    Value_parameters.feedback ~current:true
      "Precise construction of result impossible.%t"
      Value_util.pp_callstack;
    { string = Buffer.contents result; partial = true}
  | Interpret_format_finished ->
      if !args != []
      then begin
	  Value_parameters.feedback ~current:true
	    "Too many arguments for format. This is technically allowed.%t"
	    Value_util.pp_callstack;
	end;
    { string = Buffer.contents result; partial = false }


let interpret_format_char x = 
  interpret_format ~character_width:(Bit_utils.sizeofchar()) x

let interpret_format_wchar x = 
  interpret_format
    ~character_width:
    (Integer.of_int(Cil.bitsSizeOf Cil.theMachine.Cil.wcharType))
    x

let abstract_length fmtres =
  let s = fmtres.string in
  let length = Int.of_int (String.length s) in
  if fmtres.partial
  then Cvalue.V.inject_ival 
    (Ival.inject_range (Some length) (Some Int.billion_one))
  else Cvalue.V.inject_int length

let frama_c_printf state args =
  try
    match args with
    | (_,format,_) :: rest -> 
	let formating_result = interpret_format_char state format rest in
        let v = abstract_length formating_result in
	Format.printf "@\n%s@." formating_result.string;
	{ Value_types.c_values = [ Eval_op.wrap_int v, state];
	  c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.NoCache;
        }
    | [] -> raise (Builtins.Invalid_nb_of_args 1)
  with 
  | Return_bottom -> bottom_result

let frama_c_wprintf state args =
  try
    match args with
    | (_,format,_) :: rest -> 
	let fmtres = interpret_format_wchar state format rest in
        let v = abstract_length fmtres in
	Format.printf "@\n%s@." fmtres.string;
	{ Value_types.c_values = [ Eval_op.wrap_int v, state];
	  c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.NoCache;
        }
    | [] -> raise (Builtins.Invalid_nb_of_args 1)
  with 
  | Return_bottom -> bottom_result

let frama_c_sprintf state args =
  try
    match args with
    | (_,dest,_) :: (_,format,_) :: rest -> 
	let fmtres = interpret_format_char state format rest in
	let state = write_string_to_memory dest state fmtres in
        let v = abstract_length fmtres in
	{ Value_types.c_values = [Eval_op.wrap_int v, state];
	  c_clobbered = Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.Cacheable;
        }
    | _ ->  raise (Builtins.Invalid_nb_of_args 2)
  with 
  | Return_bottom -> bottom_result

let frama_c_snprintf state args =
  try
    match args with
    | (_,dest,_) :: (_,nv,_) :: (_,format,_) :: rest -> 
	let n = Cvalue.V.project_ival nv in
        let mi, ma = Ival.min_and_max n in
	let n = Extlib.the mi in
        let precise_length = Int.equal n (Extlib.the ma) in
	let pn = Int.pred n in
        (* The order of the next two lines as per 
           https://twitter.com/fanf/status/429213105927626752 *)
	let fmtres = interpret_format_char state format rest in
        if Int.is_zero n
        then begin 
          if precise_length 
          then
	    { Value_types.c_values = 
                [Eval_op.wrap_int Cvalue.V.singleton_zero, state];
	      c_clobbered = Base.SetLattice.bottom;
              c_from = None;
              c_cacheable = Value_types.Cacheable;
            }
          else
          let state_with_some_writing = 
            write_string_to_memory dest state { string = "" ; partial = true } 
          in
          (* Necessary for if the destination is totally invalid. When the
             argument is zero, the call still terminates even if 
             the destination is invalid. *)
          let state = Cvalue.Model.join state state_with_some_writing in
	  { Value_types.c_values = 
              [Eval_op.wrap_int nv, state];
	    c_clobbered = Base.SetLattice.bottom;
            c_from = None;
            c_cacheable = Value_types.Cacheable;
          }
        end
        else
          let fmtres = 
            if precise_length 
            then fmtres
            else { fmtres with partial = true }
          in
	  let orig_length = String.length fmtres.string in
          let s = fmtres.string in
	  let s, actual_length = 
	    if Int.le (Int.of_int orig_length) pn
	    then s, Int.of_int orig_length
	    else String.sub s 0 (Int.to_int pn), n
	  in 
          (* overapproximated: *)
          let state = 
            write_string_to_memory dest state { fmtres with string = s } 
          in
          let v = 
            if fmtres.partial
            then 
              Cvalue.V.inject_ival 
                (Ival.inject_range (Some actual_length) ma)
            else
              Cvalue.V.inject_int actual_length
          in
	  { Value_types.c_values = [Eval_op.wrap_int v, state];
	    c_clobbered = Base.SetLattice.bottom;
            c_from = None;
            c_cacheable = Value_types.Cacheable;
          }
    | _ -> raise (Builtins.Invalid_nb_of_args 3)
  with 
  | Return_bottom -> bottom_result

let () =
  Builtins.register_builtin "Frama_C_printf" frama_c_printf;
  Builtins.register_builtin "Frama_C_wprintf" frama_c_wprintf;
  Builtins.register_builtin "Frama_C_sprintf" frama_c_sprintf;
  Builtins.register_builtin "Frama_C_snprintf" frama_c_snprintf

let () =
  if false then (
    Builtins.register_builtin "printf" frama_c_printf;
    Builtins.register_builtin "sprintf" frama_c_sprintf;
    Builtins.register_builtin "snprintf" frama_c_snprintf;
  )


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
