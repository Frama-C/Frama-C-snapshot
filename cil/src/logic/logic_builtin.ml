(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

open Cil_types

let add = Logic_env.add_builtin_logic_function_gen
  Logic_utils.is_same_builtin_profile

let float_type = Ctype Cil.floatType
let double_type = Ctype Cil.doubleType
let string_type = Ctype Cil.charConstPtrType
let long_double_type = Ctype Cil.longDoubleType
let object_ptr = Ctype Cil.voidPtrType
let fun_ptr = Ctype (TPtr(TFun(Cil.voidType,None,false,[]),[]))

let init =
  let called = ref false in
  (* Since hooks are not projectified this function must be added exactly
     once per session, otherwise we might end up with several built-ins with
     the same name.
   *)
  fun () ->
    if !called then (fun () -> ())
    else begin
      called:=true;
      fun () ->
        (* types *)
        (*
          let tvar v = new_identified_term (tvar v) in
         *)
        let boolean =
          { lt_name = Utf8_logic.boolean; lt_params = []; lt_def = None; }
        in
        let set = { lt_name = "set"; lt_params = ["elem"]; lt_def = None; } in
        let typetag = {lt_name = "typetag"; lt_params = []; lt_def = None; } in
        let sign = {lt_name = "sign"; lt_params = []; lt_def = None; } in
        let float_format =
          {lt_name = "float_format"; lt_params = []; lt_def = None; }
        in
        let rounding_mode =
          {lt_name = "rounding_mode"; lt_params = []; lt_def = None; }
        in
        List.iter (fun x -> Logic_env.add_builtin_logic_type x.lt_name x)
          [ boolean; set; typetag; sign; float_format; rounding_mode ];
        (* constructors *)
        List.iter
          (fun (typename, constrs) ->
             let l =
	       List.map
	         (fun cname ->
                    let c =
                      { ctor_name = cname; ctor_type = typename;
                        ctor_params = [] }
                    in
	            Logic_env.add_builtin_logic_ctor cname c;
	            c)
	         constrs
             in
             typename.lt_def <- Some (LTsum l))
          [ boolean, ["\\true"; "\\false"];
            sign , [ "\\Positive"; "\\Negative"] ;
            float_format, [ "\\Single"; "\\Double"; "\\Quad" ] ;
            rounding_mode, [ "\\Up"; "\\Down"; "\\ToZero"; "\\NearestAway";
                             "\\NearestEven" ];
          ];
        let float_format = Ltype(float_format,[]) in
        let rounding_mode = Ltype(rounding_mode,[]) in
        (* predicates *)
        List.iter
          (fun (f,tparams,params)  ->
             add { bl_name = f; bl_params = tparams; bl_profile = params;
                   bl_type = None; bl_labels = []})
          [ "\\is_finite", [], ["x", float_type] ;
            "\\is_finite", [], ["x", double_type] ;
            "\\is_finite", [], ["x", long_double_type] ;
            "\\is_infinite", [], ["x", float_type] ;
            "\\is_infinite", [], ["x", double_type] ;
            "\\is_infinite", [], ["x", long_double_type] ;
            "\\is_NaN", [], ["x", float_type] ;
            "\\is_NaN", [], ["x", double_type] ;
            "\\is_NaN", [], ["x", long_double_type] ;
            "\\is_minus_infinity", [], ["x", float_type] ;
            "\\is_minus_infinity", [], ["x", double_type] ;
            "\\is_minus_infinity", [], ["x", long_double_type] ;
            "\\is_plus_infinity", [], ["x", float_type] ;
            "\\is_plus_infinity", [], ["x", double_type] ;
            "\\is_plus_infinity", [], ["x", long_double_type] ;
            "\\le_float", [], ["x", float_type; "y", float_type];
            "\\lt_float", [], ["x", float_type; "y", float_type];
            "\\ge_float", [], ["x", float_type; "y", float_type];
            "\\gt_float", [], ["x", float_type; "y", float_type];
            "\\eq_float", [], ["x", float_type; "y", float_type];
            "\\ne_float", [], ["x", float_type; "y", float_type];
            "\\le_float", [], ["x", double_type; "y", double_type];
            "\\lt_float", [], ["x", double_type; "y", double_type];
            "\\ge_float", [], ["x", double_type; "y", double_type];
            "\\gt_float", [], ["x", double_type; "y", double_type];
            "\\eq_float", [], ["x", double_type; "y", double_type];
            "\\ne_float", [], ["x", double_type; "y", double_type];
            "\\no_overflow_single", [], ["m", rounding_mode; "x", Lreal] ;
            "\\no_overflow_double", [], ["m", rounding_mode; "x", Lreal] ;
            "\\subset", ["a"], ["s1", Ltype (set, [Lvar "a"]);
                                "s2", Ltype (set, [Lvar "a"])];
            "\\pointer_comparable", [], [("p1", object_ptr);
                                         ("p2", object_ptr)];
            "\\pointer_comparable", [], [("p1", fun_ptr);
                                         ("p2", fun_ptr)];
            "\\pointer_comparable", [], [("p1", fun_ptr);
                                         ("p2", object_ptr)];
            "\\pointer_comparable", [], [("p1", object_ptr);
                                         ("p2", fun_ptr)];
            "\\points_to_valid_string", [], ["p", object_ptr];
	    "\\warning", [], [("str", string_type)];
          ];
        (* functions *)
        List.iter
          (fun (f,params,ret_type)  ->
             add { bl_name = f; bl_params = []; bl_profile = params;
                   bl_type = Some ret_type; bl_labels = []})
          [ 
            "\\min", ["x",Linteger;"y",Linteger], Linteger ;
            "\\max", ["x",Linteger;"y",Linteger], Linteger ;
            "\\min", ["x",Lreal;"y",Lreal], Lreal ;
            "\\max", ["x",Lreal;"y",Lreal], Lreal ;

            "\\abs", ["x",Linteger], Linteger ;
            "\\labs", ["x",Linteger], Linteger ;
            "\\abs", ["x",Lreal], Lreal ;
            "\\fabs", ["x",Lreal], Lreal ;

            "\\sqrt", ["x",Lreal], Lreal ;
            "\\pow", ["x",Lreal;"y",Lreal], Lreal ;

            "\\ceil", ["x",Lreal], Linteger ;
            "\\floor", ["x",Lreal], Linteger ;

            (* transcendantal functions *)
            "\\exp", ["x",Lreal], Lreal ;
            "\\log", ["x",Lreal], Lreal ;
            "\\log10", ["x",Lreal], Lreal ;

            "\\cos", ["x",Lreal], Lreal ;
            "\\sin", ["x",Lreal], Lreal ;
            "\\tan", ["x",Lreal], Lreal ;

            "\\pi", [], Lreal ;

            "\\cosh", ["x",Lreal], Lreal ;
            "\\sinh", ["x",Lreal], Lreal ;
            "\\tanh", ["x",Lreal], Lreal ;

            "\\acos", ["x",Lreal], Lreal ;
            "\\asin", ["x",Lreal], Lreal ;
            "\\atan", ["x",Lreal], Lreal ;

            "\\atan2", ["x",Lreal;"y",Lreal], Lreal ;
            "\\hypot", ["x",Lreal;"y",Lreal], Lreal ;

            (* TODO ?
             * div() fmod() frexp() ldexp()
             * ldiv() modf() modf()
             *)

            "\\sum", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\sum", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\product", ["min",Linteger;
		          "max", Linteger;
		          "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\product", ["min",Linteger;
		          "max", Linteger;
		          "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\min", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\min", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\max", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Linteger))], Linteger ;
            "\\max", ["min",Linteger;
		      "max", Linteger;
		      "f",(Larrow ([Linteger],Lreal))], Lreal ;
            "\\numof", ["min",Linteger;
		        "max", Linteger;
		        "f",(Larrow ([Linteger],Ltype(boolean,[])))], Linteger ;


            (* for floats special values *)

            "\\round_float", ["f", float_format; "m", rounding_mode; "x", Lreal], Lreal ;

            "\\sign", ["x",float_type], Ltype(sign,[]) ;
            "\\sign", ["x",double_type], Ltype(sign,[]) ;
            "\\sign", ["x",long_double_type], Ltype(sign,[]) ;

            "\\model", ["x",float_type], Lreal;
            "\\model", ["x",double_type], Lreal;
            (*"\\model", ["x",long_double_type], Lreal;*)

            "\\exact", ["x",float_type], Lreal;
            "\\exact", ["x",double_type], Lreal;
            (*"\\exact", ["x",long_double_type], Lreal;*)

            "\\total_error", ["x",float_type], Lreal;
            "\\total_error", ["x",double_type], Lreal;
            (*"\\total_error", ["x",long_double_type], Lreal;*)

            "\\round_error", ["x",float_type], Lreal;
            "\\round_error", ["x",double_type], Lreal;
            (*"\\round_error", ["x",long_double_type], Lreal;*)

            "\\relative_error", ["x",float_type], Lreal;
            "\\relative_error", ["x",double_type], Lreal;
            (*"\\relative_error", ["x",long_double_type], Lreal;*)

            "\\round_float", ["m",  rounding_mode; "x", Lreal], float_type;
            "\\round_double", ["m", rounding_mode ; "x", Lreal], double_type;
            (*"\\round_quad", ["m",  rounding_mode; "x", Lreal], long_double_type;*)
          ]
    end

(*
Local Variables:
compile-command: "make -j -C ../../.."
End:
*)
