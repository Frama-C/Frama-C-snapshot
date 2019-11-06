open Cil_types

let update_func f =
  let insert_circ f = Cil.makeFormalVar f ~where:"^" "x" Cil.intType in
  let insert_dollar f = Cil.makeFormalVar f ~where:"$" "x" Cil.intType in
  let insert_circ_g f = Cil.makeFormalVar f ~ghost:true ~where:"^" "x" Cil.intType in
  let insert_dollar_g f = Cil.makeFormalVar f ~ghost:true ~where:"$" "x" Cil.intType in
  let insert_a f = Cil.makeFormalVar f ~where:"a" "x" Cil.intType in
  let insert_a_g f = Cil.makeFormalVar f ~ghost:true ~where:"a" "x" Cil.intType in
  let circ_list = [
    "void_circumflex" ;
    "a_circumflex" ;
    "ghost_a_circumflex"
  ] in
  let dollar_list = [
    "void_dollar" ;
    "a_dollar" ;
    "ghost_a_dollar"
  ] in
  let circ_g_list = [
    "void_circumflex_g" ;
    "a_circumflex_g" ;
    "ghost_a_circumflex_g" ;
    "g_void_circumflex" ;
    "g_a_circumflex"
  ] in
  let dollar_g_list = [
    "void_dollar_g" ;
    "a_dollar_g" ;
    "ghost_a_dollar_g" ;
    "g_void_dollar" ;
    "g_a_dollar"
  ] in
  let a_list = [
    "a_a" ;
    "a_b_c_a" ;
    "b_a_c_a" ;
    "a_ghost_b_c_a" ;
  ] in
  let a_g_list = [
    "ghost_a_a" ;
    "all_ghost_a_b_c_a" ;
    "all_ghost_b_a_c_a" ;
    "b_ghost_a_c_a" ;
    "g_a_a" ;
    "g_a_b_c_a" ;
    "g_b_a_c_a"
  ] in
  if List.mem f.svar.vname circ_list then ignore(insert_circ f) ;
  if List.mem f.svar.vname dollar_list then ignore(insert_dollar f) ;
  if List.mem f.svar.vname circ_g_list then ignore(insert_circ_g f) ;
  if List.mem f.svar.vname dollar_g_list then ignore(insert_dollar_g f) ;
  if List.mem f.svar.vname a_list then ignore(insert_a f) ;
  if List.mem f.svar.vname a_g_list then ignore(insert_a_g f) ;
  ()

let run () =
  Globals.Functions.iter_on_fundecs update_func

let () = Db.Main.extend run
