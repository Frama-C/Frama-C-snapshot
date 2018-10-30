include
  Plugin.Register(
    struct
      let name = "test"
      let shortname = "test"
      let help = "test"
    end)

let akey = register_category "a"
let ckey = register_category "a:b:c"
let bkey = register_category "a:b"
let dkey = register_category "d"

let wkey = register_warn_category "a"
let wkey_vis_err = register_warn_category "test-vis-err"
let wkey_inv_err = register_warn_category "test-inv-err"
let wkey_failure = register_warn_category "test-failure"
let () = set_warn_status wkey_vis_err Log.Winactive
let () = set_warn_status wkey_inv_err Log.Winactive
let () = set_warn_status wkey_failure Log.Winactive

let wkey_active wkey = get_warn_status wkey <> Log.Winactive

let run () =
  (* no backtraces in oracles. *)
  Printexc.record_backtrace false;
  warning "Uncategorized warning";
  warning ~wkey "Warning A";
  if wkey_active wkey_vis_err then error "Testing error function";
  if wkey_active wkey_inv_err then error "";
  if wkey_active wkey_failure then failure "Testing failure function";
  debug ~dkey:akey "A is enabled";
  debug ~dkey:bkey "B is enabled";
  debug ~dkey:ckey "C is enabled";
  debug ~dkey "D is enabled";
  result ~dkey:akey "A is enabled";
  result ~dkey:bkey "B is enabled";
  result ~dkey:ckey "C is enabled";
  result ~dkey "D is enabled";
  feedback ~dkey:akey "A is enabled";
  feedback ~dkey:bkey "B is enabled";
  feedback ~dkey:ckey "C is enabled";
  feedback ~dkey "D is enabled";
  warning ~wkey "Another Warning A"

let () = Db.Main.extend run
