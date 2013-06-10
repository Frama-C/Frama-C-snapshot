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

let run () =
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
  feedback ~dkey "D is enabled"

let () = Db.Main.extend run
  
