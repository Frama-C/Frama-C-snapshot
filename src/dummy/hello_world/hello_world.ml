(** The traditional Hello world! plugin.
    It contains one boolean state [Enabled] which can be set by the
    command line option "-hello".
    When this option is set it just pretty prints a message on the standard
    error.
*)


module Enabled = Cmdline.Dynamic.Register.False(
  struct 
    let name = "hello enabled" 
  end)

let startup () = 
  if Enabled.get () then 
    prerr_endline "Hello world!"

let options = 
  [ "-hello",
    Arg.Unit Enabled.on,
    ": pretty print \"Hello world!\"" ]

let () = 
  Options.add_plugin
    ~name:"hello world"
    ~descr:"Hello world plugin"
    options;
  Dynamic.Main.extend startup

