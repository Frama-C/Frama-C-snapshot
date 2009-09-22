let default_suites = ref [ "occurrence"; "idct"; "test"; "misc"; "float"; "constant_propagation"; "wp"; "security"; "impact"; "pdg"; "scope"; "sparecode"; "slicing"; "slicing2"; "ltl_to_acsl"; "dynamic"; "dynamic_plugin"; "journal"; "saveload"; "spec"; ];;
let no_native_dynlink =  false ;;
let toplevel_path = ref "bin/toplevel.opt";;
let framac_share = ref (Filename.concat Filename.current_dir_name "share");;
let framac_plugin = ref (Filename.concat (Filename.concat Filename.current_dir_name "lib") "plugins");;
let framac_plugin_gui = ref (Filename.concat (Filename.concat Filename.current_dir_name "lib") "gui");;
