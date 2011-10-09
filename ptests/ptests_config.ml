let default_suites : string list ref = ref [ "occurrence"; "metrics"; "rte"; "idct"; "test"; "float"; "constant_propagation"; "impact"; "pdg"; "scope"; "sparecode"; "slicing"; "slicing2"; "dynamic"; "dynamic_plugin"; "journal"; "saveload"; "spec"; "misc"; ];;
let no_native_dynlink =  false ;;
let toplevel_path = ref "bin/toplevel.opt";;
let framac_share = ref (Filename.concat Filename.current_dir_name "share");;
let framac_plugin = ref (Filename.concat (Filename.concat Filename.current_dir_name "lib") "plugins");;
let framac_plugin_gui = ref (Filename.concat !framac_plugin "gui");;
let framac_lib = ref (Filename.concat (Filename.concat Filename.current_dir_name "lib")"fc");;
