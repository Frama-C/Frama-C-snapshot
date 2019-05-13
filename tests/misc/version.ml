let re_version = Str.regexp "^\\([0-9]+\\)\\.\\([0-9]+\\)"

let run () =
  let version_str = Config.version in
  if Str.string_match re_version version_str 0 then
    let major = Str.matched_group 1 version_str in
    let minor = Str.matched_group 2 version_str in
    if major = string_of_int Config.major_version &&
       minor = string_of_int Config.minor_version
    then
      Kernel.feedback "version numbers match"
    else
      Kernel.abort
        "error parsing major/minor version: expected %s.%s, got %d.%d"
        major minor Config.major_version Config.minor_version
  else
    Kernel.abort
      "could not parse Config.version"

let () = Db.Main.extend run
