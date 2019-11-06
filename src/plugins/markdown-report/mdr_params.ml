(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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

include Plugin.Register(
  struct
    let name = "Markdown report"
    let shortname = "mdr"
    let help = "generates a report in markdown format"
  end)

module Output = String(
  struct
    let option_name = "-mdr-out"
    let arg_name = "f"
    let default = "report.md"
    let help = "sets the name of the output file to <f>"
  end)

module Generate = String(
  struct
    let option_name = "-mdr-gen"
    let arg_name = "kind"
    let default = "none"
    let help =
      "select the <kind> of report to generate among: \
       none (default), md, draft and sarif"
  end)

let () =
  Generate.set_possible_values [ "none"; "md"; "draft"; "sarif" ]

module Remarks = Empty_string(
  struct
    let option_name = "-mdr-remarks"
    let arg_name = "f"
    let help =
      "reads file <f> to add additional remarks to various sections of the report. \
       Must be in a format compatible with the file produced by -mdr-gen-draft. \
       Remarks themselves must be written in pandoc's markdown, although this is \
       not enforced by the plug-in"
  end
  )

module FlameGraph = Empty_string(
  struct
    let option_name = "-mdr-flamegraph"
    let arg_name = "f"
    let help =
      "reads file <f> to include a FlameGraph (https://github.com/brendangregg/FlameGraph.git),\
       allowing the most analysis-intensive callstacks to be identified\
       quickly and accurately"
  end
  )

module Authors = String_list(
  struct
    let option_name = "-mdr-authors"
    let arg_name = "l"
    let help = "list of authors of the report"
  end)

module Title = Empty_string(
  struct
    let option_name = "-mdr-title"
    let arg_name = "t"
    let help = "title of the generated document"
  end)

module Date = Empty_string(
  struct
    let option_name = "-mdr-date"
    let arg_name = "d"
    let help = "date of the report"
  end)

module Stubs = String_list(
  struct
    let option_name = "-mdr-stubs"
    let arg_name = "f1,...,fn"
    let help = "list of C files containing stub functions"
  end)
