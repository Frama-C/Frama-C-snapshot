(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

include Plugin.Register
  (struct
     let name = "report"
     let shortname = "report"
     let help = "Properties Status Report (experimental)"
   end)

module Print =
  False
    (struct
      let option_name = "-report"
      let help = "display a summary of properties status"
     end)

let printing = add_group "Printing Property Report"

let () = Parameter_customize.set_group printing
module PrintProperties =
  False
    (struct
      let option_name = "-report-print-properties"
      let help = "print not only the locations, but also the \
                             properties themselves"
     end)

let () = Parameter_customize.set_group printing
module Untried =
  False
    (struct
      let option_name = "-report-untried"
      let help = "display properties which no plug-in tried to prove"
     end)

let () = Parameter_customize.set_group printing
module Specialized =
  True
    (struct
      let option_name = "-report-specialized"
      let help = "display properties that are auxiliary instances of other \
          properties."
     end)

let () = Parameter_customize.set_group printing
module Proven =
  True
    (struct
      let option_name = "-report-proven"
      let help = "if set, output proven properties. Otherwise, only unproven \
                   ones are shown."
     end)

let () = Parameter_customize.set_group printing
module CSVFile =
  String
    (struct
      let option_name = "-report-csv"
      let arg_name = "name"
      let default = ""
      let help = "if set, output properties as a csv file of the given name"
     end)

let monitoring = add_group "Monitoring of Properties, Errors and Warnings"

let () = Parameter_customize.set_group monitoring
module Classify =
  False
    (struct
      let option_name = "-report-classify"
      let help = "Report classification of all properties, errors and warnings"
    end)

let () = Parameter_customize.set_group monitoring
module Rules =
  String_list
    (struct
      let option_name = "-report-rules"
      let arg_name = "*.json,..."
      let help = "Configure the rules to apply for classification,\
                  and start monitoring."
    end)

let () = Parameter_customize.set_group monitoring
module Warning =
  String
    (struct
      let option_name = "-report-unclassified-warning"
      let arg_name = "action"
      let default = "REVIEW"
      let help = "Action to be taken on unclassified warnings\
                  (default is: 'REVIEW')"
    end)

let () = Parameter_customize.set_group monitoring
module Error =
  String
    (struct
      let option_name = "-report-unclassified-error"
      let arg_name = "action"
      let default = "ERROR"
      let help = "Action to be taken on unclassified errors\
                  (default is: 'ERROR')"
    end)

let () = Parameter_customize.set_group monitoring
module Status =
  True
    (struct
      let option_name = "-report-status"
      let help = "Classify also property statuses"
    end)

let () = Parameter_customize.set_group monitoring
module UntriedStatus =
  String
    (struct
      let option_name = "-report-unclassified-untried"
      let arg_name = "action"
      let default = "SKIP"
      let help = "Action to be taken on untried properties \
                  (default is: 'SKIP')"
    end)

let () = Parameter_customize.set_group monitoring
module UnknownStatus =
  String
    (struct
      let option_name = "-report-unclassified-unknown"
      let arg_name = "action"
      let default = "REVIEW"
      let help = "Action to be taken on unknown properties\
                  (default is: 'REVIEW')"
    end)

let () = Parameter_customize.set_group monitoring
module InvalidStatus =
  String
    (struct
      let option_name = "-report-unclassified-invalid"
      let arg_name = "action"
      let default = "ERROR"
      let help = "Action to be taken on invalid properties\
                  (default is: 'ERROR')"
    end)

let () = Parameter_customize.set_group monitoring
module Output =
  String
    (struct
      let option_name = "-report-output"
      let arg_name = "*.json"
      let help = "Output -report-classify in JSON format"
      let default = ""
    end)

let () = Parameter_customize.set_group monitoring
module AbsolutePath =
  False
    (struct
      let option_name = "-report-absolute-path"
      let help = "Report absolute path locations"
    end)

let () = Parameter_customize.set_group monitoring
module OutputReviews =
  String
    (struct
      let option_name = "-report-output-reviews"
      let arg_name = "file"
      let help = "Output number of reviews to <file>"
      let default = ""
    end)

let () = Parameter_customize.set_group monitoring
module OutputErrors =
  String
    (struct
      let option_name = "-report-output-errors"
      let arg_name = "file"
      let help = "Output number of errors to <file>"
      let default = ""
    end)

let () = Parameter_customize.set_group monitoring
module OutputUnclassified =
  String
    (struct
      let option_name = "-report-output-unclassified"
      let arg_name = "file"
      let help = "Output number of unclassified to <file>"
      let default = ""
    end)

let () = Parameter_customize.set_group monitoring
module Stderr =
  False
    (struct
      let option_name = "-report-stderr"
      let help = "Output detailed textual classification on stderr"
    end)

let () = Parameter_customize.set_group monitoring
module Stdout =
  False
    (struct
      let option_name = "-report-stdout"
      let help = "Force detailed textual classification on stdout"
    end)

let () = Parameter_customize.set_group monitoring
module Exit =
  True
    (struct
      let option_name = "-report-exit"
      let help = "Exit on error"
     end)
