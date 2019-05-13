#!/usr/bin/env python3
#-*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2018                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# This script serves as wrapper to 'make' (when using the analysis-scripts
# GNUmakefile template): it parses the output and suggests useful commands
# whenever it can, by calling frama-c-script itself.

import subprocess
import sys
import os
import re
from functools import partial

if len(sys.argv) < 3:
   print("usage: %s path-to-frama-c-script target" % sys.argv[0])
   print("       Builds the specified target, parsing the output to")
   print("       identify and recommend actions in case of failure.")
   print("       The first argument must be the path to the frama-c-script")
   print("       binary.")
   sys.exit(1)

framac_script = sys.argv[1]
target = sys.argv[2]
args = sys.argv[3:]

out = subprocess.Popen(['make', target] + args,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

output = out.communicate()[0].decode('utf-8')

re_missing_spec = re.compile("Neither code nor specification for function ([^,]+),")
re_recursive_call_start = re.compile("detected recursive call")
re_recursive_call_end = re.compile("Use -eva-ignore-recursive-calls to ignore")

tips = []

lines = iter(output.splitlines())
for line in lines:
    print(line)
    match = re_missing_spec.search(line)
    if match:
       fname = match.group(1)
       def action(fname):
           out = subprocess.Popen([framac_script, "find-fun", fname],
                                  stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
           output = out.communicate()[0].decode('utf-8')
           re_possible_definers = re.compile("Possible definitions for function")
           find_fun_lines = iter(output.splitlines())
           for find_fun_line in find_fun_lines:
              if re_possible_definers.match(find_fun_line):
                 found_files = [next(find_fun_lines)]
                 while True:
                    try:
                       found_files.append(next(find_fun_lines))
                    except StopIteration:
                       if len(found_files) > 1:
                          print("Found several files defining function '"
                                + fname + "', cannot recommend automatically.")
                          print("Check which one is appropriate and add it " +
                                "to the list of sources to be parsed:")
                          print("\n".join(found_files))
                       else:
                          print("Add the following file to the list of "
                                + "sources to be parsed:\n" + found_files[0])
                       return
           print("Could not find any files defining " + fname + ".")
           print("Find the sources defining it and add them, " +
                 "or provide a stub.")
       tip = {"message": "Found function with missing spec: " + fname + "\n" +
              "   Looking for files defining it...",
              "action":partial(action, fname)
       }
       tips.append(tip)
    else:
       match = re_recursive_call_start.search(line)
       if match:
          def action():
             print("Consider patching or stubbing the recursive call, " +
                   "then re-run the analysis.")
          msg_lines = []
          line = next(lines)
          while True:
             match = re_recursive_call_end.search(line)
             if match:
                tip = {"message": "Found recursive call at:\n" +
                       "\n".join(msg_lines),
                       "action":action
                       }
                tips.append(tip)
                break
             else:
                msg_lines.append(line)
                try:
                   line = next(lines)
                except StopIteration:
                   print("** Error: EOF without ending recursive call stack?")
                   assert False

if tips != []:
   print("")
   print("***** make-wrapper recommendations *****")
   print("")
   counter = 1
   print("*** recommendation #" + str(counter) + " ***")
   print("")
   for tip in tips:
      if counter > 1:
         print("")
         print("*** recommendation #" + str(counter) + " ***")
      print(str(counter) + ". " + tip["message"])
      counter += 1
      tip["action"]()
