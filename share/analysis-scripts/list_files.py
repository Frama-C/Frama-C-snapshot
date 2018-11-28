#!/usr/bin/env python
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

# This script parses a compile_commands.json[1] file and lists the C files
# in it.
#
# [1] See: http://clang.llvm.org/docs/JSONCompilationDatabase.html

import sys
import os
import json
import re

arg = ""
if len(sys.argv) < 2:
   # no argument, assume default name
   arg = "compile_commands.json"
else:
   arg = sys.argv[1]

if not os.path.exists(arg):
   print("error: file '%s' not found" % arg)
   print("usage: %s [compile_commands.json]" % sys.argv[0])
   sys.exit(1)

# check if arg has a known extension
def has_known_c_extension(arg):
   return arg.endswith(".c") or arg.endswith(".i") or arg.endswith(".h")

pwd = os.getcwd()
json = json.loads(open(arg).read())
includes = set()
defines = set()
files = set()
for entry in json:
   arg_includes = [] # before normalization
   dir = entry["directory"]
   file = entry["file"]
   # json compile spec says either command or arguments are mandatory
   if os.path.isabs(file):
      filepath = file
   else:
      filepath = os.path.join(dir, file)
   if not has_known_c_extension(filepath):
      print("warning: ignoring file of unknown type: %s" % filepath)
   else:
      files.add(os.path.relpath(filepath, pwd))

print("SRCS=\\\n" + " \\\n".join(sorted(files)) + " \\")
print("")

files_defining_main = set()
re_main = re.compile("(int|void)\s+main\s*\([^)]*\)\s*\{")
for file in files:
   assert os.path.exists(file), "file does not exist: %s" % file
   with open(file, 'r') as content_file:
      content = content_file.read()
      res = re.search(re_main, content)
      if res is not None:
         files_defining_main.add(file)

if files_defining_main != []:
   print("")
   print("# Possible definition of main function in the following file(s):")
   print("\n".join(sorted(files_defining_main)))
