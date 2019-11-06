#!/usr/bin/env python3
#-*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2019                                               #
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

# This script finds files containing likely declarations and definitions
# for a given function name, via heuristic syntactic matching.

import sys
import os
import re
import glob

MIN_PYTHON = (3, 5) # for glob(recursive)
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)

debug = False

arg = ""
if len(sys.argv) < 2:
   print("usage: %s fname [dir1 dir2 ...]" % sys.argv[0])
   print("       looks for likely declarations/definitions of function fname")
   print("       in files with extensions '.c', '.h' and '.i';")
   print("       if dir1, dir2, etc, are specified, looks inside them,")
   print("       otherwise looks inside PWD and /usr/include.")
   print("       Subdirectories are always considered recursively.")
   sys.exit(1)
else:
   fname = sys.argv[1]
   if re.match('[a-zA-Z_][a-zA-Z0-9_]*$', fname) == None:
      print("error: function name contains invalid characters: %s" % fname)
      print("       (only letters/digits/underscore allowed)")
      sys.exit(1)

dirs = set()
if len(sys.argv) < 3:
   pwd = os.getcwd()
   dirs = [pwd, "/usr/include"]
else:
   dirs = set(sys.argv[2:])

if debug:
   print("Looking for files in dirs (and their subdirs): %s" % dirs)

files = []
for d in dirs:
   files += glob.glob(d + "/**/*.[ich]", recursive=True)

print("Looking for '%s' inside %d file(s)..." % (fname, len(files)))
#print("\n".join(files))

# To minimize the amount of false positives, we try to match the following:
# - the line must begin with a C identifier (declarations and definitions in C
#   rarely start with spaces in the line), or with the function name itself
#   (supposing the return type is in the previous line)
# - any number of identifiers are allowed (to allow for 'struct', 'volatile',
#   'extern', etc)
# - asterisks are allowed both before and after identifiers, except for the
#   first one (to allow for 'char *', 'struct **ptr', etc)
# - identifiers are allowed after the parentheses, to allow for some macros/
#   modifiers

possible_declarators = []
possible_definers = []
c_identifier = "[a-zA-Z_][a-zA-Z0-9_]*"
c_id_maybe_pointer = c_identifier + "\**"
type_prefix = c_id_maybe_pointer + "(?:\s+\**" + c_id_maybe_pointer + ")*\s+\**"
parentheses_suffix = "\s*\([^)]*\)"
re_fun = re.compile("^(?:" + type_prefix + "\s*)?" + fname + parentheses_suffix
                  + "\s*(?:" + c_identifier + ")?\s*(;|{)", flags=re.MULTILINE)
for f in files:
   with open(f, encoding="ascii", errors='ignore') as content_file:
      content = content_file.read()
      has_decl_or_def = re_fun.search(content)
      if has_decl_or_def is not None:
         is_decl = has_decl_or_def.group(1) == ";"
         if is_decl:
            possible_declarators.append(f)
         else:
            possible_definers.append(f)

if possible_declarators == [] and possible_definers == []:
   print("No declaration/definition found for function '%s'" % fname)
else:
   if possible_declarators != []:
      print("Possible declarations for function '%s' in the following file(s):"
            % fname)
      print("  " + "\n  ".join(map(os.path.relpath, possible_declarators)))
   if possible_definers != []:
      print("Possible definitions for function '%s' in the following file(s):"
            % fname)
      print("  " + "\n  ".join(map(os.path.relpath, possible_definers)))
