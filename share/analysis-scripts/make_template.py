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

# This script is used to interactively fill template.mk, converting it
# into a GNUmakefile ready for analysis.

import sys
import os
import re
from subprocess import Popen, PIPE
from pathlib import Path

MIN_PYTHON = (3, 6) # for glob(recursive) and automatic Path conversions
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)

if len(sys.argv) > 3:
    print("usage: %s path-to-frama-c-script [dir]" % sys.argv[0])
    print("       creates a GNUmakefile for running Frama-C on a set of files,")
    print("       interactively filling a template.")
    sys.exit(1)

if not os.path.isfile(sys.argv[1]):
    print("error: path to frama-c-script is not a file: " + sys.argv[1])
    sys.exit(1)

jcdb = Path("compile_commands.json")

if "PTESTS_TESTING" in os.environ:
    print("Running ptests: setting up mock files...")
    jcdb.touch()

bindir = Path(os.path.dirname(os.path.abspath(sys.argv[1])))
frama_c_config = bindir / "frama-c-config"
process = Popen([frama_c_config, "-print-share-path"], stdout=PIPE)
(output, err) = process.communicate()
output = output.decode('utf-8')
exit_code = process.wait()
if exit_code != 0:
    print("error running frama-c-config")
    sys.exit(1)
sharedir = Path(output)

def get_known_machdeps():
    process = Popen([bindir / "frama-c", "-machdep", "help"], stdout=PIPE)
    (output, err) = process.communicate()
    output = output.decode('utf-8')
    exit_code = process.wait()
    if exit_code != 0:
        print("error getting machdeps: " + output)
        sys.exit(1)
    match = re.match("\[kernel\] supported machines are (.*) \(default is (.*)\).", output, re.DOTALL)
    if not match:
        print("error getting known machdeps: " + output)
        sys.exit(1)
    machdeps = match.group(1).split()
    default_machdep = match.group(2)
    return (default_machdep, machdeps)

dir = Path(sys.argv[2] if len(sys.argv) == 3 else ".")
gnumakefile = dir / "GNUmakefile"

def check_path_exists(path):
    if os.path.exists(path):
        yn = input("warning: {} already exists. Overwrite? [y/N] ".format(path))
        if yn == "" or not (yn[0] == "Y" or yn[0] == "y"):
            print("Exiting without overwriting.")
            sys.exit(0)

check_path_exists(gnumakefile)
main = input("Main target name: ")
if not re.match("^[a-zA-Z_0-9]+$", main):
    print("error: invalid main target name")
    sys.exit(1)

sources = input("Source files separated by spaces (default if empty: *.c): ")
if not sources:
    sources="*.c"

json_compilation_database = None
if jcdb.is_file():
    yn = input("compile_commands.json exists, add option -json-compilation-database? [Y/n] ")
    if yn == "" or not (yn[0] == "N" or yn[0] == "n"):
        json_compilation_database = "."
    else:
        print("Option not added; you can later add it to FCFLAGS.")

add_main_stub = False
yn = input("Add stub for function main (only needed if it uses command-line arguments)? [y/N] ")
if yn != "" and (yn[0] == "Y" or yn[0] == "y"):
    add_main_stub = True
    sources = "fc_stubs.c " + sources

print("Please define the architectural model (machdep) of the target machine.")
(default_machdep, machdeps) = get_known_machdeps()
print("Known machdeps: " + " ".join(machdeps))
machdep_chosen = False
while not machdep_chosen:
    machdep = input("Please enter the machdep [" + default_machdep + "]: ")
    if not machdep:
        machdep = default_machdep
        machdep_chosen = True
    else:
        if not (machdep in machdeps):
            yn = input("'{}' is not a standard machdep. Proceed anyway? [y/N]".format(machdep))
            if yn != "" and (yn[0] == "Y" or yn[0] == "y"):
                machdep_chosen = True
        else:
            machdep_chosen = True

def insert_line_after(lines, line_pattern, newline):
    re_line = re.compile(line_pattern)
    for i in range(0, len(lines)):
        if re_line.search(lines[i]):
            lines.insert(i+1, newline)
            return lines
    print("error: no lines found matching pattern: " + line_pattern)
    sys.exit(1)

def replace_line(lines, line_pattern, value):
    re_line = re.compile(line_pattern)
    for i in range(0, len(lines)):
        if re_line.search(lines[i]):
            lines[i] = value
            return lines
    print("error: no lines found matching pattern: " + line_pattern)
    sys.exit(1)

def remove_lines_between(lines, start_pattern, end_pattern):
    re_start = re.compile(start_pattern)
    re_end = re.compile(end_pattern)
    first_to_remove = -1
    last_to_remove = -1
    for i in range(0, len(lines)):
        if first_to_remove == -1 and re_start.search(lines[i]):
            first_to_remove = i
        elif re_end.search(lines[i]):
            last_to_remove = i
            break
    if first_to_remove == -1:
        print("error: could not find start pattern: " + start_pattern)
        sys.exit(1)
    elif last_to_remove == -1:
        print("error: could not find end pattern: " + end_pattern)
        sys.exit(1)
    return (lines[:first_to_remove-1] if first_to_remove > 0 else []) + (lines[last_to_remove+1:] if last_to_remove < len(lines)-1 else [])

with open(sharedir / "analysis-scripts" / "template.mk") as f:
    lines = list(f)
    lines = replace_line(lines, "^MAIN_TARGET :=", "MAIN_TARGET := {}\n".format(main))
    lines = remove_lines_between(lines, "Remove these lines.*main target", "^endif")
    lines = replace_line(lines, "^\$\(MAIN_TARGET\).parse:", "$(MAIN_TARGET).parse: {}\n".format(sources))
    if json_compilation_database:
      lines = insert_line_after(lines, "^FCFLAGS", "  -json-compilation-database {} \\\n".format(json_compilation_database))
    lines = insert_line_after(lines, "^FCFLAGS", "  -machdep {} \\\n".format(machdep))
    if add_main_stub:
        check_path_exists("fc_stubs.c")
        from shutil import copyfile
        copyfile(sharedir / "analysis-scripts" / "fc_stubs.c", "fc_stubs.c")
        lines = insert_line_after(lines, "^FCFLAGS", "  -main eva_main \\\n")
        print("Created stub for main function: fc_stubs.c")

gnumakefile.write_text("".join(lines))

print("Template created: " + gnumakefile.name)

if "PTESTS_TESTING" in os.environ:
    print("Running ptests: cleaning up after tests...")
    jcdb.unlink()
    if add_main_stub:
        Path("fc_stubs.c").unlink()
