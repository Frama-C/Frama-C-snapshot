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

import subprocess

def rev_parse(gitdir, rev):
    res = subprocess.run(
        ['git', 'rev-parse', rev],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding='ascii',
        cwd=gitdir)
    name = res.stdout.strip()
    return name if name else None

def name_rev(gitdir, rev):
    res = subprocess.run(
        ['git', 'name-rev', '--name-only', rev],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding='ascii',
        cwd=gitdir)
    name = res.stdout.strip()
    return name if name else None

def current_rev(gitdir):
    return name_rev(gitdir, "HEAD")

def is_clean(gitdir):
    # git diff and diff-index are not working on some of our case studies to
    # decide whether the workingin dir is clean or not ; git status is more
    # reliable
    res = subprocess.run(
        ['git', 'status', '--untracked-files=no', '--porcelain'],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        encoding='ascii',
        cwd=gitdir)
    return res.returncode == 0 and not res.stdout

