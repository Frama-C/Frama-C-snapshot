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

import re
import sys
import subprocess
import time
import os
import signal
import argparse
import uuid

import frama_c_results
import results_display
import benchmark_database

class OperationException(Exception):
    pass

def build_env(framac):
    if framac is None:
        return { **os.environ }
    else:
        bindir = framac + '/build/bin'
        return { **os.environ,  'PATH' : bindir + ':' + os.environ['PATH'] }

def list_targets():
    env = build_env(framac)
    res = subprocess.run(
        ["make", "--quiet", "display-targets"],
        env=env,
        stdout=subprocess.PIPE,
        encoding='ascii')
    return res.stdout.split()

def clone_frama_c(clonedir, hash):
    print("Cloning Frama-C", hash, "...")
    res = subprocess.run(
        ["./scripts/clone.sh", "--clone-dir", clonedir, hash],
        stdout=subprocess.PIPE,
        encoding='ascii')
    if res.returncode != 0:
        raise OperationException("Cannot clone repository. Try to manually"
            "remove the broken clone in " + clonedir)
    return res.stdout.strip()

def run_make(framac, benchmark_tag=None):
    args = ['make', '--keep-going', 'all']
    env = build_env(framac)
    if not framac is None:
        bindir = framac + '/build/bin'
        args += [
            'FRAMAC_DIR=' + bindir,
            'FRAMAC=' + bindir + '/frama-c',
            'FRAMAC_CONFIG=' + bindir + '/frama-c-config']
    if benchmark_tag is None:
        args += ['-j', '8']
    else:
        args += ['BENCHMARK=' + benchmark_tag]
    return subprocess.Popen(args, env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        preexec_fn=os.setsid)

def terminate_process(process):
    if process is None:
        return b""
    else:
        try:
            os.killpg(os.getpgid(process.pid), signal.SIGTERM)
            pass
        except ProcessLookupError:
            pass
        output,errors = process.communicate()
        return errors

def smart_rename(target):
    target = re.sub('main\.eva$', '', target)
    target = re.sub('\.eva$', '', target)
    target = re.sub('qds/frama-c', 'qds', target)
    return target

def is_running(target):
    return os.path.isfile(target + '/running')

def poll_results(targets, benchmark_tag):
    results = []
    for target in targets:
        filename = target + '/stats.txt'
        result = frama_c_results.read(filename)
        result["target"] = target
        result["target_name"] = smart_rename(target)
        result["is_running"] = is_running(target)
        result["up_to_date"] = benchmark_tag is None or benchmark_tag == result['benchmark_tag']
        results.append(result);
    return results


def run_analyses(display, database, framac, benchmark_tag):
    results = []
    targets = list_targets()
    process = run_make(framac, benchmark_tag)
    errors = b""
    next_poll = time.time()

    def update():
        nonlocal  display, database, targets, benchmark_tag, results
        results = poll_results(targets, benchmark_tag)
        if not database is None:
            database.update(results)
        display.needs_update = True

    try:
        while process.poll() is None:
            if time.time() >= next_poll:
                update()
                next_poll = time.time() + 2.0
            display.process_inputs()
            if display.needs_update:
                display.print_table(results)
            time.sleep(0.05)
        update()
    except (KeyboardInterrupt, results_display.UserExitRequest):
        print("Analyzes interrupted by user.")
    except Exception as e:
        # terminate_process below is somehow blocking the exception printing
        errors += bytearray(str(e), 'ascii')
        raise e
    finally:
        errors += terminate_process(process)
    return results,errors


parser = argparse.ArgumentParser(
    description="Run analyses and summarize the results. Must be run in a "
                "directory with a Makefile having two rules: 'all', a target "
                "that runs the analysis, and 'display-targets', the target that "
                "lists the built results.")
parser.add_argument('rev', nargs='?', metavar="REVISION",
    help="a Frama-C revision to use for analyses (default: use the "
        "default configuration for Frama-C)")
parser.add_argument('-b', '--benchmark',
    action="store_true",
    help="sets benchmark mode: do not run analyses in parallel and rerun all "
        "analyses")
parser.add_argument('-v', '--vs',
    action="store", metavar="REVISION", default="master",
    help="a revision to compare the results to")
parser.add_argument('-c', '--comment',
    action="store", metavar="COMMENT",
    help="when benchmarking, add this comment inside the database")
parser.add_argument('-p', '--repository-path',
    action="store", metavar="PATH",
    help="don't clone Frama-C, use this git repository instead")


errors = b''

try:
    args = parser.parse_args()

    if args.repository_path is None:
        if args.rev is None:
            gitdir = None
            framac = None
        else:
            clonedir = "./frama-c-clones"
            gitdir = clonedir + "/frama-c.git"
            framac = clone_frama_c(clonedir, args.rev)
    else:
        framac = args.repository_path
        gitdir = framac

    if args.benchmark:
        benchmark_tag=str(uuid.uuid1())
        print("Running benchmarks with benchmark tag", benchmark_tag, "...")
    else:
        benchmark_tag=None
        print("Running analyses ...")

    benchmark_comment = args.comment

    if gitdir is None:
        database = None
    else:
        database = benchmark_database.Database(benchmark_tag, benchmark_comment,
            gitdir, args.rev, args.vs)

    results,errors = results_display.wrapper(run_analyses, database, framac,
        benchmark_tag, curses=True)

    print("Results:\n")
    results_display.PlainDisplay().print_table(results)

except OperationException as e:
    errors += bytearray(str(e), 'ascii')

sys.stderr.buffer.write(errors + b'\n')
