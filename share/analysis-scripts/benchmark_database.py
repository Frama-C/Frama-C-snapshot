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

import time
import os
import csv
import sqlite3

import git_utils

def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d


class Database:
    inserted_targets = {}

    def __init__(self, benchmark_tag, benchmark_comment, gitdir, analyzer_rev, reference_rev):
        self.benchmark_tag = benchmark_tag
        self.benchmark_comment = benchmark_comment
        if analyzer_rev is None:
            self.analyzer_hash = None
            self.analyzer = None
        else:
            self.analyzer_hash = git_utils.rev_parse(gitdir, analyzer_rev)
            self.analyzer = git_utils.name_rev(gitdir, analyzer_rev)
        self.reference_hash = git_utils.rev_parse(gitdir, reference_rev)
        self.connection = sqlite3.connect('benchmark-results.db')
        self.connection.row_factory = dict_factory
        self.setup_rdb()
        self.reference_results = self.query_rdb(self.reference_hash)

    def update(self, results):
        if not self.benchmark_tag is None:
            for result in results:
                inserted = result["target"] in self.inserted_targets
                if result["up_to_date"] and not inserted:
                    self.insert(result)
                    self.inserted_targets[result["target"]] = True

        for result in results:
            if result['target'] in self.reference_results:
                ref = self.reference_results[result['target']]
                def compute_diff(column, ratio):
                    nonlocal result, ref
                    try:
                        if ratio:
                            diff = result[column] / ref[column] - 1.0
                        else:
                            diff = result[column] - ref[column]
                    except TypeError:
                        diff =None
                    result['diff_' + column] = diff

                compute_diff('alarms', False)
                compute_diff('warnings', False)
                compute_diff('user_time', True)
                compute_diff('memory', True)
                compute_diff('coverage', False)
            else:
                result['diff_alarms'] = None
                result['diff_warnings'] = None
                result['diff_user_time'] = None
                result['diff_memory'] = None
                result['diff_coverage'] = None

    def insert_csv(self, result):
        filename="benchmark-results.csv"
        file_already_exists=os.path.isfile(filename)
        fieldnames = [
            'benchmark_tag', 'timestamp',
            'analyzer', 'analyzer_hash',
            'target', 'target_hash',
            'user_time', 'memory', 'alarms', 'warnings',
            'sem_reach_fun',  'syn_reach_fun', 'total_fun',
            'sem_reach_stmt', 'syn_reach_stmt',
            'cmd_args', 'benchmark_comment']
        with open(filename, 'a', newline='') as file:
            writer = csv.DictWriter(file,
                fieldnames=fieldnames, extrasaction='ignore',
                delimiter="\t", quotechar='"')
            if not file_already_exists:
                writer.writeheader()
            writer.writerow(result)

    def insert(self, result):
        completed_result = { **result,
            'benchmark_tag' : self.benchmark_tag,
            'benchmark_comment' : self.benchmark_comment,
            'target_hash' : git_utils.current_rev(result["target"]),
            'analyzer' : self.analyzer,
            'analyzer_hash' : self.analyzer_hash }
        self.insert_csv(completed_result)
        self.insert_rdb(completed_result)

    def setup_rdb(self):
        cursor = self.connection.cursor()
        cursor.execute(
            "CREATE TABLE IF NOT EXISTS benchmark_results ("
                "benchmark_tag TEXT NOT NULL,"
                "timestamp TEXT NOT NULL,"
                "analyzer TEXT NOT NULL,"
                "analyzer_hash TEXT NOT NULL,"
                "target TEXT NOT NULL,"
                "target_hash TEXT NOT NULL,"
                "user_time REAL NOT NULL,"
                "memory INTEGER NOT NULL,"
                "alarms INTEGER NOT NULL,"
                "warnings INTEGER NOT NULL,"
                "sem_reach_fun INTEGER NOT NULL,"
                "syn_reach_fun INTEGER NOT NULL,"
                "total_fun INTEGER NOT NULL,"
                "sem_reach_stmt INTEGER NOT NULL,"
                "syn_reach_stmt INTEGER NOT NULL,"
                "cmd_args TEXT NOT NULL,"
                "benchmark_comment TEXT);")
        self.connection.commit()

    def insert_rdb(self, result):
        cursor = self.connection.cursor()
        cursor.execute(
            "INSERT INTO benchmark_results("
                "benchmark_tag, timestamp, "
                "analyzer, analyzer_hash, target, target_hash, "
                "user_time, memory, alarms, warnings, "
                "sem_reach_fun, syn_reach_fun, total_fun, "
                "sem_reach_stmt, syn_reach_stmt, "
                "cmd_args, benchmark_comment) "
            "VALUES("
                "DATETIME('now','localtime'), "
                ":benchmark_tag, :analyzer, "
                ":analyzer_hash, :target, :target_hash, "
                ":user_time, :memory, :alarms, :warnings, "
                ":sem_reach_fun, :syn_reach_fun, :total_fun, "
                ":sem_reach_stmt, :syn_reach_stmt, "
                ":cmd_args, :benchmark_comment)", result)
        self.connection.commit()

    def query_rdb(self, analyzer_hash):
        cursor = self.connection.cursor()
        cursor.execute(
            "SELECT "
                "target, "
                "avg(user_time) as user_time, avg(memory) as memory, "
                "min(alarms) as alarms, min(warnings) as warnings, "
                "max(sem_reach_stmt) as sem_reach_stmt, "
                "max(syn_reach_stmt) as syn_reach_stmt "
            "FROM benchmark_results "
            "WHERE analyzer_hash=? "
            "GROUP BY target", (analyzer_hash,))
        results = {}
        for r in cursor.fetchall():
            r['coverage'] = r['sem_reach_stmt'] / r['syn_reach_stmt']
            results[r['target']] = r
        return results
