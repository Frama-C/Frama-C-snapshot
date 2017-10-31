#!/bin/bash

log_file=$1
stats_file=$2

read syn_reach_fun total_fun \
  <<<`sed -n 's/^[[:blank:]]*Syntactically reachable functions = \([[:digit:]]*\) (out of \([[:digit:]]*\))$/\1 \2/p' $log_file`
read sem_reach_fun \
  <<<`sed -n 's/^[[:blank:]]*Semantically reached functions = \([[:digit:]]*\)$/\1/p' $log_file`
read reach_fun_percent \
  <<<`sed -n 's/^[[:blank:]]*Coverage estimation = \([[:digit:].]*\)%/\1/p' $log_file`
read syn_reach_stmt sem_reach_stmt reach_stmt_percent \
  <<<`sed -n 's/^[[:blank:]]*\([[:digit:]]*\) stmts in analyzed functions, \([[:digit:]]*\) stmts analyzed (\([[:digit:].]*\)%)$/\1 \2 \3/p' $log_file`

(
  printf 'sem_reach_fun=%s\n' $sem_reach_fun;
  printf 'syn_reach_fun=%s\n' $syn_reach_fun;
  printf 'total_fun=%s\n' $total_fun;
  printf 'sem_reach_stmt=%s\n' $sem_reach_stmt;
  printf 'syn_reach_stmt=%s\n' $syn_reach_stmt; 
) >> $stats_file

