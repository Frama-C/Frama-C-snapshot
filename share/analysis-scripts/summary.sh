#!/bin/bash -u

declare -A stats

function pretty_size()
{
  ([[ $# -lt 1 ]] || ! [[ $1 =~ ^[0-9]+$ ]]) && return
  KB=$1
  [ $KB -lt 4096 ] && echo ${KB} kiB && return
  MB=$(((KB+512)/1024))
  [ $MB -lt 4096 ] && echo ${MB} MiB && return
  GB=$(((MB+512)/1024))
  echo $GB GiB
}

function pretty_coverage()
{
  if [[ $# -gt 1 ]] && [ -n '$1' -a $2 -ne 0 ]
  then
    echo $(bc <<<"scale=1; 100 * $2 / $1")%
  fi
}

function print_results()
{
    local t
    local format

    format="%20s %10s %10s %10s %10s %10s\n"

    if [ -z "$quiet" ]
    then
        echo -e '\e\0143'
        printf "$format" 'target' 'coverage' 'alarms' 'warnings' 'time' 'memory'
        printf "%s\n" " ----------------------------------------------------------------------------"
        for t in $targets
        do
            printf "$format" $t \
              "${stats["$t,coverage"]-}" \
              "${stats["$t,alarms"]-}" \
              "${stats["$t,warnings"]-}" \
              "${stats["$t,user_time"]-}" \
              "${stats["$t,memory"]-}"
        done
        printf "%s\n" " ----------------------------------------------------------------------------"
        printf "$format" 'total' '' "${stats["total_alarms"]-}" "${stats["total_warnings"]-}" "${stats["total_user_time"]-}" ''
        printf "\n"
    fi
}

function print_csv()
{
    local t
    local format

    format="%s\t%s\t%s\t%s\t%s\t%s\n"

    printf "$format" 'target' 'coverage' 'alarms' 'warnings' 'time' 'memory'
    for t in $targets
    do
        printf "$format" $t \
          "${stats["$t,coverage"]-}" \
          "${stats["$t,alarms"]-}" \
          "${stats["$t,warnings"]-}" \
          "${stats["$t,user_time"]-}" \
          "${stats["$t,memory"]-}"
    done
}

function poll_results()
{
  stats["total_alarms"]=0
  stats["total_warnings"]=0
  stats["total_user_time"]=0

  for t in $targets
  do
      if [ -f "$t/stats.txt" ]
      then
          read stats["$t,syn_reach"] stats["$t,sem_reach"] \
               stats["$t,alarms"]    stats["$t,warnings"] \
               stats["$t,user_time"] stats["$t,mem_bytes"] <<< $(
              source $t/stats.txt
              echo ${syn_reach_stmt:-0} ${sem_reach_stmt:-0} \
                   ${alarms:-x} ${warnings:-x} \
                   ${user_time:-x} ${memory:-'x'}
          )
          stats["$t,coverage"]=$(pretty_coverage ${stats["$t,syn_reach"]} ${stats["$t,sem_reach"]})
          stats["$t,memory"]=$(pretty_size ${stats["$t,mem_bytes"]})
          stats["total_alarms"]=$(bc <<<"${stats["total_alarms"]} + ${stats["$t,alarms"]-0}")
          stats["total_warnings"]=$(bc <<<"${stats["total_warnings"]} + ${stats["$t,warnings"]-0}")
          stats["total_user_time"]=$(bc <<<"${stats["total_user_time"]} + ${stats["$t,user_time"]-0}")
      fi
  done
}


# Parse command Line

run="make"
targets=""
quiet=""

while [[ $# > 0 ]]
do
    case $1 in
        -f|--file|--makefile)
            run="$run $1 $2"
            shift
            ;;

        -B|--always-make)
            run="$run $1"
            ;;

        -q|--quiet)
            quiet="yes"
            ;;

        *)
            targets="$targets $1"
            ;;
    esac
    shift
done


# List make targets

for t in $targets
do
    run="$run $t"
done


# Run and display

{
    $run > /dev/null &
    pid=$!

    poll_results
    print_results

    while ps -p $pid >/dev/null
    do
        sleep 1
        poll_results
        print_results
    done
} 2> summary.log

cat summary.log >&2
rm -f summary.log
print_csv > summary.csv
