#!/bin/sh

CMD="$(dirname $0)/../bin/frama-c"
OPT=""

while [ "$1" != "" ];
do
    case $1 in
        "-h"|"--help")
            echo "fc.sh [files] [options...]" ;
            echo "  -h,--help  print help and exit" ;
            echo "  -g,--gui   run frama-c GUI" ;
            exit 0 ;
            ;;
        "-g"|"--gui"|"-gui")
            CMD="../bin/frama-c-gui -gui-no-undo" ;;
        *)
            OPT="${OPT} $1"
            ;;
    esac
    shift
done

${CMD} \
    -val \
    -eva-inout-domain \
    -value-msg-key d-inout,d-sign,-final-states \
    -journal-disable \
    ${OPT} ${PRV}
