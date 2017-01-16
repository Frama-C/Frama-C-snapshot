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
        "-inout")
            OPT="${OPT} -eva-inout-domain -value-msg-key d-inout"
            ;;
        "-signs")
            OPT="${OPT} -eva-signs-domain -value-msg-key d-sign"
            ;;
        *)
            OPT="${OPT} $1"
            ;;
    esac
    shift
done

${CMD} \
    -val \
    -value-msg-key=-final-states \
    -journal-disable \
    ${OPT} ${PRV}
