#!/bin/sh

# --------------------------------------------------------------------------
# ---  TUTO OPTIONS
# --------------------------------------------------------------------------


CMD="$(dirname $0)/../bin/frama-c"
PRV="-wp-prover tip,alt-ergo"
OPT=""

while [ "$1" != "" ];
do
    case $1 in
        "-h"|"--help")
            echo "fc.sh [options...]" ;
            echo "  -h,--help  print help and exit" ;
            echo "  -g,--gui   run frama-c GUI" ;
            exit 0 ;
            ;;
        "-g"|"--gui")
            CMD="../bin/frama-c-gui -gui-no-undo" ;;
        "-tip")
            CMD="../bin/frama-c-gui -gui-no-undo -no-unicode"
            PRV="-wp-prover tip" ;;
        "-s")
            PRV="-wp-prover script,alt-ergo" ;;
        *)
            OPT="${OPT} $1"
            ;;
    esac
    shift
done

${CMD} \
    -journal-disable \
    -pp-annot \
    -session session \
    -wp-extensional \
    -wp-init-const \
    -wp ${OPT} ${PRV}
