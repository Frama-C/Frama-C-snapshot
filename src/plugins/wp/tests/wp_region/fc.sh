# Visualize output of WP/Region tests

OPT=
CMD=fc
TEST="<none>"
NAME="none"
OPEN="none"
DEFAULT="-wp-msg-key dot,chunk,roots,garbled"

if type open &> /dev/null ; then
    OPEN=open
elif type xpdf &> /dev/null ; then
    OPEN=xpdf
elif type evince &> /dev/null ; then
    OPEN=evince
fi

while [ "$1" != "" ];
do
    case $1 in
        "-h"|"--help")
            echo "fc.sh [options...] <test.[ic]>" ;
            echo "  -h,--help     help and exit" ;
            echo "  -D,--delete   clean output directory and exit" ;
            echo "  -g,--gui      run in Frama-C Gui" ;
            echo "  -r,--region   visualize region graph" ;
            echo "  -u,--update   commit region graph in oracle" ;
            echo "  -t,--test     run ptests.opt on test file (or all files)" ;
            echo "  -q,--qualif   run ptests.opt with test-config qualif" ;
            echo "  --open <cmd>  opens pdf with '<cmd>'" ;
            echo "  -k <keys>     set message keys" ;
            echo "  *             any other Frama-C options" ;
            exit 0 ;
            ;;
        *.i) TEST=${1}; NAME=${TEST/.i/} ;;
        *.c) TEST=${1}; NAME=${TEST/.c/} ;;
        "-D"|"--delete") CMD=delete ;;
        "-u"|"--update") CMD=update ;;
        "-t"|"--test") CMD=test ;;
        "-q"|"--qualif") CMD=qualif ;;
        "-g"|"--gui") CMD=gui ;;
        "-r"|"--region") CMD=region ; OPT="${OPT} -wp-msg-key pdf" ;;
        "--open") shift ; CMD=region ; OPEN=${1} ;;
        "-k") shift ; CMD=region ; DEFAULT="" ; OPT="${OPT} -wp-msg-key $1" ;;
        *)
            OPT="${OPT} $1"
            ;;
    esac
    shift
done

BIN=../../../../../bin
WP="-wp-region -wp-model Region -wp-fct job -wp-out result/${NAME}"

case $CMD in
    "fc"|"region")
        echo "Running frama-c $TEST"
        $BIN/frama-c $WP $TEST $DEFAULT $OPT
        PDF="./result/${NAME}/region/job.pdf"
        if [ $CMD = region ] && [ -f $PDF ]
        then
            if [ $OPEN != none ] ; then
                echo "Source File:"
                cat $TEST
                $OPEN $PDF
            else
                echo "No command found for opening $PDF"
                echo "Use --open <cmd> option"
            fi
        fi
        ;;
    "gui")
        echo "Running frama-c $TEST (Gui)"
        $BIN/frama-c-gui $WP $TEST $OPT
        ;;
    "test")
        if [ $TEST == "<none>" ]
        then
            echo "Testing directory..."
            ( cd ../.. ; ../../../bin/ptests.opt tests/wp_region > /dev/null )
            for test in *.i
            do
                name=${test/.i/}
                oracle=oracle/$name/region/job.dot
                result=result/$name/region/job.dot
                if [ -f $oracle ] && !( diff -q $oracle $result > /dev/null )
                then
                    echo "Diff: ./fc.sh $test -r"
                fi
            done
        else
            echo "Testing $TEST$OPT"
            ( cd ../.. ; ../../../bin/ptests.opt tests/wp_region/$TEST $OPT )
        fi
        ;;
    "qualif")
        echo "Testing $TEST -config qualif$OPT"
        ( cd ../.. ; ../../../bin/ptests.opt tests/wp_region/$TEST -config qualif $OPT )
        ;;
    "update")
        echo "Update './oracle/$NAME/region/job.dot"
        mkdir -p ./oracle/$NAME/region
        cp -f ./result/$NAME/region/job.dot ./oracle/$NAME/region/
        ;;
    "delete")
        echo "Cleaning './result/$NAME'"
        rm -fr result/$NAME/*
        ;;
esac
