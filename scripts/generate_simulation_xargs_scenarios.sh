#############
# Generates command line arguments to run simulation scenarios in parallel with xargs.
# You can save the results in a file to feed the xargs execution.
# Sample xargs usage with 5 processes running in parallel:
#    xargs -n 1 -P 3 -I {} -a args.txt ./src/admission_control_sim.R {}
#
# You can change arguments you want to vary. For example, if you want to vary CPU capacity
# from 0.7 to 1.3 by 0.1, you need to change this variable to:
# CPU_CAPACITY_FACTOR=`seq 0.7 0.1 1.3`
#
# You can also vary multiple parameters; in that case it would have scenarios for all combinations.
#############

METHODS="forecast-ets-quota"
CPU_CAPACITY_FACTOR="1"
CPU_LOAD_FACTOR="1"
MEM_CAPACITY_FACTOR="1"
MEM_LOAD_FACTOR="1"
SLO_SCENARIO="1"
CONSIDER_MEM="-cmem"

for m in $METHODS
do
    for ccf in $CPU_CAPACITY_FACTOR
    do
        for clf in $CPU_LOAD_FACTOR
        do
            for mcf in $MEM_CAPACITY_FACTOR
            do
                for mlf in $MEM_LOAD_FACTOR
                do
                    for s in $SLO_SCENARIO
                    do
                        echo $m -ccf $ccf -clf $clf -mcf $mcf -mlf $mlf -s $s $CONSIDER_MEM
                    done
                done
            done
        done
    done
done

