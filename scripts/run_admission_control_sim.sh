#!/bin/bash
#
# Input arguments:
# - method: name of the admission control method (greedy-norejection, greedy-quota,
#   forecast-mean-quota, forecast-ets-quota)
# - cpu_capacity_factor: decimal factor applied to the original cloud CPU capacity. A factor = 1 simulates
#   the cloud with the same CPU capacity found in the traces.
# - demand_factor: decimal factor applied to the original cloud demand. A factor = 1 simulates
#   the cloud with the same demand (requested resources) found in the traces.
# - slo_scenario: integer that identifies the availability SLO scenario. Possible values:
#   - 1: medium
#   - 2: very low
#   - 3: low
#   - 4: high
#   - 5: very high
#
# - mem_capacity_factor: decimal factor applied to the original cloud memory capacity. A factor = 1 simulates
#   the cloud with the same memory capacity found in the traces.
# - mem_considered: string ("yes" or "no") defining if memory is considered in admission control
#   decisions. 
#
# Sample run:
# scripts/run_admission_control_sim.sh "forecast-ets-quota" 1 1 1 1 "yes"
#
# For details, see paper:
# Prediction-Based Admission Control for IaaS Clouds with Multiple Service. CloudCom'2015.
# 

if [ "$#" -ne 6 ]; then
    echo "Error: Illegal number of parameters."
    echo "Usage: scripts/run_simulation <method> <cpu_capacity_factor> <demand_factor> <slo_scenario> <mem_capacity_factor> <mem_considered>"
    exit 
fi

METHOD=$1
CAPACITY_FRACTION=$2
CPUREQ_FACTOR=$3
SLO_SCENARIO=$4
MEM_CAPACITY_FRACTION=$5
MEM_CONSIDERED=$6
BUNDLE="nobundle"
SEED=0 # deprecated
INTERVAL_SIZE=300000000 # 5 minutes in micro-seconds 
BASE_DIR="."
MAX_TIME="Inf"
#MAX_TIME="1000"

OUTPUT_FILE="output/res_mem_is-${INTERVAL_SIZE}_${METHOD}_cf-${CAPACITY_FRACTION}_sc-${SLO_SCENARIO}_cpuf-${CPUREQ_FACTOR}_memcf-${MEM_CAPACITY_FRACTION}_withmem-${MEM_CONSIDERED}_ac.txt"

SCRIPT_FILE="src/probabilistic_admission_control.R"

Rscript $SCRIPT_FILE $METHOD $BASE_DIR $BUNDLE $CAPACITY_FRACTION $SEED $MAX_TIME $OUTPUT_FILE $INTERVAL_SIZE $SLO_SCENARIO $CPUREQ_FACTOR $MEM_CAPACITY_FRACTION $MEM_CONSIDERED
