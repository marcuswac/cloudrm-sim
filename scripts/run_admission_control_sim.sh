#!/bin/bash
#
# Input arguments:
# - method: name of the admission control method (greedy-norejection, greedy-quota,
#   forecast-mean-quota, forecast-ets-quota)
# - cpu_capacity_factor: decimal factor applied to the original cloud CPU capacity. A factor = 1 simulates
#   the cloud with the same CPU capacity found in the traces.
# - cpu_load_factor: decimal factor applied to the original cloud CPU load. A factor = 1 simulates
#   the cloud with the same CPU load (requested resources) found in the traces.
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
# - mem_load_factor: decimal factor applied to the original cloud Memory load. A factor = 1 simulates
#   the cloud with the same Memory load (requested resources) found in the traces.
#
# Sample run:
# scripts/run_admission_control_sim.sh "forecast-ets-quota" 1 1 1 1 "yes" 1
#
# For details, see paper:
# Prediction-Based Admission Control for IaaS Clouds with Multiple Service. CloudCom'2015.
# 

ARGS=$*

SCRIPT_FILE="src/probabilistic_admission_control.R"

Rscript $SCRIPT_FILE $ARGS  
