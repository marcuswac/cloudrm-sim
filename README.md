#cloudrm-sim

Cloud resource management simulator

## Dependencies

- [*R*](ihttps://cran.r-project.org/) 3.2.x or higher.

## Setup environment

After having *R* installed, you need to download the input data, create the directory structure
and install *R* packages. In order to do that, go the root directory and run this script:

```
./download_input_and_setup.sh
```

## Admission control simulation 

To execute the admission control simulation, run this command from the root directiory:

``` 
scripts/run_admission_control_sim.sh <method> <cpu_capacity_factor> <cpu_load_factor> <slo_scenario> <mem_capacity_factor> <mem_considered>"
```

Input parameters:
 - method: name of the admission control method (greedy-norejection, greedy-quota,
   forecast-mean-quota, forecast-ets-quota)

 - cpu_capacity_factor: decimal factor applied to the original cloud CPU capacity. A factor = 1 simulates
   the cloud with the same CPU capacity found in the traces.

 - cpu_load_factor: decimal factor applied to the original cloud demand. A factor = 1 simulates
   the cloud with the same load demand (requested resources) found in the traces.

 - slo_scenario: integer that identifies the availability SLO scenario. Possible values:
   - 1: medium
   - 2: very low
   - 3: low
   - 4: high
   - 5: very high

 - mem_capacity_factor: decimal factor applied to the original cloud memory capacity. A factor = 1 simulates
   the cloud with the same memory capacity found in the traces.

 - mem_considered: string ("yes" or "no") defining if memory is considered in admission control
   decisions.

Sample execution of a base scenario:

```
scripts/run_admission_control_sim.sh "forecast-ets-quota" 1 1 1 1 "yes"
```


For details, see paper:
[Prediction-Based Admission Control for IaaS Clouds with Multiple Service Classes](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=7396141). CloudCom'2015.

