#cloudrm-sim

Cloud resource management simulator

## Dependencies

- [*R*](https://cran.r-project.org/) 3.2.x or higher.

## Setup environment

After having *R* installed, you need to download the input data, create the directory structure
and install *R* packages. In order to do that, go the root directory and run this script:

```
./download_input_and_setup.sh
```

## Admission control simulation 

To execute the admission control simulation, run this command from the project root directory:

``` 
./src/admission_control_sim.R [--] [--help] [--consider-mem] [--opts opts] [--cpu-capacity-factor factor] \
                            [--mem-capacity-factor factor] [--cpu-load-factor factor] [--mem-load-factor factor] \
                            [--slo-scenario scenario] [--output-file-prefix prefix] method
```

Input parameters for admission control simulation:
```
mandatory arguments:

  method                    Name of the admission control method.
                            Options: <greedy-norejection, greedy-quota, forecast-mean-quota, forecast-ets-quota>

flags:

  -h, --help                          Show this help message and exit

  -cmem, --consider-mem               Flag that defines if memory is considered in admission control decisions.
                                      [default: FALSE]

optional arguments:

  -x, --opts OPTS                     RDS file containing argument values
  
  -ccf, --cpu-capacity-factor FACTOR  Decimal factor applied to the original cloud CPU capacity.
                                      A factor = 1 simulates the cloud with the same CPU capacity found in the
                                      traces. [default: 1]
  
  -mcf, --mem-capacity-factor FACTOR  Decimal factor applied to the original cloud memory capacity.
                                      A factor = 1 simulates the cloud with the same memory capacity found in
                                      the traces. [default: 1]
  
  -clf, --cpu-load-factor FACTOR      Decimal factor applied to the original cloud CPU load.
                                      A factor = 1 simulates the cloud with the same CPU load
                                      (requested resources) found in the traces. [default: 1]
  
  -mlf, --mem-load-factor FACTOR      Decimal factor applied to the original cloud Memory load.
                                      A factor = 1 simulates the cloud with the same Memory load
                                      (requested resources)found in the traces. [default: 1]
  
  -s, --slo-scenario SCENARIO         Integer that identifies the availability SLO scenario. Possible values:
                                      1 (medium); 2 (very low); 3 (low); 4 (high); 5 (very high). [default: 1]
  
  -o, --output-file-prefix PREFIX     Prefix for the CSV output file names resulted from simulations.
                                      [default: "res"]
```

Sample execution of a base scenario:

```
./src/admission_control_sim.R forecast-ets-quota --cpu-capacity-factor 1 --mem-capacity-factor 1 --cpu-load-factor 1 \
                            --mem-load-factor 1 --slo-scenario 1 --consider-mem
```

For details, see paper:
[Prediction-Based Admission Control for IaaS Clouds with Multiple Service Classes](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=7396141). CloudCom'2015.

