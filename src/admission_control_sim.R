#!/usr/bin/env Rscript

############################################################################################
# This script implements a simulator for the resource management of IaaS cloud computing
# providers. The focus is the evaluation of different admission control strategies.
# Other cloud resource management phases (e.g. VM placement) are simplified.
#
# More details can be found in the paper:
# Prediction-Based Admission Control for IaaS Clouds with Multiple Service.
# Marcus Carvalho, Daniel Menasce, Francisco Brasileiro.
# IEEE International Conference on Cloud Computing Technology and Science (CloudCom), 2015.
############################################################################################

# Before running the script, you need to have install the following R libraries:
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(argparser))

source("src/admission_control_utils.R")

base.dir <- "."
setwd(base.dir)
input.dir <- paste(base.dir, "/data/", sep="")
output.dir <- paste(base.dir, "/output/", sep="")
dir.create(output.dir, showWarnings = FALSE)

# Implements an aggressive backfill scheduling strategy. If highest priority VMs in the scheduling
# queue do not fit in the available capacity, other lower priority VMs that fit may skip them
# and get scheduled until no VM in the queue fits the remaining resource capacity.
AgressiveBackfillScheduling <- function(demand, cpuCapacity, memCapacity=Inf) {
  allocated.id <- vector()
  demand <- filter(demand, cpuReq <= cpuCapacity, memReq <= memCapacity)
  while (cpuCapacity > 0 && memCapacity > 0 && nrow(demand) > 0) {
    cumCpuAlloc <- cumsum(demand$cpuReq)
    cumMemAlloc <- cumsum(demand$memReq)
    newAllocs.id <- demand[which(cumCpuAlloc <= cpuCapacity & cumMemAlloc <= memCapacity), ]$i
    n.admitted <- length(newAllocs.id)
    if (n.admitted > 0) {
      allocated.id <- c(allocated.id, newAllocs.id)
      cpuCapacity <- cpuCapacity - cumCpuAlloc[n.admitted]
      memCapacity <- memCapacity - cumMemAlloc[n.admitted]
    }
    demand <- filter(demand, !(id %in% newAllocs.id), cpuReq <= cpuCapacity, memReq <= memCapacity)
  }
  return(list(allocated.id=allocated.id, remainingCapacity=cpuCapacity,
              remainingMemCapacity=memCapacity))
}

# Implements a preemptive priority scheduling strategy. Each VM has a priority class associated
# to it. Highest priority VMs are scheduled first. An aggressive backfill strategy is also used.
# A lower priority VM may be preempted if a higher priority VM arrives.
PriorityScheduling <- function(t, state, demand, capacity, memCapacity) {
  allocation <- AgressiveBackfillScheduling(demand, capacity, memCapacity)
  state$allocated.id <- c(state$allocated.id, allocation$allocated.id)
  state$remainingCapacity <- allocation$remainingCapacity
  state$remainingMemCapacity <- allocation$remainingMemCapacity
  return(state)
}

# Admission control method based on quota. A new VM request is admitted iif the quota for the
# priority class associated to the VM is not exceeded.
QuotaAdmissionControl <- function(t, demand, state, cpu.quota, mem.quota=Inf) {
  currentDemand <- filter(demand, submitTime < t) %>%
                   summarise(cpu=sum(cpuReq), mem=sum(memReq))
  cpuDemand <- max(0, currentDemand$cpu)
  memDemand <- max(0, currentDemand$mem)
  
  cpuRemainingQuota <- cpu.quota - cpuDemand
  memRemainingQuota <- mem.quota - memDemand
  
  arrivals <- filter(demand, submitTime == t)
  
  admission <- AgressiveBackfillScheduling(arrivals, cpuRemainingQuota, memRemainingQuota)
  admission$rejected.id <- (filter(arrivals, !(id %in% admission$allocated.id)))$id
  return(admission)
}

# Performs VM allocations in the current cloud capacity, after applying a quota-based
# admission control to reject VMs that exceeds the quota for a priority class.
QuotaAllocation <- function(t, state, quota.fun=GreedyQuotaDefinition, mem.considered=F) {
  state$remainingCapacity <- state$totalCapacity
  state$remainingMemCapacity <- state$totalMemCapacity
  
  if (is.null(state$cpu.quota)) {
    state$cpu.quota <- list()
  }
  
  if (is.null(state$mem.quota)) {
    state$mem.quota <- list()
  }
  
  for (k in 1:N_CLASSES) {
    uc <- USER_CLASSES[k]
    avTarget <- state$slo.availability[k]
    demand.uc <- filter(state$demand, userClass == uc)
    
    # Quota definition
    state <- quota.fun(uc, state, state$remainingCapacity, avTarget,
                       mem.considered = mem.considered)
    classCpuQuota <- state$cpu.quota[[uc]]
    classMemQuota <- ifelse(mem.considered, state$mem.quota[[uc]], Inf)
    
    # Admission control
    admission <- QuotaAdmissionControl(t, demand.uc, state, classCpuQuota, classMemQuota)
    
    if (length(admission$rejected.id) > 0) {
      demand.uc <- filter(demand.uc, !(id %in% admission$rejected.id))
      state$rejected.id <- c(state$rejected.id, admission$rejected.id)
    }
    
    state <- PriorityScheduling(t, state, demand.uc, state$remainingCapacity,
                                state$remainingMemCapacity)
  }
  return(state)
}

# Defines the quota for a priority class in the next time period, based ona timeseries forecast
# method.
ForecastQuotaDefinition <- function(uc, state, capacity, avTarget, forecast.fun=meanf,
                                    error.correction=T, conf.level=.95,
                                    ts.frequency=24*12, min.train.size=12, max.train.size=30*24*12,
                                    fcast.h=12, init.safe.margin=.1, mem.considered=F) {
  if (is.null(state$capacity.sample)) {
    state$capacity.sample <- list()
  }
  
  state$capacity.sample[[uc]] <- c(state$capacity.sample[[uc]], capacity)
  
  x <- tail(state$capacity.sample[[uc]], max.train.size)
  
  ts.frequency <- ifelse(length(x) > 2 * ts.frequency, ts.frequency, 1)
  x <- ts(x, frequency=ts.frequency)
   
  if (mem.considered) {
    if (is.null(state$capacity.sample.mem)) {
      state$capacity.sample.mem <- list()
    }
    state$capacity.sample.mem[[uc]] <- c(state$capacity.sample.mem[[uc]], state$remainingMemCapacity)
    x.mem <- tail(state$capacity.sample.mem[[uc]], max.train.size)
    x.mem <- ts(x.mem, frequency=ts.frequency)
  } else {
    x.mem <- NA
  }
  
  level <- 0
  x.n <- length(x)
  sd.q <- .95
  if (x.n >= min.train.size) {
    estimatedCapacity <- tryCatch(min(forecast.fun(x, fcast.h, level=level)))
    estimatedMemCapacity <- ifelse(!mem.considered, 0,
                                   tryCatch(min(forecast.fun(x.mem, fcast.h, level=level))))

    if (error.correction && is.finite(avTarget) && avTarget > 0 && nrow(state$stats) > 0) {
      estimatedCapacity <- estimatedCapacity - sd(x) * qt(sd.q, x.n - 1) * sqrt(1 + 1/x.n)
      estimatedMemCapacity <- ifelse(!mem.considered, 0,
                                     estimatedMemCapacity - sd(x.mem) * qt(sd.q, x.n - 1) * sqrt(1 + 1/x.n))
    }
  } else {
    estimatedCapacity <- capacity * avTarget
    estimatedMemCapacity <- ifelse(!mem.considered, 0, state$remainingMemCapacity * avTarget)
  }
  
  state$cpu.quota[[uc]] <- max(0, ifelse(avTarget > 0, estimatedCapacity / avTarget, Inf))
  state$mem.quota[[uc]] <- max(0, ifelse(avTarget > 0, estimatedMemCapacity / avTarget, Inf))
  
  return(state)
}

# Forecast method based on a conservative mean calculation (smooth mean). It calculates
# the mean values for different time periods (e.g., hour, day, week) and uses the lowest one
# as the prediction value.
SmoothMeanForecastFunction <- function(x, h, level, train.sizes=c(24*12, 12, 7),
                                       frequencies=c(1, 1, 24*12)) {
  quota <- mean(x)
  for (i in 1:length(train.sizes)) {
    by <- ifelse(length(frequencies) >= i, frequencies[i], frequencies[1])
    train.size <- ifelse(length(train.sizes) >= i, train.sizes[i], train.sizes[1])    
    train.x <- tail(x[rev(seq(length(x), 1, -by))], train.size)
    if (length(train.x) < train.size) {
      next
    }
    quota.i <- meanf(train.x, h)$mean
    if (i == 1 || quota.i < quota) {
      quota <- quota.i
    }
  }
  return(quota)
}

# Method to allocate VMs based on the conservative-mean forecast quota admission control.
MeanForecastQuotaAllocation <- function(t, state, name="forecast-mean-quota",
                                        interval.size=300000000, mem.considered=F) {
  intervals.perhour <- 3600000000 / interval.size
  intervals.perday <- 24 * intervals.perhour
  days.perweek <- 7
  
  forecast.f <- function(x, h, level) {
    SmoothMeanForecastFunction(x, h, level,
                               train.sizes=c(intervals.perhour, intervals.perday, days.perweek),
                               frequencies=c(1, 1, intervals.perday))
  }
 
  quota.f <- function(...) {
    ForecastQuotaDefinition(..., forecast.fun=forecast.f, ts.frequency=intervals.perday,
                            min.train.size=intervals.perhour, max.train.size=30*intervals.perday,
                            fcast.h=intervals.perhour)
  }
  state <- QuotaAllocation(t, state, quota.f)
  state$method <- name
  return(state)
}

# Method to allocate VMs based on the Exponential Smoothing forecast quota admission control.
ETSForecastQuotaAllocation <- function(t, state, name="forecast-ets-quota",
                                       interval.size=300000000, mem.considered=F) {
  intervals.perhour <- 3600000000 / interval.size
  intervals.perday <- 24 * intervals.perhour
  
  forecast.f <- function(x, h, level) forecast(x, h=h, level=level)$mean
  quota.f <- function(...) {
    ForecastQuotaDefinition(..., forecast.fun=forecast.f, ts.frequency=intervals.perday, 
                            min.train.size=intervals.perhour, max.train.size=30*intervals.perday,
                            fcast.h=intervals.perhour)
  }
  state <- QuotaAllocation(t, state, quota.f, mem.considered=mem.considered)
  state$method <- name
  return(state)
}

# Greedy method to define the quota for a priority class. It considers only the current
# available capacity value for a class to define the next quota.
GreedyQuotaDefinition <- function(uc, state, capacity, avTarget, mem.considered=F) {
  state$cpu.quota[[uc]] <- ifelse(avTarget != 0, capacity / avTarget, Inf)
  return(state)
}

# Method to allocate VMs based on the Greedy-quota admission control.
GreedyQuotaAllocation <- function(t, state, name="greedy-quota", mem.considered=F) {
  state <- QuotaAllocation(t, state, quota.fun=GreedyQuotaDefinition)
  state$method = name
  return(state)
}

# Method to allocate VMs without using admission control (i.e., with no VM rejections)
GreedyNoRejectionAllocation <- function(t, state, name="greedy-noreject", mem.considered=F) {
  state <- PriorityScheduling(t, state, state$demand, state$totalCapacity, state$totalMemCapacity)
  state$method <- name
  return(state)
}

# Greedy admission control method that rejects all new VM requests that do not fit in the
# current cloud capacity.
GreedyRejectionAllocation <- function(t, state, name="greedy-reject", mem.considered=F) {
  state <- PriorityScheduling(t, state, state$demand, state$totalCapacity, state$totalMemCapacity)
  state$rejected.id <- (filter(state$demand, submitTime == t, !(id %in% state$allocated.id)))$id
  state$method <- name
  return(state)
}

# Calculates VM availability statistics for the VM departures (i.e., finished)
CalculateVmAvailability <- function(departures, time, out.file) {
  vm.availability <- with(departures,
                          data_frame(userClass, cpuReq, memReq, runtime, 
                                     elapsedTime=(time-submitTime), 
                                     availability=runtime/elapsedTime))
  return(vm.availability)
}

# Update the current cloud demand with the new VM request arrivals in a time window,
# update the runtime of the VMs allocated in the previous time window and update the
# VM departures by filtering the VMs that completed their service times.
UpdateDemand <- function(t, tasks, state, bundle=T, interval.size=300000000, cpu.load.factor=1,
                         mem.load.factor=1) {
  arrivals <- tasks %>%
              filter(between(submitTime, (t-1)*interval.size + 1, t*interval.size),
                     runtime > 0, cpuReq > 0) %>%
              select(userClass, priority, schedulingClass, submitTime, runtime, endTime, cpuReq,
                     memReq) %>%
              collect(n = Inf) %>%
              transmute(userClass = factor(DefineUserClass(priority, schedulingClass),
                                           levels=USER_CLASSES),
                        submitTime = DefineTimeIntervals(submitTime, interval.size),
                        serviceDemand = ifelse(endTime != -1,
                                               DefineTimeIntervals(runtime, interval.size), Inf),
                        runtime = 0,
                        cpuReq = DefineVmSize(cpuReq, bundle, cpu.load.factor),
                        memReq = memReq * mem.load.factor,
                        id = state$ntasks + row_number())
  
  state$ntasks <- max(state$ntasks, arrivals$id)
  
  # Update runtime of allocated VMs
  
  if (nrow(state$demand) > 0) {
    state$demand <- state$demand %>%
                    mutate(runtime = runtime + (id %in% state$allocated.id))
  }
  
  state$demand <- bind_rows(state$demand, arrivals)
  
  departures <- state$demand %>%
                filter(runtime >= serviceDemand)
  
  # Remove rejected and departed VMs from active demand
  if (nrow(state$demand) > 0) {
    state$demand <- state$demand %>%
                    filter(!(id %in% c(state$rejected.id, departures$id))) %>%
                    arrange(userClass, submitTime)
  }
  state$vm.availability <- CalculateVmAvailability(departures, t)
  
  state$allocated.id <- vector()
  state$rejected.id <- vector()
  
  return(state)
}

# Calculates general statistics for VMs for the current time window.
CalculateAllocationStats <- function(t, state, max.time) {
  state$stats.t <- data.frame()
  allocatedCpu <- 0
  allocatedMem <- 0
  for(i in 1:length(USER_CLASSES)) {
  #stats <- foreach(i = 1:length(USER_CLASSES), .combine=rbind) %do% {
    uc <- USER_CLASSES[i]
    demand.uc <- filter(state$demand, userClass == uc)
    allocated.uc <- filter(demand.uc, id %in% state$allocated.id)
    rejected.uc <- filter(demand.uc, id %in% state$rejected.id)
    arrivals.uc <- filter(demand.uc, submitTime == t)
    cpu.quota <- ifelse(!is.null(state$cpu.quota), state$cpu.quota[[uc]], NA)
    mem.quota <- ifelse(!is.null(state$mem.quota), state$mem.quota[[uc]], NA)
    remainingCapacity <- state$totalCapacity - allocatedCpu
    remainingMemCapacity <- state$totalMemCapacity - allocatedMem
    allocatedCpu <- allocatedCpu + sum(allocated.uc$cpuReq)
    allocatedMem <- allocatedMem + sum(allocated.uc$memReq)
    
    if (t == max.time) {
      vm.availability <- filter(demand.uc, submitTime < t) %>%
                         CalculateVmAvailability(t)
      state$vm.availability <- rbind(state$vm.availability, vm.availability)
    }
    
    slo.av <- state$slo.availability[i]
    vm.availability.stats <- filter(state$vm.availability, userClass == uc) %>%
                             summarise(departures.n=n(),
                                       departures.cpu=sum(cpuReq),
                                       departures.mem=sum(memReq),
                                       vm.availability.mean=mean(availability),
                                       vm.slo.violated.n=sum(availability < slo.av),
                                       vm.slo.violated.cputime=sum(cpuReq*elapsedTime*(availability < slo.av)),
                                       vm.slo.violated.memtime=sum(memReq*elapsedTime*(availability < slo.av)))
    
    state$stats.t <- rbind(state$stats.t,
                           bind_cols(data_frame(time=t, userClass=uc,
                                                capacity.rem.cpu=remainingCapacity,
                                                capacity.rem.mem=remainingMemCapacity,
                                                demand.n=nrow(demand.uc),
                                                demand.cpu=sum(demand.uc$cpuReq),
                                                demand.mem=sum(demand.uc$memReq),
                                                allocated.n=nrow(allocated.uc),
                                                allocated.cpu=sum(allocated.uc$cpuReq),
                                                allocated.mem=sum(allocated.uc$memReq),
                                                rejected.n=nrow(rejected.uc),
                                                rejected.cpu=sum(rejected.uc$cpuReq),
                                                rejected.mem=sum(rejected.uc$memReq),
                                                arrivals.n=nrow(arrivals.uc), 
                                                arrivals.cpu=sum(arrivals.uc$cpuReq), 
                                                arrivals.mem=sum(arrivals.uc$memReq),
                                                cpu.quota, mem.quota),
                                      vm.availability.stats))
  }
  return(state)
}

# Execute the simulation by performing admission control and VM allocation strategies over time.
ExecuteResourceAllocation <- function(tasks, capacities, max.time, allocation.fun, out.file,
                                      bundle=T, cpu.capacity.factor=1, seed, interval.size=300000000,
                                      slo.scenario=1, cpu.load.factor=1, write.vm.summary=F,
				                              mem.capacity.factor=1, mem.considered=F, mem.load.factor=1) {
  max.time <- min(max.time, max(capacities$interval))
  state <- list(method="", demand=data.frame(), stats=data.frame(), ntasks=0,
                allocated.id=vector(), rejected.id=vector(),
                slo.availability=AVAILABILITY_SLOS_SCENARIOS[[slo.scenario]])
  firstVmFile <- TRUE
  for (t in 0:max.time) {
    state <- UpdateDemand(t, tasks, state, bundle, interval.size, cpu.load.factor, mem.load.factor)
    state$totalCapacity <- (filter(capacities, interval == t))$cpu
    state$totalMemCapacity <- (filter(capacities, interval == t))$mem
    
    state <- allocation.fun(t, state, mem.considered=mem.considered)
    
    state <- CalculateAllocationStats(t, state, max.time)
    stats <- data.frame(cpu.capacity.factor, mem.capacity.factor, slo.scenario, cpu.load.factor,
                        mem.load.factor, slo.availability=state$slo.availability, method=state$method,
                        state$stats.t)
    state$stats <- rbind(state$stats, stats)
    
    vm.av.file <- paste(out.file, "vm-avail.csv", sep="_")
    
    if (!is.null(out.file)) {
      WriteResults(t, stats, out.file, first = t == 0)
      if (nrow(state$vm.availability) > 0) {
        vm.availability.df <- data.frame(cpu.capacity.factor, mem.capacity.factor,
                                         method=state$method, slo.scenario, cpu.load.factor,
                                         mem.load.factor, state$vm.availability)
        WriteResults(t, vm.availability.df, vm.av.file, first = firstVmFile)
        firstVmFile <- FALSE
      }
    }
  }
 
  rm(state)
  gc()
 
  if (write.vm.summary) {
    vm.av.summary <- SummarizeVmAvailability(vm.av.file)
    vm.av.summary.file <- paste(out.file, "vm-avail-summary.csv", sep="_")
    write.table(vm.av.summary, vm.av.summary.file, col.names = T, quote = F, row.names = F,
                sep = ",")
  }
  
  return(vm.av.summary)
}

# Writes the output results in a file for each time window.
WriteResults <- function(t, stats, out.file, first) {
  write.table(stats, out.file, append = !first, quote = F, row.names = F, col.names = first,
              sep = ",")
}

# Main function used to run the simulations. The simulator parameters are passed as arguments.
Main <- function(argv=NULL) {

  # Creating a object type "option" to store the options of the simulation and setting default values
  
  opts <- arg_parser('Options for admission control simulation')

  opts <- add_argument(opts, "method",
                       help = "Name of the admission control method. Options: \
                              (greedy-norejection, greedy-quota, forecast-mean-quota,
                              forecast-ets-quota)")

  opts <- add_argument(opts, "--cpu-capacity-factor",
                       help = "Decimal factor applied to the original \
                               cloud CPU capacity. A factor = 1 simulates the cloud with the same
                               CPU capacity found in the traces", default = 1,
                       short = "-ccf")
                                 
  opts <- add_argument(opts, "--mem-capacity-factor",
                       help = "Decimal factor applied to the original \
  	                           cloud memory capacity. A factor = 1 simulates the cloud with the \
                               same memory capacity found in the traces.", default = 1, 
                       short = "-mcf")

  opts <- add_argument(opts, "--cpu-load-factor",
                       help = "Decimal factor applied to the original \
                               cloud CPU load. A factor = 1 simulates the cloud with the same CPU \
                               load (requested resources) found in the traces.", default = 1,
                       short = "-clf")
                          
  opts <- add_argument(opts, "--mem-load-factor",
                       help = "Decimal factor applied to the original cloud \ 
                               Memory load. A factor = 1 simulates the cloud with the same Memory \
                               load (requested resources) found in the traces.", default = 1,
                       short = "-mlf")

  opts <- add_argument(opts, "--slo-scenario",
                       help = "Integer that identifies the \
                               availability SLO scenario. Possible values: 1 (medium); 2 \
                               (very low); 3 (low); 4 (high); 5 (very high)", default = 1,
                       short = "-s")
  
  opts <- add_argument(opts, "--consider-mem",
                       help = "Flag that defines if memory is considered in admission \
                               control decisions.", flag = TRUE, short = "-cmem")
                       
  opts <- add_argument(opts, "--output-file-prefix",
                       help = "Prefix for the CSV file name output file with simulation results",
                       default = "res", "-o")
  
  opts <- add_argument(opts, "--summarize-vm-availability",
                       help = "Prefix for the CSV file name output file with simulation results",
                       flag = TRUE, short = "-sva")

  # assign the options to variable
  params <- parse_args(opts, argv)
  
  # Default arguments:

  # base directory for the scripts, input and output files
  base.dir <- "."
  # the VM sizes are bundled in discrete values ("bundle") or can be continuous ("nobundle")
  bundle <- "nobundle"
  # seed for random number generation
  seed <- 0
  # the maximum time to consider from the trace (use Inf to use the whole trace as input)
  max.time <- Inf
  # duration of a time window (in micro-seconds). Default: 300000000 microsec = 5 minutes)
  interval.size <- 300000000
  
  # name that identifies the admission control method
  method.name <- params$method
  
  output.file <- with(params,
                      paste(output.dir, output_file_prefix, "_", method.name, "_ccf-",
                            cpu_capacity_factor, "_clf-", cpu_load_factor, "_mcf-",
                            mem_capacity_factor, "_mlf-", mem_load_factor, "_slo-", slo_scenario,
                            "_cmem-", consider_mem, "_ac.csv", sep=""))

  # Expected SQLite database input file, containing the cloud demand over time
  db.file <- paste(input.dir, "gtrace_data.sqlite3", sep="/")
  # Expected text input file, containing the cloud capacity over time
  capacity.file <- paste(input.dir, "gtrace_total_capacity.txt", sep="/")
  
  # Defines the appropriate admission control function 
  if (method.name == "greedy-norejection") {
    method.f <- GreedyNoRejectionAllocation
  } else if (method.name == "greedy-rejection") {
    method.f <- GreedyRejectionAllocation
  } else if (method.name == "greedy-quota") {
    method.f <- GreedyQuotaAllocation
  } else if (method.name == "forecast-mean-quota") {
    method.f <- function(...) MeanForecastQuotaAllocation(..., interval.size=interval.size)
  } else if (method.name == "forecast-arima-quota") {
    method.f <- ArimaForecastQuotaAllocation
  } else if (method.name == "forecast-ets-quota") {
    method.f <- function(...) ETSForecastQuotaAllocation(..., interval.size=interval.size)
  } else {
    stop("Unknow method: ", method.name)
  }

  # Load cloud demand over time from database file
  tasks <- LoadTaskEvents(db.file, from.sqlite=T)
  
  # Load cloud capacity over time from text file
  capacities <- with(params,
                     CalculateTotalCapacityPerInterval(LoadMachineEvents(db.file), cpu_capacity_factor,
                                                       interval.size, mem_capacity_factor))
  
  # Discretize VM sizes if "bundled" option is chosen
  if (bundle == "bundle") {
    capacities$cpu <- capacities$cpu * MAX_CPU
  }
 
  # Simulates admission control and resource allocation over time
  print("Starting simulation...")
  state <- with(params,
                ExecuteResourceAllocation(tasks, capacities, max.time, method.f, output.file,
                                          bundle = bundle == "bundle", cpu_capacity_factor, seed,
                                          interval.size, slo_scenario, cpu_load_factor,
                                          summarize_vm_availability, mem_capacity_factor,
                                          consider_mem, mem_load_factor))
  
  return(state)
}

# Read command-line arguments
argv <- commandArgs(trailingOnly = TRUE)

# If any input argument is found, execute the main function
Main(argv)
