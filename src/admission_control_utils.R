# This file contains utils functions that are common for different scripts.
library(dplyr)
library(readr)
library(foreach)

USER_CLASSES <- c("prod", "batch", "free")
slo.scenario <- 1
AVAILABILITY_SLOS_SCENARIOS <- list(c(1, .9, .5), c(1, .7, .3), c(1, .5, .1), c(1, .99, .7),
                                    c(1, .999, .9))

VM_SIZES <- c(1, 2, 4, 8, 16, 32)
MAX_CPU <- 64
N_CLASSES <- length(USER_CLASSES)

DefineTimeIntervals <- function(x, interval.size=300000000, sec=FALSE) {
  if (sec) {
    interval.size <- interval.size / 1000000
  }
  intervals <- ceiling(x / interval.size)
  return(intervals)
}

DefineVmSize <- function(cpu, bundle=T, cpu.load.factor=1) {
  if (bundle) {
    vmSize <- 2^ceiling(pmax(0, log2(cpu * cpu.load.factor * MAX_CPU)))
  } else {
    vmSize <- cpu * cpu.load.factor
  }
  return(vmSize)
}

DefineUserClass <- function(priority, schedulingClass) {
  userClass <- factor(ifelse(priority <= 1, "free",
                             ifelse(priority <= 8, "batch",
                                    "prod")))
  return(userClass)
}

DefineUserClass2 <- function(priority, schedulingClass) {
  userClass <- factor(ifelse(priority <= 8, "batch",
                             ifelse(schedulingClass >= 2, "prod",
                                    "batch-prod")))
  return(userClass)
}

DefineUserClass3 <- function(priority, schedulingClass) {
  userClass <- factor(ifelse(priority <= 1, "free",
                             ifelse(priority <= 8, "batch",
                                    ifelse(schedulingClass >= 2, "prod",
                                           "batch-prod"))))
  return(userClass)
}

LoadTaskEvents <- function(file, from.sqlite=F) {
  if (from.sqlite) {
    db <- src_sqlite(file)
    tasks <- tbl(db, "tasks")
  } else{
    tasks <- read.table(file, header=F, col.names=c('submitTime', 'jid', 'tid',
                                                    'user', 'schedulingClass',
                                                    'priority', 'runtime', 
                                                    'endTime', 'cpuReq',
                                                    'memReq'))
    
    #tasks$userClass <- DefineUserClass(tasks$priority, tasks$schedulingClass)
    tasks <- tbl_df(tasks)
  }
  
  return(tasks)
}

LoadDemandCapacity <- function(file) {
  df <- read.table(file, col.names=c('eventTime', 'userClass', 'demand', 'capacity'))
  return(df)
}

LoadTotalCapacity <- function(file) {
  df <- read.table(file, col.names=c('timeWindow', 'capacity.cpu', 'capacity.mem'))
  return(df)
}

LoadMachineEvents <- function(file, from.sqlite=T) {
  if (from.sqlite) {
    db <- src_sqlite(file)
    machineEvents <- tbl(db, "machine_events") %>%
                     collect(n = Inf) %>%
                     mutate(time = as.numeric(time),
                            cpu = ifelse(is.na(as.numeric(cpu)), 0, as.numeric(cpu)),
                            memory = ifelse(is.na(as.numeric(memory)), 0, as.numeric(memory)))
  }
  return(machineEvents)
}

GenerateCapacityPerIntervalFromMachineSample <- function(machineEvents, sample.fraction=1, seed=NA,
                                                         replace=F, interval.size=300000000) {
  machine.ids <- unique(machineEvents$machineId)
  sample.size <- round(length(machine.ids) * sample.fraction)
  if (!is.na(seed)) {
    set.seed(seed)
  }
  sample.ids <- sample(machine.ids, sample.size, replace=replace)
  machineEvents <- filter(machineEvents, machineId %in% sample.ids)
  capacities <- CalculateTotalCapacityPerInterval(machineEvents, interval.size)
  return(capacities)
}

CalculateCapacityChange <- function(eventType, capacity) {
  capacityChange <- ifelse(eventType == 0, capacity,
                           ifelse(eventType == 1, -lag(capacity),
                                  capacity - lag(capacity)))
  return(capacityChange)
}

CalculateTotalCapacityPerInterval <- function(machineEvents, cpu.capacity.factor=1,
                                              interval.size=300000000, mem.capacity.factor=1) {
  machineEvents <- mutate(machineEvents, interval = ceiling(time / interval.size))
  intervals <- 0:max(machineEvents$interval)
  capacities <- machineEvents %>%
                   group_by(machineId) %>%
                   mutate(cpuChange = CalculateCapacityChange(eventType, cpu),
                          memChange = CalculateCapacityChange(eventType, memory)) %>%
                   select(machineId, interval, cpuChange, memChange) %>% 
                   bind_rows(data.frame(interval=intervals, cpuChange=0, memChange=0)) %>%
                   arrange(interval) %>%
                   ungroup() %>%
                   transmute(interval, cpu=cumsum(cpuChange), mem=cumsum(memChange)) %>%
                   group_by(interval) %>%
                   summarise(cpu=last(cpu)*cpu.capacity.factor, mem=last(mem)*mem.capacity.factor)
  return(capacities)
}

GetAvailabilitySLO <- function(userClass, slo.scenario=1) {
  avSlos <- AVAILABILITY_SLOS_SCENARIOS[[slo.scenario]]
  sapply(userClass, function(x) avSlos[which(USER_CLASSES == x)])
}

ExtractJobsInfo <- function(tasks) {
  jobs <- tasks %>%
    select(jid, userClass, schedulingClass, submitTime, endTime, runtime, cpuReq, memReq) %>%
    group_by(jid) %>%
    summarise(userClass=userClass[1], schedulingClass=schedulingClass[1],
              submitTime=min(submitTime), submitTime.max=max(submitTime), 
              endTime=max(endTime), runtime=mean(runtime), ntasks=n(),
              cpuReq=median(cpuReq), memReq=median(memReq))
  return(jobs)
}

GetRevenueRate <- function(userClass, slo.scenario=1) {
  rate <- GetAvailabilitySLO(userClass, slo.scenario)
  return(rate)
}

# penalty per cpu-interval
GetAvailabilityPenaltyRate <- function(userClass, slo.scenario=1) {
  rate <- GetAvailabilitySLO(userClass, slo.scenario)
  return(rate)
}

SummaryStatistics <- function(x) {
  data_frame(mean.x = mean(x), sd.x = sd(x), min.x = min(x), q25.x = quantile(x, .25),
             median.x = median(x),q75.x = quantile(x, .75), max.x = max(x))
}

LoadResultsFiles <- function(stats.files=list.files("output", "res22_ac_.*0.5.*", full.names=T),
                             method.levels=c("greedy-noreject", "greedy-reject", "greedy-quota",
                                             "forecast-mean-quota", "forecast-ets-quota"),
                             method.labels=c("no-adm-ctrl", "greedy-reject", "greedy-quota",
                                             "pred-cmean", "pred-ets"),
                             userClass.levels=c("prod", "batch", "free")) {
  stats <- foreach(file = stats.files, .combine=rbind) %do% {
    if (endsWith(file, "csv")) {
      df <- read.csv(file, header=T)
    } else {
      df <- read.table(file, header=T)
    }
    if (!("cpu.capacity.factor" %in% colnames(df))) {
      if ("capacity.fraction" %in% colnames(df)) {
        df$cpu.capacity.factor <- df$capacity.fraction
        df$capacity.fraction <- NULL
      } else {
        df$cpu.capacity.factor <- 1
      }
    }
    if (!("mem.capacity.factor" %in% colnames(df))) {
      if ("mem.capacity.fraction" %in% colnames(df)) {
        df$mem.capacity.factor <- df$mem.capacity.fraction
        df$mem.capacity.fraction <- NULL
      } else {
        df$mem.capacity.factor <- 1
      }
    }
    if (!("cpu.load.factor" %in% colnames(df))) {
      if ("cpureq.factor" %in% colnames(df)) {
        df$cpu.load.factor <- df$cpureq.factor
        df$cpureq.factor <- NULL
      } else {
        df$cpu.load.factor <- 1
      }
    }
    if (!("mem.load.factor" %in% colnames(df))) {
      if ("memreq.factor" %in% colnames(df)) {
        df$mem.load.factor <- df$memreq.factor
        df$memreq.factor <- NULL
      } else {
        df$mem.load.factor <- 1
      }
    }
    if (!("cpuReq" %in% colnames(df))) {
      if ("vmSize" %in% colnames(df)) {
        df$cpuReq <- df$vmSize
        df$vmSize <- NULL
      } else {
        df$cpuReq <- NA
      }
    }
    df
  }
  stats$method <- factor(stats$method, levels=method.levels, labels=method.labels)
  stats$userClass <- factor(stats$userClass, levels=userClass.levels)
  return(stats)
}

SummarizeVmAvailability <- function(files) {
  res <- data.frame()
  for (f in files) {
    gc()
    if (endsWith(f, "csv")) {
      res.df <- read.csv(f)
    } else {
      res.df <- read.table(f, header=T)
    }
    
    if (!("mem.capacity.factor" %in% colnames(res.df))) {
      res.df$mem.capacity.factor <- 1
    }
    
    res.df <- res.df %>%
      mutate(slo.availability=GetAvailabilitySLO(userClass, first(slo.scenario)),
             unavail=1-availability) %>%
      group_by(cpu.capacity.factor, mem.capacity.factor, slo.scenario, cpu.load.factor,
               mem.load.factor, method, userClass) %>%
      summarise(vm.n=n(), vm.av.mean=mean(availability), vm.av.sd=sd(availability),
                vm.av.median=median(availability), vm.av.q01=quantile(availability, .01),
                vm.av.q05=quantile(availability, .05), vm.av.q25=quantile(availability, .25),
                vm.av.q25=quantile(availability, .75), vm.av.q25=quantile(availability, .95),
                vm.av.q25=quantile(availability, .99),
                vm.av.slo.fulfill=ecdf(unavail)(1 - slo.availability[1]),
                vm.slo.fulfilled.n=sum(availability >= slo.availability[1]),
                vm.cpuruntime.fulfilled.mean=mean(cpuReq * runtime),
                vm.cpuruntime.fulfilled.mean=mean((availability >= slo.availability[1]) *
                                                    cpuReq * runtime),
                vm.cpuruntime.violated.mean=mean((availability < slo.availability[1]) *
                                                   cpuReq * runtime),
                vm.revenue.total = sum((availability >= slo.availability[1]) * runtime * cpuReq *
                                         slo.availability[1], na.rm=T),
                vm.penalty.total = sum((availability < slo.availability[1]) * elapsedTime * cpuReq *
                                         slo.availability[1], na.rm=T))
    res <- rbind(res, res.df)
  }
  return(res)
}
