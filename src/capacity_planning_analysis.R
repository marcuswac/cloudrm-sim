#!/usr/bin/env Rscript

library(queueing)
library(RSQLite)

source("src/admission_control_utils.R")

second <- 1000000
minute <- second * 60
hour <- minute * 60
min5 <- 5 * minute

#####
# Formulas from paper: "A Diffusion Approximation for the G/GI/n/m Queue"
#####

# From Equations (0.2) and (3.2)
alpha.f <- function(beta, kappa=NA) {
  if (!is.finite(kappa)) {
    1 / (1 + beta * pnorm(beta) / dnorm(beta))
  } else if (beta != 0) {
    1 / (1 + beta * pnorm(beta) / (dnorm(beta) * (1 - exp(-kappa * beta))))
  } else {
    (1 + kappa^(-1) * sqrt(pi/2))^(-1)
  }
}

# From Equation (0.1) and (5.1)
beta.f <- function(n, rho) {
  sqrt(n) * (1 - rho)
}

# From Equation (5.3)
beta.f2 <- function(n, rho) {
  (sqrt(n) * (1 - rho)) / sqrt(rho)
}

# From Equation (0.3)
kappa.f <- function(m, n) {
  m/sqrt(n)
}

# From Equation (1.3) -- asymptotic peakedness
z.f <- function(ca2, cs2, p=1) {
  return((p * (ca2 + cs2)) / 2)
}

# From Equation (1.6)
z.f2 <- function(ca2, ng) {
  1 + (ca2 - 1) * ng
}

# From Equation (2.13)
z.f3 <- function(ca2, p) {
  1 + (p * (ca2 - 1)) / 2
}

# From Equation (4.12) -- peakedness approximation for cs2 >= 1, based on H2b distribution
z.f4 <- function(ca2, cs2) {
  1 + ((ca2 - 1) * (cs2 + 3)) / (4 * (cs2 + 1))
}

# From Equation (4.4)
p.f <- function(ca2, cs2, z) {
  (2 * z) / (ca2 + cs2)  
} 

# From Equation (4.11) -- approximation for cs2 >= 1 based on H2b distribution 
p.f2 <- function(cs2) {
  (1 + sqrt((cs2 - 1)/cs2 + 1)) / 2 
}

# From Equation (3.7)
v.f <- function(ca2, cs2, w=1) {
  (ca2 + w * cs2 + 1 - w)/2
}

# From Equation (5.7) -- approximation for the mean number of customers waiting
eqn.waiting.f <- function(beta, z, rho, v) {
  alpha <- alpha.f(beta/sqrt(z))
  (alpha * rho * v)/(1 - rho)
}

# From Equation (5.8) -- approximation for the mean number of customers in the system
eqn.f <- function(beta, z, rho, v, n) {
  alpha <- alpha.f(beta/sqrt(z))
  n * rho + (alpha * rho * v)/(1 - rho)
}

# From Equations (7.2) and (7.8)
fqk.f <- function(kappa, p, beta, v, ca2, cs2, ng=NA) {
  #z <- z.f3(ca2, p)
  if (is.na(ng)) {
    z <- z.f(ca2, cs2, p)
  } else {
    z <- z.f2(ca2, ng)
  }
  
  alpha <- alpha.f(beta/sqrt(z), p * kappa / sqrt(z))
  if (!is.finite(kappa)) {
    return(NaN)
  } else if (kappa != 0) {
    (alpha * beta * exp(-kappa * beta / v)) / (v * (1 - exp(-kappa * beta / v)))
  } else {
    dnorm(beta / sqrt(z)) / (sqrt(z) * pnorm(beta/sqrt(z)))
  }
}

# From Equation (3.14)
fqx.f <- function(x, p, beta, z, kappa) {
  alpha <- alpha.f(beta/sqrt(z), p * kappa / sqrt(z))
  if (x < 0) {
    (1 - alpha) * dnorm((x + beta) / sqrt(z)) / (sqrt(z) * pnorm(beta / sqrt(z)))      
  } else if (beta != 0) {
    alpha.f(p * beta / z) * exp(-p * x * beta / z) * (1 - exp(-p * kappa * beta / z))
  } else {
    alpha / kappa
  }
}

# From Equations (7.3) and (7.9) -- infinitesimal variance of Q evaluated at kappa
sigma2q.f <- function(kappa, mu, v, p, z) {
  if (!is.finite(kappa)) {
    return(NaN)
  } else if (kappa != 0) {
    2 * mu * v
  } else {
    2 * p * mu * z
  }
}

# From Equations (7.1) and (7.4)
rqn.f <- function(kappa, p, beta, v, z, mu, n, ca2, cs2) {
  fqk <- fqk.f(kappa, p, beta, v, ca2, cs2)
  sigma2q <- sigma2q.f(kappa, mu, v, p, z)
  rq <- (fqk * sigma2q) / 2
  sqrt(n) * rq
}

# From Equation (3.4) -- delay probability
pw.f <- function(beta, z, kappa, p=1) {
  alpha.f(beta / sqrt(z), p * kappa / sqrt(z))
}

# From Equation (7.5) and (7.10) -- blocking probability
pbl.f <- function(kappa, v, rho, n, z, beta, p, ca2, cs2, ng=NA) {
  #if (beta != 0) {
    fqk <- fqk.f(kappa, p, beta, v, ca2, cs2, ng)
  #} else {
  #  fqk <- fqx.f(kappa, p, beta, z, kappa)
  #}
  if (!is.finite(kappa)) {
    return(NaN)
  } else if (kappa != 0) {
    (fqk * v)/(rho * sqrt(n))
  } else {
    (p * sqrt(z) * dnorm(beta/sqrt(z))) / (rho * sqrt(n) * pnorm(beta/sqrt(z)))
  }
}

scv.f <- function(x.mean, x.var) {
  x.var/(x.mean^2)
}

rho.f <- function(lambda, mu, nservers) {
  lambda / (mu * nservers)
}

CalculateApproximationStats <- function(m, ca2, cs2, lambda=NA, mu=NA, nservers=NA, rho=NA, ng=NA, p=NA, w=1) {
  if (nservers <= 0) {
    return(NA)
  }
  
  if (is.na(nservers)) {
    nservers <- lambda/(rho*mu)
  } else if (is.na(mu)) {
    mu <- lambda / (rho * nservers)
  } else if (is.na(rho)) {
    rho <- lambda / (mu * nservers)
  } else if (is.na(lambda)) {
    lambda <- rho * mu * nservers
  }
  
  if (!is.na(ng)) {
    z <- z.f2(ca2, ng)
  } else {
    z <- z.f(ca2, cs2)
  }
  
  if (!is.na(p)) {
    v <- z / p
  } else {
    v <- v.f(ca2, cs2, w)
    p <- p.f(ca2, cs2, z)
  }
  
  kappa <- kappa.f(m, nservers)
  beta <- beta.f(nservers, rho)
  beta2 <- beta.f2(nservers, rho)
  alpha <- alpha.f(beta/sqrt(z))
  
  pw <- pw.f(beta, z, kappa, p)
  pbl <- pbl.f(kappa, v, rho, nservers, z, beta, p, ca2, cs2, ng)
  pbl2 <- pbl.f(kappa, v, rho, nservers, z, beta2, p, ca2, cs2, ng)
  
  pw.inf <- alpha.f(beta / sqrt(z))
  
  eqn.inf <- eqn.f(beta, z, rho, v, nservers)
  eqn.inf2 <- eqn.f(beta2, z, rho, v, nservers)
  eqn.waiting.inf <- eqn.waiting.f(beta, z, rho, v)
  
  fqk <- fqk.f(kappa, p, beta, v, ca2, cs2)
  #fqxk <- fqx.f(kappa, p, beta, z, kappa)
  sigma2q <- sigma2q.f(kappa, mu, v, p, z)
  rqn <- rqn.f(kappa, p, beta, v, z, mu, nservers, ca2, cs2)
  
  #fqx.mean <- integrate(function(x) sapply(x, function(x) x * fqx.f(x, p, beta, z, kappa)),
  #                      lower=-Inf, upper = kappa)$value
  
  return(data.frame(m, ca2, cs2, lambda, mu, nservers, rho, z, v, p, kappa, beta, beta2, alpha, pw.inf, pw,
                    pbl, pbl2, eqn.inf, eqn.inf2, eqn.waiting.inf, fqk, sigma2q, rqn))
}

CalculateApproximationSummaryAll <- function(df, quota.factor = 1) {
  df$userClass <- factor(df$userClass, levels=c("prod", "batch", "free"))
  df <- arrange(df, userClass)
  capacity.total <- df$capacity.rem.cpu.mean[1]
  permanent.cum <- 0
  admittedCum <- 0
  admittedCum2 <- 0
  admittedCum3 <- 0
  res.df <- data.frame()
  admitted.est <- 0
  for (i in 1:nrow(df)) {
    row <- df[i,]
    ca2 <- with(row, iat.n.var/(iat.n.mean^2))
    cs2 <- with(row, runtime.var/(runtime.mean^2))
    permanent.cum <- permanent.cum + row$cpu.permanent
    nservers <- with(row, max(capacity.total - admittedCum - permanent.cum, 0)) * quota.factor
    nservers2 <- with(row, max(capacity.total - admittedCum2 - permanent.cum, 0)) * quota.factor
    nservers3 <- with(row, max(capacity.total - admittedCum3 - permanent.cum, 0)) * quota.factor
    m <- with(row, nservers / slo.availability - nservers)
    m2 <- with(row, nservers2 / slo.availability - nservers2)
    m3 <- with(row, nservers3 / slo.availability - nservers3)
    res <- with(row, data.frame(CalculateApproximationStats(m, ca2, cs2, arrivalrate.cpu,
                                                            1/runtime.est, nservers, w=1)))
    res3 <- with(row, data.frame(CalculateApproximationStats(m3, ca2, cs2, arrivalrate.cpu,
                                                            1/runtime.est, nservers3, w=1)))
    
    if (nservers2 > 0) {
      f <- (nservers2+m2) / 100
      mmck.i <- NewInput.MMCK(row$arrivalrate.cpu/f, 1/row$runtime.est, round(nservers2/f),
                              round((nservers2+m2)/f))
      mmck.m <- QueueingModel(mmck.i)
      #mmck.pbl <- last(Qn(mmck.m))
      mmck.pbl <- last(Pn(mmck.m))
      admitted.est2 <- L(mmck.m)*f
      rho <- row$arrivalrate.cpu * row$runtime.est / nservers2
      if (rho <= 1) {
        mmc.i <- NewInput.MMC(row$arrivalrate.cpu/f, 1/row$runtime.est, round(nservers2/f),
                              round(nservers2/f))
        mmc.m <- QueueingModel(mmc.i)
        #av.est2 <- 1 - Wq(mmc.m) / W(mmc.m)
        av.est2 <- sum(Pn(mmc.m))
        av.norej.est2 <- av.est2
      } else {
        av.est2 <- 0
        av.norej.est2 <- 0
      }
    } else {
      mmck.pbl <- 1
      admitted.est2 <- 0
      av.est2 <- 0
      av.norej.est2 <- 0
    }
    
    demand.est <- row$arrivalrate.cpu * row$runtime.est
    demand.est3 <- ifelse(!is.na(res3) && is.finite(res3$eqn.inf), res3$eqn.inf, demand.est)
    admitted.est1 <- ifelse(row$method != "no-adm-ctrl" && !is.na(res) && is.finite(res$pbl),
                           (1 - res$pbl) * demand.est, demand.est)
    admitted.est3 <- ifelse(row$method != "no-adm-ctrl" && !is.na(res3) && is.finite(res3$pbl),
                           (1 - res3$pbl) * demand.est3, demand.est3)
    ob.est <- ifelse(!is.na(res) && is.finite(res$pbl), 1 - res$pbl, 0)
    ob.est2 <- ifelse(is.finite(mmck.pbl), mmck.pbl, 0)
    ob.est3 <- ifelse(!is.na(res3) && is.finite(res3$pbl), 1 - res3$pbl, 0)
    av.est <- ifelse(!is.na(res) && is.finite(res$pw) && res$rho <= 1, 1 - res$pw, 0)
    av.est3 <- ifelse(!is.na(res3) && is.finite(res3$pw) && res3$rho <= 1, 1 - res3$pw, 0)
    av.norej.est <- ifelse(!is.na(res) && is.finite(res$pw.inf) && res$rho <= 1, 1 - res$pw.inf, 0)
    av.norej.est3 <- ifelse(!is.na(res3) && is.finite(res3$pw.inf) && res3$rho <= 1, 1 - res3$pw.inf, 0)
    res.df <- rbind(res.df, data_frame(userClass=row$userClass,
                                       model=c("G/GI/c/K", "M/M/c/K", "G/GI/c/K slots"),
                                       admitted.est=c(admitted.est1, admitted.est2, admitted.est3),
                                       admitted.est.all=c(admitted.est1+row$cpu.permanent,
                                                          admitted.est2+row$cpu.permanent,
                                                          admitted.est3+row$cpu.permanent),
                                       rem.capacity.est=c(max(capacity.total - admittedCum - permanent.cum + row$cpu.permanent, 0),
                                                          max(capacity.total - admittedCum2 - permanent.cum + row$cpu.permanent, 0),
                                                          max(capacity.total - admittedCum3 - permanent.cum + row$cpu.permanent, 0)),
                                       ob.est=c(ob.est, 1 - mmck.pbl, ob.est3),
                                       av.est=c(av.est, av.est2, av.est3),
                                       av.norej.est=c(av.norej.est, av.norej.est2, av.norej.est3)
                                       ))
    admittedCum <- admittedCum + max(admitted.est1, 0)
    admittedCum2 <- admittedCum2 + max(admitted.est2, 0)
    admittedCum3 <- admittedCum3 + max(admitted.est3, 0)
  }
  res.df
}


CalculateApproximationSummary <- function(df, quota.factor = 1) {
  df$userClass <- factor(df$userClass, levels=c("prod", "batch", "free"))
  df <- arrange(df, userClass)
  capacity.total <- df$capacity.rem.res.mean[1] #c
  permanent.cum <- 0
  admittedCum <- 0
  res.df <- data.frame()
  admitted.est <- 0
  for (i in 1:nrow(df)) {
    row <- df[i,]
    ca2 <- with(row, iat.n.var/(iat.n.mean^2))
    cs2 <- with(row, runtime.var/(runtime.mean^2))
    permanent.cum <- permanent.cum + row$res.permanent #c
    nservers <- with(row, max(capacity.total - admittedCum - permanent.cum, 0)) * quota.factor
    m <- with(row, nservers / slo.availability - nservers)
    res <- with(row, data.frame(CalculateApproximationStats(m, ca2, cs2, arrivalrate,
                                                            1/runtime.est, nservers, w=1))) #c
     
    nservers.before <- max(0, capacity.total - admittedCum - permanent.cum + row$res.permanent) #c
    admitted.permanent <- min(nservers.before, row$res.permanent) #c
    rem.capacity.est <- max(0, capacity.total - admittedCum - permanent.cum + row$res.permanent - admitted.permanent) #c
    demand.est <- row$arrivalrate * row$runtime.est  #c
    ob.est.nonpermanent <- ifelse(!is.na(res) && is.finite(res$pbl), 1 - res$pbl, 0)
    admitted.est <- ifelse(row$method != "no-adm-ctrl", ob.est.nonpermanent * demand.est, demand.est)
    admitted.est.all <- admitted.est + admitted.permanent
    ob.est <- ob.est.nonpermanent
    av.est <- ifelse(!is.na(res) && is.finite(res$pw) && res$rho <= 1, 1 - res$pw, 0)
    av.norej.est <- ifelse(!is.na(res) && is.finite(res$pw.inf) && res$rho <= 1, 1 - res$pw.inf, 0)
    res.df <- rbind(res.df, data_frame(userClass=row$userClass,
                                       model="G/GI/c/K",
                                       nservers.before,
                                       admitted.permanent,
                                       rem.capacity.est,
                                       admitted.est,
                                       admitted.est.all,
                                       ob.est,
                                       av.est,
                                       av.norej.est)
    )
    admittedCum <- admittedCum + max(admitted.est, 0)
  }
  res.df
}

CapacityObjectiveFunction <- function(x, df, ob.min = 1, upper.fraction = 100) {
  df2 <- mutate(df, capacity.rem.cpu.mean = capacity.rem.cpu.mean * x)
  ob.est <- filter(CalculateApproximationSummary(df2), model == "G/GI/c/K")$ob.est
  
  obj <- max(ifelse(isTRUE(all.equal(rep(0, 3), ob.est - ob.min, tolerance=.001)), x,
                    (2 - ob.est) * upper.fraction))
  print(paste(x, obj))
  return(obj)
}

FindBestCapacity <- function(input.df, lower.fraction=0, upper.fraction=100, precision=.05, ob.min=1,
                             tolerance=.001) {
  for (cf in seq(lower.fraction, upper.fraction, precision)) {
    df2 <- mutate(input.df, capacity.rem.res.mean = (capacity.rem.res.mean / res.capacity.factor) * cf)
    ob.est <- filter(CalculateApproximationSummary(df2), model == "G/GI/c/K")$ob.est
    if (all(ob.est + tolerance >= ob.min)) {
      return(cf)
    }
  }
  return(NA)
}

LoadModelInputData <- function(tasks, input.res.files)  {
  
  #####
  # Generating input statistics for capacity planning model
  #####
  
  stats.tasks <- tasks %>%
    filter(submitTime > 1 | endTime != -1) %>%
    select(userClass, runtime, cpuReq, memReq) %>%
    collect(n = Inf) %>%
    #mutate(endTime = ifelse(endTime == -1, max(submitTime), endTime)) %>%
    group_by(userClass) %>%
    summarise(n=n(),
              #availability=mean(runtime/(endTime-submitTime)),
              runtime.mean=mean(runtime/min5, na.rm=T),
              runtime.var=var(runtime/min5, na.rm=T),
              cpuReq=mean(cpuReq),
              memReq=mean(memReq))
  
  stats.interarrival <- tasks %>%
    select(submitTime, userClass, cpuReq, memReq) %>%
    filter(submitTime > 0, cpuReq > 0) %>%
    collect(n = Inf) %>%
    group_by(userClass) %>%
    transmute(iat.cpu=c(NA, diff(submitTime/min5))/cpuReq,
              iat.mem=c(NA, diff(submitTime/min5))/memReq,
              iat.n=c(NA, diff(submitTime/min5))) %>%
    summarise(
      iat.n.mean=mean(iat.n, na.rm=T), iat.n.var=var(iat.n, na.rm=T),
      iat.cpu.mean=mean(iat.cpu, na.rm=T), iat.cpu.var=var(iat.cpu, na.rm=T),
      iat.mem.mean=mean(iat.mem, na.rm=T), iat.mem.var=var(iat.mem, na.rm=T))
  
  tasks.arrivalrates <- tasks %>%
    filter(submitTime > 1 | endTime != -1) %>%
    select(userClass, submitTime, cpuReq, memReq) %>%
    collect(n = Inf) %>%
    group_by(submitTime=ceiling(submitTime/min5), userClass) %>%
    summarise(rate=n(), rate.cpu=sum(cpuReq), rate.mem=sum(memReq)) %>%
    group_by(userClass) %>%
    do(rbind(., data.frame(submitTime=setdiff(0:max(.$submitTime), .$submitTime),
                           userClass=.$userClass[1], rate=0, rate.cpu=0, rate.mem=0))) %>%
    ungroup() %>%
    arrange(submitTime, userClass)
  
  stats.arrivalrates <- tasks.arrivalrates %>%
    filter(submitTime > 1) %>%
    group_by(userClass, submitTime) %>%
    summarise(rate=sum(rate), rate.cpu=sum(rate.cpu), rate.mem=sum(rate.mem)) %>%
    summarise(arrivalrate=mean(rate), arrivalrate.cpu=mean(rate.cpu),
              arrivalrate.cpu.var=var(rate.cpu), arrivalrate.mem=mean(rate.mem),
              arrivalrate.mem.var=var(rate.mem), total.tasks=sum(rate), total.cpu=sum(rate.cpu),
              total.mem=sum(rate.mem))
  
  stats.permanent <- tasks %>%
    filter(submitTime == 0, endTime == -1) %>%
    select(userClass, cpuReq, memReq) %>%
    group_by(userClass) %>%
    summarise(cpu.permanent=sum(cpuReq),
              mem.permanent=sum(memReq),
              n.permanent=n()) %>%
    collect(n = Inf)
  
  stats.gtrace <- left_join(stats.tasks, stats.arrivalrates, by="userClass") %>%
    left_join(stats.permanent, by="userClass") %>%
    left_join(stats.interarrival, by="userClass") %>%
    mutate(permanent.n.share.all = n.permanent / total.tasks,
           permanent.cpu.share.all = cpu.permanent / total.cpu,
           permanent.mem.share.all = mem.permanent / total.mem,
           userClass = ifelse(userClass == "middle", "batch", userClass))
  
  #####
  # Loading admission control simulation results and calculating statistics
  #####
  
  #stats.files <- list.files("output", "res_forecast-ets-quota.*ac.csv$", full.names=T)
  #stats.files.inf <- "output/res_greedy-norejection_ccf-Inf_clf-1_mcf-Inf_mlf-1_slo-1_cmem-TRUE_ac.csv"
  #stats.in <- LoadResultsFiles(c(stats.files, stats.files.inf))
  
  stats.in <- LoadResultsFiles(input.res.files)
  
  stats <- stats.in %>%
    filter(method != "greedy-reject") %>%
    group_by(method, cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, time, userClass) %>%
    mutate(availability.cpu=mean(allocated.cpu/(demand.cpu - rejected.cpu), na.rm=T),
           availability.mem=mean(allocated.mem/(demand.mem - rejected.mem), na.rm=T),
           availability.n=mean(allocated.n/(demand.n - rejected.n), na.rm=T),
           obtainability.cpu=mean(1 - rejected.cpu/arrivals.cpu, na.rm=T),
           obtainability.mem=mean(1 - rejected.mem/arrivals.mem, na.rm=T),
           obtainability.n=mean(1 - rejected.n/arrivals.n, na.rm=T)) %>%
    group_by(method, cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, time) %>%
    mutate(cum.allocated=cumsum(allocated.cpu),
           rem.capacity.pred=ifelse(is.na(cpu.quota), NA, cpu.quota*slo.availability),
           rem.capacity.pred.error=capacity.rem.cpu-rem.capacity.pred,
           cpu.quota=pmax(0, cpu.quota),
           utilization.cpu=sum(allocated.cpu)/max(capacity.rem.cpu),
           utilization.mem=sum(allocated.mem)/max(capacity.rem.mem)
           ) %>%
  group_by(cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, method, userClass) %>%
    mutate(cum.slo.fulfill=1 - cumsum(vm.slo.violated.n)/cumsum(departures.n),
           cum.av.mean=cummean(availability.n),
           cum.ob.mean=1 - cumsum(rejected.n)/cumsum(arrivals.n))
  
  rm(stats.in)
  gc()
  
  stats.agg <- stats %>%
    group_by(cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, method, userClass, slo.availability) %>%
    summarise(cpu.quota.mean=mean(cpu.quota),
              capacity.rem.cpu.mean=mean(capacity.rem.cpu),
              capacity.rem.cpu.median=median(capacity.rem.cpu),
              capacity.rem.cpu.var=var(capacity.rem.cpu),
              capacity.rem.cpu.scv=capacity.rem.cpu.var / (capacity.rem.cpu.mean^2),
              capacity.rem.mem.mean=mean(capacity.rem.mem),
              capacity.rem.mem.median=median(capacity.rem.mem),
              capacity.rem.mem.var=var(capacity.rem.mem),
              capacity.rem.mem.scv=capacity.rem.mem.var / (capacity.rem.mem.mean^2),
              demand.n.mean=mean(demand.n),
              demand.n.var=var(demand.n),
              demand.cpu.mean=mean(demand.cpu),
              demand.cpu.var=var(demand.cpu),
              demand.mem.mean=mean(demand.mem),
              demand.mem.var=var(demand.mem),
              utilization.cpu.mean=mean(utilization.cpu),
              utilization.mem.mean=mean(utilization.mem),
              allocated.cpu.mean=mean(allocated.cpu),
              allocated.mem.mean=mean(allocated.mem),
              nqueue.cpu.mean=mean(demand.cpu-allocated.cpu),
              av.mean=mean(availability.n, na.rm=T),
              av.median=median(availability.n, na.rm=T),
              av.q25=quantile(availability.n, .25, na.rm=T),
              av.q75=quantile(availability.n, .75, na.rm=T),
              av.min=min(availability.n),
              av.max=max(availability.n),
              ob.mean=sum(arrivals.n - rejected.n) / sum(arrivals.n),
              ob.cpu.mean=sum(arrivals.cpu - rejected.cpu) / sum(arrivals.cpu),
              ob.mem.mean=sum(arrivals.mem - rejected.mem) / sum(arrivals.mem),
              ob.cpu.mean.notime0=sum(arrivals.cpu[time > 0] - rejected.cpu[time > 0]) / sum(arrivals.cpu[time > 0]),
              vm.availability=weighted.mean(vm.availability.mean, departures.n),
              vm.slo.fulfillment=weighted.mean(1 - vm.slo.violated.n/departures.n,
                                               departures.n, na.rm=T)) %>%
    mutate(av.mean = ifelse(ob.mean >= 0, av.mean, NaN),
           slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                   labels=c("very low", "low", "medium", "high", "very high")))
  
  rm(stats)
  gc()
  
  #####
  # Estimating mean task runtimes from an overprovisioned scenario (CPU cpu.capacity.factor == 1.3)
  #####
  
  input.stats <- stats.agg %>%
    left_join(stats.gtrace, by="userClass") %>%
    group_by(userClass) %>%
    mutate(demand.cpu.var.inf=demand.cpu.var[cpu.capacity.factor == Inf & mem.capacity.factor == Inf],
           demand.cpu.mean.inf=demand.cpu.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf],
           runtime.est = max(demand.cpu.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf] - cpu.permanent, 0)/arrivalrate.cpu,
           runtime.est2 = max(demand.cpu.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf], 0)/arrivalrate.cpu,
           runtime.n.est = max(demand.n.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf] - n.permanent, 0)/arrivalrate,
           permanent.share=cpu.permanent / demand.cpu.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf],
           permanent.mem.share=mem.permanent / demand.mem.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf],
           permanent.n.share=n.permanent / demand.n.mean[cpu.capacity.factor == Inf & mem.capacity.factor == Inf],
           arrivalrate.cpu = arrivalrate.cpu * cpu.load.factor,
           arrivalrate.mem = arrivalrate.mem * mem.load.factor,
           cpu.permanent = cpu.permanent * cpu.load.factor,
           mem.permanent = mem.permanent * mem.load.factor)
  
  return(input.stats)
}

ExecuteCapacityPlanning <- function(tasks, capacity, input.res.files) {
  print("Loading files and calculating input statistics...")
  input.stats <- LoadModelInputData(tasks, input.res.files)
  
  # saved in: "output/cp_results_stats.csv"
  print("Performing capacity planning for CPU...")
  res.cpu <- input.stats %>%
    filter(slo.scenario == 1, cpu.capacity.factor != Inf) %>%
    mutate(capacity.rem.res.mean = capacity.rem.cpu.mean,
           res.permanent = cpu.permanent,
           arrivalrate = arrivalrate.cpu,
           res.capacity.factor = cpu.capacity.factor) %>%
    group_by(method, cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario) %>%
    do(data.frame(CalculateApproximationSummary(.),
                  best.cpu.capacity.01=FindBestCapacity(., .3, 10, .01, ob.min = c(.999, .99, 0),
                                                    tolerance = 0))) %>%
    mutate(best.cpu.capacity=round(best.cpu.capacity.01, 1)) %>%
    rename(ob.est.cpu = ob.est, admitted.est.all.cpu = admitted.est.all) %>%
    left_join(input.stats, c("method", "cpu.capacity.factor", "mem.capacity.factor", "cpu.load.factor",
                         "mem.load.factor", "slo.scenario", "userClass")) %>%
    mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))
  
  print("Performing capacity planning for Memory...")  
  res <- input.stats %>%
    filter(slo.scenario == 1, cpu.capacity.factor != Inf) %>%
    mutate(capacity.rem.res.mean = capacity.rem.mem.mean,
           res.permanent = mem.permanent,
           arrivalrate = arrivalrate.mem,
           res.capacity.factor = mem.capacity.factor) %>%
    group_by(method, cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario) %>%
    do(data.frame(CalculateApproximationSummary(.),
                  best.mem.capacity.01=FindBestCapacity(., .3, 10, .01, ob.min = c(.999, .99, 0),
                                                        tolerance = 0))) %>%
    mutate(best.mem.capacity=round(best.mem.capacity.01, 1)) %>%
    rename(ob.est.mem = ob.est, admitted.est.all.mem = admitted.est.all) %>%
    left_join(res.cpu, c("method", "cpu.capacity.factor", "mem.capacity.factor", "cpu.load.factor",
                             "mem.load.factor", "slo.scenario", "userClass")) %>%
    mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))
  
  return(res)
}

Main <- function(argv) {
  input.res.files <- argv
  #input.res.files <- list.files("output", "res_forecast-ets-quota_ccf-10_clf-1_mcf-1_mlf-.*_slo-1_cmem-TRUE.*_ac.csv", full.names = T)
  #input.res.files <- c(input.res.files, "output/res_greedy-norejection_ccf-Inf_clf-1_mcf-Inf_mlf-1_slo-1_cmem-TRUE_ac.csv")
  output.file <- "output/cp_results.csv"
  
  tasks <- LoadTaskEvents("data/gtrace_data.sqlite3", from.sqlite=T)
  capacity <- LoadTotalCapacity("data/gtrace_total_capacity.txt")
  
  results <- ExecuteCapacityPlanning(tasks, capacity, input.res.files)
  
  write.table(results, output.file, quote = F, col.names = T, row.names = F, sep = ",")
}

# Read command-line arguments
argv <- commandArgs(trailingOnly = TRUE)

# If any input argument is found, execute the main function
if (length(argv) > 0) {
  Main(argv)
}
