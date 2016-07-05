library(queueing)

base.dir <- "~/local/admission-control-model/"

setwd(base.dir)

source("src/admission_control_utils.R")
source("src/admission_control_paper_plots.R")

plots.dir <- paste(base.dir, "/cp-plots/", sep="")

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
  capacity.total <- df$capacity.rem.cpu.mean[1]
  permanent.cum <- 0
  admittedCum <- 0
  admittedCum2 <- 0
  res.df <- data.frame()
  admitted.est <- 0
  admitted.est2 <- 0
  for (i in 1:nrow(df)) {
    row <- df[i,]
    ca2.1 <- with(row, iat.n.var/(iat.n.mean^2))
    ca2.2 <- with(row, iat.cpu.var/(iat.cpu.mean^2))
    cs2 <- with(row, runtime.var/(runtime.mean^2))
    permanent.cum <- permanent.cum + row$cpu.permanent
    nservers <- with(row, max(capacity.total - admittedCum - permanent.cum, 0)) * quota.factor
    nservers2 <- with(row, max(capacity.total - admittedCum2 - permanent.cum, 0) / cpuReq) * quota.factor
    m <- with(row, nservers / slo.availability - nservers)
    m2 <- with(row, nservers2 / slo.availability - nservers2)
    res <- with(row, data.frame(CalculateApproximationStats(m, ca2.1, cs2, arrivalrate.cpu,
                                                            1/runtime.est, nservers, w=1)))
    res2 <- with(row, data.frame(CalculateApproximationStats(m2, ca2.1, cs2, arrivalrate,
                                                            1/runtime.est2, nservers2, w=1)))
    
    nservers.before <- max(0, capacity.total - admittedCum - permanent.cum + row$cpu.permanent)
    nservers.before2 <- max(0, capacity.total - admittedCum2 - permanent.cum + row$cpu.permanent)
    admitted.permanent <- min(nservers.before, row$cpu.permanent)
    admitted.permanent2 <- min(nservers.before2, row$cpu.permanent)
    rem.capacity.est <- max(0, capacity.total - admittedCum - permanent.cum + row$cpu.permanent - admitted.permanent)
    rem.capacity.est2 <- max(0, capacity.total - admittedCum2 - permanent.cum + row$cpu.permanent - admitted.permanent2)
    demand.est <- row$arrivalrate.cpu * row$runtime.est
    demand.est2 <- row$arrivalrate * row$runtime.est * row$cpuReq
    #demand.est2 <- ifelse(!is.na(res2) && is.finite(res2$eqn.inf), res2$eqn.inf * row$cpuReq, row$arrivalrate * row$runtime.est2 * row$cpuReq)
    ob.est.nonpermanent <- ifelse(!is.na(res) && is.finite(res$pbl), 1 - res$pbl, 0)
    ob.est.nonpermanent2 <- ifelse(!is.na(res2) && is.finite(res2$pbl), 1 - res2$pbl, 0)
    admitted.est <- ifelse(row$method != "no-adm-ctrl", ob.est.nonpermanent * demand.est, demand.est)
    admitted.est2 <- ifelse(row$method != "no-adm-ctrl", ob.est.nonpermanent2 * demand.est2, demand.est2)
    admitted.est.all <- admitted.est + admitted.permanent
    #admitted.est.all2 <- admitted.est2 + admitted.permanent2
    admitted.est.all2 <- admitted.est2 + admitted.permanent2
    #ob.est <- ob.est.nonpermanent * (1 - row$permanent.cpu.share.all) + (admitted.permanent / row$cpu.permanent) * row$permanent.cpu.share.all
    ob.est <- ob.est.nonpermanent
    #ob.est2 <- ob.est.nonpermanent2 * (1 - row$permanent.n.share.all) + (admitted.permanent2 / row$cpu.permanent) * row$permanent.n.share.all
    ob.est2 <- ob.est.nonpermanent2
    av.est <- ifelse(!is.na(res) && is.finite(res$pw) && res$rho <= 1, 1 - res$pw, 0)
    av.est2 <- ifelse(!is.na(res2) && is.finite(res2$pw) && res2$rho <= 1, 1 - res2$pw, 0)
    av.norej.est <- ifelse(!is.na(res) && is.finite(res$pw.inf) && res$rho <= 1, 1 - res$pw.inf, 0)
    av.norej.est2 <- ifelse(!is.na(res2) && is.finite(res2$pw.inf) && res2$rho <= 1, 1 - res2$pw.inf, 0)
    res.df <- rbind(res.df, data_frame(userClass=row$userClass,
                                       model=c("G/GI/c/K", "G/GI/c/K slots"),
                                       nservers.before = c(nservers.before, nservers.before2),
                                       admitted.permanent = c(admitted.permanent, admitted.permanent2),
                                       rem.capacity.est = c(rem.capacity.est, rem.capacity.est2),
                                       admitted.est = c(admitted.est, admitted.est2),
                                       admitted.est.all = c(admitted.est.all, admitted.est2),
                                       ob.est = c(ob.est, ob.est2),
                                       av.est = c(av.est, av.est2),
                                       av.norej.est = c(av.norej.est, av.norej.est2))
    )
    admittedCum <- admittedCum + max(admitted.est, 0)
    admittedCum2 <- admittedCum2 + max(admitted.est2, 0)
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
    df2 <- mutate(input.df, capacity.rem.cpu.mean = (capacity.rem.cpu.mean / capacity.fraction) * cf)
    ob.est <- filter(CalculateApproximationSummary(df2), model == "G/GI/c/K")$ob.est
    if (all(ob.est + tolerance >= ob.min)) {
      return(cf)
    }
  }
  return(NA)
}

Main <- function() {

second <- 1000000
minute <- second * 60
hour <- minute * 60
min5 <- 5 * minute

tasks <- LoadTaskEvents("output/gtrace_data.sqlite3", from.sqlite=T)
capacity <- LoadTotalCapacity("output/gtrace_total_capacity.txt")

stats.tasks <- tasks %>%
  select(userClass, submitTime, endTime, runtime, cpuReq) %>%
  filter(submitTime > 1 | endTime != -1) %>%
  collect() %>%
  #mutate(endTime = ifelse(endTime == -1, max(submitTime), endTime)) %>%
  group_by(userClass) %>%
  summarise(n=n(),
            #availability=mean(runtime/(endTime-submitTime)),
            runtime.mean=mean(runtime/min5, na.rm=T),
            runtime.var=var(runtime/min5, na.rm=T),
            cpuReq=mean(cpuReq))

stats.interarrival <- tasks %>%
  select(submitTime, userClass, cpuReq) %>%
  filter(submitTime > 0, cpuReq > 0) %>%
  collect() %>%
  group_by(userClass) %>%
  transmute(iat.cpu=c(NA, diff(submitTime/min5))/cpuReq,
            iat.n=c(NA, diff(submitTime/min5))) %>%
  summarise(
    iat.n.mean=mean(iat.n, na.rm=T), iat.n.var=var(iat.n, na.rm=T),
    iat.cpu.mean=mean(iat.cpu, na.rm=T), iat.cpu.var=var(iat.cpu, na.rm=T))

tasks.arrivalrates <- tasks %>%
  select(userClass, submitTime, runtime, endTime, cpuReq) %>%
  filter(submitTime > 1 | endTime != -1) %>%
  collect() %>%
  group_by(submitTime=ceiling(submitTime/min5), userClass) %>%
  summarise(rate=n(), rate.cpu=sum(cpuReq)) %>%
  group_by(userClass) %>%
  do(rbind(., data.frame(submitTime=setdiff(0:max(.$submitTime), .$submitTime),
                         userClass=.$userClass[1], rate=0, rate.cpu=0))) %>%
  ungroup() %>%
  arrange(submitTime, userClass)

stats.arrivalrates <- tasks.arrivalrates %>%
  filter(submitTime > 1) %>%
  group_by(userClass, submitTime) %>%
  summarise(rate=sum(rate), rate.cpu=sum(rate.cpu)) %>%
  summarise(arrivalrate=mean(rate), arrivalrate.cpu=mean(rate.cpu),
            arrivalrate.cpu.var=var(rate.cpu), total.tasks=sum(rate), total.cpu=sum(rate.cpu))

stats.permanent <- tasks %>%
  select(userClass, submitTime, endTime, runtime, cpuReq) %>%
  filter(submitTime == 0, endTime == -1) %>%
  group_by(userClass) %>%
  summarise(cpu.permanent=sum(cpuReq),
            n.permanent=n(),
            cpuReq.permanent=mean(cpuReq)) %>%
  collect()

stats.gtrace <- left_join(stats.tasks, stats.arrivalrates, by="userClass") %>%
  left_join(stats.permanent, by="userClass") %>%
  left_join(stats.interarrival, by="userClass") %>%
  mutate(permanent.n.share.all = n.permanent / total.tasks,
         permanent.cpu.share.all = cpu.permanent / total.cpu,
         userClass = ifelse(userClass == "middle", "batch", userClass))
#  mutate(demand.est=arrivalrate.cpu * runtime.mean + cpu.permanent)

#stats.gtrace$userClass <- ifelse(stats.gtrace$userClass == "middle", "batch", stats.gtrace$userClass)

stats.files <- list.files("output", "res55_is-300000000.*ac.txt$", full.names=T)
stats.files.new <- list.files("output", "cp_res.*ac.txt$", full.names=T)
#stats.files.new2 <- list.files("output", "cp_res2_is-300000000.*ac.txt$", full.names=T)
#stats <- LoadResultsFiles(stats.files)
stats.in <- LoadResultsFiles(c(stats.files, stats.files.new))

stats <- stats.in %>%
  filter(method != "greedy-reject") %>%
  group_by(capacity.fraction, slo.scenario, cpureq.factor, method, time, userClass) %>%
  mutate(availability=mean(allocated.cpu/(demand.cpu - rejected.cpu), na.rm=T),
         obtainability=mean(1 - rejected.cpu/arrivals.cpu, na.rm=T)) %>%
  group_by(capacity.fraction, slo.scenario, cpureq.factor, method, time) %>%
  mutate(cum.allocated=cumsum(allocated.cpu),
         rem.capacity.pred=quota*slo.availability,
         rem.capacity.pred.error=capacity.rem.cpu-rem.capacity.pred,
         quota=pmax(0, quota),
         utilization=sum(allocated.cpu)/max(capacity.rem.cpu)) %>%
  group_by(capacity.fraction, slo.scenario, cpureq.factor, method, userClass) %>%
  mutate(cum.slo.fulfill=1 - cumsum(vm.slo.violated.n)/cumsum(departures.n),
         cum.av.mean=cummean(availability),
         cum.ob.mean=1 - cumsum(rejected.n)/cumsum(arrivals.n))

stats.agg <- stats %>%
  group_by(capacity.fraction, slo.scenario, cpureq.factor, method, userClass, slo.availability) %>%
  summarise(quota.mean=mean(quota),
            capacity.rem.cpu.mean=mean(capacity.rem.cpu),
            capacity.rem.cpu.median=median(capacity.rem.cpu),
            capacity.rem.cpu.var=var(capacity.rem.cpu),
            capacity.rem.scv=capacity.rem.cpu.var / (capacity.rem.cpu.mean^2),
            demand.n.mean=mean(demand.n),
            demand.n.var=var(demand.n),
            demand.cpu.mean=mean(demand.cpu),
            demand.cpu.var=var(demand.cpu),
            utilization.mean=mean(utilization),
            allocated.cpu.mean=mean(allocated.cpu),
            nqueue.cpu.mean=mean(demand.cpu-allocated.cpu),
            av.mean=mean(availability, na.rm=T),
            av.median=median(availability, na.rm=T),
            av.q25=quantile(availability, .25, na.rm=T),
            av.q75=quantile(availability, .75, na.rm=T),
            av.min=min(availability),
            av.max=max(availability),
            ob.mean=sum(arrivals.n - rejected.n) / sum(arrivals.n),
            ob.cpu.mean=sum(arrivals.cpu - rejected.cpu) / sum(arrivals.cpu),
            ob.cpu.mean.notime0=sum(arrivals.cpu[time > 0] - rejected.cpu[time > 0]) / sum(arrivals.cpu[time > 0]),
            vm.availability=weighted.mean(vm.availability.mean, departures.n),
            vm.slo.fulfillment=weighted.mean(1 - vm.slo.violated.n/departures.n,
                                             departures.n, na.rm=T)) %>%
  mutate(av.mean = ifelse(ob.mean >= 0, av.mean, NaN),
         slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                 labels=c("very low", "low", "medium", "high", "very high")))

stats.f <- stats.agg %>%
  left_join(stats.gtrace, by="userClass") %>%
  group_by(userClass) %>%
  mutate(demand.cpu.var.inf=demand.cpu.var[method == "no-adm-ctrl" & capacity.fraction == 1.3],
         demand.cpu.mean.inf=demand.cpu.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3],
         runtime.est = max(demand.cpu.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3] - cpu.permanent, 0)/arrivalrate.cpu,
         runtime.est2 = max(demand.cpu.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3], 0)/arrivalrate.cpu,
         runtime.n.est = max(demand.n.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3] - n.permanent, 0)/arrivalrate,
         permanent.share=cpu.permanent / demand.cpu.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3],
         permanent.n.share=n.permanent / demand.n.mean[method == "no-adm-ctrl" & capacity.fraction == 1.3],
         arrivalrate.cpu = arrivalrate.cpu * cpureq.factor,
         cpu.permanent = cpu.permanent * cpureq.factor)

# saved in: "output/cp_results_stats.csv"
stats.f2 <- stats.f %>%
  filter(slo.scenario == 1) %>%
  group_by(method, capacity.fraction, slo.scenario, cpureq.factor) %>%
  do(data.frame(CalculateApproximationSummary(.),
                best.capacity.01=FindBestCapacity(., .3, 10, .01, ob.min = c(.999, .99, 0), tolerance = 0))) %>%
  mutate(best.capacity=round(best.capacity.01, 1)) %>%
  left_join(stats.f, c("method", "capacity.fraction", "slo.scenario", "cpureq.factor", "userClass")) %>%
  mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))
  
data.frame(stats.f2 %>%
             filter(slo.scenario == 1, method == "pred-ets", model == "G/GI/c/K",
                    cpureq.factor == 1, between(capacity.fraction, .6, 1.3)) %>%
             group_by(userClass, model) %>%
             summarise(error.mean=mean(ob.cpu.mean - ob.est),
                       error.abs.mean=round(mean(abs(ob.cpu.mean-ob.est)), 3),
                       error.abs.max=round(max(abs(ob.cpu.mean-ob.est)), 3)))


###########
# Plots
###########

plot.margin <- unit(c(1, 1, 1, 1), "mm")

df.plot <- stats.f2 %>%
           filter(between(capacity.fraction, .6, 1.3), method == "pred-ets", slo.scenario == 1,
                  model == "G/GI/c/K",
                  cpureq.factor == 1) %>%
           group_by(capacity.fraction) %>%
           mutate(total.capacity=max(capacity.rem.cpu.mean))

p <- ggplot(df.plot, aes(capacity.fraction, ob.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
     geom_line() +
     geom_point(size=4) +
     geom_line(aes(y=ob.est, col="model", lty="model")) +
     geom_point(aes(y=ob.est, col="model", pch="model"), size=4, alpha=.8) +
     facet_wrap(~userClass, ncol=1) +
     scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
     scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
     scale_color_discrete("") +
     scale_shape_discrete("") +
     scale_linetype_manual("", values = c(2, 1)) +
     theme(plot.margin = plot.margin)
p
plot.file <- paste(plots.dir, "cp_capacity_admissionrate_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=6.5)

p <- ggplot(df.plot, aes(capacity.fraction, demand.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=admitted.est.all, col="model", lty="model")) +
  geom_point(aes(y=admitted.est.all, col="model", pch="model"), size=4, alpha=.8) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Mean demand") +
  facet_wrap(~userClass, ncol=1, scale="free_y") +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_manual("", values = c(2, 1))
p
plot.file <- paste(plots.dir, "cp_capacity_demand_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=6.5)

df.plot %>%
  group_by(userClass, model) %>%
  summarise(round(max(abs(ob.cpu.mean - ob.est))*100, 1), round(mean(abs(ob.cpu.mean - ob.est))*100, 1))


df.plot <- stats.f2 %>%
  filter(method == "pred-ets", slo.scenario == 1, 
         capacity.fraction %in% c(1),
         model == "G/GI/c/K", cpureq.factor %in% c(.7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) %>%
  mutate(cpureq.factor.str = paste("load factor =", cpureq.factor))

p <- ggplot(df.plot, aes(cpureq.factor, ob.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4) +
  geom_line(aes(y=ob.est, col="model", lty="model")) +
  geom_point(aes(y=ob.est, col="model", pch="model"), size=4, alpha=.8) +
  #geom_vline(aes(xintercept=best.capacity), lty=2) +
  facet_wrap(~userClass, ncol=1) +
  #facet_grid(userClass~capacity.fraction) +
  scale_x_continuous("Load factor", breaks=seq(0, 3, .1)) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_manual("", values = c(2, 1)) +
  theme(legend.position = "right", plot.margin = plot.margin)
p
plot.file <- paste(plots.dir, "cp_loadfactor_admissionrate_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=6.5)

df.plot %>%
  group_by(userClass, model) %>%
  summarise(round(max(abs(ob.cpu.mean - ob.est))*100, 1), round(mean(abs(ob.cpu.mean - ob.est))*100, 1))

df.plot <- stats.f2 %>%
  filter(method == "pred-ets", slo.scenario == 1, 
         capacity.fraction %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5),
         model == "G/GI/c/K", cpureq.factor %in% c(.5, .75, 1, 1.25, 1.5, 1.75, 2)) %>%
  group_by(cpureq.factor, capacity.fraction) %>%
  summarise(best.capacity=min(best.capacity.01), fulfilled=all(ob.cpu.mean >= c(.999, .99, 0))) %>%
  filter(fulfilled) %>%
  summarise(sim.best.capacity=min(capacity.fraction), model.best.capacity=min(best.capacity))

p <- ggplot(df.plot, aes(cpureq.factor, sim.best.capacity, col = "simulation", lty = "simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4) +
  geom_line(aes(y=model.best.capacity, col = "model", lty = "model")) +
  geom_point(aes(y=model.best.capacity, col = "model", pch = "model"), size=4, alpha=.8) +
  scale_x_continuous("Load factor", breaks=seq(0, 3, .25)) +
  scale_y_continuous("Best capacity size factor", breaks=seq(0, 3, .2)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_manual("", values = c(2, 1)) +
  theme(plot.margin = plot.margin)
p
plot.file <- paste(plots.dir, "cp_loadfactor_bestcapacity_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=2.5)




stats.all <- stats %>%
  filter(method != "greedy-reject") %>%
  group_by(capacity.fraction, cpureq.factor, slo.scenario, method, time) %>%
  summarise(utilization=sum(allocated.cpu)/max(capacity.rem.cpu),
            slo.violated.n=sum(vm.slo.violated.n), departures.n=sum(departures.n),
            admission.rate=sum(arrivals.n - rejected.n) / sum(arrivals.n),
            arrivals.n=sum(arrivals.n)) %>%
  summarise(utilization.mean=mean(utilization), utilization.sd=sd(utilization),
            utilization.error=qnorm(.975) * utilization.sd / sqrt(n()),
            utilization.lower=utilization.mean - utilization.error,
            utilization.upper=utilization.mean + utilization.error,
            slo.fulfillment=weighted.mean(1 - slo.violated.n/departures.n,
                                          departures.n, na.rm=T),
            admission.rate=weighted.mean(admission.rate, arrivals.n, na.rm=T)) %>%
  mutate(slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                 labels=c("very-low", "low", "medium", "high", "very-high")))

###################
# Admission control evaluation plots
###################
dodge <- position_dodge(width=.9)
plot.margin <- unit(c(1, 1, 1, 1), "mm")

stats.agg.cf1 <- filter(stats.agg, capacity.fraction == 1, slo.scenario == 1, cpureq.factor == 1,
                        method != "greedy-reject")

stats.agg.cf1.g <- gather(stats.agg.cf1, metric, value, vm.slo.fulfillment, ob.mean) %>%
  mutate(metric=factor(metric, levels=c("vm.slo.fulfillment", "ob.mean"),
                       labels=c("SLO fulfillment", "Admission rate")))

p <- ggplot(stats.agg.cf1.g, aes(method, value, fill=method)) +
  geom_bar(stat="identity") +
  #geom_text(aes(label=sprintf("%1.1f%%", 100*value)), size=3, vjust=-.2) +
  geom_text(aes(label=paste(round(100*value, 1), "%", sep="")), size=3, vjust=-.3) +
  scale_y_continuous("Availability SLO fulfillment", breaks=seq(0, 1, .1), limits=c(0, 1.05), labels=percent) +
  scale_fill_brewer(palette="Spectral") +
  facet_grid(metric~userClass) +
  theme(axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust=.95), plot.margin=plot.margin,
        strip.text.y = element_blank())
p
plot.file <- paste(plots.dir, "ac_slo-fulfillment_admission-rate_cf1.png", sep="/")
png(plot.file, width=6, height=5, units="in", res=300)
g <- ggplotGrob(p)
yax <- which(g$layout$name=="ylab")
g[["grobs"]][[yax]]$label <- c("Admission rate", "Availability SLO fulfillment")
g[["grobs"]][[yax]]$y <- grid::unit(c(0.25, 0.75), "npc")
grid.newpage()
grid.draw(g)
dev.off()

df.plot <- gather(stats.all, "metric.name", "metric.value", slo.fulfillment, admission.rate) %>%
  mutate(metric.name=factor(metric.name, levels=c("slo.fulfillment", "admission.rate"),
                            labels=c("Availability SLO fulfillment", "Admission rate")))

p <- ggplot(filter(df.plot, slo.scenario == 1, cpureq.factor == 1,
                   capacity.fraction %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3)),
            aes(capacity.fraction, metric.value, col=method, pch=method, group=method)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Metric value", breaks=seq(0, 1, .1), labels=percent) +
  scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Heuristic:") +
  facet_grid(~ metric.name) +
  theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
p
plot.file <- paste(plots.dir, "ac_metrics_cfs_agg.png", sep="/")
ggsave(plot.file, p, width=6, height=3.6)


p <- ggplot(filter(df.plot, capacity.fraction == 1, cpureq.factor == 1),
            aes(slo.scenario.str, metric.value, col=method, pch=method, group=method)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_discrete("SLO strength") +
  scale_y_continuous("Metric value", breaks=seq(0, 1, .05), labels=percent) +
  scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Heuristic:") +
  facet_grid(~ metric.name) +
  theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
p
plot.file <- paste(plots.dir, "ac_metrics_slos_agg.png", sep="/")
ggsave(plot.file, p, width=6, height=3.2)

p <- ggplot(filter(df.plot, slo.scenario == 1, capacity.fraction == 1,
                   cpureq.factor %in% c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)),
            aes(cpureq.factor, metric.value, col=method, pch=method, group=method)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Load factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Metric value", breaks=seq(0, 1, .1), labels=percent) +
  scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Heuristic:") +
  facet_grid(~ metric.name) +
  theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
p
plot.file <- paste(plots.dir, "ac_metrics_loadfactor_agg.png", sep="/")
ggsave(plot.file, p, width=6, height=3.6)


####################
# Other analysis
####################

df.plot <- filter(stats.f2, between(capacity.fraction, .6, 1.3), method == "no-adm-ctrl",
                  model == "G/GI/c/K", slo.scenario == 1, cpureq.factor == 1)
  
p <- ggplot(df.plot, aes(capacity.fraction, vm.availability, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=av.norej.est, col="model", lty="model")) +
  geom_point(aes(y=av.norej.est, col="model", pch="model"), size=4, alpha=.8) +
  facet_wrap(~userClass, ncol=1) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("VM availability", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_manual("", values = c(2, 1))
plot.file <- paste(plots.dir, "cp_capacity_availability_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=6.5)


df.plot <- filter(stats.f2, capacity.fraction == 1, method == "no-adm-ctrl",
                  model == "G/GI/c/K", slo.scenario == 1,
                  cpureq.factor %in% c(.7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) %>%
            mutate(av.perm=pmin(1, capacity.rem.cpu.mean/cpu.permanent) * permanent.n.share.all, av.all = av.perm + av.norej.est * (1 - permanent.n.share.all))

p <- ggplot(df.plot, aes(cpureq.factor, vm.availability, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=av.all, col="model", lty="model")) +
  geom_point(aes(y=av.norej.est, col="model", pch="model"), size=4, alpha=.8) +
  facet_wrap(~userClass, ncol=1) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("VM availability", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_manual("", values = c(2, 1))
p
plot.file <- paste(plots.dir, "cp_capacity_availability_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=6.5)


df.plot %>%
  filter(capacity.fraction >= 0.8) %>%
  group_by(userClass, model) %>%
  summarise(round(max(abs(vm.availability - av.norej.est))*100, 1), round(mean(abs(vm.availability - av.norej.est))*100, 1))


df.plot <- stats.f2 %>%
           filter(between(capacity.fraction, .6, 1.3), method == "pred-ets",
                  model == "G/GI/c/K", slo.scenario == 1, cpureq.factor == 1) %>%
           group_by(capacity.fraction) %>%
           summarise(total.capacity=max(capacity.rem.cpu.mean),
                     demand.cpu.all=sum(demand.cpu.mean),
                     demand.est.all=sum(admitted.est.all),
                     utilization.mean = min(1, demand.cpu.all/total.capacity),
                     utilization.est = min(1, demand.est.all/total.capacity),
                     utilization.est2 = min(1, sum(arrivalrate.cpu * runtime.est)/total.capacity))

p <- ggplot(df.plot, aes(capacity.fraction, utilization.mean, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=utilization.est, col="model", lty="model")) +
  geom_point(aes(y=utilization.est, col="model", pch="model"), size=4, alpha=.8) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Mean utilization", label=percent, breaks=seq(0, 1, .02)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_discrete("")
p
plot.file <- paste(plots.dir, "cp_capacity_utilization_estimations.png", sep="/")
ggsave(plot.file, p, width=6, height=3)



df.plot <- filter(stats.f2, between(capacity.fraction, .6, 1.3), method == "pred-ets",
                  capacity.fraction == 1) %>%
           arrange(slo.scenario.str)

p <- ggplot(df.plot, aes(slo.scenario.str, ob.cpu.mean, group=model, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4) +
  geom_line(aes(y=ob.est, col=model, lty=model, pch=model)) +
  geom_point(aes(y=ob.est, col=model, lty=model, pch=model), size=4, alpha=.8) +
  facet_wrap(~userClass, ncol=1) +
  #scale_x_discrete("Capacity size factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("Model") +
  scale_shape_discrete("Model") +
  scale_linetype_discrete("Model")
p


ggplot(filter(stats.f2, capacity.fraction == 1, method == "greedy-quota"),
       aes(slo.availability, ob.mean, col="simulation", lty="simulation")) +
  #geom_line() +
  geom_point(size=4) +
  #geom_line(aes(y=ob.est, col="estimated", lty="estimated")) +
  geom_point(aes(y=ob.est, col="estimated", lty="estimated")) +
  facet_wrap(~userClass) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .05))
  #scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1))


ggplot(filter(stats, slo.scenario == 1, capacity.fraction <= 1.3, method == "no-adm-ctrl"), aes(time, demand.cpu - allocated.cpu, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  #geom_point(size=4) + 
  #geom_line(aes(y=ob.est, col="estimated", lty="estimated")) +
  #geom_point(aes(y=ob.est, col="estimated", pch="estimated"), size=4) +
  #geom_line(aes(y=ob.est2, col="estimated2", lty="estimated2")) +
  #geom_point(aes(y=ob.est2, col="estimated2", pch="estimated2"), size=4) +
  facet_wrap(capacity.fraction~userClass, scale="free_y", ncol=3)
  #scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .05)) + 
  #scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1))




df.plot <- stats.f2 %>%
  filter(method == "pred-ets", slo.scenario == 1, 
         capacity.fraction %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5),
         model == "G/GI/c/K", cpureq.factor %in% seq(.5, 2, .5)) %>%
  mutate(cpureq.factor.str = paste("load factor =", cpureq.factor))

p <- ggplot(df.plot, aes(capacity.fraction, ob.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=ob.est, col="model", lty="model")) +
  geom_point(aes(y=ob.est, col="model", pch="model"), size=4, alpha=.8) +
  geom_vline(aes(xintercept=best.capacity), lty=2) +
  #facet_wrap(~userClass, ncol=1) +
  facet_grid(userClass~cpureq.factor.str) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 3, .2)) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_discrete("") +
  theme(legend.position = "right")
p
plot.file <- paste(plots.dir, "cp_capacity_arrivalrate_admissionrate_estimations.png", sep="/")
ggsave(plot.file, p, width=12, height=6)






df.plot <- stats.f2 %>%
  filter(method == "no-adm-ctrl", slo.scenario == 1, 
         capacity.fraction %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5),
         model == "G/GI/c/K", cpureq.factor %in% seq(.5, 2, .5))

p <- ggplot(df.plot, aes(capacity.fraction, av.mean, col="simulation", lty="simulation", pch="simulation")) +
  geom_line() +
  geom_point(size=4, alpha=.8) +
  geom_line(aes(y=av.est, col="model", lty="model")) +
  geom_point(aes(y=av.est, col="model", pch="model"), size=4, alpha=.8) +
  geom_vline(aes(xintercept=best.capacity), lty=2) +
  #facet_wrap(~userClass, ncol=1) +
  facet_grid(userClass~cpureq.factor) +
  scale_x_continuous("Capacity size factor", breaks=seq(0, 3, .2)) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("") +
  scale_shape_discrete("") +
  scale_linetype_discrete("") +
  theme(legend.position = "top")
p



##############################
# Model Analysis
#####################

  fractions <- seq(.1, 5, .1)
  frac.len <- length(fractions)
  input.base <- filter(stats.f, slo.scenario == 1, capacity.fraction == 1,
                       cpureq.factor == 1, method == "pred-ets")
  
  input.arrivalrates <- input.base %>%
                        full_join(expand.grid(userClass = input.base$userClass,
                                              arrivalrate.fraction = fractions), by = "userClass") %>%
                        mutate(arrivalrate.cpu=arrivalrate.cpu * arrivalrate.fraction,
                               cpu.permanent=cpu.permanent * arrivalrate.fraction)
  
  output.arrivalrates <- input.arrivalrates %>%
                         group_by(method, capacity.fraction, slo.scenario, arrivalrate.fraction) %>%
                         do(data.frame(CalculateApproximationSummary(.), best.capacity=FindBestCapacity(., 0, 10, .01))) %>%
                         left_join(stats.f) %>%
                         mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))
  
  df.plot <- filter(output.arrivalrates, model == "G/GI/c/K")
  
  p <- ggplot(df.plot, aes(arrivalrate.fraction, ob.est, col=userClass, pch=userClass)) +
    geom_line() +
    geom_point(size=4) +
    #facet_wrap(~userClass, ncol=1) +
    scale_x_continuous("Relative traffic intensity", breaks=seq(.5, 2.5, .25)) +
    scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
    scale_color_discrete("Class") +
    scale_shape_discrete("Class")
    #theme(legend.position = "top")
  p
  plot.file <- paste(plots.dir, "cp_trafficintensity_admissionrate.png", sep="/")
  ggsave(plot.file, p, width=6, height=3)
  
  p <- ggplot(df.plot, aes(arrivalrate.fraction, av.norej.est, col=userClass, pch=userClass)) +
    geom_line() +
    geom_point(size=4) +
    #facet_wrap(~userClass, ncol=1) +
    scale_x_continuous("Relative traffic intensity", breaks=seq(.5, 2.5, .25)) +
    scale_y_continuous("VM availability", label=percent, breaks=seq(0, 1, .1)) +
    scale_color_discrete("Class") +
    scale_shape_discrete("Class")
    #theme(legend.position = "top")
  p
  plot.file <- paste(plots.dir, "cp_trafficintensity_availability.png", sep="/")
  ggsave(plot.file, p, width=6, height=3)
  
  p <- ggplot(df.plot, aes(arrivalrate.fraction, best.capacity)) +
    geom_line() +
    geom_point(size=4) +
    #facet_wrap(~userClass, ncol=1) +
    #scale_x_continuous("Relative traffic intensity", breaks=seq(.5, 2.5, .25)) +
    scale_y_continuous("Best relative capacity", breaks=seq(0, 3, .1)) +
    scale_color_discrete("Class") +
    scale_shape_discrete("Class")
    #theme(legend.position = "top")
  p
  plot.file <- paste(plots.dir, "cp_trafficintensity_bestcapacity.png", sep="/")
  ggsave(plot.file, p, width=6, height=3)

  
  input.arrivalrates.ob <- input.base %>%
    full_join(expand.grid(userClass = input.base$userClass,
                          arrivalrate.fraction = seq(.2, 4, .2),
                          ob.target=seq(.2, 1, .2)), by = "userClass") %>%
    mutate(arrivalrate.cpu=arrivalrate.cpu * arrivalrate.fraction,
           cpu.permanent=cpu.permanent * arrivalrate.fraction)
  
  output.arrivalrates.ob <- input.arrivalrates.ob %>% filter(userClass == "prod") %>%
    group_by(method, capacity.fraction, slo.scenario, arrivalrate.fraction, ob.target) %>%
    do(data.frame(CalculateApproximationSummary(.), best.capacity=FindBestCapacity(., 0, 10, .001, .$ob.target[1]),
                  total.capacity=max(.$capacity.rem.cpu.mean))) %>%
    left_join(input.arrivalrates.ob) 
  
  output.arrivalrates.ob.agg <- output.arrivalrates.ob %>%
#    mutate(userClass=factor(userClass, levels=c("prod", "batch", "free"))) %>%
    group_by(arrivalrate.fraction, ob.target) %>%
    summarise(load=sum(arrivalrate.cpu * runtime.est + cpu.permanent),
              best.capacity.abs=max(best.capacity*total.capacity),
              extra.capacity = best.capacity.abs - load)

p <- ggplot(filter(output.arrivalrates.ob.agg, ob.target == 1), aes(load, best.capacity.abs/load)) +
  geom_point(size=4) +
  geom_abline()
  #scale_x_continuous("Relative traffic intensity", breaks=seq(.5, 5, .25)) +
  #scale_y_continuous("Best relative capacity", breaks=seq(0, 5, .1))
p

  

input.runtimes <- input.base %>%
                  full_join(expand.grid(userClass = input.base$userClass,
                                        runtime.fraction = fractions), by = "userClass") %>%
                  mutate(runtime.est=runtime.est * runtime.fraction)

output.runtimes <- input.runtimes %>%
  group_by(method, capacity.fraction, slo.scenario, runtime.fraction) %>%
  do(CalculateApproximationSummary(.)) %>%
  left_join(input.runtimes) %>%
  mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))

df.plot <- filter(output.runtimes, model == "G/GI/c/K")

p <- ggplot(df.plot, aes(runtime.fraction, ob.est, col=userClass)) +
  geom_line() +
  geom_point(size=4) +
  #facet_wrap(~userClass, ncol=1) +
  scale_x_continuous("Service time fraction", breaks=fractions) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1))
p
plot.file <- paste(plots.dir, "cp_servicetime_admissionrate.png", sep="/")
ggsave(plot.file, p, width=10, height=6.5)



input.capacities <- input.base %>%
  full_join(expand.grid(userClass = input.base$userClass,
                        capacity.fraction2 = fractions), by = "userClass") %>%
  mutate(capacity.rem.cpu.mean=capacity.rem.cpu.mean * capacity.fraction2)

output.capacities <- input.capacities %>%
  group_by(method, capacity.fraction2, slo.scenario) %>%
  do(CalculateApproximationSummary(.)) %>%
  left_join(input.capacities) %>%
  mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))

df.plot <- filter(output.capacities, model == "G/GI/c/K")

p <- ggplot(df.plot, aes(capacity.fraction2, ob.est, col=userClass)) +
  geom_line() +
  geom_point(size=4) +
  #facet_wrap(~userClass, ncol=1) +
  scale_x_continuous("Capacity fraction", breaks=fractions) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1))
p

fractions <- seq(.6, 50, .1)
frac.len <- length(fractions)
input.iatvar <- input.base %>%
  #full_join(data_frame(userClass=rep(input.base$userClass, rep(frac.len, 3)),
  #                     arrivalrate.fraction=rep(fractions, 3)),
  #          by = "userClass") %>%
  full_join(expand.grid(userClass = input.base$userClass,
                        iatvar.fraction = fractions), by = "userClass") %>%
  mutate(iat.n.var=iat.n.var * iatvar.fraction)

output.iatvar <- input.iatvar %>%
  group_by(method, capacity.fraction, slo.scenario, iatvar.fraction) %>%
  do(CalculateApproximationSummary(.)) %>%
  left_join(stats.f) %>%
  mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))

df.plot <- filter(output.iatvar, model == "G/GI/c/K")

p <- ggplot(df.plot, aes(iatvar.fraction, ob.est, col=userClass)) +
  geom_line() +
  geom_point(size=4) +
  #facet_wrap(~userClass, ncol=1) +
  #scale_x_continuous("Arrival rate fraction", breaks=fractions) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1))
p
plot.file <- paste(plots.dir, "cp_arrivalrate_admissionrate.png", sep="/")
ggsave(plot.file, p, width=10, height=6.5)


fractions <- seq(.5, 2, .1)
frac.len <- length(fractions)

input.slo.availability <- filter(stats.f, capacity.fraction == 1, method == "greedy-quota")

#input.slo.availability <- input.base %>%
#  full_join(expand.grid(userClass = input.base$userClass,
#                        slo.availability.fraction = fractions), by = "userClass") %>%
#  mutate(slo.availability=pmin(1, slo.availability * slo.availability.fraction))

output.slo.availability <- input.slo.availability %>%
  group_by(method, capacity.fraction, slo.scenario) %>%
  do(data.frame(CalculateApproximationSummary(.), best.capacity=FindBestCapacity(., 0, 10, .01))) %>%
  left_join(stats.f) %>%
  mutate(userClass=factor(userClass, levels=c("prod", "batch", "free")))

df.plot <- filter(output.slo.availability, model == "G/GI/c/K")

p <- ggplot(df.plot, aes(slo.availability, ob.est, col=userClass)) +
  geom_line() +
  geom_point(aes(y=ob.mean), size=4) +
  #facet_wrap(~userClass, ncol=1) +
  scale_x_continuous("Relative traffic intensity", breaks=seq(0, 2, .2), label=percent) +
  scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1)) +
  scale_color_discrete("Class") +
  facet_wrap(~userClass, scale="free_x") +
  theme(legend.position = "top")
p
plot.file <- paste(plots.dir, "cp_trafficintensity_admissionrate.png", sep="/")
ggsave(plot.file, p, width=6, height=3)



df.plot2 <- filter(output.capacities, model == "G/GI/c/K", between(capacity.fraction2, .5, 1.3)) %>%
           group_by(capacity.fraction2) %>%
           mutate(traffic.load = (arrivalrate.cpu * runtime.est) / rem.capacity.est)

df.plot <- filter(stats.agg, method == "pred-ets", slo.scenario == 1,
                  between(capacity.fraction, .5, 1.3))

p <- ggplot(df.plot, aes(capacity.fraction, capacity.rem.cpu.mean, col=userClass)) +
  geom_line() +
  geom_point(size=4) +
  geom_line(aes(capacity.fraction2, rem.capacity.est), data=df.plot2, lty=2) +
  facet_wrap(~userClass, ncol=1, scale="free_y") 
  #scale_x_continuous("Traffic load", breaks=fractions) +
  #scale_y_continuous("Admission rate", label=percent, breaks=seq(0, 1, .1))
p
  

input.base.prod <- filter(input.base, userClass == "prod")

for (uc in c("prod", "batch", "free")) {
  input.base.uc <- filter(input.base, userClass == uc)
  for (cf in seq(.5, 1.3, .1)) {
    nservers <- with(input.base.uc, capacity.rem.cpu.mean * cf)
    rho <- with(input.base.uc, rho.f(arrivalrate.cpu, 1/runtime.est, nservers))
    ca2 <- with(input.base.uc, scv.f(iat.n.mean, iat.n.var))
    cs2 <- with(input.base.uc, scv.f(runtime.var, runtime.mean))
    z <- z.f(ca2, cs2, 1)
    beta <- beta.f(nservers, rho)
    kappa <- with(input.base.uc, kappa.f(m = nservers / slo.availability - nservers, nservers))
    pw <- alpha.f(beta / sqrt(z), kappa / sqrt(z))
    pw.inf <- alpha.f(beta / sqrt(z))
    pbl <- pbl.f(kappa, z, rho, nservers, z, beta, 1, ca2, cs2)
    
    with(input.base.uc, print(paste(uc, cf, nservers, arrivalrate.cpu * runtime.est, rho, round(pw, 4), round(pw.inf, 4), round(pbl, 4))))
  }
}

BestCapacityDelayProbability <- function(demand, z, pw.target=.1, slo.availability=0, precision=1) {
  for (n in seq(demand, demand*10, precision)) {
    rho <- demand/n
    m <- n/slo.availability - n
    kappa <- kappa.f(m, n)
    beta <- beta.f(n, rho)
    print(paste(beta, rho))
    alpha <- alpha.f(beta/sqrt(z), kappa/sqrt(z))
    if (alpha <= pw.target) {
      return(n)
    }
  }
  return(NA)
}

}