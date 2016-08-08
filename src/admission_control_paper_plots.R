# Generate plots from the simulation results. It expects that all scenarios were
# executed and the results are found in the "./output" directory. The output plots
# are saved in the "./plots" directory.
library(dplyr)
library(ggplot2)
library(foreach)
library(scales)
library(tidyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
theme_set(theme_bw())

base.dir <- "."

source("src/admission_control_utils.R")

CalculateStatisticsPerTime <- function(stats) {
  stats <- stats %>%
    filter(method != "greedy-reject") %>%
    group_by(cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, consider.mem, method, time, userClass) %>%
    mutate(availability=mean(allocated.cpu/(demand.cpu - rejected.cpu), na.rm=T),
           obtainability=mean(1 - rejected.cpu/arrivals.cpu, na.rm=T)) %>%
    group_by(cpu.capacity.factor, slo.scenario, method, time) %>%
    mutate(cum.allocated=cumsum(allocated.cpu),
           rem.capacity.pred=quota*slo.availability,
           rem.capacity.pred.error=capacity.rem.cpu-rem.capacity.pred,
           quota=pmax(0, quota)) %>%
    group_by(cpu.capacity.factor, slo.scenario, method, userClass) %>%
    mutate(cum.slo.fulfill=1 - cumsum(vm.slo.violated.n)/cumsum(departures.n),
           cum.av.mean=cummean(availability),
           cum.ob.mean=1 - cumsum(rejected.n)/cumsum(arrivals.n))
  return(stats)
}

CalculateStatisticsPerClass <- function(stats) {
  stats.class <- stats %>%
    group_by(cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, consider.mem, method, userClass, slo.availability) %>%
    summarise(capacity.rem.cpu.mean=mean(capacity.rem.cpu),
              demand.cpu.mean=mean(demand.cpu),
              nqueue.cpu.mean=mean(demand.cpu-allocated.cpu),
              av.mean=mean(availability, na.rm=T),
              av.median=median(availability, na.rm=T),
              av.q25=quantile(availability, .25, na.rm=T),
              av.q75=quantile(availability, .75, na.rm=T),
              av.min=min(availability),
              av.max=max(availability),
              ob.mean=sum(arrivals.n - rejected.n) / sum(arrivals.n),
              vm.availability=weighted.mean(vm.availability.mean, departures.n),
              vm.slo.fulfillment=weighted.mean(1 - vm.slo.violated.n/departures.n,
                                               departures.n, na.rm=T)) %>%
    mutate(av.mean = ifelse(ob.mean >= 0, av.mean, NaN),
           slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                   labels=c("very low", "low", "medium", "high", "very high")))
  return(stats.class)
}

CalculateOverallStatistics <- function(stats) {
  stats.all <- stats %>%
    filter(method != "greedy-reject") %>%
    group_by(cpu.capacity.factor, mem.capacity.factor, cpu.load.factor, mem.load.factor,
             slo.scenario, consider.mem, method, time) %>%
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
    
    
  return(stats.all)
}

PlotResults <- function(results.files=list.files("output", "res_.*_ac.csv$",
                                                 full.names=T),
                        plots.dir="./plots", cpu.capacity.inf = 10, mem.capacity.inf = 10) {
  
  stats <- LoadResultsFiles(results.files)
  stats <- CalculateStatisticsPerTime(stats)
  stats.class <- CalculateStatisticsPerClass(stats)
  stats.all <- CalculateOverallStatistics(stats)
  stats.all.gather <- gather(stats.all, "metric.name", "metric.value", slo.fulfillment, admission.rate) %>%
                      mutate(metric.name=factor(metric.name, levels=c("slo.fulfillment", "admission.rate"),
                                                labels=c("Availability SLO fulfillment", "Admission rate")))
  
  plot.height <- 3.5
  plot.width <- 6
  plot.margin <- unit(c(1, 1, 1, 1), "mm")
  dodge <- position_dodge(width=.9)
  
  df.plot <- filter(stats.class, cpu.capacity.factor == 1, mem.capacity.factor == 1,
                            cpu.load.factor = 1, mem.load.factor = 1, slo.scenario = 1,
                            consider.mem = T, method != "greedy-reject") %>%
             gather(metric, value, vm.slo.fulfillment, ob.mean) %>%
             mutate(metric=factor(metric, levels=c("vm.slo.fulfillment", "ob.mean"),
                                  labels=c("SLO fulfillment", "Admission rate")))
          
  p <- ggplot(df.plot, aes(method, value, fill=method)) +
    geom_bar(stat="identity", width = .8) +
    #geom_text(aes(label=sprintf("%1.1f%%", 100*value)), size=3, vjust=-.2) +
    geom_text(aes(label=paste(round(100*value, 1), "%", sep="")), size=3, vjust=-.3) +
    scale_y_continuous("Availability SLO fulfillment", breaks=seq(0, 1, .1), limits=c(0, 1.05), labels=percent) +
    scale_fill_brewer(palette="Spectral") +
    facet_grid(metric~userClass) +
    theme(axis.title.x = element_blank(), legend.position = "none",
          axis.text.x = element_text(angle = 25, hjust=1), plot.margin=plot.margin,
          strip.text.y = element_blank())
  p
  plot.file <- paste(plots.dir, "ac_slo-fulfillment_admission-rate_cf1.png", sep="/")
  png(plot.file, width=7.5, height=5.5, units="in", res=300)
  g <- ggplotGrob(p)
  yax <- which(g$layout$name=="ylab")
  g[["grobs"]][[yax]]$label <- c("Admission rate", "Availability SLO fulfillment")
  g[["grobs"]][[yax]]$y <- grid::unit(c(0.25, 0.75), "npc")
  grid.newpage()
  grid.draw(g)
  dev.off()
  
  
  df.plot <- filter(df.plot, cpu.capacity.factor %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3),
                    mem.capacity.factor == 1, cpu.load.factor == 1, mem.load.factor == 1,
                    slo.scenario == 1, consider.mem == T)
  
  p <- ggplot(df.plot, aes(cpu.capacity.factor, metric.value, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("Metric value", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ metric.name) +
    theme(panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_metrics_cfs_agg.png", sep="/")
  ggsave(plot.file, p, width=7.5, height=3)
  
  df.plot <- stats.all.gather %>% 
             filter(cpu.capacity.factor %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3),
                    mem.capacity.factor == 1, cpu.load.factor == 1, mem.load.factor == 1,
                    slo.scenario == 1, consider.mem == T)
  
  p <- ggplot(df.plot, aes(cpu.capacity.factor, utilization.mean, col=method, pch=method,
                           group=method)) +
    geom_line(alpha=.3) +
    #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.7, alpha=.6) + 
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("Capacity size factor") +
    scale_y_continuous("Mean cloud utilization", breaks=seq(0, 1, .02), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_utilization_cfs.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.5)
  
  
  
  p <- ggplot(filter(df.plot, capacity.fraction == 1, cpureq.factor == 1),
              aes(slo.scenario.str, metric.value, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("SLO strength") +
    scale_y_continuous("Metric value", breaks=seq(0, 1, .05), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ metric.name) +
    theme(panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_metrics_slos_agg.png", sep="/")
  ggsave(plot.file, p, width=7.5, height=3)
  
  p <- ggplot(filter(ac.res.all, capacity.fraction == 1, cpureq.factor == 1),
              aes(slo.scenario.str, utilization.mean, col=method, pch=method,
                  group=method)) +
    geom_line(alpha=.3) +
    #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.5, alpha=.6) + 
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("SLO Strength") +
    scale_y_continuous("Mean cloud utilization", breaks=seq(.9, 1, .005), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_utilization_slos.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.5)
  
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
    theme(panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_metrics_loadfactor_agg.png", sep="/")
  ggsave(plot.file, p, width=7.5, height=3)
  
  
  p <- ggplot(filter(ac.res.all, slo.scenario == 1, capacity.fraction == 1,
                     cpureq.factor %in% c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)),
              aes(cpureq.factor, utilization.mean, col=method, pch=method,
                  group=method)) +
    geom_line(alpha=.3) +
    #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.5, alpha=.6) + 
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Load factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("Mean cloud utilization", breaks=seq(.7, 1, .02), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_utilization_loadfactor.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.5)

}

PlotResultsOld <- function(results.files=list.files("output", "res_.*_ac.txt$",
                                                 full.names=T),
                        plots.dir="./plots") {
  
  stats <- LoadResultsFiles(results.files)
  stats <- CalculateGeneralStatistics(stats)
  stats.class <- CalculateStatisticsPerClass(stats)
  
  plot.height <- 3.5
  plot.width <- 6
  plot.margin <- unit(c(1, 1, 1, 1), "mm")
  
  # Base scenario analysis
  
  stats.class.cf1 <- filter(stats.class, cpu.capacity.factor == 1, slo.scenario == 1,
                          method != "greedy-reject")
  p <- ggplot(stats.class.cf1, aes(method, ob.mean, fill=method)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=sprintf("%1.1f%%", 100*ob.mean)), size=2.8, vjust=0) +
    scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
    scale_fill_brewer(palette="Spectral") +
    facet_wrap(~userClass) +
    theme(axis.title.x = element_blank(), legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust=.95), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_obtainability_cf1.png", sep="/")
  ggsave(plot.file, p, width=6, height=plot.height)
  
  p <- ggplot(stats.class.cf1, aes(method, vm.slo.fulfillment, fill=method)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=sprintf("%1.1f%%", 100*vm.slo.fulfillment)), size=2.8, vjust=0) +
    scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .1), labels=percent) +
    scale_fill_brewer(palette="Spectral") +
    facet_wrap(~userClass) +
    theme(axis.title.x = element_blank(), legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust=.95), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_availability_cf1.png", sep="/")
  ggsave(plot.file, p, width=6, height=plot.height)
  
  stats.class.cf1.g <- gather(stats.class.cf1, metric, value, vm.slo.fulfillment, ob.mean) %>%
                     mutate(metric=factor(metric, levels=c("vm.slo.fulfillment", "ob.mean"),
                                          labels=c("SLO fulfillment", "Admission rate")))
  p <- ggplot(stats.class.cf1.g, aes(method, value, fill=method)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=sprintf("%1.1f%%", 100*value)), size=2.8, vjust=-.2) +
    scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .1), limits=c(0, 1.05), labels=percent) +
    scale_fill_brewer("Heuristic:", palette="Spectral") +
    facet_grid(metric~userClass) +
    #theme(axis.title.x = element_blank(), legend.position = "none",
    theme(axis.title.x = element_blank(), legend.position = "top",
          axis.text.x = element_text(angle = 30, hjust=.95), plot.margin=plot.margin,
          strip.text.y = element_blank())
  
  plot.file <- paste(plots.dir, "ac_slo-fulfillment_admission-rate_cf1.png", sep="/")
  #png(plot.file, width=6, height=4.5, units="in", res=300)
  png(plot.file, width=6, height=5, units="in", res=300)
  g <- ggplotGrob(p)
  yax <- which(g$layout$name=="ylab")
  g[["grobs"]][[yax]]$label <- c("Admission rate", "SLO fulfillment")
  g[["grobs"]][[yax]]$y <- grid::unit(c(0.25, 0.75), "npc")
  grid.newpage()
  grid.draw(g)
  dev.off()
  
  # Capacity planning analysis
  
  stats.class.f <- stats.class %>%
                 filter(slo.scenario == 1, method != "greedy-reject") %>%
                 mutate(vm.slo.fulfillment = ifelse(ob.mean < 0, NaN, vm.slo.fulfillment))
  
  p <- ggplot(stats.class.f, aes(cpu.capacity.factor, ob.mean, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ userClass) +
    theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_obtainability_cfs.png", sep="/")
  ggsave(plot.file, p, width=6, height=plot.height)

  p <- ggplot(stats.class.f, aes(cpu.capacity.factor, vm.slo.fulfillment, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ userClass) +
    theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_availability_cfs.png", sep="/")
  ggsave(plot.file, p, width=6, height=plot.height)

  
  # Profit analysis
  
  profit <- stats %>%
    filter(method != "greedy-reject") %>%
    mutate(revenue = allocated.cpu * GetRevenueRate(userClass),
           penalty = vm.slo.violated.cputime * GetAvailabilityPenaltyRate(userClass),
           revenue.cum = cumsum(revenue), penalty.cum = cumsum(penalty), 
           profit.cum = cumsum(revenue - penalty))
  
  profit.agg <- profit %>%
    group_by(cpu.capacity.factor, slo.scenario, method) %>%
    summarise(revenue.total = sum(revenue, na.rm=T), penalty.total = sum(penalty, na.rm=T),
              profit = revenue.total - penalty.total,
              profit.positive = ifelse(profit > 0, profit, 0)) %>%
    group_by(slo.scenario, cpu.capacity.factor) %>%
    mutate(profit.min=min(profit.positive), profit.max=max(profit.positive),
           profit.efficiency=ifelse(profit.min != profit.max,
                                    (profit.positive - profit.min) / (profit.max - profit.min),
                                    1))
  
  profit.agg.noreject <- profit.agg %>%
    filter(method == "no-adm-ctrl") %>%
    select(cpu.capacity.factor, slo.scenario, method, profit) %>%
    rename(method.norej = method, profit.noreject = profit)
  
  profit.agg.rel <- profit.agg %>%
    left_join(profit.agg.noreject,
              by=c("cpu.capacity.factor", "slo.scenario")) %>%
    mutate(profit.rel = (profit - profit.noreject) / profit.noreject,
           profit.rel.efficiency = (profit) / (profit.max),
           profit.abs = ifelse(profit > 0, profit, -1),
           cpu.capacity.factor.str = paste("CSF: ", cpu.capacity.factor, sep=""))
  
  
  # SLO scenarios analysis
  
  stats.all <- stats %>%
    filter(method != "greedy-reject") %>%
    group_by(cpu.capacity.factor, slo.scenario, method, time) %>%
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
  
  dodge <- position_dodge(width=.9)
  p <- ggplot(filter(stats.all, cpu.capacity.factor == 1),
              aes(slo.scenario.str, utilization.mean, col=method, pch=method,
                                                             group=method)) +
    geom_line(alpha=.3) +
    #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.5, alpha=.6) + 
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("SLO Strength") +
    scale_y_continuous("Mean cloud utilization", breaks=seq(.9, 1, .005), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_utilization_slos.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
  
  p <- ggplot(filter(stats.class, cpu.capacity.factor == 1),
              aes(slo.scenario.str, vm.slo.fulfillment, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("SLO Strength") +
    scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .05), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ userClass) +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_availability_slos.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
  
  p <- ggplot(filter(stats.class, cpu.capacity.factor == 1, userClass != "prod"),
              aes(slo.scenario.str, ob.mean, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("SLO strength") +
    scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    facet_grid(~ userClass) +
    theme(legend.position = "top", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_obtainability_slos.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
  
  
  p <- ggplot(filter(stats.all, slo.scenario == 1),
              aes(factor(cpu.capacity.factor), utilization.mean, col=method, pch=method,
                  group=method)) +
    geom_line(alpha=.3) +
    #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.7, alpha=.6) + 
    geom_point(size=4, alpha=.8) +
    scale_x_discrete("Capacity size factor") +
    scale_y_continuous("Mean cloud utilization", breaks=seq(0, 1, .02), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    theme(legend.position = "right", plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_utilization_cfs.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
  
  
  p <- ggplot(filter(stats.all, slo.scenario == 1),
              aes(cpu.capacity.factor, slo.fulfillment, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    #facet_grid(~ userClass) +
    theme(legend.position = "none", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_slo-fulfillment_cfs_agg.png", sep="/")
  ggsave(plot.file, p, width=2.3, height=3)
  
  p <- ggplot(filter(stats.all, slo.scenario == 1),
              aes(cpu.capacity.factor, admission.rate, col=method, pch=method, group=method)) +
    geom_line(alpha=.3) +
    geom_point(size=4, alpha=.8) +
    scale_x_continuous("Capacity size factor", breaks=seq(0, 2, .1)) +
    scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
    scale_color_brewer("Heuristic:", palette="Spectral") +
    scale_shape("Heuristic:") +
    #facet_grid(~ userClass) +
    theme(legend.position = "right", panel.grid.minor.x = element_blank(), plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_admission-rate_cfs_agg.png", sep="/")
  ggsave(plot.file, p, width=3.7, height=3)
  
  
  stats.all.g <- gather(stats.all, "metric.name", "metric.value", slo.fulfillment, admission.rate) %>%
                 mutate(metric.name=factor(metric.name, levels=c("slo.fulfillment", "admission.rate"),
                                           labels=c("SLO fulfillment", "Admission rate")))
  
  p <- ggplot(filter(stats.all.g, slo.scenario == 1),
              aes(cpu.capacity.factor, metric.value, col=method, pch=method, group=method)) +
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
  
  
  p <- ggplot(filter(stats.all.g, cpu.capacity.factor == 1),
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
  
}

PlotProfitResults <- function(vm.summary.files=list.files("output", "res_.*_vm-avail-summary.txt$",
                                                       full.names=T),
                              plots.dir="./plots") {

  #vm.summary.files <- paste(results.files, "_vm-avail-summary.txt", sep="")
  vm.summary <- LoadResultsFiles(vm.summary.files)
  
  vm.summary <- vm.summary %>%
    filter(method != "greedy-reject") %>%
    mutate(vm.profit.total=vm.revenue.total - vm.penalty.total)
  
  vm.summary.agg <- vm.summary %>%
    group_by(cpu.capacity.factor, slo.scenario, method) %>%
    summarise(revenue=sum(vm.revenue.total),
              penalty=sum(vm.penalty.total),
              profit=sum(vm.profit.total)) %>%
    group_by(cpu.capacity.factor, slo.scenario) %>%
    mutate(profit.max=max(profit), profit.efficiency=profit / profit.max,
           slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                   labels=c("very-low", "low", "medium", "high", "very-high")),
           cpu.capacity.factor.str=paste("CSF: ", cpu.capacity.factor, sep=""))
  
  p <- ggplot(filter(vm.summary.agg, slo.scenario == 1),
              aes(cpu.capacity.factor, fill=method)) +
    #geom_hline(aes(yintercept=c(0, 1)), lty=2, col="grey") +
    geom_bar(aes(y=profit.efficiency), stat="identity", position="dodge", width=.08) +
    scale_x_continuous("Capacity size factor", breaks=seq(0.7, 1.3, .1)) +
    scale_y_continuous("Profit efficiency", breaks=seq(0, 1, .1), labels=percent) +
    scale_fill_brewer("Heuristic:", palette="Spectral") +
    #scale_fill_manual(values = brewer.pal(4, "Spectral")[2:4]) +
    coord_trans(limy=c(-.05, 1.05)) +
    #facet_grid(~cpu.capacity.factor.str) +
    theme(legend.position = "right",
          #axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_profit_cfs.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
  
  p <- ggplot(filter(vm.summary.agg, cpu.capacity.factor == 1),
              aes(slo.scenario.str, fill=method)) +
    #geom_hline(aes(yintercept=c(0, 1)), lty=2, col="grey") +
    geom_bar(aes(y=profit.efficiency), stat="identity", position="dodge", width=.8) +
    scale_x_discrete("SLO Strength") +
    scale_y_continuous("Profit efficiency", breaks=seq(0, 1, .1), labels=percent) +
    scale_fill_brewer("Heuristic:", palette="Spectral") +
    #scale_fill_manual(values = brewer.pal(4, "Spectral")[2:4]) +
    coord_trans(limy=c(-.05, 1.05)) +
    #facet_grid(~cpu.capacity.factor.str) +
    theme(legend.position = "right",
          #axis.text.x = element_blank(), axis.title.x = element_blank(),
          plot.margin=plot.margin)
  p
  plot.file <- paste(plots.dir, "ac_profit_slos.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.2)
  
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

Main <- function(argv) {
  results.files <- argv  
  plot.profit.analysis <- TRUE
  
  PlotResults(results.files = results.files, plots.dir = "./plots/")
  if (plot.profit.analysis) {
    vm.summary.files <- paste(results.files, "_vm-avail-summary.csv", sep="")
    PlotProfitResults(vm.summary.files, plots.dir = "./plots/")
  }
}

argv <- commandArgs(TRUE)
if (length(argv) > 0) {
  Main(argv)
}