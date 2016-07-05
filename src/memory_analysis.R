library(dplyr)
library(ggplot2)
library(foreach)
library(scales)
library(tidyr)
library(RColorBrewer)
library(grid)
library(gridExtra)
theme_set(theme_bw())

source("src/admission_control_utils.R")

res <- read.table("output/res_mem_is-300000000_forecast-ets-quota_cf-1_sc-1_cpuf-1_ac.txt",
                  header = T)

res %>%
  group_by(userClass) %>%
  summarise(cpu.utilization = mean(allocated.cpu / capacity.rem.cpu),
            memory.utilization = mean(allocated.mem / capacity.rem.mem),
            cpu.shortage = sum(allocated.cpu > capacity.rem.cpu) / n(),
            memory.shortage = sum(allocated.mem > capacity.rem.mem) / n())

res %>%
  summarise(cpu.utilization = mean(allocated.cpu / capacity.rem.cpu),
            memory.utilization = mean(allocated.mem / capacity.rem.mem),
            memory.shortage = sum(allocated.mem > capacity.rem.mem) / n(),
            memory.shortage.n = sum(allocated.mem > capacity.rem.mem))

plot.margin <- unit(c(1, 1, 1, 1), "mm")
  
ggplot(res, aes(time, capacity.rem.mem), col = 1, lty = 2) + 
  geom_step() + 
  facet_wrap(~ userClass, ncol = 1) + 
  geom_step(aes(y=allocated.mem), col = 2)

res <- LoadResultsFiles(list.files("output/", "res_mem_.*forecast-ets-quota_.*memcf-.*_withmem-no.*_ac.txt$", full.names = T))
res$consider.mem <- FALSE
res.withmem <- LoadResultsFiles(list.files("output/", "res_mem_.*forecast-ets-quota_.*memcf-.*_withmem-yes.*_ac.txt$", full.names = T))
res.withmem$consider.mem <- TRUE
res <- rbind(res, res.withmem)

res.agg <- res %>%
           #filter(mem.capacity.fraction >= 0.6 & mem.capacity.fraction <= 1.2) %>%
           group_by(mem.capacity.fraction, consider.mem, time) %>%
           summarise(cpu.capacity = max(capacity.rem.cpu),
                     mem.capacity = max(capacity.rem.mem),
                     cpu.allocated = sum(allocated.cpu),
                     mem.allocated = sum(allocated.mem),
                     cpu.utilization = cpu.allocated / cpu.capacity,
                     mem.utilization = mem.allocated / mem.capacity,
                     cpu.wastage = 1 - min(cpu.utilization, 1), 
                     mem.wastage = 1 - min(mem.utilization, 1))

p <- ggplot(res.agg, aes(time, mem.capacity, col = "capacity", lty = "capacity"), col = 1, lty = 2) + 
     geom_step() + 
     facet_grid(mem.capacity.fraction ~ consider.mem) + 
     geom_step(aes(y=mem.allocated, col = "allocated", lty = "allocated"))
p
ggsave("plots/ma_capacity_demand_time_by-mem-capacity-factor.png", p, width=6, height=8)

stats <- res %>%
  filter(method != "greedy-reject") %>%
  group_by(capacity.fraction, mem.capacity.fraction, consider.mem, slo.scenario, method, time, userClass) %>%
  mutate(availability=mean(allocated.cpu/(demand.cpu - rejected.cpu), na.rm=T),
         obtainability=mean(1 - rejected.cpu/arrivals.cpu, na.rm=T)) %>%
  group_by(capacity.fraction, mem.capacity.fraction, slo.scenario, method, time) %>%
  mutate(cum.allocated=cumsum(allocated.cpu),
         rem.capacity.pred=quota*slo.availability,
         rem.capacity.pred.error=capacity.rem.cpu-rem.capacity.pred,
         quota=pmax(0, quota)) %>%
  group_by(capacity.fraction, mem.capacity.fraction, slo.scenario, method, userClass) %>%
  mutate(cum.slo.fulfill=1 - cumsum(vm.slo.violated.n)/cumsum(departures.n),
         cum.av.mean=cummean(availability),
         cum.ob.mean=1 - cumsum(rejected.n)/cumsum(arrivals.n))

stats.agg <- stats %>%
  group_by(capacity.fraction, mem.capacity.fraction, consider.mem, slo.scenario, method, userClass, slo.availability) %>%
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

stats.agg.f <- stats.agg %>%
  filter(slo.scenario == 1, method != "greedy-reject") %>%
  mutate(vm.slo.fulfillment = ifelse(ob.mean < 0, NaN, vm.slo.fulfillment))

p <- ggplot(stats.agg.f, aes(mem.capacity.fraction, vm.slo.fulfillment, col=consider.mem,
                             pch=consider.mem, group=consider.mem)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Memory capacity factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("SLO fulfillment", breaks=seq(0, 1, .1), labels=percent) +
  #scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Multi-dimensional") +
  scale_color_discrete("Multi-dimensional") +
  facet_grid(~ userClass) +
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_slofulfillment_memcapacity_by-multidimension.png", p, width=8, height=4)

p <- ggplot(stats.agg.f, aes(mem.capacity.fraction, ob.mean, col=consider.mem, 
                             pch=consider.mem, group=consider.mem)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Memory capacity factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
  #scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Multi-dimensional") +
  scale_color_discrete("Multi-dimensional") +
  facet_grid(~ userClass) +
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_admissionrate_memcapacity_by-multidimension.png", p, width=8, height=4)


stats.agg.g <- gather(stats.agg.f, metric, value, vm.slo.fulfillment, ob.mean) %>%
  mutate(metric=factor(metric, levels=c("vm.slo.fulfillment", "ob.mean"),
                       labels=c("SLO fulfillment", "Admission rate")))

p <- ggplot(filter(stats.agg.g, consider.mem == F), 
            aes(mem.capacity.fraction, value, col=metric, pch=metric, group=metric)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Memory capacity factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Admission rate", breaks=seq(0, 1, .1), labels=percent) +
  #scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Multi-dimensional") +
  scale_color_discrete("Multi-dimensional") +
  facet_grid(~ userClass) +
  theme(plot.margin=plot.margin)
p



stats.all <- stats %>%
  filter(method != "greedy-reject") %>%
  group_by(capacity.fraction, mem.capacity.fraction, consider.mem, slo.scenario, method, time) %>%
  summarise(utilization=sum(allocated.cpu)/max(capacity.rem.cpu),
            utilization.mem=sum(allocated.mem)/max(capacity.rem.mem),
            slo.violated.n=sum(vm.slo.violated.n), departures.n=sum(departures.n),
            av.mean=mean(availability, na.rm=T),
            admission.rate=sum(arrivals.n - rejected.n) / sum(arrivals.n),
            arrivals.n=sum(arrivals.n)) %>%
  summarise(utilization.mean=mean(utilization), utilization.sd=sd(utilization),
            utilization.mem.mean=mean(utilization.mem), utilization.sd=sd(utilization),
            utilization.error=qnorm(.975) * utilization.sd / sqrt(n()),
            utilization.lower=utilization.mean - utilization.error,
            utilization.upper=utilization.mean + utilization.error,
            slo.fulfillment=weighted.mean(1 - slo.violated.n/departures.n,
                                          departures.n, na.rm=T),
            admission.rate=weighted.mean(admission.rate, arrivals.n, na.rm=T),
            av.mean=weighted.mean(admission.rate, arrivals.n, na.rm=T)) %>%
  mutate(slo.scenario.str=factor(slo.scenario, levels=c(3, 2, 1, 4, 5),
                                 labels=c("very-low", "low", "medium", "high", "very-high")))

stats.all.f <- filter(stats.all, capacity.fraction == 1)


p <- ggplot(stats.all.f,
            aes(mem.capacity.fraction, utilization.mem.mean, col=consider.mem, pch=consider.mem,
                group=consider.mem)) +
  geom_line(alpha=.3) +
  #geom_errorbar(aes(ymin=utilization.lower, ymax=utilization.upper), width=.5, alpha=.6) + 
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Memory capacity factor") +
  scale_y_continuous("Memory utilization", labels=percent) +
  scale_shape("Multi-dimensional") +
  scale_color_discrete("Multi-dimensional") +
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_utilization_memcapacity_by-multidimension.png", p, width=8, height=4)


stats.all.g <- gather(stats.all.f, metric, value, slo.fulfillment, utilization.mem.mean) %>%
  mutate(metric=factor(metric, levels=c("slo.fulfillment", "utilization.mem.mean"),
                       labels=c("SLO fulfillment", "Utilization")))

p <- ggplot(filter(stats.all.g, consider.mem == F, mem.capacity.fraction >= 0.7), 
            aes(mem.capacity.fraction, value)) +
  geom_line(alpha=.3) +
  geom_point(size=4, alpha=.8) +
  scale_x_continuous("Memory capacity factor", breaks=seq(0, 2, .1)) +
  scale_y_continuous("Metric value", breaks=seq(0, 1, .1), labels=percent) +
  #scale_color_brewer("Heuristic:", palette="Spectral") +
  scale_shape("Multi-dimensional") +
  scale_color_discrete("Multi-dimensional") +
  facet_grid(~ metric) +
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_slofulfillment_slofulfillment_memcapacity_singleresource.png", p, width=8, height=4)



### Old analysis

res.files <- paste("output/res_mem_is-300000000_forecast-ets-quota_cf-1_sc-1_cpuf-1_memcf-",
                   seq(0.5, 1.2, .1), "_ac.txt", sep = "")


res <- LoadResultsFiles(res.files)
res$consider.mem <- "no"

res.agg <- res %>%
  #filter(mem.capacity.fraction >= 0.6 & mem.capacity.fraction <= 1.2) %>%
  group_by(mem.capacity.fraction, consider.mem, time) %>%
  summarise(cpu.capacity = max(capacity.rem.cpu),
            mem.capacity = max(capacity.rem.mem),
            cpu.allocated = sum(allocated.cpu),
            mem.allocated = sum(allocated.mem),
            cpu.utilization = cpu.allocated / cpu.capacity,
            mem.utilization = mem.allocated / mem.capacity,
            cpu.wastage = 1 - min(cpu.utilization, 1), 
            mem.wastage = 1 - min(mem.utilization, 1))

res.agg.stats <- res.agg %>%
                 group_by(mem.capacity.fraction, consider.mem) %>%
                 summarise(cpu.utilization.mean = mean(cpu.utilization),
                           mem.utilization.mean = mean(mem.utilization),
                           cpu.wastage.mean = mean(cpu.wastage),
                           mem.wastage.mean = mean(mem.wastage),
                           cpu.shortage = mean(pmax(0, cpu.allocated - cpu.capacity) / cpu.capacity),
                           mem.shortage = mean(pmax(0, mem.allocated - mem.capacity) / mem.capacity),
                           cpu.shortage.time = sum(cpu.utilization > 1) / n(),
                           mem.shortage.time = sum(mem.utilization > 1) / n())
                           
p <- ggplot(filter(res.agg.stats, mem.capacity.fraction <= 1.2), aes(x = mem.capacity.fraction)) +
     geom_point(aes(y = mem.shortage, col = "shortage", pch = "shortage"), size = 4, alpha = .8) +
     geom_line(aes(y = mem.shortage, col = "shortage"), alpha = .5) +
     geom_point(aes(y = mem.wastage.mean, col = "wastage", pch = "wastage"), size = 4, alpha = .8) +
     geom_line(aes(y = mem.wastage.mean, col = "wastage"), alpha = .5) +
     scale_y_continuous("Memory shortage and wastage (% capacity)", labels = percent) +
     scale_shape("Memory") +
     scale_color_discrete("Memory") +
     scale_x_continuous("Memory capacity fraction")
ggsave("plots/ma_shortage_wastage_mem-capacity-factor.png", p, width=6, height=4)


res.agg <- res %>%
  filter(mem.capacity.fraction >= 0.6 & mem.capacity.fraction <= 1.2) %>%
  group_by(mem.capacity.fraction, time) %>%
  summarise(cpu.capacity = max(capacity.rem.cpu),
            mem.capacity = max(capacity.rem.mem),
            cpu.allocated = sum(allocated.cpu),
            mem.allocated = sum(allocated.mem),
            cpu.utilization = cpu.allocated / cpu.capacity,
            mem.utilization = mem.allocated / mem.capacity,
            cpu.wastage = 1 - min(cpu.utilization, 1), 
            mem.wastage = 1 - min(mem.utilization, 1))

p <- ggplot(filter(res.agg, mem.capacity.fraction == 1),
            aes(time / 12 / 24, mem.capacity, col = "capacity", lty = "capacity"), col = 1, lty = 2) + 
  geom_step() + 
  #facet_wrap(~mem.capacity.fraction, ncol = 1) +
  geom_step(aes(y=mem.allocated, col = "allocated", lty = "allocated")) +
  scale_color_discrete("Memory") +
  scale_linetype("Memory") +
  scale_x_continuous("Time (days)") +
  scale_y_continuous("Memory allocation and capacity (normalized)") + 
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_withoutmem_capacity_demand_time_by-mem-capacity-factor.png", p, width=8, height=5)


res.agg.stats <- res.agg %>%
  group_by(mem.capacity.fraction) %>%
  summarise(cpu.utilization.mean = mean(cpu.utilization),
            mem.utilization.mean = mean(mem.utilization),
            cpu.wastage.mean = mean(cpu.wastage),
            mem.wastage.mean = mean(mem.wastage),
            cpu.shortage = mean(pmax(0, cpu.allocated - cpu.capacity) / cpu.allocated),
            mem.shortage = mean(pmax(0, mem.allocated - mem.capacity) / mem.allocated),
            cpu.shortage.time = sum(cpu.utilization > 1) / n(),
            mem.shortage.time = sum(mem.utilization > 1) / n())

p <- ggplot(filter(res.agg.stats, mem.capacity.fraction <= 1.2), aes(x = mem.capacity.fraction)) +
  geom_point(aes(y = mem.shortage, col = "shortage", pch = "shortage"), size = 4, alpha = .8) +
  geom_line(aes(y = mem.shortage, col = "shortage"), alpha = .5) +
  geom_point(aes(y = mem.wastage.mean, col = "wastage", pch = "wastage"), size = 4, alpha = .8) +
  geom_line(aes(y = mem.wastage.mean, col = "wastage"), alpha = .5) +
  scale_y_continuous("Memory shortage (% allocation) and wastage (% capacity)", labels = percent) +
  scale_shape("Memory") +
  scale_color_discrete("Memory") +
  scale_x_continuous("Memory capacity factor (fraction of original capacity)",
                     breaks = seq(.5, 1.2, .1)) +
  theme(plot.margin=plot.margin)
p
ggsave("plots/ma_withoutmem_shortage_wastage_mem-capacity-factor.png", p, width=8, height=5)
