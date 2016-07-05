library(dplyr)
library(ggplot2)
library(forecast)
library(RSQLite)
library(GGally)
library(scales)
theme_set(theme_bw())

base.dir <- "~/local/admission-control-model/"
setwd(base.dir)

source("src/admission_control_utils.R")
source("src/admission_control_paper_plots.R")

plots.dir <- paste(base.dir, "/tese-plots/", sep="")

tasks <- LoadTaskEvents("output/gtrace_data.sqlite3", from.sqlite=T)
capacity <- LoadTotalCapacity("output/gtrace_total_capacity.txt")

tasks.l <- filter(tasks, runtime > 0) %>%
           collect() %>%
           mutate(userClass = factor(userClass, levels = c("prod", "middle", "free"),
                                     labels = c("prod", "batch", "free")))

jobs.l <- tasks.l %>%
          group_by(userClass, jid) %>%
          summarise(ntasks = n(), runtime.mean = mean(runtime), cpuReq.mean = mean(cpuReq))

second <- 1000000
minute <- 60 * second
window.size <- 5 * minute
hour <- 60 * minute
day <- 24 * hour

capacity.perhour <- capacity %>%
                    group_by(hour=timeWindow/hour) %>%
                    summarise(capacity.cpu=min(capacity.cpu))

tasks.cdf <- tasks.l %>%
             filter(runtime > 0) %>%
             group_by(userClass) %>%
             do(data_frame(q = seq(0, 1, .005), cpuReq.x = quantile(.$cpuReq, probs = q),
                           runtime.x = quantile(.$runtime/hour, probs = q),
                           iat.x = quantile(diff(filter(., submitTime > 0)$submitTime/second), probs = q)))


####################################################################################
# Arrival rates and inter-arrival times
####################################################################################

arrivals.perhour <- tasks.l %>%
                    filter(submitTime > 0) %>%
                    group_by(userClass, hour=ceiling(submitTime/hour)) %>%
                    summarise(cpu=sum(cpuReq), n=n())

tasks.iat <- tasks.l %>%
             filter(submitTime > 0) %>%
             group_by(userClass) %>%
             transmute(iat = c(NA, diff(submitTime)))

p <- ggplot(arrivals.perhour, aes(hour / 24, cpu)) +
     geom_bar(stat = "identity", width = 1 / 24) +
     scale_x_continuous("Tempo (dias)", breaks = seq(0, 30, 7)) +
     scale_y_continuous("Taxa de chegada (CPU normalizada por hora)") +
     facet_wrap(~userClass, ncol = 1, scales = "free_y")
p
plot.file <- paste(plots.dir, "wc_taxachegada_tempo.png", sep = "/")
ggsave(plot.file, p, width=7.5, height=6)

tasks.iat %>%
  filter(!is.na(iat)) %>%
  group_by(userClass) %>%
  do(SummaryStatistics(.$iat / second))

arrivals.perhour %>%
  group_by(userClass) %>%
  do(SummaryStatistics(.$cpu))



p <- ggplot(arrange(tasks.cdf, q), aes(iat.x, q, col = userClass, lty = userClass)) +
     geom_step() +
     scale_x_log10("Intervalo entre chegadas (segundos)", breaks = 10^(-5:5),
                labels = trans_format("log10", math_format(10^.x))) +
     scale_y_continuous("Fração acumulada P[X <= x]", labels = percent, breaks = seq(0, 1, .1)) +
     scale_color_discrete("Classe") +
     scale_linetype("Classe")
p
plot.file <- paste(plots.dir, "wc_iat_ecdf.png", sep = "/")
ggsave(plot.file, p, width=6, height=3)



####################################################################################
# CPU requested
####################################################################################

p <- ggplot(arrange(tasks.cdf, q), aes(cpuReq.x, q, col = userClass, lty = userClass)) +
  geom_step() +
  scale_x_continuous("Capacidade requisitada por tarefa (CPU normalizada)") +
  scale_y_continuous("Fração acumulada P[X <= x]", labels = percent, breaks = seq(0, 1, .1)) +
  scale_color_discrete("Classe") +
  scale_linetype("Classe")
plot.file <- paste(plots.dir, "wc_cpureq_ecdf.png", sep = "/")
ggsave(plot.file, p, width=6, height=3)

tasks.l %>%
  group_by(userClass) %>%
  do(SummaryStatistics(.$cpuReq))


####################################################################################
# Task runtime
####################################################################################

p <- ggplot(tasks.cdf, aes(runtime.x*60, q, col = userClass, lty = userClass)) +
  geom_step() +
  scale_x_log10("Tempo de execução (minutos)", breaks = 10^(-8:5),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous("Fração acumulada P[X <= x]", labels = percent, breaks = seq(0, 1, .1)) +
  scale_color_discrete("Classe") +
  scale_linetype("Classe")
plot.file <- paste(plots.dir, "wc_runtime_ecdf.png", sep = "/")
ggsave(plot.file, p, width=6, height=3)

tasks.l %>%
  group_by(userClass) %>%
  do(SummaryStatistics(.$runtime / minute))


####################################################################################
# Tasks per job
####################################################################################

p <- ggplot(jobs.l, aes(ntasks, col = userClass, lty = userClass)) +
  stat_ecdf() +
  scale_color_discrete("Classe") +
  scale_linetype("Classe") +
  scale_x_log10("Quantidade de tarefas por job", breaks = 10^(0:6),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous("Fração acumulada P[X <= x]", labels = percent, breaks = seq(0, 1, .1))
p
plot.file <- paste(plots.dir, "wc_ntasks_ecdf.png", sep = "/")
ggsave(plot.file, p, width = 6, height = 3)  

jobs.l %>%
  group_by(userClass) %>%
  summarise(sum(ntasks == 1) / n())


####################################################################################
# Cloud demand
####################################################################################

res.files <- list.files("output", "res55_is-300000000.*ac.txt$", full.names=T)
res.in <- LoadResultsFiles(res.files)

res.cf13 <- res.in %>%
            filter(capacity.fraction == 1.3, slo.scenario == 1, cpureq.factor == 1,
                   method == "greedy-reject")
                  

p <- ggplot(res.cf13, aes(factor(time / 12 / 24), demand.cpu)) +
     geom_bar(aes(col = userClass, fill = userClass), stat = "identity", position = "stack") +
     #geom_point(aes(factor(timeWindow / day), capacity.cpu, shape = "total"), size = .5,
     #           data = capacity) +
     scale_color_brewer("Classe", palette="Spectral", guide = guide_legend(reverse=TRUE)) +
     scale_fill_brewer("Classe", palette="Spectral", guide = guide_legend(reverse=TRUE)) +
     #scale_shape_manual("Disponível", values = 15) +
     #scale_x_continuous("Tempo (dias)", breaks = seq(0, 30, 5)) +
     scale_x_discrete("Tempo", breaks = NULL) +
     scale_y_continuous("Capacidade total (CPU normalizada)")
p
plot.file <- paste(plots.dir, "wc_demanda_capacidade_tempo.png", sep = "/")
ggsave(plot.file, p, width=7.5, height=4)

res.cf13 %>%
  group_by(userClass) %>%
  do(SummaryStatistics(.$demand.cpu))

res.cf13.agg <- res.cf13 %>%
                group_by(time) %>%
                summarise(demand.cpu = sum(demand.cpu))


SummaryStatistics(res.cf13.agg$demand.cpu)

##########################
# Cloud capacity
##########################

p <- ggplot(capacity, aes(capacity.cpu)) +
     stat_ecdf() +
     scale_x_continuous("Capacidade total da nuvem (CPU normalizada)") +
     scale_y_continuous("Fração acumulada P[X <= x]", labels = percent, breaks = seq(0, 1, .1))
p
plot.file <- paste(plots.dir, "wc_capacity_ecdf.png", sep = "/")
ggsave(plot.file, p, width=6, height=3)

p <- ggplot(capacity, aes(timeWindow / day, capacity.cpu)) +
     geom_step() +
     scale_x_continuous("Tempo (dias)", breaks = seq(0, 30, 7)) +
     scale_y_continuous("Capacidade disponível (CPU normalizada)")
p
plot.file <- paste(plots.dir, "wc_capacity_tempo.png", sep = "/")
ggsave(plot.file, p, width=6, height=3.5)

capacity %>%
  do(SummaryStatistics(.$capacity.cpu))

machines <- LoadMachineEvents("output/gtrace_data.sqlite3")

machines.stats <- machines %>%
                  group_by(machineId) %>%
                  summarise(cpu.max = max(cpu), mem.max = max(memory))

p <- ggplot(machines.stats, aes(factor(cpu.max))) +
     geom_bar(width = 0.5) +
     scale_x_discrete("Capacidade (CPU normalizada)") +
     scale_y_continuous("Quantidade de máquinas [X = x]",
                        breaks = seq(0, 12000, 2000))
p
plot.file <- paste(plots.dir, "wc_machinecapacity_hist.png", sep = "/")
ggsave(plot.file, p, width=6, height=3)
