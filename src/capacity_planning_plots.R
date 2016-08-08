###########
# Plots
###########

plots.dir <- "plots/"

PlotCapacityPlanningResults <- function(input.res) {
  input.res$userClass <- factor(input.res$userClass, levels=c("prod", "batch", "free"))
  data.frame(input.res %>%
               filter(slo.scenario == 1, method == "pred-ets",
                      #model == "G/GI/c/K",
                      cpu.load.factor == 1, between(cpu.capacity.factor, .6, 1.3)) %>%
               group_by(userClass) %>%
               summarise(error.mean=mean(ob.cpu.mean - ob.est.cpu),
                         error.abs.mean=round(mean(abs(ob.cpu.mean-ob.est.cpu)), 3),
                         error.abs.max=round(max(abs(ob.cpu.mean-ob.est.cpu)), 3)))
  
  
  plot.margin <- unit(c(1, 1, 1, 1), "mm")
  
  df.plot <- input.res %>%
    filter(between(cpu.capacity.factor, .6, 1.3), method == "pred-ets", slo.scenario == 1,
           cpu.load.factor == 1, mem.load.factor == 1, mem.capacity.factor == 10) %>%
    group_by(cpu.capacity.factor) %>%
    mutate(total.capacity=max(capacity.rem.cpu.mean))
  
  # CPU analysis
  p <- ggplot(df.plot, aes(cpu.capacity.factor, ob.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4) +
    geom_line(aes(y=ob.est.cpu, col="model", lty="model")) +
    geom_point(aes(y=ob.est.cpu, col="model", pch="model"), size=4, alpha=.8) +
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

  p <- ggplot(df.plot, aes(cpu.capacity.factor, demand.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4, alpha=.8) +
    geom_line(aes(y=admitted.est.all.cpu, col="model", lty="model")) +
    geom_point(aes(y=admitted.est.all.cpu, col="model", pch="model"), size=4, alpha=.8) +
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
    group_by(userClass) %>%
    summarise(round(max(abs(ob.cpu.mean - ob.est))*100, 1), round(mean(abs(ob.cpu.mean - ob.est))*100, 1))
  
  
  df.plot <- input.res %>%
    filter(method == "pred-ets", slo.scenario == 1, 
           cpu.capacity.factor == 1, mem.capacity.factor == 10, mem.load.factor == 1,
           cpu.load.factor %in% c(.7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) %>%
    mutate(cpu.load.factor.str = paste("load factor =", cpu.load.factor))
  
  p <- ggplot(df.plot, aes(cpu.load.factor, ob.cpu.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4) +
    geom_line(aes(y=ob.est.cpu, col="model", lty="model")) +
    geom_point(aes(y=ob.est.cpu, col="model", pch="model"), size=4, alpha=.8) +
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
  

  # Memory: what-if analysis
  df.plot <- input.res %>%
    filter(between(mem.capacity.factor, .5, 1.4), method == "pred-ets", slo.scenario == 1,
           cpu.load.factor == 1, mem.load.factor == 1, cpu.capacity.factor == 10) %>%
    group_by(mem.capacity.factor) %>%
    mutate(total.capacity=max(capacity.rem.mem.mean))
  
  p <- ggplot(df.plot, aes(mem.capacity.factor, ob.mem.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4) +
    geom_line(aes(y=ob.est.mem, col="model", lty="model")) +
    geom_point(aes(y=ob.est.mem, col="model", pch="model"), size=4, alpha=.8) +
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
  
  p <- ggplot(df.plot, aes(mem.capacity.factor, demand.mem.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4, alpha=.8) +
    geom_line(aes(y=admitted.est.all.mem, col="model", lty="model")) +
    geom_point(aes(y=admitted.est.all.mem, col="model", pch="model"), size=4, alpha=.8) +
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
    group_by(userClass) %>%
    summarise(round(max(abs(ob.cpu.mean - ob.est.cpu))*100, 1), round(mean(abs(ob.cpu.mean - ob.est.cpu))*100, 1))
  
  
  df.plot <- input.res %>%
    filter(method == "pred-ets", slo.scenario == 1, 
           cpu.capacity.factor == 10, mem.capacity.factor == 1, cpu.load.factor == 1,
           #model == "G/GI/c/K",
           mem.load.factor %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6)) %>%
    mutate(mem.load.factor.str = paste("load factor =", mem.load.factor))
  
  p <- ggplot(df.plot, aes(mem.load.factor, ob.mem.mean, col="simulation", lty="simulation", pch="simulation")) +
    geom_line() +
    geom_point(size=4) +
    geom_line(aes(y=ob.est.mem, col="model", lty="model")) +
    geom_point(aes(y=ob.est.mem, col="model", pch="model"), size=4, alpha=.8) +
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
  
  
  # CPU Capacity planning
  df.plot %>%
    group_by(userClass) %>%
    summarise(round(max(abs(ob.cpu.mean - ob.est.cpu))*100, 1), round(mean(abs(ob.cpu.mean - ob.est.cpu))*100, 1))
  
  df.plot <- input.res %>%
    filter(method == "pred-ets", slo.scenario == 1, 
           cpu.capacity.factor %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5),
           mem.capacity.factor == 10,
           cpu.load.factor %in% c(.5, .75, 1, 1.25, 1.5, 1.75, 2),
           mem.load.factor == 1) %>%
    group_by(cpu.load.factor, cpu.capacity.factor) %>%
    summarise(best.cpu.capacity=min(best.cpu.capacity.01), fulfilled=all(ob.cpu.mean >= c(.999, .99, 0))) %>%
    filter(fulfilled) %>%
    summarise(sim.best.capacity=min(cpu.capacity.factor), model.best.capacity=min(best.cpu.capacity))
  
  p <- ggplot(df.plot, aes(cpu.load.factor, sim.best.capacity, col = "simulation", lty = "simulation", pch="simulation")) +
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
  plot.file <- paste(plots.dir, "cp_cpu_loadfactor_bestcapacity_estimations.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.5)
  
  # Memory Capacity planning
  df.plot <- input.res %>%
             filter(method == "pred-ets", slo.scenario == 1,
                    cpu.capacity.factor == 10,
                    mem.capacity.factor %in% c(.6, .7, .8, .9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5),
                    cpu.load.factor == 1,
                    mem.load.factor %in% as.character(c(.5, .75, 1, 1.25, 1.5, 1.75, 2))) %>%
    group_by(mem.load.factor, mem.capacity.factor) %>%
    summarise(best.mem.capacity=min(best.mem.capacity.01), fulfilled=all(ob.mem.mean >= c(.999, .99, 0))) %>%
    filter(fulfilled) %>%
    summarise(sim.best.capacity=min(mem.capacity.factor), model.best.capacity=min(best.mem.capacity))
  
  p <- ggplot(df.plot, aes(mem.load.factor, sim.best.capacity, col = "simulation", lty = "simulation", pch="simulation")) +
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
  plot.file <- paste(plots.dir, "cp_mem_loadfactor_bestcapacity_estimations.png", sep="/")
  ggsave(plot.file, p, width=6, height=2.5)
  
}

OtherAnalysis <- function(stats) {
  
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
  
  df.plot <- filter(input.res, between(capacity.fraction, .6, 1.3), method == "no-adm-ctrl",
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
  
  
  df.plot <- filter(input.res, capacity.fraction == 1, method == "no-adm-ctrl",
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
  
  
  df.plot <- input.res %>%
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
  
  
  
  df.plot <- filter(input.res, between(capacity.fraction, .6, 1.3), method == "pred-ets",
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
  
  
  ggplot(filter(input.res, capacity.fraction == 1, method == "greedy-quota"),
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
  
  
  
  
  df.plot <- input.res %>%
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
  
  
  
  
  
  
  df.plot <- input.res %>%
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

Main <- function(argv) {
  #res.file <- argv
  res.file <- "output/cp_results.csv"
  
  input.res <- read.csv(res.file, header = T)
  PlotCapacityPlanningResults(input.res)
  
}
# Read command-line arguments
argv <- commandArgs(trailingOnly = TRUE)

# If any input argument is found, execute the main function
if (length(argv) > 0) {
  Main(argv)
}