#!/usr/bin/env Rscript
# Usage: ./install_R_packages.R

install.packages(c("dplyr", "RSQLite", "foreach", "forecast", "ggplot2", "queueing", "tidyr",
                   "scales", "RColorBrewer", "gridExtra", "argparser"),
                 repos = "https://cloud.r-project.org/")
