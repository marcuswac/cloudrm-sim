#!/usr/bin/env Rscript
# Usage: ./install_R_packages.R

lib_dir <- Sys.getenv("R_LIBS_USER")
dir.create(lib_dir, showWarnings = T, recursive = T)

install.packages(c("dplyr", "RSQLite", "foreach", "forecast", "ggplot2", "queueing", "tidyr",
                   "scales", "RColorBrewer", "gridExtra", "argparser"),
                 lib = lib_dir, repos = "https://cloud.r-project.org/")
