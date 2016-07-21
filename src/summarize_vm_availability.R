library(dplyr)

source("src/admission_control_utils.R")

Main <- function(argv) {
  if (length(argv) < 1) {
    stop("Missing arguments. Usage: Rscript summarize_vm_availability.R <availability_files>")
  } 
  av.files <- argv[1]
  
  for (av.file in av.files) {
    gc()
    vm.av.summary <- SummarizeVmAvailability(av.file)
    vm.av.summary.file <- paste(av.file, "vm-avail-summary.csv", sep="_")
    write.table(vm.av.summary, vm.av.summary.file, quote=F, row.names=F, col.names=T,
                sep=",")
  }
}

argv <- commandArgs(trailingOnly = TRUE)
print(argv)

if (length(argv) > 0) {
  Main(argv)
}
