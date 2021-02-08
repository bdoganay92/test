.all.batch <- c("low_zeros","moderate_zeros","high_zeros")

for(.idx.batch in 1:length(.all.batch)){
  print(paste("Current iteration is: ",.idx.batch,sep=""))
  .path.output_data <- Sys.getenv("path.output_data")
  .this.folder <- paste("sim_size_test/", .all.batch[.idx.batch], sep="")
  source(file.path(.path.output_data, "sim_size_test/calc-estimates.R"))
}

