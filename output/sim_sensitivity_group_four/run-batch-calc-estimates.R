for(.idx.batch in 0:9){
  print(paste("Current iteration is: ",.idx.batch,sep=""))
  # Specify file paths
  .path.output_data <- Sys.getenv("path.output_data")
  .this.folder <- paste("sim_sensitivity_group_four/sim_results_", .idx.batch, sep="")
  source(file.path(.path.output_data, "sim_sensitivity_group_four/calc-estimates.R"))
}


