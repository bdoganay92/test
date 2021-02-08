for(.idx.batch in 0:9){
  print(paste("Current iteration is: ",.idx.batch,sep=""))
  # Specify file paths
  .path.output_data <- Sys.getenv("path.output_data")
  .this.folder.alternative <- paste("sim_vary_effect/sim_results_", .idx.batch, sep="")
  source(file.path(.path.output_data, "sim_vary_effect/calc-power.R"))
}

