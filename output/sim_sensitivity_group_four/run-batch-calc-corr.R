for(.idx.batch in 0:9){
  print(paste("Current iteration is: ",.idx.batch,sep=""))
  # Specify file paths
  path.code <- Sys.getenv("path.code")
  this.folder <- paste("sim_sensitivity_group_four/sim_results_", .idx.batch, sep="")
  source(file.path(path.code, "calc-params-curve.R"))
}


