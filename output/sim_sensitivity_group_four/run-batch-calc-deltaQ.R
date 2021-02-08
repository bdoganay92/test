.df.collect.truth <- data.frame(Scenario = seq(1,10,1),
                                p = rep(NA, 10),
                                q = rep(NA, 10),
                                truth.delta.eos.means = rep(NA, 10),
                                truth.delta.AUC = rep(NA, 10))

for(.idx.batch in 0:9){
  print(paste("Current iteration is: ",.idx.batch,sep=""))
  # Specify file paths
  path.code <- Sys.getenv("path.code")
  this.folder <- paste("sim_sensitivity_group_four/sim_results_", .idx.batch, sep="")
  source(file.path(path.code, "calc-truth-deltaQ.R"))
  .df.collect.truth[.idx.batch + 1, "p"] <- p
  .df.collect.truth[.idx.batch + 1, "q"] <- q
  
  truth.delta.eos.means <- case_when(
    this.pair==1 ~ diff.eos.means.plusplus.plusminus,
    this.pair==2 ~ diff.eos.means.plusplus.minusplus,
    this.pair==3 ~ diff.eos.means.plusplus.minusminus,
    this.pair==4 ~ diff.eos.means.plusplus.plusminus,
    this.pair==5 ~ diff.eos.means.plusplus.minusplus,
    this.pair==6 ~ diff.eos.means.plusplus.minusminus,
    TRUE ~ NA_real_
  )
  
  truth.delta.AUC <- case_when(
    this.pair==1 ~ diff.AUC.plusplus.plusminus,
    this.pair==2 ~ diff.AUC.plusplus.minusplus,
    this.pair==3 ~ diff.AUC.plusplus.minusminus,
    this.pair==4 ~ diff.AUC.plusplus.plusminus,
    this.pair==5 ~ diff.AUC.plusplus.minusplus,
    this.pair==6 ~ diff.AUC.plusplus.minusminus,
    TRUE ~ NA_real_
  )
  
  .df.collect.truth[.idx.batch + 1, "truth.delta.eos.means"] <- truth.delta.eos.means
  .df.collect.truth[.idx.batch + 1, "truth.delta.AUC"] <- truth.delta.AUC
}

.df.collect.truth <- round(.df.collect.truth, digits=3)
print(.df.collect.truth)

# Save to file
path.output_data <- Sys.getenv("path.output_data")
write.csv(.df.collect.truth, file.path(path.output_data, "sim_sensitivity_group_four/truth-delta-Q.csv"), row.names=FALSE)

