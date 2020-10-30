# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")

# Specify grid of values for which power is calculated
.df.vary.params <- expand.grid(rho = 0.80, N = seq(100,500,25))

###############################################################################
# Calculate truth
###############################################################################
.this.folder <- "sim_study_main/sim_results_null/below_truth"

.df.vary.params$truth.diff.eos.means <- NA_real_
.df.vary.params$truth.diff.AUC <- NA_real_
.df.vary.params$truth.stderr.est.diff.eos.means <- NA_real_
.df.vary.params$truth.stderr.est.diff.AUC <- NA_real_

.df.vary.params$bias.diff.eos.means <- NA_real_
.df.vary.params$bias.diff.AUC <- NA_real_
.df.vary.params$bias.stderr.est.diff.eos.means <- NA_real_
.df.vary.params$bias.stderr.est.diff.AUC <- NA_real_

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  
  library(dplyr)
  
  # User-specified design parameters
  input.N <- .df.vary.params[.idx.vary.params, "N"]
  input.alpha <- 0.05
  
  this.pair <- 2
  input.rand.time <- 2
  input.tot.time <- 6
  input.cutoff <- 0
  
  input.rho <- .df.vary.params[.idx.vary.params, "rho"]
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(.path.output_data, .this.folder, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  
  # Calculate standard error
  truth.var.est.diff.eos.means <- var(est.diff.eos.means[,"estimates"], na.rm=TRUE)
  truth.var.est.diff.AUC <- var(est.diff.AUC[,"estimates"], na.rm=TRUE)
  
  truth.stderr.est.diff.eos.means <- sqrt(truth.var.est.diff.eos.means)
  truth.stderr.est.diff.AUC <- sqrt(truth.var.est.diff.AUC)
  
  # Save calculated quantities
  .df.vary.params[.idx.vary.params, "truth.diff.eos.means"] <- diff.eos.means
  .df.vary.params[.idx.vary.params, "truth.diff.AUC"] <- diff.AUC
  
  .df.vary.params[.idx.vary.params, "truth.stderr.est.diff.eos.means"] <- truth.stderr.est.diff.eos.means
  .df.vary.params[.idx.vary.params, "truth.stderr.est.diff.AUC"] <- truth.stderr.est.diff.AUC
  
  # Calculate bias in estimates
  est.diff.eos.means$bias <- est.diff.eos.means$estimates - diff.eos.means
  est.diff.AUC$bias <- est.diff.AUC$estimates - diff.AUC
  
  est.stderr.diff.eos.means$bias <- est.stderr.diff.eos.means$estimates - truth.stderr.est.diff.eos.means
  est.stderr.diff.AUC$bias <- est.stderr.diff.AUC$estimates - truth.stderr.est.diff.AUC
  
  # Save calculated quantities
  .df.vary.params[.idx.vary.params, "bias.diff.eos.means"] <- mean(est.diff.eos.means$bias, na.rm=TRUE)
  .df.vary.params[.idx.vary.params, "bias.diff.AUC"] <- mean(est.diff.AUC$bias, na.rm=TRUE)
  
  .df.vary.params[.idx.vary.params, "bias.stderr.est.diff.eos.means"] <- mean(est.stderr.diff.eos.means$bias, na.rm=TRUE)
  .df.vary.params[.idx.vary.params, "bias.stderr.est.diff.AUC"] <- mean(est.stderr.diff.AUC$bias, na.rm=TRUE)
  
  remove(list=ls())
}


save(.df.vary.params, file = file.path(.path.output_data, .this.folder, "truth.RData"))


