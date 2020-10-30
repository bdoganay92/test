# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")

# Specify grid of values for which power is calculated
.df.vary.params <- expand.grid(rho = 0.80, N = seq(100,500,25))

.df.vary.params$nullstderr.eos.means.truth <- NA_real_
.df.vary.params$nullstderr.eos.means.below.truth <- NA_real_
.df.vary.params$nullstderr.eos.means.above.truth <- NA_real_
.df.vary.params$alternativestderr.eos.means <- NA_real_

.df.vary.params$nullstderr.AUC.truth <- NA_real_
.df.vary.params$nullstderr.AUC.below.truth <- NA_real_
.df.vary.params$nullstderr.AUC.above.truth <- NA_real_
.df.vary.params$alternativestderr.AUC <- NA_real_

###############################################################################
# Calculate estimated standard error using estimator in Lu et al (2016)
# Use mean trajectory under the null
###############################################################################

.this.folder.null1 <- "sim_study_main/sim_results_null/truth"
.this.folder.null2 <- "sim_study_main/sim_results_null/below_truth"
.this.folder.null3 <- "sim_study_main/sim_results_null/above_truth"
.this.folder.alternative <- "sim_study_main/sim_results_alternative"

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  
  library(dplyr)
  path.code <- Sys.getenv("path.code")
  path.output_data <- Sys.getenv("path.output_data")
  
  input.N <- .df.vary.params[.idx.vary.params, "N"]
  input.alpha <- 0.05
  
  this.pair <- 2
  input.rand.time <- 2
  input.tot.time <- 6
  input.cutoff <- 0
  
  input.rho <- .df.vary.params[.idx.vary.params, "rho"]
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  load(file = file.path(path.output_data, .this.folder.null1, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  use.this.est.stderr.diff.eos.means <- mean(use.this.est.stderr.diff.eos.means$estimates, na.rm=TRUE)
  use.this.est.stderr.diff.AUC <- mean(use.this.est.stderr.diff.AUC$estimates, na.rm=TRUE)
  .df.vary.params[.idx.vary.params,"nullstderr.eos.means.truth"] <- use.this.est.stderr.diff.eos.means
  .df.vary.params[.idx.vary.params,"nullstderr.AUC.truth"] <- use.this.est.stderr.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  load(file = file.path(path.output_data, .this.folder.null2, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  use.this.est.stderr.diff.eos.means <- mean(use.this.est.stderr.diff.eos.means$estimates, na.rm=TRUE)
  use.this.est.stderr.diff.AUC <- mean(use.this.est.stderr.diff.AUC$estimates, na.rm=TRUE)
  .df.vary.params[.idx.vary.params,"nullstderr.eos.means.below.truth"] <- use.this.est.stderr.diff.eos.means
  .df.vary.params[.idx.vary.params,"nullstderr.AUC.below.truth"] <- use.this.est.stderr.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  load(file = file.path(path.output_data, .this.folder.null3, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  use.this.est.stderr.diff.eos.means <- mean(use.this.est.stderr.diff.eos.means$estimates, na.rm=TRUE)
  use.this.est.stderr.diff.AUC <- mean(use.this.est.stderr.diff.AUC$estimates, na.rm=TRUE)
  .df.vary.params[.idx.vary.params,"nullstderr.eos.means.above.truth"] <- use.this.est.stderr.diff.eos.means
  .df.vary.params[.idx.vary.params,"nullstderr.AUC.above.truth"] <- use.this.est.stderr.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  load(file = file.path(path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  use.this.est.stderr.diff.eos.means <- mean(use.this.est.stderr.diff.eos.means$estimates, na.rm=TRUE)
  use.this.est.stderr.diff.AUC <- mean(use.this.est.stderr.diff.AUC$estimates, na.rm=TRUE)
  .df.vary.params[.idx.vary.params,"alternativestderr.eos.means"] <- use.this.est.stderr.diff.eos.means
  .df.vary.params[.idx.vary.params,"alternativestderr.AUC"] <- use.this.est.stderr.diff.AUC
  
  
  rm(list = ls())
}

print(.df.vary.params)

###############################################################################
# Calculate ratio of estimated standard errors under the null and alternative
###############################################################################

library(dplyr)

.df.vary.params <- .df.vary.params %>%
  mutate(ratio.truth.eos.means = nullstderr.eos.means.truth/alternativestderr.eos.means,
         ratio.below.truth.eos.means = nullstderr.eos.means.below.truth/alternativestderr.eos.means,
         ratio.above.truth.eos.means = nullstderr.eos.means.above.truth/alternativestderr.eos.means,
         ratio.truth.AUC = nullstderr.AUC.truth/alternativestderr.AUC,
         ratio.below.truth.AUC = nullstderr.AUC.below.truth/alternativestderr.AUC,
         ratio.above.truth.AUC = nullstderr.AUC.above.truth/alternativestderr.AUC)

ratio.table <- .df.vary.params

.this.folder <- "sim_study_main"
save(ratio.table, file = file.path(.path.output_data, .this.folder, "ratio.RData"))

