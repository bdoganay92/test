# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")

# Specify grid of values for which power is calculated
.df.vary.params <- expand.grid(rho = 0.80, N = seq(100,500,25))

.df.vary.params$power.eos.means.truth <- NA_real_
.df.vary.params$power.eos.means.below.truth <- NA_real_
.df.vary.params$power.eos.means.above.truth <- NA_real_

.df.vary.params$power.AUC.truth <- NA_real_
.df.vary.params$power.AUC.below.truth <- NA_real_
.df.vary.params$power.AUC.above.truth <- NA_real_

###############################################################################
# Employ Method 1 in Power Calculation:
# Use correct EDTR mean trajectory under the null 
###############################################################################

.this.folder.alternative <- "sim_study_main/sim_results_alternative"
.this.folder.null <- "sim_study_main/sim_results_null/truth"

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
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.diff.eos.means <- est.diff.eos.means
  use.this.est.diff.AUC <- est.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.null, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  
  # Preliminary data preparation steps
  use.this.est.diff.eos.means <- use.this.est.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.diff.AUC <- use.this.est.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.stderr.diff.eos.means <- use.this.est.stderr.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  use.this.est.stderr.diff.AUC <- use.this.est.stderr.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  # Merge data frames
  use.this.eos.means <- inner_join(x = use.this.est.diff.eos.means, y = use.this.est.stderr.diff.eos.means, by = "sim")
  use.this.AUC <- inner_join(x = use.this.est.diff.AUC, y = use.this.est.stderr.diff.AUC, by = "sim")
  
  # Calculate power
  use.this.eos.means <- use.this.eos.means %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  use.this.AUC <- use.this.AUC %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  power.diff.eos.means <- mean(use.this.eos.means$is.reject, na.rm=TRUE)
  power.diff.AUC <- mean(use.this.AUC$is.reject, na.rm=TRUE)
  
  .df.vary.params[.idx.vary.params,"power.eos.means.truth"] <- power.diff.eos.means
  .df.vary.params[.idx.vary.params,"power.AUC.truth"] <- power.diff.AUC
  
  rm(list = ls())
}

print(.df.vary.params)

###############################################################################
# Employ Method 1 in Power Calculation:
# Mean trajectory under the null is below truth throughout all
# measurement occasions
###############################################################################

.this.folder.alternative <- "sim_study_main/sim_results_alternative"
.this.folder.null <- "sim_study_main/sim_results_null/below_truth"

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
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.diff.eos.means <- est.diff.eos.means
  use.this.est.diff.AUC <- est.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.null, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  
  # Preliminary data preparation steps
  use.this.est.diff.eos.means <- use.this.est.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.diff.AUC <- use.this.est.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.stderr.diff.eos.means <- use.this.est.stderr.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  use.this.est.stderr.diff.AUC <- use.this.est.stderr.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  # Merge data frames
  use.this.eos.means <- inner_join(x = use.this.est.diff.eos.means, y = use.this.est.stderr.diff.eos.means, by = "sim")
  use.this.AUC <- inner_join(x = use.this.est.diff.AUC, y = use.this.est.stderr.diff.AUC, by = "sim")
  
  # Calculate power
  use.this.eos.means <- use.this.eos.means %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  use.this.AUC <- use.this.AUC %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  power.diff.eos.means <- mean(use.this.eos.means$is.reject, na.rm=TRUE)
  power.diff.AUC <- mean(use.this.AUC$is.reject, na.rm=TRUE)
  
  .df.vary.params[.idx.vary.params,"power.eos.means.below.truth"] <- power.diff.eos.means
  .df.vary.params[.idx.vary.params,"power.AUC.below.truth"] <- power.diff.AUC
  
  rm(list = ls())
}

print(.df.vary.params)

###############################################################################
# Employ Method 1 in Power Calculation:
# Mean trajectory under the null is above truth throughout all
# measurement occasions
###############################################################################

.this.folder.alternative <- "sim_study_main/sim_results_alternative"
.this.folder.null <- "sim_study_main/sim_results_null/above_truth"

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
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.diff.eos.means <- est.diff.eos.means
  use.this.est.diff.AUC <- est.diff.AUC
  
  # Load results; these are outputs of calc-estimates.R
  # The following data frames will now be in the global environment:
  #   - est.diff.AUC
  #   - est.diff.eos.means
  #   - est.stderr.diff.AUC
  #   - est.stderr.diff.eos.means
  # Additionally, the following variables will be loaded into the global environment as well:
  #   - diff.AUC  # this is the 'truth'
  #   - diff.eos.means  # this is the 'truth'
  load(file = file.path(path.output_data, .this.folder.null, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
  use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
  use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC
  
  # Preliminary data preparation steps
  use.this.est.diff.eos.means <- use.this.est.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.diff.AUC <- use.this.est.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
    select(sim, est.diff)
  
  use.this.est.stderr.diff.eos.means <- use.this.est.stderr.diff.eos.means %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  use.this.est.stderr.diff.AUC <- use.this.est.stderr.diff.AUC %>%
    rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
    select(sim, est.stderr.diff)
  
  # Merge data frames
  use.this.eos.means <- inner_join(x = use.this.est.diff.eos.means, y = use.this.est.stderr.diff.eos.means, by = "sim")
  use.this.AUC <- inner_join(x = use.this.est.diff.AUC, y = use.this.est.stderr.diff.AUC, by = "sim")
  
  # Calculate power
  use.this.eos.means <- use.this.eos.means %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  use.this.AUC <- use.this.AUC %>% 
    mutate(z = est.diff/est.stderr.diff) %>%
    mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))
  
  power.diff.eos.means <- mean(use.this.eos.means$is.reject, na.rm=TRUE)
  power.diff.AUC <- mean(use.this.AUC$is.reject, na.rm=TRUE)
  
  .df.vary.params[.idx.vary.params,"power.eos.means.above.truth"] <- power.diff.eos.means
  .df.vary.params[.idx.vary.params,"power.AUC.above.truth"] <- power.diff.AUC
  
  rm(list = ls())
}

print(.df.vary.params)

###############################################################################
# Save results
###############################################################################
.this.folder <- "sim_study_main"

power.table <- .df.vary.params
save(power.table, file = file.path(.path.output_data, .this.folder, "power_method_01.RData"))

