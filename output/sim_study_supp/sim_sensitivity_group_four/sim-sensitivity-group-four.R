# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.alternative <- "sim_study_supp/sim_sensitivity_group_four/sim_results_alternative"
.this.folder.null <- "sim_study_supp/sim_sensitivity_group_four/sim_results_null"

.prop.responders.plusone <- 0.60  # Obtained via calc-truth-deltaQ.R
.prop.responders.minusone <- 0.62  # Obtained via calc-truth-deltaQ.R

.prop.nonresponders.plusone <- 1 - .prop.responders.plusone
.prop.nonresponders.minusone <- 1 - .prop.responders.minusone

###############################################################################
# Calculate power using Method 1
###############################################################################
.df.vary.params <- expand.grid(rho = 0.80, N = 500)
.list.params <- list()

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  max.n4 <- min((.df.vary.params[.idx.vary.params, "N"])*.prop.nonresponders.plusone, 
                (.df.vary.params[.idx.vary.params, "N"])*.prop.nonresponders.minusone)
  max.n4 <- ceiling(max.n4)
  candidate.n4 <- seq(0, max.n4, 10)
  current.grid <- expand.grid(rho = .df.vary.params[.idx.vary.params, "rho"],
                              N = .df.vary.params[.idx.vary.params, "N"],
                              n4 = candidate.n4)
  .list.params <- append(.list.params, list(current.grid))
}

.df.vary.params <- do.call(rbind, .list.params)

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  
  library(dplyr)
  
  input.N <- .df.vary.params[.idx.vary.params, "N"]
  input.rho <- .df.vary.params[.idx.vary.params, "rho"]
  input.n4 <- .df.vary.params[.idx.vary.params, "n4"]
  input.alpha <- 0.05
  
  load(file = file.path(.path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,"_n4_",input.n4,".RData", sep="")))
  use.this.est.diff.eos.means <- est.diff.eos.means
  use.this.est.diff.AUC <- est.diff.AUC
  
  load(file = file.path(.path.output_data, .this.folder.null, paste("hat_","N_",input.N,"_rho_",input.rho,"_n4_",input.n4,".RData", sep="")))
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
  
  .df.vary.params[.idx.vary.params,"power.eos.means"] <- power.diff.eos.means
  .df.vary.params[.idx.vary.params,"power.AUC"] <- power.diff.AUC
  
  rm(list = ls())
}

save(.df.vary.params, file = file.path(.path.output_data, "sim_study_supp/sim_sensitivity_group_four", "power_method_01.RData"))

###############################################################################
# Calculate power using Method 2
###############################################################################
.df.vary.params <- expand.grid(rho = 0.80, N = 500)
.list.params <- list()

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  max.n4 <- min((.df.vary.params[.idx.vary.params, "N"])*.prop.nonresponders.plusone, 
                (.df.vary.params[.idx.vary.params, "N"])*.prop.nonresponders.minusone)
  max.n4 <- ceiling(max.n4)
  candidate.n4 <- seq(0, max.n4, 10)
  current.grid <- expand.grid(rho = .df.vary.params[.idx.vary.params, "rho"],
                              N = .df.vary.params[.idx.vary.params, "N"],
                              n4 = candidate.n4)
  .list.params <- append(.list.params, list(current.grid))
}

.df.vary.params <- do.call(rbind, .list.params)

for(.idx.vary.params in 1:nrow(.df.vary.params)){
  
  library(dplyr)
  
  input.N <- .df.vary.params[.idx.vary.params, "N"]
  input.rho <- .df.vary.params[.idx.vary.params, "rho"]
  input.n4 <- .df.vary.params[.idx.vary.params, "n4"]
  input.alpha <- 0.05
  
  load(file = file.path(.path.output_data, .this.folder.alternative, paste("hat_","N_",input.N,"_rho_",input.rho,"_n4_",input.n4,".RData", sep="")))
  use.this.est.diff.eos.means <- est.diff.eos.means
  use.this.est.diff.AUC <- est.diff.AUC
  
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
  
  .df.vary.params[.idx.vary.params,"power.eos.means"] <- power.diff.eos.means
  .df.vary.params[.idx.vary.params,"power.AUC"] <- power.diff.AUC
  
  rm(list = ls())
}

save(.df.vary.params, file = file.path(.path.output_data, "sim_study_supp/sim_sensitivity_group_four", "power_method_02.RData"))

