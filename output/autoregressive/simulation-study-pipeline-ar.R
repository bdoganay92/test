# ###############################################################################
# # Create simulation scenarios
# ###############################################################################
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# source(file.path(path.output_data, this.folder, "create-scenarios-ar.R"))
# 
# 
# ###############################################################################
# # Check which values of rho will result in a positive definite
# # correlation matrix (for Z_{it}'s)
# ###############################################################################
# 
# # Fix other.corr.params and increase rho from 0 to 1 --------------------------
# path.code <- Sys.getenv("path.code")
# source(file.path(path.code,"datagen-utils.R"))
# 
# input.rand.time <- 2
# input.tot.time <- 6
# 
# list.check.pd.results <- list()
# 
# for(curr.rho in seq(from = 0, to = 1, by = 0.05)){
#   pd <- CheckPositiveDefinite(tot.time = input.tot.time,
#                               rand.time = input.rand.time,
#                               rho = curr.rho,
#                               corr.str = "ar1",
#                               other.corr.params = curr.rho/2)
# 
#   # If pd==0, then positive definite, else, if pd!=0, then not positive definite
#   list.check.pd.results <- append(list.check.pd.results,
#                                   list(data.frame(rho = curr.rho, is.pd = 1*(pd==0))))
# }
# 
# check.pd.results <- do.call(rbind, list.check.pd.results)
# print(check.pd.results)
# 
# # Clean up environment
# remove(list = ls())
# 
# # Fix rho and increase other.corr.params from 0 to 1 --------------------------
# 
# path.code <- Sys.getenv("path.code")
# source(file.path(path.code,"datagen-utils.R"))
# 
# input.rand.time <- 2
# input.tot.time <- 6
# 
# list.check.pd.results <- list()
# 
# for(curr.other.corr.params in seq(from = 0, to = 1, by = 0.05)){
#   pd <- CheckPositiveDefinite(tot.time = input.tot.time,
#                               rand.time = input.rand.time,
#                               rho = 0.6,
#                               corr.str = "ar1",
#                               other.corr.params = curr.other.corr.params)
# 
#   # If pd==0, then positive definite, else, if pd!=0, then not positive definite
#   list.check.pd.results <- append(list.check.pd.results,
#                                   list(data.frame(eta = curr.other.corr.params, is.pd = 1*(pd==0))))
# }
# 
# check.pd.results <- do.call(rbind, list.check.pd.results)
# print(check.pd.results)
# 
# # Clean up environment
# remove(list = ls())

# ###############################################################################
# # Calculate power for fixed value of means and proportion of zeros within
# # this.folder while varying total sample size and rho
# ###############################################################################
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# 
# for(i in 1:3){
#   this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
# 
#   use.grid <- expand.grid(N = seq(100,550,50), rho = c(0.2, 0.4, 0.6))
# 
#   # Initialize data frame; we will add new columns when we go through the for loop
#   dat.all.results <- data.frame(N = rep(NA_real_, nrow(use.grid)),
#                                 rho = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.eos.means = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.AUC = rep(NA_real_, nrow(use.grid)),
#                                 elapsed.secs = rep(NA_real_, nrow(use.grid)))
# 
#   for(idx in 1:nrow(use.grid)){
#     path.code <- Sys.getenv("path.code")
#     path.output_data <- Sys.getenv("path.output_data")
#     input.means <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_means.csv"))
#     input.prop.zeros <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_prop_zeros.csv"))
#     input.M <- 5000
#     input.N <- use.grid[idx, "N"]
#     input.rho <- use.grid[idx, "rho"]
#     input.n4 <- NA_real_
#     input.rand.time <- 2
#     input.tot.time <- 6
#     input.cutoff <- 0
#     input.corr.str <- "ar1"
#     input.other.corr.params <- input.rho/2
#     use.working.corr <- "ar1"
# 
#     start.time <- Sys.time()
#     source(file.path(path.code,"calc-covmat.R"))
# 
#     this.pair <- 2
#     source(file.path(path.code,"calc-estimated-contrasts.R"))
# 
#     input.alpha <- 0.05
#     source(file.path(path.code,"calc-estimated-power.R"))
#     end.time <- Sys.time()
# 
#     elapsed.secs <- difftime(time1 = end.time, time2 = start.time, units = "secs")
#     elapsed.secs <- as.numeric(elapsed.secs)
# 
#     # Save results
#     dat.all.results[idx,"N"] <- input.N
#     dat.all.results[idx,"rho"] <- input.rho
#     dat.all.results[idx,"power.diff.eos.means"] <- power.diff.eos.means
#     dat.all.results[idx,"power.diff.AUC"] <- power.diff.AUC
#     dat.all.results[idx,"elapsed.secs"] <- elapsed.secs
# 
#     # Perform further calculations --------------------------------------------
# 
#     # Calculate standard error
#     truth.var.est.diff.eos.means <- var(est.diff.eos.means[,"estimates"], na.rm=TRUE)
#     truth.var.est.diff.AUC <- var(est.diff.AUC[,"estimates"], na.rm=TRUE)
#     truth.stderr.est.diff.eos.means <- sqrt(truth.var.est.diff.eos.means)
#     truth.stderr.est.diff.AUC <- sqrt(truth.var.est.diff.AUC)
# 
#     # Save calculated quantities
#     dat.all.results[idx,"truth.diff.eos.means"] <- diff.eos.means
#     dat.all.results[idx,"truth.diff.AUC"] <- diff.AUC
#     dat.all.results[idx,"truth.stderr.est.diff.eos.means"] <- truth.stderr.est.diff.eos.means
#     dat.all.results[idx,"truth.stderr.est.diff.AUC"] <- truth.stderr.est.diff.AUC
# 
#     # Calculate bias in estimates
#     est.diff.eos.means$bias <- est.diff.eos.means$estimates - diff.eos.means
#     est.diff.AUC$bias <- est.diff.AUC$estimates - diff.AUC
# 
#     est.stderr.diff.eos.means$bias <- est.stderr.diff.eos.means$estimates - truth.stderr.est.diff.eos.means
#     est.stderr.diff.AUC$bias <- est.stderr.diff.AUC$estimates - truth.stderr.est.diff.AUC
# 
#     # Save calculated quantities
#     dat.all.results[idx,"bias.diff.eos.means"] <- mean(est.diff.eos.means$bias, na.rm=TRUE)
#     dat.all.results[idx,"bias.diff.AUC"] <- mean(est.diff.AUC$bias, na.rm=TRUE)
#     dat.all.results[idx,"bias.stderr.est.diff.eos.means"] <- mean(est.stderr.diff.eos.means$bias, na.rm=TRUE)
#     dat.all.results[idx,"bias.stderr.est.diff.AUC"] <- mean(est.stderr.diff.AUC$bias, na.rm=TRUE)
#   }
# 
#   write.csv(dat.all.results, file = file.path(path.output_data, this.folder, this.scenario, "power.csv"), row.names = FALSE)
# }
# 
# # Clean up environment
# remove(list = ls())
# 
# 
# ###############################################################################
# # Calculate power for fixed value of means and proportion of zeros within
# # this.folder while varying total sample size and rho
# ###############################################################################
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# 
# for(i in 1:10){
#   this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
# 
#   use.grid <- expand.grid(N = seq(100,550,50), rho = c(0.2, 0.4, 0.6))
#   dat.all.results <- data.frame(N = rep(NA_real_, nrow(use.grid)),
#                                 rho = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.eos.means = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.AUC = rep(NA_real_, nrow(use.grid)),
#                                 elapsed.secs = rep(NA_real_, nrow(use.grid)))
# 
#   for(idx in 1:nrow(use.grid)){
#     path.code <- Sys.getenv("path.code")
#     path.output_data <- Sys.getenv("path.output_data")
#     input.means <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_means.csv"))
#     input.prop.zeros <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_prop_zeros.csv"))
#     input.M <- 5000
#     input.N <- use.grid[idx, "N"]
#     input.rho <- use.grid[idx, "rho"]
#     input.n4 <- NA_real_
#     input.rand.time <- 2
#     input.tot.time <- 6
#     input.cutoff <- 0
#     input.corr.str <- "ar1"
#     input.other.corr.params <- input.rho/2
#     use.working.corr <- "ar1"
# 
#     start.time <- Sys.time()
#     source(file.path(path.code,"calc-covmat.R"))
# 
#     this.pair <- 2
#     source(file.path(path.code,"calc-estimated-contrasts.R"))
# 
#     input.alpha <- 0.05
#     source(file.path(path.code,"calc-estimated-power.R"))
#     end.time <- Sys.time()
# 
#     elapsed.secs <- difftime(time1 = end.time, time2 = start.time, units = "secs")
#     elapsed.secs <- as.numeric(elapsed.secs)
# 
#     dat.all.results[idx,"N"] <- input.N
#     dat.all.results[idx,"rho"] <- input.rho
#     dat.all.results[idx,"power.diff.eos.means"] <- power.diff.eos.means
#     dat.all.results[idx,"power.diff.AUC"] <- power.diff.AUC
#     dat.all.results[idx,"elapsed.secs"] <- elapsed.secs
#   }
# 
#   write.csv(dat.all.results, file = file.path(path.output_data, this.folder, this.scenario, "power.csv"), row.names = FALSE)
# }
# 
# # Clean up environment
# remove(list = ls())


# ###############################################################################
# # Calculate power for fixed value of means and proportion of zeros within
# # this.folder while varying total sample size and rho
# ###############################################################################
# 
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# N <- 500
# p <- .40
# q <- .40
# 
# path.code <- Sys.getenv("path.code")
# source(file.path(path.code,"calc-possible-n4.R"))
# 
# # Clean up environment
# remove(list = ls())
# 
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# 
# for(i in 1:10){
#   this.scenario <- paste("sim_vary_n4/sim_results_", i, sep="")
# 
#   use.N <- 500
#   use.grid <- expand.grid(n4 = seq(100, 300, 50))
#   dat.all.results <- data.frame(n4 = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.eos.means = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.AUC = rep(NA_real_, nrow(use.grid)),
#                                 elapsed.secs = rep(NA_real_, nrow(use.grid)))
# 
#   for(idx in 1:nrow(use.grid)){
#     path.code <- Sys.getenv("path.code")
#     path.output_data <- Sys.getenv("path.output_data")
#     input.means <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_means.csv"))
#     input.prop.zeros <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_prop_zeros.csv"))
#     input.M <- 5000
#     input.N <- use.N
#     input.rho <- 0.6
#     input.n4 <- use.grid[idx, "n4"]
#     input.rand.time <- 2
#     input.tot.time <- 6
#     input.cutoff <- 0
#     input.corr.str <- "ar1"
#     input.other.corr.params <- input.rho/2
#     use.working.corr <- "ar1"
# 
#     start.time <- Sys.time()
#     source(file.path(path.code,"calc-covmat.R"))
# 
#     this.pair <- 2
#     source(file.path(path.code,"calc-estimated-contrasts.R"))
# 
#     input.alpha <- 0.05
#     source(file.path(path.code,"calc-estimated-power.R"))
#     end.time <- Sys.time()
# 
#     elapsed.secs <- difftime(time1 = end.time, time2 = start.time, units = "secs")
#     elapsed.secs <- as.numeric(elapsed.secs)
# 
#     dat.all.results[idx,"n4"] <- input.n4
#     dat.all.results[idx,"power.diff.eos.means"] <- power.diff.eos.means
#     dat.all.results[idx,"power.diff.AUC"] <- power.diff.AUC
#     dat.all.results[idx,"elapsed.secs"] <- elapsed.secs
#   }
# 
#   write.csv(dat.all.results, file = file.path(path.output_data, this.folder, this.scenario, "power.csv"), row.names = FALSE)
# }
# 
# # Clean up environment
# remove(list = ls())
# 
# 
# 
# ###############################################################################
# # Calculate power for fixed value of means and proportion of zeros within
# # this.folder while varying total sample size and other.corr.params;
# # rho remains fixed throughout
# ###############################################################################
# 
# path.output_data <- Sys.getenv("path.output_data")
# this.folder <- "autoregressive"
# 
# for(i in 1:10){
#   this.scenario <- paste("sim_vary_eta/sim_results_", i, sep="")
#   use.grid <- expand.grid(eta = seq(0, 0.45, 0.05))
#   dat.all.results <- data.frame(eta = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.eos.means = rep(NA_real_, nrow(use.grid)),
#                                 power.diff.AUC = rep(NA_real_, nrow(use.grid)),
#                                 elapsed.secs = rep(NA_real_, nrow(use.grid)))
# 
#   for(idx in 1:nrow(use.grid)){
#     path.code <- Sys.getenv("path.code")
#     path.output_data <- Sys.getenv("path.output_data")
#     input.means <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_means.csv"))
#     input.prop.zeros <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_prop_zeros.csv"))
#     input.M <- 5000
#     input.N <- 500
#     input.rho <- 0.6
#     input.n4 <- NA_real_
#     input.rand.time <- 2
#     input.tot.time <- 6
#     input.cutoff <- 0
#     input.corr.str <- "ar1"
#     input.other.corr.params <- use.grid[idx, "eta"]
#     use.working.corr <- "ar1"
# 
#     start.time <- Sys.time()
#     source(file.path(path.code,"calc-covmat.R"))
# 
#     this.pair <- 2
#     source(file.path(path.code,"calc-estimated-contrasts.R"))
# 
#     input.alpha <- 0.05
#     source(file.path(path.code,"calc-estimated-power.R"))
#     end.time <- Sys.time()
# 
#     elapsed.secs <- difftime(time1 = end.time, time2 = start.time, units = "secs")
#     elapsed.secs <- as.numeric(elapsed.secs)
# 
#     dat.all.results[idx,"eta"] <- input.other.corr.params
#     dat.all.results[idx,"power.diff.eos.means"] <- power.diff.eos.means
#     dat.all.results[idx,"power.diff.AUC"] <- power.diff.AUC
#     dat.all.results[idx,"elapsed.secs"] <- elapsed.secs
#   }
# 
#   write.csv(dat.all.results, file = file.path(path.output_data, this.folder, this.scenario, "power.csv"), row.names = FALSE)
# }
# 
# # Clean up environment
# remove(list = ls())
# 

###############################################################################
# Calculate relationship between rho and tau
###############################################################################

path.code <- Sys.getenv("path.code")
path.output_data <- Sys.getenv("path.output_data")

for(i in 1:3){
  # Input parameters
  input.M <- 5000
  input.N <- 1000
  input.rand.time <- 2
  input.tot.time <- 6
  input.cutoff <- 0
  input.corr.str <- "ar1"
  min.rho <- 0
  max.rho <- 0.6
  this.folder <- "autoregressive"
  this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
  
  # Calculate correspondence between rho and tau
  source(file.path(path.code,"calc-corr-params-curve.R"))
  
  # Save output
  save(collect.seq.cormat, file = file.path(path.output_data, this.folder, this.scenario, "collect_seq_cormat.RData"))
  write.csv(collect.correlation.tau, file.path(path.output_data, this.folder, this.scenario, "collect_tau.csv"), row.names = FALSE)
}

# Clean up environment
remove(list = ls())

###############################################################################
# Calculate relationship between rho and tau
###############################################################################

path.code <- Sys.getenv("path.code")
path.output_data <- Sys.getenv("path.output_data")

for(i in 1:10){
  # Input parameters
  input.M <- 5000
  input.N <- 1000
  input.rand.time <- 2
  input.tot.time <- 6
  input.cutoff <- 0
  input.corr.str <- "ar1"
  min.rho <- 0
  max.rho <- 0.6
  this.folder <- "autoregressive"
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  
  # Calculate correspondence between rho and tau
  source(file.path(path.code,"calc-corr-params-curve.R"))
  
  # Save output
  save(collect.seq.cormat, file = file.path(path.output_data, this.folder, this.scenario, "collect_seq_cormat.RData"))
  write.csv(collect.correlation.tau, file.path(path.output_data, this.folder, this.scenario, "collect_tau.csv"), row.names = FALSE)
}

# Clean up environment
remove(list = ls())

