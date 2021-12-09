###############################################################################
# Create simulation scenarios
###############################################################################
path.output_data <- Sys.getenv("path.output_data")
this.folder <- "exchangeable"
source(file.path(path.output_data, this.folder, "create-scenarios-exch.R"))


###############################################################################
# Check which values of rho will result in a positive definite
# correlation matrix (for Z_{it}'s)
###############################################################################

# Fix other.corr.params and increase rho from 0 to 1 --------------------------
path.code <- Sys.getenv("path.code")
source(file.path(path.code,"datagen-utils.R"))

input.rand.time <- 2
input.tot.time <- 6

list.check.pd.results <- list()

for(curr.rho in seq(from = 0, to = 1, by = 0.05)){
  pd <- CheckPositiveDefinite(tot.time = input.tot.time,
                              rand.time = input.rand.time,
                              rho = curr.rho,
                              corr.str = "exch",
                              other.corr.params = curr.rho/2)

  # If pd==0, then positive definite, else, if pd!=0, then not positive definite
  list.check.pd.results <- append(list.check.pd.results,
                                  list(data.frame(rho = curr.rho, is.pd = 1*(pd==0))))
}

check.pd.results <- do.call(rbind, list.check.pd.results)
print(check.pd.results)

# Clean up environment
remove(list = ls())


###############################################################################
# Calculate power for fixed value of means and proportion of zeros within
# this.folder while varying total sample size and rho
###############################################################################
path.output_data <- Sys.getenv("path.output_data")
this.folder <- "exchangeable"

for(i in 1:10){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  
  use.grid <- expand.grid(N = seq(100,550,50), rho = c(0.2, 0.4, 0.6))
  dat.all.results <- data.frame(N = rep(NA_real_, nrow(use.grid)),
                                rho = rep(NA_real_, nrow(use.grid)),
                                power.diff.eos.means = rep(NA_real_, nrow(use.grid)),
                                power.diff.AUC = rep(NA_real_, nrow(use.grid)),
                                elapsed.secs = rep(NA_real_, nrow(use.grid)))
  
  for(idx in 1:nrow(use.grid)){
    path.code <- Sys.getenv("path.code")
    path.output_data <- Sys.getenv("path.output_data")
    input.means <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_means.csv"))
    input.prop.zeros <- read.csv(file.path(path.output_data, this.folder, this.scenario, "input_prop_zeros.csv"))
    input.M <- 5000
    input.N <- use.grid[idx, "N"]
    input.rho <- use.grid[idx, "rho"]
    input.n4 <- NA_real_
    input.rand.time <- 2
    input.tot.time <- 6
    input.cutoff <- 0
    input.corr.str <- "exch"
    input.other.corr.params <- input.rho/2
    use.working.corr <- "ar1"
    
    start.time <- Sys.time()
    source(file.path(path.code,"calc-covmat.R"))
    
    this.pair <- 2
    source(file.path(path.code,"calc-estimated-contrasts.R"))
    
    input.alpha <- 0.05
    source(file.path(path.code,"calc-estimated-power.R"))
    end.time <- Sys.time()
    
    elapsed.secs <- difftime(time1 = end.time, time2 = start.time, units = "secs")
    elapsed.secs <- as.numeric(elapsed.secs)
    
    dat.all.results[idx,"N"] <- input.N
    dat.all.results[idx,"rho"] <- input.rho
    dat.all.results[idx,"power.diff.eos.means"] <- power.diff.eos.means
    dat.all.results[idx,"power.diff.AUC"] <- power.diff.AUC
    dat.all.results[idx,"elapsed.secs"] <- elapsed.secs
  }
  
  write.csv(dat.all.results, file = file.path(path.output_data, this.folder, this.scenario, "power.csv"), row.names = FALSE)
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
  input.corr.str <- "exch"
  min.rho <- 0
  max.rho <- 0.6
  this.folder <- "exchangeable"
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  
  # Calculate correspondence between rho and tau
  source(file.path(path.code,"calc-corr-params-curve.R"))
  
  # Save output
  save(collect.seq.cormat, file = file.path(path.output_data, this.folder, this.scenario, "collect_seq_cormat.RData"))
  write.csv(collect.correlation.tau, file.path(path.output_data, this.folder, this.scenario, "collect_tau.csv"), row.names = FALSE)
}

# Clean up environment
remove(list = ls())

