# First set of simulated datasets ---------------------------------------------
path.output_data <- Sys.getenv("path.output_data")
this.folder <- "autoregressive"
dir.create(path = file.path(path.output_data, this.folder, "base_case"), recursive = TRUE)

dat.mu.base.case <- data.frame(seq = c("plus.r", 
                                       "plus.nr.plus", 
                                       "plus.nr.minus",
                                       "minus.r", 
                                       "minus.nr.plus", 
                                       "minus.nr.minus"),
                               time.1 = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5),
                               time.2 = c(4.8, 4.8, 4.8, 4.8, 4.8, 4.8),
                               time.3 = c(2.6, 2.6, 2.6, 2.6, 2.6, 2.6),
                               time.4 = c(2.7, 2.7, 2.7, 2.7, 2.7, 2.7),
                               time.5 = c(2.75, 2.75, 2.75, 2.75, 2.75, 2.75),
                               time.6 = c(2.8, 2.8, 2.8, 2.8, 2.8, 2.8))

dat.tau.base.case <- data.frame(seq = c("plus.r", 
                                        "plus.nr.plus", 
                                        "plus.nr.minus",
                                        "minus.r", 
                                        "minus.nr.plus", 
                                        "minus.nr.minus"),
                                time.1 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                                time.2 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                                time.3 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                                time.4 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                                time.5 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                                time.6 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2))

write.csv(dat.mu.base.case, file.path(path.output_data, this.folder, "base_case/input_means.csv"), row.names = FALSE)
write.csv(dat.tau.base.case, file.path(path.output_data, this.folder, "base_case/input_prop_zeros.csv"), row.names = FALSE)

remove(list = ls())


# Subsequent set of simulated datasets ----------------------------------------
path.output_data <- Sys.getenv("path.output_data")
this.folder <- "autoregressive"
dir.create(path = file.path(path.output_data, this.folder, "sim_size_test"), recursive = TRUE)

dat.mu.base.case <- read.csv(file.path(path.output_data, this.folder, "base_case/input_means.csv"))
dat.tau.base.case <- read.csv(file.path(path.output_data, this.folder, "base_case/input_prop_zeros.csv"))

for(idx.sim.scenarios in 1:3){
  
  dat.mu <- dat.mu.base.case
  
  dat.tau <- dat.tau.base.case
  dat.tau[1:6, "time.1"] <- dat.tau[1:6, "time.1"] * idx.sim.scenarios
  dat.tau[1:6, "time.2"] <- dat.tau[1:6, "time.2"] * idx.sim.scenarios
  dat.tau[1:6, "time.3"] <- dat.tau[1:6, "time.3"] * idx.sim.scenarios
  dat.tau[1:6, "time.4"] <- dat.tau[1:6, "time.4"] * idx.sim.scenarios
  dat.tau[1:6, "time.5"] <- dat.tau[1:6, "time.5"] * idx.sim.scenarios
  dat.tau[1:6, "time.6"] <- dat.tau[1:6, "time.6"] * idx.sim.scenarios
  
  dir.create(path = file.path(path.output_data, this.folder, "sim_size_test", paste("sim_results", idx.sim.scenarios, sep="_")), recursive = TRUE)
  write.csv(dat.mu, file = file.path(path.output_data, this.folder, "sim_size_test", paste("sim_results", idx.sim.scenarios, sep="_"), "input_means.csv"), row.names = FALSE)
  write.csv(dat.tau, file = file.path(path.output_data, this.folder, "sim_size_test", paste("sim_results", idx.sim.scenarios, sep="_"), "input_prop_zeros.csv"), row.names = FALSE)
}

remove(list = ls())


# Subsequent set of simulated datasets ----------------------------------------
path.output_data <- Sys.getenv("path.output_data")
this.folder <- "autoregressive"
dir.create(path = file.path(path.output_data, this.folder, "sim_vary_effect"), recursive = TRUE)

dat.mu.base.case <- read.csv(file.path(path.output_data, this.folder, "base_case/input_means.csv"))
dat.tau.base.case <- read.csv(file.path(path.output_data, this.folder, "base_case/input_prop_zeros.csv"))

for(idx.sim.scenarios in 1:10){
  
  dat.mu <- dat.mu.base.case
  dat.mu[1:3, "time.3"] <- dat.mu[1:3, "time.3"] * (1 + (idx.sim.scenarios * 10 * 0.7)/100)
  dat.mu[1:3, "time.4"] <- dat.mu[1:3, "time.4"] * (1 + (idx.sim.scenarios * 10 * 0.7)/100)
  dat.mu[1:3, "time.5"] <- dat.mu[1:3, "time.5"] * (1 + (idx.sim.scenarios * 10 * 0.7)/100)
  dat.mu[1:3, "time.6"] <- dat.mu[1:3, "time.6"] * (1 + (idx.sim.scenarios * 10)/100)
  
  dat.tau <- dat.tau.base.case
  dat.tau[1:6, "time.1"] <- dat.tau[1:6, "time.1"] * 2
  dat.tau[1:6, "time.2"] <- dat.tau[1:6, "time.2"] * 2
  dat.tau[1:6, "time.3"] <- dat.tau[1:6, "time.3"] * 2
  dat.tau[1:6, "time.4"] <- dat.tau[1:6, "time.4"] * 2
  dat.tau[1:6, "time.5"] <- dat.tau[1:6, "time.5"] * 2
  dat.tau[1:6, "time.6"] <- dat.tau[1:6, "time.6"] * 2
  
  dir.create(path = file.path(path.output_data, this.folder, "sim_vary_effect", paste("sim_results", idx.sim.scenarios, sep="_")), recursive = TRUE)
  write.csv(dat.mu, file = file.path(path.output_data, this.folder, "sim_vary_effect", paste("sim_results", idx.sim.scenarios, sep="_"), "input_means.csv"), row.names = FALSE)
  write.csv(dat.tau, file = file.path(path.output_data, this.folder, "sim_vary_effect", paste("sim_results", idx.sim.scenarios, sep="_"), "input_prop_zeros.csv"), row.names = FALSE)
}

remove(list = ls())



