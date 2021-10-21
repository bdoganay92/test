library(dplyr)
path.output_data <- Sys.getenv("path.output_data")
path.plots <- Sys.getenv("path.plots")
path.code <- Sys.getenv("path.code")
this.folder <- "autoregressive"

###############################################################################
# Plot power for detecting difference in end-of-study means
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  for(i in 1:3){
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/eos_means_power_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(0.01,0.09),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Empirical Type-I Error Rate", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0.01, 0.09, 0.02), cex.axis = 2.5, lwd = 5)
    abline(h = 0.05, lty = 2, lwd = 8)
    abline(h = 0.07, lty = 2, lwd = 8)
    abline(h = 0.03, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$power.diff.eos.means, pch = 21, bg = "cornflowerblue", col = "black", cex = 7)
    
    input.rand.time <- 2
    input.tot.time <- 6 
    input.cutoff <- 0
    is.plot <- FALSE
    source(file.path(path.code, "plot-truth-deltaQ.R"))
    deltaQ <- c(diff.eos.means.plusplus.minusplus)
    print(deltaQ)
    
    dev.off()
  }
}

###############################################################################
# Plot power for detecting difference in AUC
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  for(i in 1:3){
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/AUC_power_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(0.01,0.09),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Empirical Type-I Error Rate", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0.01, 0.09, 0.02), cex.axis = 2.5, lwd = 5)
    abline(h = 0.05, lty = 2, lwd = 8)
    abline(h = 0.07, lty = 2, lwd = 8)
    abline(h = 0.03, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$power.diff.AUC, pch = 21, bg = "cornflowerblue", col = "black", cex = 7)
    
    input.rand.time <- 2
    input.tot.time <- 6 
    input.cutoff <- 0
    is.plot <- FALSE
    source(file.path(path.code, "plot-truth-deltaQ.R"))
    deltaQ <- c(diff.AUC.plusplus.minusplus)
    print(deltaQ)
    
    dev.off()
  }
}

###############################################################################
# Plot bias in estimates of difference in end of study means
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  for(i in 1:3){
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/eos_means_bias_estimates_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(-1,1),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Bias", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(-1,1,0.5), cex.axis = 2.5, lwd = 5)
    abline(h = 0, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$bias.diff.eos.means, pch = 21, bg = "darkgoldenrod", col = "black", cex = 7)
    
    dev.off()
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/eos_means_bias_stderr_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(-1,1),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Bias", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(-1,1,0.5), cex.axis = 2.5, lwd = 5)
    abline(h = 0, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$bias.stderr.est.diff.eos.means, pch = 21, bg = "darkgoldenrod", col = "black", cex = 7)
    
    dev.off()
  }
}

###############################################################################
# Plot bias in estimates of difference in AUC
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  for(i in 1:3){
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/AUC_bias_estimates_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(-1,1),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Bias", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(-1,1,0.5), cex.axis = 2.5, lwd = 5)
    abline(h = 0, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$bias.diff.AUC, pch = 21, bg = "darkgoldenrod", col = "black", cex = 7)
    
    dev.off()
    
    jpeg(file.path(path.plots,
                   this.folder, 
                   paste("sim_size_test/AUC_bias_stderr_",
                         current.rho,
                         "_scenario_",
                         i,
                         ".jpg",sep="")), 
         width = 800, 
         height = 700)
    
    par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
    plot(-1, 
         type="n",
         xlim = c(100,600),
         ylim = c(-1,1),
         xaxt="n",
         yaxt="n",
         xlab = "Total sample size N",
         ylab = "Bias", 
         cex.lab = 2.5,
         frame.plot = FALSE)
    
    this.scenario <- paste("sim_size_test/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(-1,1,0.5), cex.axis = 2.5, lwd = 5)
    abline(h = 0, lty = 2, lwd = 8)
    points(dat.plot$N, dat.plot$bias.stderr.est.diff.AUC, pch = 21, bg = "darkgoldenrod", col = "black", cex = 7)
    
    dev.off()
  }
}


