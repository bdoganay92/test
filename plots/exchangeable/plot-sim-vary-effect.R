library(dplyr)
path.output_data <- Sys.getenv("path.output_data")
path.plots <- Sys.getenv("path.plots")
path.code <- Sys.getenv("path.code")
this.folder <- "exchangeable"
palette <- c("darkgoldenrod",
             "forestgreen",
             "firebrick3",
             "lightcoral",
             "mistyrose1",
             "bisque3",
             "lightblue4",
             "steelblue4",
             "wheat3",
             "grey30")

###############################################################################
# Plot power for detecting difference in end-of-study means
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  jpeg(file.path(path.plots,
                 this.folder,
                 paste("sim_vary_effect/eos_means_",current.rho,".jpg",sep="")),
       width = 800,
       height = 800)
  
  par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
  plot(-1, 
       type="n",
       xlim = c(100,700),
       ylim = c(0,1),
       xaxt="n",
       yaxt="n",
       xlab = "Total sample size N",
       ylab = "Power", 
       cex.lab = 2.5,
       frame.plot = FALSE)
  
  all.yloc <- NULL
  
  for(i in 1:10){
    this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
    abline(h = 0.80, lty = 2, lwd = 3)
    abline(h = 0.60, lty = 2, lwd = 3)
    abline(h = 0.40, lty = 2, lwd = 3)
    abline(h = 0.20, lty = 2, lwd = 3)
    lines(dat.plot$N, dat.plot$power.diff.eos.means, lty = 2, lwd = 3, col = palette[i])
    points(dat.plot$N, dat.plot$power.diff.eos.means, pch = 21, bg = palette[i], col = "black", cex = 3)
    all.yloc <- c(all.yloc, dat.plot[which(dat.plot$N==550),"power.diff.eos.means"])
  }
  
  all.deltaQ <- NULL
  
  for(i in 1:10){
    this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
    input.rand.time <- 2
    input.tot.time <- 6 
    input.cutoff <- 0
    is.plot <- FALSE
    source(file.path(path.code, "plot-truth-deltaQ.R"))
    deltaQ <- c(diff.eos.means.plusplus.minusplus)
    all.deltaQ <- c(all.deltaQ, deltaQ)
  }
  
  text(x = 100, y = 1, paste("rho=", current.rho, sep=""), col = "red", cex = 1.5, adj = 0)
  
  for(i in 1:10){
    text(x = 580, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[i], cex = 1.8, adj = 0)
  }
  
  dev.off()
}


###############################################################################
# Plot power for detecting difference in AUC
###############################################################################

all.rho <- c(0.2, 0.4, 0.6)

for(idx.rho in 1:3){
  current.rho <- all.rho[idx.rho]
  
  jpeg(file.path(path.plots,
                 this.folder,
                 paste("sim_vary_effect/AUC_",current.rho,".jpg",sep="")),
       width = 800,
       height = 800)
  
  par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
  plot(-1, 
       type="n",
       xlim = c(100,700),
       ylim = c(0,1),
       xaxt="n",
       yaxt="n",
       xlab = "Total sample size N",
       ylab = "Power", 
       cex.lab = 2.5,
       frame.plot = FALSE)
  
  all.yloc <- NULL
  
  for(i in 1:10){
    this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
    dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
    abline(h = 0.80, lty = 2, lwd = 3)
    abline(h = 0.60, lty = 2, lwd = 3)
    abline(h = 0.40, lty = 2, lwd = 3)
    abline(h = 0.20, lty = 2, lwd = 3)
    lines(dat.plot$N, dat.plot$power.diff.AUC, lty = 2, lwd = 3, col = palette[i])
    points(dat.plot$N, dat.plot$power.diff.AUC, pch = 21, bg = palette[i], col = "black", cex = 3)
    all.yloc <- c(all.yloc, dat.plot[which(dat.plot$N==550),"power.diff.AUC"])
  }
  
  all.deltaQ <- NULL
  
  for(i in 1:10){
    this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
    input.rand.time <- 2
    input.tot.time <- 6 
    input.cutoff <- 0
    is.plot <- FALSE
    source(file.path(path.code, "plot-truth-deltaQ.R"))
    deltaQ <- c(diff.AUC.plusplus.minusplus)
    all.deltaQ <- c(all.deltaQ, deltaQ)
  }
  
  text(x = 100, y = 1, paste("rho=", current.rho, sep=""), col = "red", cex = 1.5, adj = 0)
  
  for(i in 1:10){
    text(x = 580, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[i], cex = 1.8, adj = 0)
  }
  
  dev.off()
}

