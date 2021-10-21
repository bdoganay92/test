library(dplyr)
path.output_data <- Sys.getenv("path.output_data")
path.plots <- Sys.getenv("path.plots")
path.code <- Sys.getenv("path.code")
this.folder <- "autoregressive"
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

jpeg(file.path(path.plots,this.folder, "sim_vary_n4/eos_means.jpg"), width = 800, height = 800)

par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
plot(-1, 
     type="n",
     xlim = c(100,350),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "n4",
     ylab = "Power", 
     cex.lab = 2.5,
     frame.plot = FALSE)

all.yloc <- NULL

for(i in 1:10){
  this.scenario <- paste("sim_vary_n4/sim_results_", i, sep="")
  dat.plot <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
  axis(1, at = seq(100,300,50), cex.axis = 2.5, lwd = 5)
  axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
  lines(dat.plot$n4, dat.plot$power.diff.eos.means, lty = 2, lwd = 3, col = palette[i])
  points(dat.plot$n4, dat.plot$power.diff.eos.means, pch = 21, bg = palette[i], col = "black", cex = 3)
  all.yloc <- c(all.yloc, dat.plot$power.diff.eos.means[1])
}

all.deltaQ <- NULL

for(i in 1:10){
  this.scenario <- paste("sim_vary_eta/sim_results_", i, sep="")
  input.rand.time <- 2
  input.tot.time <- 6 
  input.cutoff <- 0
  is.plot <- FALSE
  source(file.path(path.code, "plot-truth-deltaQ.R"))
  deltaQ <- c(diff.eos.means.plusplus.minusplus)
  all.deltaQ <- c(all.deltaQ, deltaQ)
}

for(i in 1:10){
  text(x = 305, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[i], cex = 1.8, adj = 0)
}

dev.off()

###############################################################################
# Plot power for detecting difference in AUC
###############################################################################

jpeg(file.path(path.plots,this.folder, "sim_vary_n4/AUC.jpg"), width = 800, height = 800)

par(mar = c(6, 5, 0.5, 0.5) + 0.1)  # Bottom, left, top, right
plot(-1, 
     type="n",
     xlim = c(100,350),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "n4",
     ylab = "Power", 
     cex.lab = 2.5,
     frame.plot = FALSE)

all.yloc <- NULL

for(i in 1:10){
  this.scenario <- paste("sim_vary_n4/sim_results_", i, sep="")
  dat.plot <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
  axis(1, at = seq(100,300,50), cex.axis = 2.5, lwd = 5)
  axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
  lines(dat.plot$n4, dat.plot$power.diff.AUC, lty = 2, lwd = 3, col = palette[i])
  points(dat.plot$n4, dat.plot$power.diff.AUC, pch = 21, bg = palette[i], col = "black", cex = 3)
  all.yloc <- c(all.yloc, dat.plot$power.diff.AUC[1])
}

all.deltaQ <- NULL

for(i in 1:10){
  this.scenario <- paste("sim_vary_eta/sim_results_", i, sep="")
  input.rand.time <- 2
  input.tot.time <- 6 
  input.cutoff <- 0
  is.plot <- FALSE
  source(file.path(path.code, "plot-truth-deltaQ.R"))
  deltaQ <- c(diff.AUC.plusplus.minusplus)
  all.deltaQ <- c(all.deltaQ, deltaQ)
}

for(i in 1:10){
  text(x = 305, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[i], cex = 1.8, adj = 0)
}

dev.off()

