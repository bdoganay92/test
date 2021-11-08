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

these.scenarios <- 1:10

jpeg(file.path(path.plots,
               this.folder,
               paste("sim_vary_effect/eos_means",".jpg",sep="")),
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

for(increment in seq(0,1,.10)){
  segments(x0 = 100, y0 = increment, x1 = 550, y1 = increment, col = "snow2")
}

for(increment in seq(100,550,10)){
  segments(x0 = increment, y0 = 0, x1 = increment, y1 = 1, col = "snow2")
}

all.yloc <- NULL
all.rho <- c(0.2, 0.6)
linestyle <- c(3,1)

for(i in these.scenarios){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
  
  for(idx.rho in 1:length(all.rho)){
    current.rho <- all.rho[idx.rho]
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
    lines(dat.plot$N, dat.plot$power.diff.eos.means, lty = linestyle[idx.rho], lwd = 2, col = palette[i])
    points(dat.plot$N, dat.plot$power.diff.eos.means, pch = 21, bg = palette[i], col = "black", cex = 0.7)
    
    if(idx.rho == 1){
      all.yloc <- c(all.yloc, dat.plot[which(dat.plot$N==550),"power.diff.eos.means"])
    }
  }
}

all.deltaQ <- NULL

for(i in these.scenarios){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  input.rand.time <- 2
  input.tot.time <- 6 
  input.cutoff <- 0
  is.plot <- FALSE
  source(file.path(path.code, "plot-truth-deltaQ.R"))
  deltaQ <- c(diff.eos.means.plusplus.minusplus)
  all.deltaQ <- c(all.deltaQ, deltaQ)
}

for(i in 1:length(all.deltaQ)){
  text(x = 580, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[these.scenarios[i]], cex = 1.8, adj = 0)
}

legend(x = 100, y = 1, c("rho = 0.6","rho = 0.2"), lty = c(1,3), lwd = c(3,3), cex = 1.5)

dev.off()

###############################################################################
# Plot power for detecting difference in AUC
###############################################################################

these.scenarios <- c(3, 6, 9)

jpeg(file.path(path.plots,
               this.folder,
               paste("sim_vary_effect/AUC",".jpg",sep="")),
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

for(increment in seq(0,1,.10)){
  segments(x0 = 100, y0 = increment, x1 = 550, y1 = increment, col = "snow2")
}

for(increment in seq(100,550,10)){
  segments(x0 = increment, y0 = 0, x1 = increment, y1 = 1, col = "snow2")
}

all.yloc <- NULL
all.rho <- c(0.2, 0.6)
linestyle <- c(3,1)

for(i in these.scenarios){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  dat.current <- read.csv(file.path(path.output_data, this.folder, this.scenario, "power.csv"))
  
  for(idx.rho in 1:length(all.rho)){
    current.rho <- all.rho[idx.rho]
    dat.plot <- dat.current %>% filter(rho == current.rho)
    axis(1, at = seq(100, 600, 50), cex.axis = 2.5, lwd = 5)
    axis(2, at = seq(0, 1, 0.20), cex.axis = 2.5, lwd = 5)
    lines(dat.plot$N, dat.plot$power.diff.AUC, lty = linestyle[idx.rho], lwd = 2, col = palette[i])
    points(dat.plot$N, dat.plot$power.diff.AUC, pch = 21, bg = palette[i], col = "black", cex = 0.7)
    
    if(idx.rho == 1){
      all.yloc <- c(all.yloc, dat.plot[which(dat.plot$N==550),"power.diff.AUC"])
    }
  }
}

all.deltaQ <- NULL

for(i in these.scenarios){
  this.scenario <- paste("sim_vary_effect/sim_results_", i, sep="")
  input.rand.time <- 2
  input.tot.time <- 6 
  input.cutoff <- 0
  is.plot <- FALSE
  source(file.path(path.code, "plot-truth-deltaQ.R"))
  deltaQ <- c(diff.AUC.plusplus.minusplus)
  all.deltaQ <- c(all.deltaQ, deltaQ)
}

for(i in 1:length(all.deltaQ)){
  text(x = 580, y = all.yloc[i], paste("delta=", round(all.deltaQ[i], 2),sep=""), col = palette[these.scenarios[i]], cex = 1.8, adj = 0)
}

legend(x = 100, y = 1, c("rho = 0.6","rho = 0.2"), lty = c(1,3), lwd = c(3,3), cex = 1.5)

dev.off()

