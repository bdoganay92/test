.path.output_data <- Sys.getenv("path.output_data")

##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################

.list.all <- list()

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder, "correspondence_between_rho_and_tau.RData"))
        .list.all <- append(.list.all, list(collect.correlation.tau))      
}

.df.tau.mean <- data.frame(rho = .list.all[[1]]$datagen.params.rho)

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  .df.tau.mean[[paste("tau.mean.scenario.",idx+1,sep="")]] <- .list.all[[idx+1]]$tau.mean
}

.df.tau.min <- data.frame(rho = .list.all[[1]]$datagen.params.rho)

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  .df.tau.min[[paste("tau.min.scenario.",idx+1,sep="")]] <- .list.all[[idx+1]]$tau.min
}

.df.tau.max <- data.frame(rho = .list.all[[1]]$datagen.params.rho)

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  .df.tau.max[[paste("tau.max.scenario.",idx+1,sep="")]] <- .list.all[[idx+1]]$tau.max
}

##########################################################################################
# Prepare for plotting
##########################################################################################
.df.tau.mean <- round(.df.tau.mean, digits=3)
.df.tau.min <- round(.df.tau.min, digits=3)
.df.tau.max <- round(.df.tau.max, digits=3)

##########################################################################################
# Set up plot region
##########################################################################################
jpeg(file.path(.path.output_data, "sim_sensitivity_group_four/plot_correspondence_between_rho_and_tau_mean.jpeg"), width = 800, height = 800)

palette <- c("darkgoldenrod","forestgreen","firebrick3","lightcoral","mistyrose1",
             "bisque3","lightblue4","steelblue4","wheat3","grey30")

plot(x=-2, 
     type="n", 
     xlim=c(-.05,1), 
     ylim=c(-.05,1), 
     xaxt="n", 
     yaxt="n", 
     xlab="rho", 
     ylab="tau_mean")

abline(a=0,b=1,lty=2)
axis(1, seq(0, 1, 0.10))
axis(2, seq(0,1, 0.10))
  
# Begin drawing lines corresponding to tau.mean
for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  lines(x = .df.tau.mean[["rho"]], 
        y = .df.tau.mean[[paste("tau.mean.scenario.",idx+1,sep="")]], 
        type="l", col=palette[1+idx], lwd=2, lty=3)
  points(x = .df.tau.mean[["rho"]], 
         y = .df.tau.mean[[paste("tau.mean.scenario.",idx+1,sep="")]], 
         pch=21, bg = palette[1+idx], col="black", cex=1.7)
}

legend("topleft", 
       legend=paste("Solid dots representing (rho,tau_MEAN) under Scenario ", 1+c(0,1,2,3,4,5,6,7,8,9), sep=""), 
       pt.cex = 2, 
       pch=21, 
       pt.bg = palette[1+c(0,1,2,3,4,5,6,7,8,9)])

title(main = "Across Scenarios 1-10, the relationship between rho and tau_MEAN\nremained largely the same, as shown by the position of solid dots")

dev.off()

