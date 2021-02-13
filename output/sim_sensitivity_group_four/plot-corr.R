.path.output_data <- Sys.getenv("path.output_data")

##########################################################################################
# Loading this RData file will place collect.correlation.tau in the global environment
##########################################################################################

.list.all <- list()

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder, "correspondence_between_rho_and_tau.RData"))
        collect.correlation.tau$diff <- collect.correlation.tau$tau.max - collect.correlation.tau$tau.min
        print(max(collect.correlation.tau$diff))
        .list.all <- append(.list.all, list(collect.correlation.tau))      
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
     ylab="tau")

axis(1, seq(0, 1, 0.10))
axis(2, seq(0,1, 0.10))
  
# Begin drawing lines corresponding to tau.max
for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  lines(x = .df.tau.max[["rho"]], 
        y = .df.tau.max[[paste("tau.max.scenario.",idx+1,sep="")]], 
        type="l", col=palette[1+idx], lwd=2, lty=3)
  points(x = .df.tau.max[["rho"]], 
         y = .df.tau.max[[paste("tau.max.scenario.",idx+1,sep="")]], 
         pch=21, bg = palette[1+idx], col="black", cex=1.7)
}

# Begin drawing lines corresponding to tau.min
for(idx in c(0,1,2,3,4,5,6,7,8,9)){
  lines(x = .df.tau.min[["rho"]], 
        y = .df.tau.min[[paste("tau.min.scenario.",idx+1,sep="")]], 
        type="l", col=palette[1+idx], lwd=2, lty=3)
  points(x = .df.tau.min[["rho"]], 
         y = .df.tau.min[[paste("tau.min.scenario.",idx+1,sep="")]], 
         pch=24, bg = palette[1+idx], col="black", cex=1)
}


title(main = "Across Scenarios 1-10, relationship between rho and tau_max, and rho and tau_min")

legend("topleft", 
       legend=c(paste("Solid dots representing tau_max in Scenario ", 1+c(0,1,2,3,4,5,6,7,8,9), sep=""),
                paste("Solid triangles representing tau_min in Scenario ", 1+c(0,1,2,3,4,5,6,7,8,9), sep="")), 
       pt.cex = 2, 
       pch=c(rep(21,10),rep(24,10)), 
       pt.bg = c(palette[1+c(0,1,2,3,4,5,6,7,8,9)],
                 palette[1+c(0,1,2,3,4,5,6,7,8,9)]))



text(0.12+0.15, 0, labels = "All Scenarios (rho=0.15, tau_max=0.11, tau_min=0.08)", cex = 0.8, col = "red")
text(0.12+0.55, 0.27, labels = "All Scenarios (rho=0.55, tau_max=0.45, tau_min=0.31)", cex = 0.8, col = "red")
text(0.07+0.80, 0.5, labels = "All Scenarios (rho=0.80, tau_max=0.73, tau_min=0.67)", cex = 0.8, col = "red")

dev.off()

