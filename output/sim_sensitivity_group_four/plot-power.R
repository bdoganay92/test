# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")

jpeg(file.path(.path.output_data, "sim_sensitivity_group_four", "plot_power_sensitivity_group_four.jpeg"), width = 1332, height = 1020)

op <- par() # save default settings
par(mfrow = c(1,2), pty="m", mar = c(5, 4, 6, 2) + 0.1)
palette <- c("darkgoldenrod","forestgreen","firebrick3","lightcoral","mistyrose1",
             "bisque3","lightblue4","steelblue4","wheat3","grey30")

plot(-1, 
     type="n",
     xlim = c(100, 190),
     ylim = c(-0.3,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4 (n4)",
     ylab = "Power")

axis(1, at = seq(100, 190, 10))
axis(2, at = seq(0, 1, 0.10))

title(main = "Difference in End-of-Study Means (delta_EOS) where, across all points, N=500\n Power increased from Scenario 1 (bottom-most solid dots; delta_EOS=0)\n to Scenario 10 (top-most solid dots; delta_EOS=1.755)")

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder.alternative <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder.alternative, "power.RData"))
        plotdat <- .df.vary.params
        points(plotdat$n4, plotdat$power.eos.means, pch=21, bg = palette[1+idx], col="black", cex=2)       
}

legend("bottomleft", 
       legend=paste("Solid dots representing power calculated under Scenario ", 1+c(0,1,2,3,4,5,6,7,8,9), sep=""), 
       pt.cex = 2, 
       pch=21, 
       pt.bg = palette[1+c(0,1,2,3,4,5,6,7,8,9)])

plot(-1, 
     type="n",
     xlim = c(100, 190),
     ylim = c(-0.3,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4 (n4)",
     ylab = "Power")

axis(1, at = seq(100, 190, 10))
axis(2, at = seq(0, 1, 0.10))

title(main = "Difference in AUC (delta_AUC) where, across all points, N=500\n Power increased from Scenario 1 (bottom-most solid dots; delta_AUC=0)\n to Scenario 10 (top-most solid dots; delta_AUC=8.033)")

for(idx in c(0,1,2,3,4,5,6,7,8,9)){
        .this.folder.alternative <- paste("sim_sensitivity_group_four/sim_results_", idx, sep="")
        load(file = file.path(.path.output_data, .this.folder.alternative, "power.RData"))
        plotdat <- .df.vary.params
        points(plotdat$n4, plotdat$power.AUC, pch=21, bg = palette[1+idx], col="black", cex=2)       
}

legend("bottomleft", 
       legend=paste("Solid dots representing power calculated under Scenario ", 1+c(0,1,2,3,4,5,6,7,8,9), sep=""), 
       pt.cex = 2, 
       pch=21, 
       pt.bg = palette[1+c(0,1,2,3,4,5,6,7,8,9)])


dev.off()

