# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.alternative <- "sim_sensitivity_group_four/sim_results_1"

load(file = file.path(.path.output_data, .this.folder.alternative, "power.RData"))
plotdat <- .df.vary.params

###############################################################################
# Power for difference in estimates of end-of-study means and AUC
###############################################################################

op <- par() # save default settings
par(mfrow = c(1,2), pty="m", mar = c(5, 4, 6, 2) + 0.1)

plot(-1, 
     type="n",
     xlim = c(100, 190),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4",
     ylab = "Power")

axis(1, at = seq(100, 190, 20))
axis(2, at = seq(0, 1, 0.10))

title(main = "Estimates of Difference in End-of-Study Means\nPower at varying n4\nwith total sample size N fixed to 500 all throughout")
points(plotdat$n4, plotdat$power.eos.means, pch=21, bg = "darkgoldenrod", col="black")

plot(-1, 
     type="n",
     xlim = c(100, 190),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "Number of individuals in Group 4",
     ylab = "Power")

axis(1, at = seq(100, 190, 20))
axis(2, at = seq(0, 1, 0.10))

title(main = "Estimates of Difference in AUC\nPower at varying n4\nwith total sample size N fixed to 500 all throughout")
points(plotdat$n4, plotdat$power.AUC, pch=21, bg = "darkgoldenrod", col="black")

par(op)

