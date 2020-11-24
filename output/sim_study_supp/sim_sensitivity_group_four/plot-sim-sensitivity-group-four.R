# Specify file paths
.path.output_data <- Sys.getenv("path.output_data")
.this.folder.alternative <- "sim_study_supp/sim_sensitivity_group_four/sim_results_alternative"
.this.folder.null <- "sim_study_supp/sim_sensitivity_group_four/sim_results_null"

load(file = file.path(.path.output_data, "sim_study_supp/sim_sensitivity_group_four", "power_method_01.RData"))
df1 <- .df.vary.params

load(file = file.path(.path.output_data, "sim_study_supp/sim_sensitivity_group_four", "power_method_02.RData"))
df2 <- .df.vary.params

###############################################################################
# Difference in estimates of end-of-study means and AUC: Method 1
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

title(main = "Estimates of Difference in End-of-Study Means\nPower calculated using Method 1\nwith total sample size N fixed to 500 all throughout")
points(df1$n4, df1$power.eos.means, pch=21, bg = "darkgoldenrod", col="black")

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

title(main = "Estimates of Difference in AUC\nPower calculated using Method 1\nwith total sample size N fixed to 500 all throughout")
points(df1$n4, df1$power.AUC, pch=21, bg = "darkgoldenrod", col="black")

par(op)

###############################################################################
# Difference in estimates of end-of-study means and AUC: Method 2
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

title(main = "Estimates of Difference in End-of-Study Means\nPower calculated using Method 2\nwith total sample size N fixed to 500 all throughout")
points(df2$n4, df2$power.eos.means, pch=21, bg = "darkgoldenrod", col="black")

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

title(main = "Estimates of Difference in AUC\nPower calculated using Method 2\nwith total sample size N fixed to 500 all throughout")
points(df2$n4, df2$power.AUC, pch=21, bg = "darkgoldenrod", col="black")

par(op)

