###############################################################################
# User-specified design parameters
###############################################################################
.N.min <- 100
.N.max <- 600
.N.increment <- 25
.rho.grid <- c(0.80)
.rho.colors <- c("goldenrod3")

###############################################################################
# Proceed with steps required to plot output of the script
# calc-coverage-and-power.R
#
# Note: the call rm(list = ls(all.names = FALSE)) will remove all objects
# in the current namespace except for those beginning with a "dot"
# e.g., after the call rm(list = ls(all.names = FALSE)), the object
# .path.output_data and .df.grid will still be available and will retain 
# their value
###############################################################################

# .path.output_data is the location of the output of calc-coverage-and-power.R
.path.output_data <- Sys.getenv("path.output_data")

# Tabulate all possible combinations of N and rho to plot
.df.grid <- expand.grid(N = seq(.N.min, .N.max, .N.increment), rho = .rho.grid)

# Add columns to .df.grid
.df.grid$power.diff.eos.means <- NA_real_
.df.grid$power.diff.AUC <- NA_real_

.df.grid$bias.diff.eos.means.estimates <- NA_real_
.df.grid$bias.diff.eos.means.stderr <- NA_real_

.df.grid$bias.diff.AUC.estimates <- NA_real_
.df.grid$bias.diff.AUC.stderr <- NA_real_

# Go through each combination of values in .df.grid
# and read in results obtained using calc-coverage-and-power.R
for(.idx.grid in 1:nrow(.df.grid)){
  input.N <- .df.grid[.idx.grid, "N"]
  input.rho <- .df.grid[.idx.grid, "rho"]
  load(file.path(.path.output_data, paste("coverage_and_power_","_N_",input.N,"_rho_",input.rho,".RData", sep="")))
  
  # Fill in .df.grid
  .df.grid[.idx.grid, "power.diff.eos.means"] <- power.diff.eos.means$power
  .df.grid[.idx.grid, "power.diff.AUC"] <- power.diff.AUC$power
  
  .df.grid[.idx.grid, "bias.diff.eos.means.estimates"] <- bias.diff.eos.means$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.eos.means.stderr"] <- bias.diff.eos.means$ave.bias.stderr
  
  .df.grid[.idx.grid, "bias.diff.AUC.estimates"] <- bias.diff.AUC$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.AUC.stderr"] <- bias.diff.AUC$ave.bias.stderr
  
  # Remove objects from namespace and proceed to next iteration of loop
  rm(list = ls(all.names = FALSE))
}

###############################################################################
# Code for plotting when, in truth, Delta_Q = 0
###############################################################################

# Plot sample size vs. power
# Display plots for end-of-study means and AUC side-by-side
for(.idx.grid in 1:length(.rho.grid)){
  op <- par(mfrow = c(1,2), pty="m")
  par(mfrow = c(1,2), pty="m")
  
  # Power: difference in end-of-study means
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(0,0.10),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Power to Reject H0 when difference is zero")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.03, lty=2)
  abline(h = 0.07, lty=2)
  legend("bottomright", c("alpha=0.05 - 0.02", "alpha=0.05", "alpha=0.05 + 0.02"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in End-of-Study Means")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.eos.means"],
         pch=21, 
         bg=.rho.colors[.idx.grid])
  
  # Power: difference in AUC
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(0,0.10),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Power to Reject H0 when difference is zero")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.03, lty=2)
  abline(h = 0.07, lty=2)
  legend("bottomright", c("alpha=0.05 - 0.02", "alpha=0.05", "alpha=0.05 + 0.02"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.AUC"], 
         pch=21, 
         bg=.rho.colors[.idx.grid])
  
  par(op)
}

# Plot of bias in estimates
# Display plots for end-of-study means and AUC side-by-side

for(.idx.grid in 1:length(.rho.grid)){
  op <- par(mfrow = c(2,2), pty="m")
  par(mfrow = c(2,2), pty="m")
  
  # Estimates of difference in end-of-study means
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(-0.20, 0.20),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Mean bias in estimates of difference")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(-0.20, 0.20, 0.05))
  abline(h = 0, lty=1)
  abline(h = 0.01, lty=2)
  abline(h = -0.01, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in End-of-Study Means")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bias.diff.eos.means.estimates"], 
         pch=21, 
         bg="goldenrod3")
  
  # Estimates of difference in AUC
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(-0.20, 0.20),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Mean bias in estimates of difference")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(-0.20, 0.20, 0.05))
  abline(h = 0, lty=1)
  abline(h = 0.01, lty=2)
  abline(h = -0.01, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bias.diff.AUC.estimates"], 
         pch=21, 
         bg="goldenrod3")
  
  # Estimates of Standard error: difference in end-of-study means
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(-0.20,0.20),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Mean bias in estimates of standard error")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(-0.20, 0.20, 0.05))
  abline(h = 0, lty=1)
  abline(h = 0.01, lty=2)
  abline(h = -0.01, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in End-of-Study Means")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], 
                      "bias.diff.eos.means.stderr"], 
         pch=21, 
         bg="goldenrod3")
  
  # Estimates of standard error: difference in AUC
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(-0.20, 0.20),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Mean bias in estimates of standard error")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(-0.20, 0.20, 0.05))
  abline(h = 0, lty=1)
  abline(h = 0.01, lty=2)
  abline(h = -0.01, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bias.diff.AUC.stderr"], 
         pch=21, 
         bg="goldenrod3")
  
  par(op)
}

###############################################################################
# Code for plotting when, in truth, Delta_Q > 0
###############################################################################








