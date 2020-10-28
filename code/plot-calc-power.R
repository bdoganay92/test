###############################################################################
# User-specified design parameters when using input_means_d_-1.csv
# to calculate power
###############################################################################
.N.min <- 100
.N.max <- 700
.N.increment <- 50
.rho.grid <- c(0.30,0.80)
.rho.colors <- c("goldenrod3")

.path.code <- Sys.getenv("path.code")
source(file.path(.path.code, "calc-power-bootstrap.R"))

###############################################################################
# Code for plotting when, in truth, Delta_Q = 0
###############################################################################

.df.grid

# Plot sample size vs. power
# Display plots for end-of-study means and AUC side-by-side
for(.idx.grid in 1:length(.rho.grid)){
  op <- par() # save default settings
  par(mfrow = c(2,2), pty="m", mai = c(1.02, 0.82+0.5, 0.82, 0.42))
  
  # Power: difference in end-of-study means
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(0,0.10),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Power to Reject H0 when difference is zero \n(Normal Approximation)")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.06, lty=2)
  abline(h = 0.04, lty=2)
  legend("bottomright", c("alpha=0.05", "alpha=0.05 + 0.01", "alpha=0.05 - 0.01"), lty=c(1,2,2), cex = 0.80)
  
  title(main = "Difference in End-of-Study Means")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "normal.power.diff.eos.means"],
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
       ylab = "Power to Reject H0 when difference is zero \n(Normal Approximation)")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.06, lty=2)
  abline(h = 0.04, lty=2)
  legend("bottomright", c("alpha=0.05", "alpha=0.05 + 0.01", "alpha=0.05 - 0.01"), lty=c(1,2,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "normal.power.diff.AUC"], 
         pch=21, 
         bg=.rho.colors[.idx.grid])
  
  # Power: difference in end-of-study means
  plot(-1, 
       type="n",
       xlim = c(.N.min, .N.max),
       ylim = c(0,0.10),
       xaxt="n",
       yaxt="n",
       xlab = "N",
       ylab = "Power to Reject H0 when difference is zero \n(Parametric Bootstrap)")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.06, lty=2)
  abline(h = 0.04, lty=2)
  legend("bottomright", c("alpha=0.05", "alpha=0.05 + 0.01", "alpha=0.05 - 0.01"), lty=c(1,2,2), cex = 0.80)
  
  title(main = "Difference in End-of-Study Means")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bootstrap.power.diff.eos.means"],
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
       ylab = "Power to Reject H0 when difference is zero \n(Parametric Bootstrap)")
  
  axis(1, at = seq(.N.min, .N.max, .N.increment))
  axis(2, at = seq(0, 0.10, 0.01))
  abline(h = 0.05, lty=1)
  abline(h = 0.06, lty=2)
  abline(h = 0.04, lty=2)
  legend("bottomright", c("alpha=0.05", "alpha=0.05 + 0.01", "alpha=0.05 - 0.01"), lty=c(1,2,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bootstrap.power.diff.AUC"], 
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
  abline(h = 0.05, lty=2)
  abline(h = -0.05, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.05", "mean bias=-0.05"), lty=c(2,1,2), cex = 0.80)
  
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
  abline(h = 0.05, lty=2)
  abline(h = -0.05, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.05", "mean bias=-0.05"), lty=c(2,1,2), cex = 0.80)
  
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
  abline(h = 0.05, lty=2)
  abline(h = -0.05, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.05", "mean bias=-0.05"), lty=c(2,1,2), cex = 0.80)
  
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
  abline(h = 0.05, lty=2)
  abline(h = -0.05, lty=2)
  legend("bottomright", c("mean bias=0", "mean bias=+0.05", "mean bias=-0.05"), lty=c(2,1,2), cex = 0.80)
  
  title(main = "Difference in AUC")
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "bias.diff.AUC.stderr"], 
         pch=21, 
         bg="goldenrod3")
  
  par(op)
}

###############################################################################
# User-specified design parameters when using input_means_d_0.csv
# to calculate power
###############################################################################
.N.min <- 100
.N.max <- 800
.N.increment <- 25
.rho.grid <- c(0.30, 0.60, 0.80)
.rho.colors <- c("tomato", "darkolivegreen","cornflowerblue")
.this.folder <- "sim_results_d_0"

###############################################################################
# Code for plotting when, in truth, Delta_Q > 0
###############################################################################

# Plot sample size vs. power
# Display plots for end-of-study means and AUC side-by-side

op <- par() # save default settings
par(mfrow = c(1,2), pty="m")

# Power: difference in end-of-study means
plot(-1, 
     type="n",
     xlim = c(.N.min, .N.max),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0")

axis(1, at = seq(.N.min, .N.max, .N.increment*2))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in End-of-Study Means")

for(.idx.grid in 1:length(.rho.grid)){
  lines(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
        y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.eos.means"],
        type="l", col = .rho.colors[.idx.grid])
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.eos.means"],
         pch=21, 
         bg=.rho.colors[.idx.grid])
}

legend("bottomright", 
       c("rho=0.30", "rho=0.60", "rho=0.80"), 
       pch=21,
       lwd=c(1,1,1),
       col = c("tomato", "darkolivegreen","cornflowerblue"),
       pt.bg = c("tomato", "darkolivegreen","cornflowerblue"), 
       pt.lwd = c(3,3,3),
       cex = 0.80)

# Power: difference in AUC
plot(-1, 
     type="n",
     xlim = c(.N.min, .N.max),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0")

axis(1, at = seq(.N.min, .N.max, .N.increment*2))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in AUC")

for(.idx.grid in 1:length(.rho.grid)){
  lines(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
        y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.AUC"],
        type="l", col = .rho.colors[.idx.grid])
  points(x = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$rho==.rho.grid[.idx.grid], "power.diff.AUC"],
         pch=21, 
         bg=.rho.colors[.idx.grid])
}

legend("bottomright", 
       c("rho=0.30", "rho=0.60", "rho=0.80"), 
       pch=21,
       lwd=c(1,1,1),
       col = c("tomato", "darkolivegreen","cornflowerblue"),
       pt.bg = c("tomato", "darkolivegreen","cornflowerblue"), 
       pt.lwd=c(3,3,3),
       cex = 0.80)

par(op)


