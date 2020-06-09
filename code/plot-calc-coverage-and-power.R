.path.output_data <- Sys.getenv("path.output_data")

# Define grid
.df.grid <- expand.grid(d = c(-2, -1.5, -1, -0.5, 0, 0.5, 1),
                               N = c(250, 500))


.df.grid$coverage.diff.eos.means <- NA_real_
.df.grid$coverage.diff.AUC <- NA_real_
.df.grid$power.diff.eos.means <- NA_real_
.df.grid$power.diff.AUC <- NA_real_
.df.grid$truth.var.est.diff.eos.means <- NA_real_
.df.grid$truth.var.est.diff.AUC <- NA_real_

# Setting all.names=FALSE in the call to ls() removes all variables in the
# current environment except those beginning with a "."

for(.idx.grid in 1:nrow(.df.grid)){
  input.N <- .df.grid[.idx.grid, "N"]
  input.d <- .df.grid[.idx.grid, "d"]
  load(file.path(.path.output_data, paste("coverage_and_power_","d_",input.d,"_N_",input.N,".RData", sep="")))
  .df.grid[.idx.grid, "coverage.diff.eos.means"] <- coverage.diff.eos.means$coverage
  .df.grid[.idx.grid, "coverage.diff.AUC"] <- coverage.diff.AUC$coverage
  .df.grid[.idx.grid, "power.diff.eos.means"] <- power.diff.eos.means$power
  .df.grid[.idx.grid, "power.diff.AUC"] <- power.diff.AUC$power
  .df.grid[.idx.grid, "truth.var.est.diff.eos.means"] <- unique(truth.var.est.diff.eos.means$truth.var.est)
  .df.grid[.idx.grid, "truth.var.est.diff.AUC"] <- unique(truth.var.est.diff.AUC$truth.var.est)
  
  print(.idx.grid)
  rm(list = ls(all.names = FALSE))
}

.df.grid <- .df.grid[order(.df.grid$N, .df.grid$d),]
.df.grid$DeltaQ.eos.means <- .df.grid$d+2
.df.grid$DeltaQ.AUC <- 2*(.df.grid$d+2)

###############################################################################
# End of study means
###############################################################################

op <- par(mfrow = c(1,3), pty="m")
par(mfcol = c(1,3), pty="m")

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 3.5),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Variance of DeltaQ Estimates")

axis(1, at = 2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1))
axis(2, at = seq(0,1,0.10))
lines(x = .df.grid[.df.grid$N==500, "DeltaQ.eos.means"], y = .df.grid[.df.grid$N==500, "truth.var.est.diff.eos.means"], lwd=2, type="o")
title(main = "Difference in End-of-Study Means")

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 3.5),
     ylim = c(0.85,1),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Coverage of DeltaQ")

axis(1, at = 2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1))
axis(2, at = seq(0.85, 1, 0.05))
lines(x = .df.grid[.df.grid$N==500, "DeltaQ.eos.means"], y = .df.grid[.df.grid$N==500, "coverage.diff.eos.means"], lwd=2, type="o")
title(main = "Difference in End-of-Study Means")
abline(h = 0.95, lty=2)
legend("topright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 3.5),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Power to Reject H0")

axis(1, at = 2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1))
axis(2, at = seq(0, 1, 0.10))
lines(x = .df.grid[.df.grid$N==500, "DeltaQ.eos.means"], y = .df.grid[.df.grid$N==500, "power.diff.eos.means"], lwd=2, type="o")
title(main = "Difference in End-of-Study Means")
abline(h = 0.05, lty=2)
legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

par(op)

###############################################################################
# AUC
###############################################################################

op <- par(mfrow = c(1,3), pty="m")
par(mfcol = c(1,3), pty="m")

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 6.5),
     ylim = c(0,14),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Variance of DeltaQ Estimates")

axis(1, at = 2*(2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1)))
axis(2, at = seq(0,14, 2))
lines(x = .df.grid[.df.grid$N==250, "DeltaQ.AUC"], y = .df.grid[.df.grid$N==250, "truth.var.est.diff.AUC"], lwd=2, type="o")
title(main = "Difference in AUC")

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 6.5),
     ylim = c(0.85,1),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Coverage of DeltaQ")

axis(1, at = 2*(2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1)))
axis(2, at = seq(0.85, 1, 0.05))
lines(x = .df.grid[.df.grid$N==250, "DeltaQ.AUC"], y = .df.grid[.df.grid$N==250, "coverage.diff.AUC"], lwd=2, type="o")
title(main = "Difference in AUC")
abline(h = 0.95, lty=2)
legend("topright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(-0.5, 6.5),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "Power to Reject H0")

axis(1, at = 2*(2+c(-2,-1.5, -1.0, -0.5, 0, 0.5, 1)))
axis(2, at = seq(0, 1, 0.10))
lines(x = .df.grid[.df.grid$N==250, "DeltaQ.AUC"], y = .df.grid[.df.grid$N==250, "power.diff.AUC"], lwd=2, type="o")
title(main = "Difference in AUC")
abline(h = 0.05, lty=2)
legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

par(op)

