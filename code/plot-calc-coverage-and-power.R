.path.output_data <- "C:/Users/jamieyap/Box/Simulation Results"

###############################################################################################
# Define grid
.df.grid <- expand.grid(d = seq(-1.5,1,0.5), rho = c(0.2,0.45,0.75))
.df.grid$N.required.diff.eos.means <- NA_real_
.df.grid$N.required.diff.AUC <- NA_real_

for(.idx.grid in 1:nrow(.df.grid)){
  input.d <- .df.grid[.idx.grid, "d"]
  input.rho <- .df.grid[.idx.grid, "rho"]
  #Nrequired_rho_0.2_d_-1.5.RData
  load(file.path(.path.output_data, paste("Nrequired","_rho_",input.rho,"_d_",input.d,".RData", sep="")))
  .df.grid[.idx.grid, "N.required.diff.eos.means"] <- N.required.eos.means
  .df.grid[.idx.grid, "N.required.diff.AUC"] <- N.required.AUC
  rm(list = ls(all.names = FALSE))
}

.df.grid$DeltaQ.eos.means <- .df.grid$d+2
.df.grid$DeltaQ.AUC <- 2*(.df.grid$d+2)

op <- par(mfrow = c(1,2), pty="m")
par(mfrow = c(1,2), pty="m")

plot(-1, 
     type="n",
     xlim = c(0.5,3),
     ylim = c(400,1100),
     xaxt="n",
     yaxt="n",
     xlab = "Difference in end-of-study means",
     ylab = "Required total sample size N")

axis(1, at = seq(0.5,3,0.5))
axis(2, at = seq(400,1100,100))
abline(v = 1, lty=2, lwd=1.5)
lines(x = .df.grid[.df.grid$rho==0.20, "DeltaQ.eos.means"], y = .df.grid[.df.grid$rho==0.20, "N.required.diff.eos.means"], type="o", lwd=1.75, col="red")
lines(x = .df.grid[.df.grid$rho==0.45, "DeltaQ.eos.means"], y = .df.grid[.df.grid$rho==0.45, "N.required.diff.eos.means"], type="o", lwd=1.75, col="cornflowerblue")
lines(x = .df.grid[.df.grid$rho==0.75, "DeltaQ.eos.means"], y = .df.grid[.df.grid$rho==0.75, "N.required.diff.eos.means"], type="o", lwd=1.75, col="seagreen")

legend("topright", 
       c("difference in end-of-study means=1", 
         "required total sample size N when tau=0.12", 
         "required total sample size N when tau=0.32", 
         "required total sample size N when tau=0.62"), 
       lwd=c(2,2,2,2), 
       lty=c(2,1,1,1), col=c("black","red","cornflowerblue","seagreen"), cex = 0.80)
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(1,6),
     ylim = c(100,300),
     xaxt="n",
     yaxt="n",
     xlab = "Difference in AUC",
     ylab = "Required total sample size N")

axis(1, at = seq(1,6,1))
axis(2, at = seq(100,300,20))
abline(v = 2, lty=2, lwd=1.5)
lines(x = .df.grid[.df.grid$rho==0.20, "DeltaQ.AUC"], y = .df.grid[.df.grid$rho==0.20, "N.required.diff.AUC"], type="o", lwd=1.75, col="red")
lines(x = .df.grid[.df.grid$rho==0.45, "DeltaQ.AUC"], y = .df.grid[.df.grid$rho==0.45, "N.required.diff.AUC"], type="o", lwd=1.75, col="cornflowerblue")
lines(x = .df.grid[.df.grid$rho==0.75, "DeltaQ.AUC"], y = .df.grid[.df.grid$rho==0.75, "N.required.diff.AUC"], type="o", lwd=1.75, col="seagreen")

legend("topright", 
       c("difference in AUC=2", 
         "required total sample size N when tau=0.12", 
         "required total sample size N when tau=0.32", 
         "required total sample size N when tau=0.62"), 
       lwd=c(2,2,2,2), 
       lty=c(2,1,1,1), col=c("black","red","cornflowerblue","seagreen"), cex = 0.80)
title(main = "Difference in AUC")

par(op)











.path.output_data <- "C:\\Users\\jamieyap\\Box\\Simulation Results"

###############################################################################################
# Define grid
.df.grid <- expand.grid(d = c(-1), N = seq(100, 500, 25), rho = c(0.20,0.45,0.75))
.df.grid$power.diff.eos.means <- NA_real_
.df.grid$power.diff.AUC <- NA_real_

for(.idx.grid in 1:nrow(.df.grid)){
  input.N <- .df.grid[.idx.grid, "N"]
  input.d <- .df.grid[.idx.grid, "d"]
  input.rho <- .df.grid[.idx.grid, "rho"]
  load(file.path(.path.output_data, paste("M10000_coverage_and_power_","d_",input.d,"_N_",input.N,"_rho_",input.rho,".RData", sep="")))
  .df.grid[.idx.grid, "power.diff.eos.means"] <- power.diff.eos.means$power
  .df.grid[.idx.grid, "power.diff.AUC"] <- power.diff.AUC$power
  
  .df.grid[.idx.grid, "bias.diff.eos.means.estimates"] <- bias.diff.eos.means$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.eos.means.stderr"] <- bias.diff.eos.means$ave.bias.stderr
  
  .df.grid[.idx.grid, "bias.diff.AUC.estimates"] <- bias.diff.AUC$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.AUC.stderr"] <- bias.diff.AUC$ave.bias.stderr
  rm(list = ls(all.names = FALSE))
}

.df.grid <- .df.grid[order(.df.grid$N, .df.grid$d),]
.df.grid$DeltaQ.eos.means <- .df.grid$d+2
.df.grid$DeltaQ.AUC <- 2*(.df.grid$d+2)

op <- par(mfrow = c(1,2), pty="m")
par(mfrow = c(1,2), pty="m")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when difference in end-of-study means is 1")

axis(1, at = seq(100,500,25))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2, lwd=1.5)
lines(x = .df.grid[.df.grid$rho==0.20, "N"], y = .df.grid[.df.grid$rho==0.20, "power.diff.eos.means"], type="o", lwd=1.75, col="red")
lines(x = .df.grid[.df.grid$rho==0.45, "N"], y = .df.grid[.df.grid$rho==0.45, "power.diff.eos.means"], type="o", lwd=1.75, col="cornflowerblue")
lines(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.eos.means"], type="o", lwd=1.75, col="seagreen")

legend("bottomright", c("power=0.80", "power when tau=0.12", "power when tau=0.32", "power when tau=0.62"), lwd=c(2,2,2,2), lty=c(2,1,1,1), col=c("black","red","cornflowerblue","seagreen"), cex = 0.90)
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(0,1),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when difference in AUC is 2")

axis(1, at = seq(100,500,25))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2, lwd=1.5)
lines(x = .df.grid[.df.grid$rho==0.20, "N"], y = .df.grid[.df.grid$rho==0.20, "power.diff.AUC"], type="o", lwd=1.75, col="red")
lines(x = .df.grid[.df.grid$rho==0.45, "N"], y = .df.grid[.df.grid$rho==0.45, "power.diff.AUC"], type="o", lwd=1.75, col="cornflowerblue")
lines(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.AUC"], type="o", lwd=1.75, col="seagreen")

legend("bottomright", c("power=0.80", "power when tau=0.12", "power when tau=0.32", "power when tau=0.62"), lwd=c(2,2,2,2), lty=c(2,1,1,1), col=c("black","red","cornflowerblue","seagreen"), cex = 0.90)
title(main = "Difference in AUC")

par(op)







###############################################################################################
# Define grid
.df.grid <- expand.grid(d = c(-2), N = seq(100, 500, 25), rho = c(0.75))
.df.grid$power.diff.eos.means <- NA_real_
.df.grid$power.diff.AUC <- NA_real_

.df.grid$bias.diff.eos.means.estimates <- NA_real_
.df.grid$bias.diff.eos.means.stderr <- NA_real_

.df.grid$bias.diff.AUC.estimates <- NA_real_
.df.grid$bias.diff.AUC.stderr <- NA_real_

for(.idx.grid in 1:nrow(.df.grid)){
  input.N <- .df.grid[.idx.grid, "N"]
  input.d <- .df.grid[.idx.grid, "d"]
  input.rho <- .df.grid[.idx.grid, "rho"]
  load(file.path(.path.output_data, paste("M10000_coverage_and_power_","d_",input.d,"_N_",input.N,"_rho_",input.rho,".RData", sep="")))
  .df.grid[.idx.grid, "power.diff.eos.means"] <- power.diff.eos.means$power
  .df.grid[.idx.grid, "power.diff.AUC"] <- power.diff.AUC$power
  
  .df.grid[.idx.grid, "bias.diff.eos.means.estimates"] <- bias.diff.eos.means$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.eos.means.stderr"] <- bias.diff.eos.means$ave.bias.stderr
  
  .df.grid[.idx.grid, "bias.diff.AUC.estimates"] <- bias.diff.AUC$ave.bias.diff
  .df.grid[.idx.grid, "bias.diff.AUC.stderr"] <- bias.diff.AUC$ave.bias.stderr
  rm(list = ls(all.names = FALSE))
}

.df.grid <- .df.grid[order(.df.grid$N, .df.grid$d),]
.df.grid$DeltaQ.eos.means <- .df.grid$d+2
.df.grid$DeltaQ.AUC <- 2*(.df.grid$d+2)


op <- par(mfrow = c(1,2), pty="m")
par(mfrow = c(1,2), pty="m")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when difference is zero")

axis(1, at = seq(100,500,25))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=1)
abline(h = 0.04, lty=2)
abline(h = 0.06, lty=2)
legend("bottomright", 
       c("alpha=0.04", "alpha=0.05", "alpha=0.06"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.eos.means"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when difference is zero")

axis(1, at = seq(100,500,25))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=1)
abline(h = 0.04, lty=2)
abline(h = 0.06, lty=2)
legend("bottomright", 
       c("alpha=0.04", "alpha=0.05", "alpha=0.06"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.AUC"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")

par(op)

op <- par(mfrow = c(2,2), pty="m")
par(mfrow = c(2,2), pty="m")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(-0.10,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Mean bias in estimates of difference")

axis(1, at = seq(100,500,25))
axis(2, at = seq(-0.10, 0.10, 0.02))
abline(h = 0, lty=1)
abline(h = 0.01, lty=2)
abline(h = -0.01, lty=2)
legend("bottomright", 
       c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "bias.diff.eos.means.estimates"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(-0.10,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Mean bias in estimates of difference")

axis(1, at = seq(100,500,25))
axis(2, at = seq(-0.10, 0.10, 0.02))
abline(h = 0, lty=1)
abline(h = 0.01, lty=2)
abline(h = -0.01, lty=2)
legend("bottomright", 
       c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "bias.diff.AUC.estimates"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(-0.10,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Mean bias in estimates of standard error")

axis(1, at = seq(100,500,25))
axis(2, at = seq(-0.10, 0.10, 0.02))
abline(h = 0, lty=1)
abline(h = 0.01, lty=2)
abline(h = -0.01, lty=2)
legend("bottomright", 
       c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "bias.diff.eos.means.stderr"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(100,500),
     ylim = c(-0.10,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Mean bias in estimates of standard error")

axis(1, at = seq(100,500,25))
axis(2, at = seq(-0.10, 0.10, 0.02))
abline(h = 0, lty=1)
abline(h = 0.01, lty=2)
abline(h = -0.01, lty=2)
legend("bottomright", 
       c("mean bias=0", "mean bias=+0.01", "mean bias=-0.01"), 
       lty=c(2,1,2), cex = 0.90)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "bias.diff.AUC.stderr"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")


par(op)














###############################################################################
# Old code
###############################################################################

.path.output_data <- Sys.getenv("path.output_data")

# Define grid
.df.grid <- expand.grid(d = c(-2), N = seq(100, 1100, 25), rho = c(0.20, 0.45,0.75))
#.df.grid <- expand.grid(d = c(-2), N = seq(100, 1100, 25))
#.df.grid <- expand.grid(d = c(-2), N = seq(100, 750, 25))
.df.grid$power.diff.eos.means <- NA_real_
.df.grid$power.diff.AUC <- NA_real_


for(.idx.grid in 1:nrow(.df.grid)){
  input.N <- .df.grid[.idx.grid, "N"]
  input.d <- .df.grid[.idx.grid, "d"]
  input.rho <- .df.grid[.idx.grid, "rho"]
  load(file.path(.path.output_data, paste("coverage_and_power_","d_",input.d,"_N_",input.N,"_rho_",input.rho ,".RData", sep="")))
  #load(file.path(.path.output_data, paste("M5000_coverage_and_power_","d_",input.d,"_N_",input.N,"_rho_0.75",".RData", sep="")))
  .df.grid[.idx.grid, "power.diff.eos.means"] <- power.diff.eos.means$power
  .df.grid[.idx.grid, "power.diff.AUC"] <- power.diff.AUC$power
  rm(list = ls(all.names = FALSE))
}

.df.grid <- .df.grid[order(.df.grid$N, .df.grid$d),]
.df.grid$DeltaQ.eos.means <- .df.grid$d+2
.df.grid$DeltaQ.AUC <- 2*(.df.grid$d+2)

op <- par(mfrow = c(1,3), pty="m")
par(mfrow = c(1,3), pty="m")
# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)

points(x = .df.grid[.df.grid$rho==0.20, "N"], y = .df.grid[.df.grid$rho==0.20, "power.diff.eos.means"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")
#legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)
points(x = .df.grid[.df.grid$rho==0.45, "N"], y = .df.grid[.df.grid$rho==0.45, "power.diff.eos.means"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")

plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)
points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.eos.means"], pch=21, bg="goldenrod3")
title(main = "Difference in End-of-Study Means")

par(op)

op <- par(mfrow = c(1,3), pty="m")
par(mfrow = c(1,3), pty="m")

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)

points(x = .df.grid[.df.grid$rho==0.20, "N"], y = .df.grid[.df.grid$rho==0.20, "power.diff.AUC"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")
legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)

plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)

points(x = .df.grid[.df.grid$rho==0.45, "N"], y = .df.grid[.df.grid$rho==0.45, "power.diff.AUC"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")
legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)


plot(-1, 
     type="n",
     xlim = c(100,1100),
     ylim = c(0,0.10),
     xaxt="n",
     yaxt="n",
     xlab = "N",
     ylab = "Power to Reject H0 when DeltaQ=0")

axis(1, at = seq(100,1100,100))
axis(2, at = seq(0, 0.10, 0.01))
abline(h = 0.05, lty=2)
abline(h = 0.06, lty=2)
abline(h = 0.04, lty=2)

points(x = .df.grid[.df.grid$rho==0.75, "N"], y = .df.grid[.df.grid$rho==0.75, "power.diff.AUC"], pch=21, bg="goldenrod3")
title(main = "Difference in AUC")
legend("bottomright", "Nominal alpha=0.05 threshold", lty=2, cex = 0.90)


par(op)





###############################################################################
# Old code
###############################################################################
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

