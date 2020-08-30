.path.output_data <- Sys.getenv("path.output_data")

# Define grid
.gridx <- expand.grid(rho = c(0.2, 0.45, 0.75), d = c(-1.5, -1.0, -0.5, 0, 0.5, 1))
.gridx$N.required.eos.means <- NA_real_
.gridx$N.required.AUC <- NA_real_

# Setting all.names=FALSE in the call to ls() removes all variables in the
# current environment except those beginning with a "."

for(i in 1:nrow(.gridx)){
  input.rho <- .gridx[i, "rho"]
  d <- .gridx[i, "d"]
  load(file.path(.path.output_data, paste("Nrequired_", "rho_", input.rho,  "_d_", d, ".RData", sep="")))
  .gridx[i, "N.required.eos.means"] <- N.required.eos.means
  .gridx[i, "N.required.AUC"] <- N.required.AUC
  rm(list = ls(all.names = FALSE))
}

.gridx <- .gridx[order(.gridx$rho, .gridx$d),]
.gridx$DeltaQ.eos.means <- .gridx$d+2
.gridx$DeltaQ.AUC <- 2*(.gridx$d+2)

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(0, 3.5),
     ylim = c(min(.gridx$N.required.eos.means)-5, max(.gridx$N.required.eos.means)+5),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "required N")

axis(1, at = 2+c(-1.5, -1.0, -0.5, 0, 0.5, 1))
axis(2, at = seq(floor(min(.gridx$N.required.eos.means)-5), ceiling(max(.gridx$N.required.eos.means)+5), 100))
lines(x = .gridx[.gridx$rho==0.2, "DeltaQ.eos.means"], y = .gridx[.gridx$rho==0.2, "N.required.eos.means"], lty = 2, lwd=2, type="o", col="cornflowerblue")
lines(x = .gridx[.gridx$rho==0.45, "DeltaQ.eos.means"], y = .gridx[.gridx$rho==0.45, "N.required.eos.means"], lty = 3, lwd=2, type="o", col="darkgoldenrod3")
lines(x = .gridx[.gridx$rho==0.75, "DeltaQ.eos.means"], y = .gridx[.gridx$rho==0.75, "N.required.eos.means"], lty = 4, lwd=2, type="o", col="maroon")
title(main = "Difference in End-of-Study Means")
legend("topright", c("tau=0.12", "tau=0.32", "tau=0.62"), lty = c(2,3,4), col = c("cornflowerblue", "darkgoldenrod3", "maroon"), lwd=c(2,2,2))

# Plot in one graph
plot(-1, 
     type="n",
     xlim = c(0.5, 6.5),
     ylim = c(min(.gridx$N.required.AUC)-5, max(.gridx$N.required.AUC)+5),
     xaxt="n",
     yaxt="n",
     xlab = "magnitude of DeltaQ",
     ylab = "required N")

axis(1, at = 2*(2+c(-1.5, -1.0, -0.5, 0, 0.5, 1)))
axis(2, at = seq(floor(min(.gridx$N.required.AUC)-5), ceiling(max(.gridx$N.required.AUC)+5), 30))
lines(x = .gridx[.gridx$rho==0.2, "DeltaQ.AUC"], y = .gridx[.gridx$rho==0.2, "N.required.AUC"], lty = 2, lwd=2, type="o", col="cornflowerblue")
lines(x = .gridx[.gridx$rho==0.45, "DeltaQ.AUC"], y = .gridx[.gridx$rho==0.45, "N.required.AUC"], lty = 3, lwd=2, type="o", col="darkgoldenrod3")
lines(x = .gridx[.gridx$rho==0.75, "DeltaQ.AUC"], y = .gridx[.gridx$rho==0.75, "N.required.AUC"], lty = 4, lwd=2, type="o", col="maroon")
title(main = "Difference in AUC")
legend("topright", c("tau=0.12", "tau=0.32", "tau=0.62"), lty = c(2,3,4), col = c("cornflowerblue", "darkgoldenrod3", "maroon"), lwd=c(2,2,2))



