.path.output_data <- Sys.getenv("path.output_data")

load(file.path(.path.output_data, "coverage_and_power__N_600_rho_0.8_d_-1.RData"))
.null.est.diff.eos.means <- est.diff.eos.means
.null.est.diff.AUC <- est.diff.AUC

rm(list=ls())

load(file.path(.path.output_data, "coverage_and_power__N_600_rho_0.8_d_0.RData"))
.alternative.est.diff.eos.means <- est.diff.eos.means
.alternative.est.diff.AUC <- est.diff.AUC

print(power.diff.eos.means)
print(power.diff.AUC)

rm(list=ls())

###############################################################################
# Now, power can be calculated
# Use parametric bootstrap
###############################################################################
bootstrap.eos.means.LB <- quantile(.null.est.diff.eos.means$estimates, .025)
bootstrap.eos.means.UB <- quantile(.null.est.diff.eos.means$estimates, .975)

bootstrap.eos.means.test <- (.alternative.est.diff.eos.means$estimates >=  bootstrap.eos.means.LB) & (.alternative.est.diff.eos.means$estimates <= bootstrap.eos.means.UB)
bootstrap.eos.means.power <- 1-mean(bootstrap.eos.means.test)

bootstrap.AUC.LB <- quantile(.null.est.diff.AUC$estimates, .025)
bootstrap.AUC.UB <- quantile(.null.est.diff.AUC$estimates, .975)

bootstrap.AUC.test <- (.alternative.est.diff.AUC$estimates >=  bootstrap.AUC.LB) & (.alternative.est.diff.AUC$estimates <= bootstrap.AUC.UB)
bootstrap.AUC.power <- 1-mean(bootstrap.AUC.test)

###############################################################################
# Display power
###############################################################################
print("power based on parametric bootstrap")
print(bootstrap.eos.means.power)
print(bootstrap.AUC.power)