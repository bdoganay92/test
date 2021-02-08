

input.N <- 500
input.rho <- 0.80
path.output_data <- Sys.getenv("path.output_data")
.this.folder <- "sim_vary_effect/sim_results_7"

load(file = file.path(path.output_data, .this.folder, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))
power.eos.means <- mean(abs(est.diff.eos.means$estimates/est.stderr.diff.eos.means$estimates) > qnorm(0.975))
power.AUC <- mean(abs(est.diff.AUC$estimates/est.stderr.diff.AUC$estimates) > qnorm(0.975))

print(power.eos.means)
print(power.AUC)





