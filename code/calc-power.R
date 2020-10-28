library(dplyr)

input.alpha <- 0.05

load(file = file.path(path.output_data, this.folder, paste("hat_","N_",input.N,"_rho_",input.rho,".RData", sep="")))


###############################################################################
# Calculate bias in estimates
###############################################################################
bias.diff.eos.means <- left_join(est.diff.eos.means, est.stderr.diff.eos.means, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(truth.diff = diff.eos.means, truth.stderr = sqrt(var(est.diff, na.rm=TRUE))) %>%
  mutate(bias.diff = est.diff - truth.diff, bias.stderr = est.stderr - truth.stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(ave.bias.diff = mean(bias.diff, na.rm=TRUE), ave.bias.stderr = mean(bias.stderr, na.rm=TRUE))

bias.diff.AUC <- left_join(est.diff.AUC, est.stderr.diff.AUC, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(truth.diff = diff.AUC, truth.stderr = sqrt(var(est.diff, na.rm=TRUE))) %>%
  mutate(bias.diff = est.diff - truth.diff, bias.stderr = est.stderr - truth.stderr) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(ave.bias.diff = mean(bias.diff, na.rm=TRUE), ave.bias.stderr = mean(bias.stderr, na.rm=TRUE))

###############################################################################
# Calculate standard error
###############################################################################
truth.var.est.diff.eos.means <- left_join(est.diff.eos.means, est.stderr.diff.eos.means, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(truth.var.est = (var(est.diff, na.rm=TRUE))) %>%
  select(datagen.params.N, datagen.params.rho, truth.var.est)


truth.var.est.diff.AUC <- left_join(est.diff.AUC, est.stderr.diff.AUC, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(truth.var.est = (var(est.diff, na.rm=TRUE))) %>%
  select(datagen.params.N, datagen.params.rho, truth.var.est)

###############################################################################
# Now, power can be calculated
# Use asymptotic distribution
###############################################################################
normal.power.diff.eos.means <- left_join(est.diff.eos.means, est.stderr.diff.eos.means, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

normal.power.diff.AUC <- left_join(est.diff.AUC, est.stderr.diff.AUC, by = c("datagen.params.N", "datagen.params.rho", "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(z = est.diff/est.stderr) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2))) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(power = mean(is.reject, na.rm=TRUE))

###############################################################################
# Display power
###############################################################################
print("Power based on Normal approximation")
print(normal.power.diff.eos.means)
print(normal.power.diff.AUC)

# Audio notification
beep("mario")

# Save RData
save(normal.power.diff.eos.means, normal.power.diff.AUC,
     bias.diff.eos.means, bias.diff.AUC,
     diff.eos.means, diff.AUC,
     est.diff.eos.means, est.diff.AUC,
     est.stderr.diff.eos.means, est.stderr.diff.AUC,
     file = file.path(path.output_data, this.folder, paste("normal_power_","_N_",input.N,"_rho_",input.rho,".RData", sep="")))
