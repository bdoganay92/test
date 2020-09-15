###############################################################################
# Calculate power for various values of N and n4
###############################################################################

.df.grid <- expand.grid(rho = c(0.60),
                               N = seq(250,650,100))

.prop.responders.plusone <- 0.65
.prop.responders.minusone <- 0.61

.prop.nonresponders.plusone <- 1 - .prop.responders.plusone
.prop.nonresponders.minusone <- 1 - .prop.responders.minusone

.list.params <- list()

for(.idx.vary.params in 1:nrow(.df.grid)){
  max.n4 <- min((.df.grid[.idx.vary.params, "N"])*.prop.nonresponders.plusone, 
                (.df.grid[.idx.vary.params, "N"])*.prop.nonresponders.minusone)
  max.n4 <- ceiling(max.n4)
  candidate.n4 <- seq(0, max.n4, 10)
  current.grid <- expand.grid(rho = .df.grid[.idx.vary.params, "rho"],
                              N = .df.grid[.idx.vary.params, "N"],
                              n4 = candidate.n4)
  .list.params <- append(.list.params, list(current.grid))
}

.df.grid <- do.call(rbind, .list.params)

rm(list = ls(all.names = FALSE))


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

# .path.output_data is the location of the output of calc-sensitivity-n4.R
.path.output_data <- Sys.getenv("path.output_data")
.this.folder <- "sim_results_d_0/sensitivity_to_n4"

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
  input.n4 <- .df.grid[.idx.grid, "n4"]
  load(file.path(.path.output_data, .this.folder, paste("sensitivity_to_n4_with_params_","_N_",input.N,"_rho_",input.rho,"_n4_",input.n4,".RData", sep="")))
  
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
# Calculate max and min of power for each combo of N and n4
###############################################################################

# .path.output_data is the location of the output of calc-sensitivity-n4.R
.path.output_data <- Sys.getenv("path.output_data")
.this.folder <- "sim_results_d_0/sensitivity_to_n4"

library(dplyr)

.df.summary.grid <- .df.grid %>% 
  group_by(rho, N) %>% 
  summarise(min.power.diff.eos.means = min(power.diff.eos.means),
            mean.power.diff.eos.means = mean(power.diff.eos.means),
            max.power.diff.eos.means = max(power.diff.eos.means),
            min.power.diff.AUC = min(power.diff.AUC),
            mean.power.diff.AUC = mean(power.diff.AUC),
            max.power.diff.AUC = max(power.diff.AUC))

.df.summary.grid[,3:ncol(.df.summary.grid)] <- round(.df.summary.grid[,3:ncol(.df.summary.grid)], digits=3)

write.csv(.df.summary.grid, file.path(.path.output_data, .this.folder, "summary_sensitivity_n4.csv"), row.names=FALSE)


###############################################################################
# User-specified design parameters when using input_means_d_0.csv
# to calculate power
###############################################################################
.N.min <- 250
.N.max <- 650
.N.increment <- 100
.N.grid <- seq(.N.min, .N.max, .N.increment)
.rho.grid <- c(0.60)
.N.colors <- c("darkolivegreen","cornflowerblue","goldenrod","brown","purple")

###############################################################################
# Code for plotting when, in truth, Delta_Q > 0
###############################################################################

# Plot sample size vs. power
# Display plots for end-of-study means and AUC side-by-side

op <- par(mfrow = c(1,2), pty="m")
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

axis(1, at = seq(.N.min, .N.max, .N.increment))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in End-of-Study Means")

for(.idx.grid in 1:length(.N.grid)){
  points(x = .df.grid[.df.grid$N==.N.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$N==.N.grid[.idx.grid], "power.diff.eos.means"],
         pch=21, 
         bg=.N.colors[.idx.grid])
}

legend("bottomright", 
       c("N=250","N=350","N=450","N=550","N=650"), 
       pch=21,
       lwd=c(1,1,1),
       col =  c("darkolivegreen","cornflowerblue","goldenrod","brown","purple"),
       pt.bg =  c("darkolivegreen","cornflowerblue","goldenrod","brown","purple"), 
       pt.lwd = c(3,3,3,3,3),
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

axis(1, at = seq(.N.min, .N.max, .N.increment))
axis(2, at = seq(0, 1, 0.10))
abline(h = 0.80, lty=2)
abline(h = 1, lty=3)
title(main = "Difference in AUC")

for(.idx.grid in 1:length(.N.grid)){
  points(x = .df.grid[.df.grid$N==.N.grid[.idx.grid], "N"], 
         y = .df.grid[.df.grid$N==.N.grid[.idx.grid], "power.diff.AUC"],
         pch=21, 
         bg=.N.colors[.idx.grid])
}

legend("bottomright", 
       c("N=250","N=350","N=450","N=550","N=650"), 
       pch=21,
       lwd=c(1,1,1),
       col =  c("darkolivegreen","cornflowerblue","goldenrod","brown","purple"),
       pt.bg =  c("darkolivegreen","cornflowerblue","goldenrod","brown","purple"), 
       pt.lwd = c(3,3,3,3,3),
       cex = 0.80)

par(op)


