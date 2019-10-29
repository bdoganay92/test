library(dplyr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

# -----------------------------------------------------------------------------
# Inputs from user
# -----------------------------------------------------------------------------
input.tot.time <- 6  # Total no. of measurement occasions
input.rand.time <- 2  # Time when second randomization occurred (time is 1-indexed)
input.tau.max <- 0.2  # Maximum correlation between time-specific outcomes under any DTR
input.alpha <- 0.05  # Type-I error rate

# -----------------------------------------------------------------------------
# Calibration step
# -----------------------------------------------------------------------------
source(file.path(path.code, "calibrate-params.R"))

# -----------------------------------------------------------------------------
# Calculate power given these inputs
# -----------------------------------------------------------------------------
# Total no. of individuals to enter SMART
num.individuals <- seq(200,2000,50)
collect.power.diff.eos.means <- list()
collect.power.diff.AUC <- list()

for(i in 1:length(num.individuals)){
  input.N <- num.individuals[i]
  source(file.path(path.code, "calc-power.R"))
  power.diff.eos.means <- list(power.diff.eos.means)
  power.diff.AUC <- list(power.diff.AUC)
  collect.power.diff.eos.means <- append(collect.power.diff.eos.means, power.diff.eos.means)
  collect.power.diff.AUC <- append(collect.power.diff.AUC, power.diff.AUC)
}

collect.power.diff.eos.means <- bind_rows(collect.power.diff.eos.means)
collect.power.diff.AUC <- bind_rows(collect.power.diff.AUC)

write.csv(collect.power.diff.eos.means, file.path(path.input_data, "power.diff.eos.means.csv"), row.names=FALSE)
write.csv(collect.power.diff.AUC, file.path(path.input_data, "power.diff.AUC.csv"), row.names=FALSE)

# -----------------------------------------------------------------------------
# Plot power curve for each pairwise difference: end-of-study means
# -----------------------------------------------------------------------------
for(j in 1:6){
  this.pair <- collect.power.diff.eos.means %>% filter(pair==j)
  gg.base <- ggplot(this.pair, aes(datagen.params.N,power))
  gg <- gg.base + xlab("N") + ylab("power")
  gg <- gg + scale_x_continuous(limits = c(0,max(num.individuals)+100), breaks = seq(0, max(num.individuals)+100, 100)) 
  gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  gg <- gg + geom_point(size=2, na.rm=TRUE)
  gg <- gg + geom_smooth(method = "loess", color = "red", linetype=3)
  ggsave(file.path(path.input_data,paste("diff.eos.means","pair",j,".jpeg", sep="")), width = 7, height = 7, units = "in")
}

# -----------------------------------------------------------------------------
# Plot power curve for each pairwise difference: AUC
# -----------------------------------------------------------------------------
for(j in 1:6){
  this.pair <- collect.power.diff.AUC %>% filter(pair==j)
  gg.base <- ggplot(this.pair, aes(datagen.params.N,power))
  gg <- gg.base + xlab("N") + ylab("power")
  gg <- gg + scale_x_continuous(limits = c(0,max(num.individuals)+100), breaks = seq(0, max(num.individuals)+100, 100)) 
  gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  gg <- gg + geom_point(size=2, na.rm=TRUE)
  gg <- gg + geom_smooth(method = "loess", color = "red", linetype=3)
  ggsave(file.path(path.input_data,paste("diff.AUC","pair",j,".jpeg", sep="")), width = 7, height = 7, units = "in")
}



