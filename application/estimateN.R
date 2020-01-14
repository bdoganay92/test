library(dplyr)
library(ggplot2)
library(gridExtra)

###############################################################################
# Specify working correlation structure
###############################################################################
use.working.corr <- "ar1"

###############################################################################
# Specify desired power
###############################################################################
input.power <- 0.80

###############################################################################
# Script begins
###############################################################################

library(dplyr)
library(purrr)
library(assertthat)
library(rootSolve)
library(mvtnorm)
library(geeM)
library(parallel)
library(ggplot2)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

load(file.path(path.output_data, "estimated-covmat.RData"))
path.code <- Sys.getenv("path.code")  # Need to change paths back
path.input_data <- Sys.getenv("path.input_data")   # Need to change paths back
path.output_data <- Sys.getenv("path.output_data")   # Need to change paths back

# Calculate DeltaQ
input.means <- read.csv(file.path(path.input_data, "input_means.csv"))
CheckInputData(input.means, rand.time = input.rand.time, tot.time = input.tot.time)
input.prop.zeros <- read.csv(file.path(path.input_data, "input_prop_zeros.csv"))
CheckInputData(input.prop.zeros, rand.time = input.rand.time, tot.time = input.tot.time)
source(file.path(path.code, "calc-truth.R"))

# Calculate required sample size
collect.N <- list()
for(idx.i in 1:length(collect.estimates)){
  
  # Calculate N.required
  z.eta <- qnorm(1-input.power)
  z.alpha <- qnorm(input.alpha/2)
  const.eos.means <- ((z.eta + z.alpha)/diff.eos.means.plusplus.minusplus)^2
  const.change.score <- ((z.eta + z.alpha)/diff.change.score.plusplus.minusplus)^2
  const.AUC <- ((z.eta + z.alpha)/diff.AUC.plusplus.minusplus)^2
  
  N.required.eos.means <- const.eos.means * collect.estimates[[idx.i]]$mean.sandwich.eos.means
  N.required.change.score <- const.change.score * collect.estimates[[idx.i]]$mean.sandwich.change.score
  N.required.AUC <- const.AUC * collect.estimates[[idx.i]]$mean.sandwich.AUC
  
  tmp <- data.frame(N.large = collect.estimates[[idx.i]]$N,
                    rho = collect.estimates[[idx.i]]$rho,
                    sim = collect.estimates[[idx.i]]$sim,
                    N.required.eos.means = N.required.eos.means,
                    N.required.change.score = N.required.change.score,
                    N.required.AUC = N.required.AUC,
                    D.eos.means = diff.eos.means.plusplus.minusplus,
                    D.change.score = diff.change.score.plusplus.minusplus,
                    D.AUC = diff.AUC.plusplus.minusplus)
  collect.N <- append(collect.N, list(tmp))
}

df.collect.N <- bind_rows(collect.N)

print(xtable(df.collect.N, digits=2), include.rownames=FALSE)

###############################################################################
# Get estimated tau_mean
###############################################################################
load(file.path(path.output_data, "params-curve.RData"))
# Reset paths
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

df.collect.correlation <- bind_rows(collect.correlation.seq)
df.collect.correlation <- df.collect.correlation %>% 
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(estimated.tau.mean = mean(estimates))

# Plot calibration
gg <- ggplot(df.collect.correlation, aes(x=datagen.params.rho, y=estimated.tau.mean, color = 1))
gg <- gg + scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_abline(slope = 1, intercept = 0, color = "black", size = 1)
gg <- gg + geom_line(size=1.2)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + xlab("rho") + ylab("tau_MEAN")
gg <- gg + theme(legend.position = "none")
gg.calibration <- gg

ggsave(file.path(path.output_data, "calibration.jpg"), 
       plot = gg.calibration,
       width = 8,
       height = 8,
       units = "in")

