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

load(file.path(path.output_data, use.working.corr, "estimated-covmat-02.RData"))
path.code <- Sys.getenv("path.code")  # Need to change paths back
path.input_data <- Sys.getenv("path.input_data")   # Need to change paths back
path.output_data <- Sys.getenv("path.output_data")   # Need to change paths back


collect.N <- list()
for(idx.i in 1:length(collect.estimates)){
  
  # Calculate DeltaQ
  idx.input.means <- collect.estimates[[idx.i]]$input.means
  input.means <- auc.list.input.means[[idx.input.means]]
  source(file.path(path.code, "calc-truth.R"))
  
  # Calculate N.required
  z.eta <- qnorm(1-input.power)
  z.alpha <- qnorm(input.alpha/2)
  const.AUC <- ((z.eta + z.alpha)/diff.AUC.plusplus.minusplus)^2
  N.required.AUC <- const.AUC * collect.estimates[[idx.i]]$mean.sandwich.AUC
  
  tmp <- data.frame(N.large = collect.estimates[[idx.i]]$N,
                    rho = collect.estimates[[idx.i]]$rho,
                    sim = collect.estimates[[idx.i]]$sim,
                    input.means = collect.estimates[[idx.i]]$input.means,
                    N.required.AUC = N.required.AUC,
                    D.AUC = diff.AUC.plusplus.minusplus)
  collect.N <- append(collect.N, list(tmp))
}

df.collect.N <- bind_rows(collect.N)

###############################################################################
# Get estimated tau_mean
###############################################################################
load(file.path(path.output_data, use.working.corr, "params-curve-02.RData"))
df.collect.correlation <- bind_rows(collect.correlation)
df.collect.correlation <- df.collect.correlation %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(estimated.tau.mean = mean(estimates))

###############################################################################
# Plot
###############################################################################
this.plotdat <- df.collect.N

# Get plots for AUC
this.plotdat$rho <- as.factor(this.plotdat$rho)
gg <- ggplot(this.plotdat, aes(x=D.AUC, y=N.required.AUC, color=rho, shape=rho))
gg <- gg + scale_x_continuous(limits = c(min(this.plotdat$D.AUC), max(this.plotdat$D.AUC)))
gg <- gg + scale_y_continuous(limits = c(min(this.plotdat$N.required.AUC), max(this.plotdat$N.required.AUC)))
gg <- gg + geom_line(size=1.2)
gg <- gg + geom_point(show.legend = TRUE, size=3, alpha=0.7)
gg <- gg + labs(title = "AUC") + xlab("DELTA: Difference between DTRs (+1,+1) and (-1,+1)") + ylab("N required")
gg

