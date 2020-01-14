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
# Script begins: end-of-study means and change score
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

load(file.path(path.output_data, use.working.corr, "estimated-covmat-01.RData"))
path.code <- Sys.getenv("path.code")  # Need to change paths back
path.input_data <- Sys.getenv("path.input_data")   # Need to change paths back
path.output_data <- Sys.getenv("path.output_data")   # Need to change paths back

collect.N <- list()
for(idx.i in 1:length(collect.estimates)){
  
  # Calculate DeltaQ
  idx.input.means <- collect.estimates[[idx.i]]$input.means
  input.means <- list.input.means[[idx.input.means]]
  source(file.path(path.code, "calc-truth.R"))
  
  # Calculate N.required
  z.eta <- qnorm(1-input.power)
  z.alpha <- qnorm(input.alpha/2)
  const.eos.means <- ((z.eta + z.alpha)/diff.eos.means.plusplus.minusplus)^2
  const.change.score <- ((z.eta + z.alpha)/diff.change.score.plusplus.minusplus)^2
  N.required.eos.means <- const.eos.means * collect.estimates[[idx.i]]$mean.sandwich.eos.means
  N.required.change.score <- const.change.score * collect.estimates[[idx.i]]$mean.sandwich.change.score
  
  tmp <- data.frame(N.large = collect.estimates[[idx.i]]$N,
                    rho = collect.estimates[[idx.i]]$rho,
                    sim = collect.estimates[[idx.i]]$sim,
                    input.means = collect.estimates[[idx.i]]$input.means,
                    N.required.eos.means = N.required.eos.means,
                    N.required.change.score = N.required.change.score,
                    D.eos.means = diff.eos.means.plusplus.minusplus,
                    D.change.score = diff.change.score.plusplus.minusplus)
  collect.N <- append(collect.N, list(tmp))
}

df.collect.N <- bind_rows(collect.N)
df.collect.N$rho <- as.factor(df.collect.N$rho)

# -----------------------------------------------------------------------------
# Get estimated tau_mean
# -----------------------------------------------------------------------------
load(file.path(path.output_data, use.working.corr, "params-curve-01.RData"))
df.collect.correlation <- bind_rows(collect.correlation)
df.collect.correlation <- df.collect.correlation %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(estimated.tau.mean = mean(estimates))
df.collect.correlation.01 <- df.collect.correlation

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
gg <- ggplot(df.collect.N, aes(x=D.eos.means, y=N.required.eos.means, color=rho, shape=rho, linetype=rho))
gg <- gg + scale_x_continuous(limits = c(-2.75,-0.25), breaks = seq(-2.5, 0, 0.5))
gg <- gg + scale_y_continuous(limits = c(0,3000), breaks = seq(0,2800,300))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Sample size required to reject H0 of no difference in end-of-study means\nwith type-I error rate 0.05 and power 0.80") + xlab("D") + ylab("Required sample size")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.eos.means <- gg

gg <- ggplot(df.collect.N, aes(x=D.change.score, y=N.required.change.score, color=rho, shape=rho, linetype=rho))
gg <- gg + scale_x_continuous(limits = c(-2.75,-0.25), breaks = seq(-2.5, 0, 0.5))
gg <- gg + scale_y_continuous(limits = c(0,4000), breaks = seq(0,4000,500))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Sample size required to reject H0 of no difference in change score\nwith type-I error rate 0.05 and power 0.80") + xlab("D") + ylab("Required sample size")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.change.score <- gg

###############################################################################
# Script begins: AUC
###############################################################################
path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

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
df.collect.N$rho <- as.factor(df.collect.N$rho)
df.collect.N$D.average.AUC <- (df.collect.N$D.AUC)/input.tot.time

# -----------------------------------------------------------------------------
# Get estimated tau_mean
# -----------------------------------------------------------------------------
load(file.path(path.output_data, use.working.corr, "params-curve-02.RData"))
df.collect.correlation <- bind_rows(collect.correlation)
df.collect.correlation <- df.collect.correlation %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(estimated.tau.mean = mean(estimates))
df.collect.correlation.02 <- df.collect.correlation

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
gg <- ggplot(df.collect.N, aes(x=D.average.AUC, y=N.required.AUC, color=rho, shape=rho, linetype=rho))
gg <- gg + scale_x_continuous(limits = c(-2.75,-0.25), breaks = seq(-2.5, 0, 0.5))
gg <- gg + scale_y_continuous(limits = c(0,700), breaks = seq(0,700,100))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Sample size required to reject H0 of no difference in AUC\nwith type-I error rate 0.05 and power 0.80") + xlab("D/6") + ylab("Required sample size")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.AUC <- gg

###############################################################################
# Combine all plots
###############################################################################
plots.grid.power <- grid.arrange(gg.power.eos.means, 
                                 gg.power.AUC,
                                 gg.power.change.score,
                                 ncol=1)

path.output_data <- Sys.getenv("path.output_data")
ggsave(file.path(path.output_data, use.working.corr, "requiredN.jpg"), 
       plot = plots.grid.power,
       width = 7,
       height = 20,
       units = "in")



