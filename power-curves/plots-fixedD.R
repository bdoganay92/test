library(ggplot2)
library(gridExtra)
library(dplyr)

path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))

use.working.corr <- "ar1"
input.alpha <- 0.05
input.power <- 0.80
input.tot.time <- 6
input.rand.time <- 2

###############################################################################
# End-of-study means and change score
###############################################################################

load(file.path(path.output_data, use.working.corr, "fixedD-curve-01.RData"))
# Reset path variables
path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

input.means <- shortlist.input.means[[1]]
source(file.path(path.code, "calc-truth.R"))
df.collect.truth <- data.frame(idx.input.means = 1,
                               D.eos.means = diff.eos.means.plusplus.minusplus,
                               D.change.score = diff.change.score.plusplus.minusplus)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
collect.power.eos.means <- lapply(collect.power, function(x){return(x$eos.means)})
collect.power.eos.means <- bind_rows(collect.power.eos.means)
collect.power.eos.means <- left_join(x = collect.power.eos.means, y = df.collect.truth, by = "idx.input.means")
collect.power.eos.means$datagen.params.rho <- as.factor(collect.power.eos.means$datagen.params.rho)

collect.power.change.score <- lapply(collect.power, function(x){return(x$change.score)})
collect.power.change.score <- bind_rows(collect.power.change.score)
collect.power.change.score <- left_join(x = collect.power.change.score, y = df.collect.truth, by = "idx.input.means")
collect.power.change.score$datagen.params.rho <- as.factor(collect.power.change.score$datagen.params.rho)

###############################################################################
# AUC
###############################################################################

load(file.path(path.output_data, use.working.corr, "fixedD-curve-02.RData"))
# Reset path variables
path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

input.means <- auc.shortlist.input.means[[1]]
source(file.path(path.code, "calc-truth.R"))
df.collect.truth <- data.frame(idx.input.means = 1,
                               D.AUC = diff.AUC.plusplus.minusplus)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
collect.power.AUC <- lapply(collect.power, function(x){return(x$AUC)})
collect.power.AUC <- bind_rows(collect.power.AUC)
collect.power.AUC <- left_join(x = collect.power.AUC, y = df.collect.truth, by = "idx.input.means")
collect.power.AUC$datagen.params.rho <- as.factor(collect.power.AUC$datagen.params.rho)
collect.power.AUC$D.average.AUC <- (collect.power.AUC$D.AUC)/input.tot.time

###############################################################################
# Plot results: power
###############################################################################

gg <- ggplot(collect.power.eos.means, aes(x=datagen.params.N, y=power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + scale_x_continuous(limits = c(120, 610), breaks = seq(120, 620, 100))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in end-of-study means\nD=-2.8") + xlab("N") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.eos.means <- gg

gg <- ggplot(collect.power.AUC, aes(x=datagen.params.N, y=power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + scale_x_continuous(limits = c(120, 610), breaks = seq(120, 620, 100))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in AUC\nD/6 = -0.58") + xlab("N") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.AUC <- gg

gg <- ggplot(collect.power.change.score, aes(x=datagen.params.N, y=power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + scale_x_continuous(limits = c(120, 610), breaks = seq(120, 620, 100))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in change score\nD=-2.8") + xlab("N") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.change.score <- gg

plots.grid.power <- grid.arrange(gg.power.eos.means, 
                                 gg.power.AUC,
                                 gg.power.change.score,
                                 nrow=1)

path.output_data <- Sys.getenv("path.output_data")
ggsave(file.path(path.output_data, use.working.corr, "powercurves.fixedD.jpg"), 
       plot = plots.grid.power,
       width = 18,
       height = 8,
       units = "in")
