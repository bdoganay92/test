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

load(file.path(path.output_data, use.working.corr, "fixedN-curve-03.RData"))
# Reset path variables
path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

list.collect.truth <- list()
for(idx.i in 1:length(list.input.means)){
  input.means <- list.input.means[[idx.i]]
  source(file.path(path.code, "calc-truth.R"))
  all.truth <- data.frame(idx.input.means = idx.i,
                          D.eos.means = diff.eos.means.plusplus.minusplus,
                          D.change.score = diff.change.score.plusplus.minusplus)
  list.collect.truth <- append(list.collect.truth, list(all.truth))
}

df.collect.truth <- bind_rows(list.collect.truth)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
collect.power.eos.means <- lapply(collect.power, function(x){return(x$eos.means)})
collect.power.eos.means <- bind_rows(collect.power.eos.means)
collect.power.eos.means <- collect.power.eos.means %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.power = mean(power), max.power = max(power), min.power = min(power))
collect.power.eos.means <- left_join(x = collect.power.eos.means, y = df.collect.truth, by = "idx.input.means")
collect.power.eos.means$datagen.params.rho <- as.factor(collect.power.eos.means$datagen.params.rho)

collect.power.change.score <- lapply(collect.power, function(x){return(x$change.score)})
collect.power.change.score <- bind_rows(collect.power.change.score)
collect.power.change.score <- collect.power.change.score %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.power = mean(power), max.power = max(power), min.power = min(power))
collect.power.change.score <- left_join(x = collect.power.change.score, y = df.collect.truth, by = "idx.input.means")
collect.power.change.score$datagen.params.rho <- as.factor(collect.power.change.score$datagen.params.rho)

collect.coverage.eos.means <- lapply(collect.coverage, function(x){return(x$eos.means)})
collect.coverage.eos.means <- bind_rows(collect.coverage.eos.means)
collect.coverage.eos.means <- collect.coverage.eos.means %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.coverage = mean(coverage), max.coverage = max(coverage), min.coverage = min(coverage))
collect.coverage.eos.means <- left_join(x = collect.coverage.eos.means, y = df.collect.truth, by = "idx.input.means")
collect.coverage.eos.means$datagen.params.rho <- as.factor(collect.coverage.eos.means$datagen.params.rho)

collect.coverage.change.score <- lapply(collect.coverage, function(x){return(x$change.score)})
collect.coverage.change.score <- bind_rows(collect.coverage.change.score)
collect.coverage.change.score <- collect.coverage.change.score %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.coverage = mean(coverage), max.coverage = max(coverage), min.coverage = min(coverage))
collect.coverage.change.score <- left_join(x = collect.coverage.change.score, y = df.collect.truth, by = "idx.input.means")
collect.coverage.change.score$datagen.params.rho <- as.factor(collect.coverage.change.score$datagen.params.rho)

###############################################################################
# AUC
###############################################################################

load(file.path(path.output_data, use.working.corr, "fixedN-curve-04.RData"))
# Reset path variables
path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")

list.collect.truth <- list()
for(idx.i in 1:length(auc.list.input.means)){
  input.means <- auc.list.input.means[[idx.i]]
  source(file.path(path.code, "calc-truth.R"))
  all.truth <- data.frame(idx.input.means = idx.i,
                          D.AUC = diff.AUC.plusplus.minusplus)
  list.collect.truth <- append(list.collect.truth, list(all.truth))
}

df.collect.truth <- bind_rows(list.collect.truth)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
collect.power.AUC <- lapply(collect.power, function(x){return(x$AUC)})
collect.power.AUC <- bind_rows(collect.power.AUC)
collect.power.AUC <- collect.power.AUC %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.power = mean(power), max.power = max(power), min.power = min(power))
collect.power.AUC <- left_join(x = collect.power.AUC, y = df.collect.truth, by = "idx.input.means")
collect.power.AUC$datagen.params.rho <- as.factor(collect.power.AUC$datagen.params.rho)
collect.power.AUC$D.average.AUC <- (collect.power.AUC$D.AUC)/input.tot.time

collect.coverage.AUC <- lapply(collect.coverage, function(x){return(x$AUC)})
collect.coverage.AUC <- bind_rows(collect.coverage.AUC)
collect.coverage.AUC <- collect.coverage.AUC %>% 
  group_by(datagen.params.N, datagen.params.rho, idx.input.means) %>%
  summarise(mean.coverage = mean(coverage), max.coverage = max(coverage), min.coverage = min(coverage))
collect.coverage.AUC <- left_join(x = collect.coverage.AUC, y = df.collect.truth, by = "idx.input.means")
collect.coverage.AUC$datagen.params.rho <- as.factor(collect.coverage.AUC$datagen.params.rho)
collect.coverage.AUC$D.average.AUC <- (collect.coverage.AUC$D.AUC)/input.tot.time

###############################################################################
# Plot results: power
###############################################################################

gg <- ggplot(collect.power.eos.means, aes(x=D.eos.means, y=mean.power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.power, ymax=max.power), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-5, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in end-of-study means") + xlab("D") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.eos.means <- gg

gg <- ggplot(collect.power.AUC, aes(x=D.average.AUC, y=mean.power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.power, ymax=max.power), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-3, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in AUC") + xlab("D/6") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.AUC <- gg

gg <- ggplot(collect.power.change.score, aes(x=D.change.score, y=mean.power, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.power, ymax=max.power), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-5, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Power to reject H0 of no difference in change score") + xlab("D") + ylab("Power")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.power.change.score <- gg

plots.grid.power <- grid.arrange(gg.power.eos.means, 
                                 gg.power.AUC,
                                 gg.power.change.score,
                                 nrow=1)

path.output_data <- Sys.getenv("path.output_data")
ggsave(file.path(path.output_data, use.working.corr, "powercurves.fixedN.sensitivity.jpg"), 
       plot = plots.grid.power,
       width = 18,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "powercurves.fixedN.sensitivity.a.jpg"), 
       plot = gg.power.eos.means,
       width = 8,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "powercurves.fixedN.sensitivity.b.jpg"), 
       plot = gg.power.AUC,
       width = 8,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "powercurves.fixedN.sensitivity.c.jpg"), 
       plot = gg.power.change.score,
       width = 8,
       height = 8,
       units = "in")

###############################################################################
# Plot results: coverage
###############################################################################

gg <- ggplot(collect.coverage.eos.means, aes(x=D.eos.means, y=mean.coverage, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.coverage, ymax=max.coverage), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-5, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Coverage: difference in end-of-study means") + xlab("D") + ylab("Coverage")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.coverage.eos.means <- gg

gg <- ggplot(collect.coverage.AUC, aes(x=D.average.AUC, y=mean.coverage, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.coverage, ymax=max.coverage), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-3, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Coverage: difference in AUC") + xlab("D/6") + ylab("Coverage")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.coverage.AUC <- gg

gg <- ggplot(collect.coverage.change.score, aes(x=D.change.score, y=mean.coverage, color=datagen.params.rho, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + geom_errorbar(aes(ymin=min.coverage, ymax=max.coverage), width=0.35, show.legend=TRUE)
gg <- gg + scale_x_continuous(limits = c(-5, 0))
gg <- gg + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.10))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "Coverage: difference in change score") + xlab("D") + ylab("Coverage")
gg <- gg + theme(legend.position="bottom") + guides(color=guide_legend(title="rho"), shape=guide_legend(title="rho"), linetype=guide_legend(title="rho"))
gg.coverage.change.score <- gg

plots.grid.coverage <- grid.arrange(gg.coverage.eos.means, 
                                 gg.coverage.AUC,
                                 gg.coverage.change.score,
                                 nrow=1)

path.output_data <- Sys.getenv("path.output_data")
ggsave(file.path(path.output_data, use.working.corr, "coveragecurves.fixedN.sensitivity.jpg"), 
       plot = plots.grid.coverage,
       width = 18,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "coveragecurves.fixedN.sensitivity.a.jpg"), 
       plot = gg.coverage.eos.means,
       width = 8,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "coveragecurves.fixedN.sensitivity.b.jpg"), 
       plot = gg.coverage.AUC,
       width = 8,
       height = 8,
       units = "in")

ggsave(file.path(path.output_data, use.working.corr, "coveragecurves.fixedN.sensitivity.c.jpg"), 
       plot = gg.coverage.change.score,
       width = 8,
       height = 8,
       units = "in")


