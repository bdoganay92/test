library(dplyr)
library(ggplot2)
library(gridExtra)

use.working.corr <- "ar1"

###############################################################################
# Fixed standardized effect size, vary N
###############################################################################

# -----------------------------------------------------------------------------
# Load data for plotting and organize output
# -----------------------------------------------------------------------------
path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "params-fixedD-curve-01.RData"))
collect.delta.eos.means <- bind_rows(collect.delta.eos.means) %>%
  select(datagen.params.rho, delta=estimates)
collect.delta.change.score <- bind_rows(collect.delta.change.score) %>%
  select(datagen.params.rho, delta=estimates)
collect.correlation <- bind_rows(collect.correlation) %>%
  select(datagen.params.rho, tau_ave=estimates)

path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, use.working.corr, "fixedD-curve-01.RData"))
collect.power.eos.means <- lapply(collect.power, function(x){return(x$eos.means)})
collect.power.eos.means <- bind_rows(collect.power.eos.means) %>% 
  left_join(x = ., y = collect.delta.eos.means, by = "datagen.params.rho") %>% 
  left_join(x = ., y = collect.correlation, by = "datagen.params.rho")

collect.power.change.score <- lapply(collect.power, function(x){return(x$change.score)})
collect.power.change.score <- bind_rows(collect.power.change.score) %>% 
  left_join(x = ., y = collect.delta.change.score, by = "datagen.params.rho") %>% 
  left_join(x = ., y = collect.correlation, by = "datagen.params.rho")

# -----------------------------------------------------------------------------
# Load data for plotting and organize output
# -----------------------------------------------------------------------------
path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "params-fixedD-curve-02.RData"))
collect.delta.AUC <- bind_rows(collect.delta.AUC) %>%
  select(datagen.params.rho, delta=estimates)
collect.correlation <- bind_rows(collect.correlation) %>%
  select(datagen.params.rho, tau_ave=estimates)

path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, use.working.corr, "fixedD-curve-02.RData"))
collect.power.AUC <- lapply(collect.power, function(x){return(x$AUC)})
collect.power.AUC <- bind_rows(collect.power.AUC) %>% 
  left_join(x = ., y = collect.delta.AUC, by = "datagen.params.rho") %>% 
  left_join(x = ., y = collect.correlation, by = "datagen.params.rho")

# -----------------------------------------------------------------------------
# Combine all outputs
# -----------------------------------------------------------------------------
plotlist.fixedD <- list(collect.power.eos.means = collect.power.eos.means,
                        collect.power.change.score = collect.power.change.score,
                        collect.power.AUC = collect.power.AUC)

###############################################################################
# Fixed N, vary standardized effect size
###############################################################################

# -----------------------------------------------------------------------------
# Load data for plotting and organize output
# -----------------------------------------------------------------------------
path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "params-fixedN-curve-01.RData"))
collect.delta.eos.means <- bind_rows(collect.delta.eos.means) %>%
  select(datagen.params.rho, delta=estimates, idx.input.means)
collect.delta.change.score <- bind_rows(collect.delta.change.score) %>%
  select(datagen.params.rho, delta=estimates, idx.input.means)
collect.correlation <- bind_rows(collect.correlation) %>%
  select(datagen.params.rho, tau_ave=estimates, idx.input.means)

path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, use.working.corr, "fixedN-curve-01.RData"))
collect.power.eos.means <- lapply(collect.power, function(x){return(x$eos.means)})
collect.power.eos.means <- bind_rows(collect.power.eos.means) %>% 
  left_join(x = ., y = collect.delta.eos.means, by = c("datagen.params.rho","idx.input.means")) %>% 
  left_join(x = ., y = collect.correlation, by = c("datagen.params.rho","idx.input.means"))

collect.power.change.score <- lapply(collect.power, function(x){return(x$change.score)})
collect.power.change.score <- bind_rows(collect.power.change.score) %>% 
  left_join(x = ., y = collect.delta.change.score, by = c("datagen.params.rho","idx.input.means")) %>% 
  left_join(x = ., y = collect.correlation, by = c("datagen.params.rho","idx.input.means"))

# -----------------------------------------------------------------------------
# Load data for plotting and organize output
# -----------------------------------------------------------------------------
path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, "params-fixedN-curve-02.RData"))
collect.delta.AUC <- bind_rows(collect.delta.AUC) %>%
  select(datagen.params.rho, delta=estimates, idx.input.means)
collect.correlation <- bind_rows(collect.correlation) %>%
  select(datagen.params.rho, tau_ave=estimates, idx.input.means)

path.output_data <- Sys.getenv("path.output_data")
load(file.path(path.output_data, use.working.corr, "fixedN-curve-02.RData"))
collect.power.AUC <- lapply(collect.power, function(x){return(x$AUC)})
collect.power.AUC <- bind_rows(collect.power.AUC) %>% 
  left_join(x = ., y = collect.delta.AUC, by = c("datagen.params.rho","idx.input.means")) %>% 
  left_join(x = ., y = collect.correlation, by = c("datagen.params.rho","idx.input.means"))

# -----------------------------------------------------------------------------
# Combine all outputs
# -----------------------------------------------------------------------------
plotlist.fixedN <- list(collect.power.eos.means = collect.power.eos.means,
                        collect.power.change.score = collect.power.change.score,
                        collect.power.AUC = collect.power.AUC)

###############################################################################
# Fixed standardized effect size, vary N
###############################################################################

# -----------------------------------------------------------------------------
# end-of-study means
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "End-of-study means"
this.plotdat <- plotlist.fixedD$collect.power.eos.means
delta <- round(unique(this.plotdat$delta), digits=2)
rho <- round(unique(this.plotdat$datagen.params.rho), digits=2)
legend.labs <- apply(cbind(rho, delta), 1, function(x){
  x <- paste("rho =",x[1],", ","delta =",x[2], sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=datagen.params.N, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(100,max(this.plotdat$datagen.params.N)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels = legend.labs) + scale_linetype_discrete(name=element_blank(), labels = legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("N")

gg.eos.means.FIXEDDELTA <- gg

# -----------------------------------------------------------------------------
# change score
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "Delayed effect"
this.plotdat <- plotlist.fixedD$collect.power.change.score
delta <- round(unique(this.plotdat$delta), digits=2)
rho <- round(unique(this.plotdat$datagen.params.rho), digits=2)
legend.labs <- apply(cbind(rho, delta), 1, function(x){
  x <- paste("rho =",x[1],", ","delta =",x[2], sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=datagen.params.N, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(100,max(this.plotdat$datagen.params.N)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels = legend.labs) + scale_linetype_discrete(name=element_blank(), labels = legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("N")

gg.change.score.FIXEDDELTA <- gg

# -----------------------------------------------------------------------------
# AUC
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "AUC"
this.plotdat <- plotlist.fixedD$collect.power.AUC
delta <- round(unique(this.plotdat$delta), digits=2)
rho <- round(unique(this.plotdat$datagen.params.rho), digits=2)
legend.labs <- apply(cbind(rho, delta), 1, function(x){
  x <- paste("rho =",x[1],", ","delta =",x[2], sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=datagen.params.N, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(100,max(this.plotdat$datagen.params.N)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels = legend.labs) + scale_linetype_discrete(name=element_blank(), labels = legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("N")

gg.AUC.FIXEDDELTA <- gg

###############################################################################
# Fixed N, vary standardized effect size
###############################################################################

# -----------------------------------------------------------------------------
# end-of-study means
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "End-of-study means"
this.plotdat <- plotlist.fixedN$collect.power.eos.means
rho <- as.matrix(round(unique(this.plotdat$datagen.params.rho), digits=2))
legend.labs <- apply(rho, 1, function(x){
  x <- paste("rho =",x[1],sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=delta, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(0,max(this.plotdat$delta)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels=legend.labs) + scale_linetype_discrete(name=element_blank(), labels=legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("delta (N=301)")

gg.eos.means.FIXEDN <- gg

# -----------------------------------------------------------------------------
# change score
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "Delayed effect"
this.plotdat <- plotlist.fixedN$collect.power.change.score
rho <- as.matrix(round(unique(this.plotdat$datagen.params.rho), digits=2))
legend.labs <- apply(rho, 1, function(x){
  x <- paste("rho =",x[1],sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=delta, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(0,max(this.plotdat$delta)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels=legend.labs) + scale_linetype_discrete(name=element_blank(), labels=legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("delta (N=301)")

gg.change.score.FIXEDN <- gg

# -----------------------------------------------------------------------------
# AUC
# -----------------------------------------------------------------------------
# Get data
this.quantity <- "AUC"
this.plotdat <- plotlist.fixedN$collect.power.AUC
rho <- as.matrix(round(unique(this.plotdat$datagen.params.rho), digits=2))
legend.labs <- apply(rho, 1, function(x){
  x <- paste("rho =",x[1],sep="")
  return(x)
}
)

# Get plots
this.plotdat$datagen.params.rho <- as.factor(this.plotdat$datagen.params.rho)
gg <- ggplot(this.plotdat, aes(x=delta, y=power, shape=datagen.params.rho, linetype=datagen.params.rho))
gg <- gg + xlim(0,max(this.plotdat$delta)) + scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))
gg <- gg + geom_point() 
gg <- gg + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="interpolate")))
gg <- gg + scale_shape_discrete(name=element_blank(), labels=legend.labs) + scale_linetype_discrete(name=element_blank(), labels=legend.labs)
gg <- gg + theme(legend.position = "bottom")
gg <- gg + geom_hline(yintercept = 0.80)
gg <- gg + labs(title = this.quantity) + xlab("delta (N=301)")

gg.AUC.FIXEDN <- gg

###############################################################################
# Combine all plots into one grid
###############################################################################
plots.grid <- grid.arrange(gg.eos.means.FIXEDN, gg.eos.means.FIXEDDELTA, 
                           gg.change.score.FIXEDN, gg.change.score.FIXEDDELTA, 
                           gg.AUC.FIXEDN, gg.AUC.FIXEDDELTA,
                           ncol=2)

path.output_data <- Sys.getenv("path.output_data")
ggsave(file.path(path.output_data, use.working.corr, "powercurves.jpg"), 
       plot = plots.grid,
       width = 12,
       height = 18,
       units = "in")


