###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "exchangeable"
dir.create(path = file.path(path.plots, this.folder, "sim_vary_effect"), recursive = TRUE)
source(file.path(path.plots, this.folder, "plot-sim-vary-effect.R"))
remove(list = ls())

###############################################################################
# Visualize correlation matrix
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "exchangeable"
dir.create(path = file.path(path.plots, this.folder, "corrviz_sim_vary_effect"), recursive = TRUE)
source(file.path(path.plots, this.folder, "corrviz-sim-vary-effect.R"))
remove(list = ls())

###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "exchangeable"
source(file.path(path.plots, this.folder, "plot-compare-power-across-corr.R"))
remove(list = ls())
