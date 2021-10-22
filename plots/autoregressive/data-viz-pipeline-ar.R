###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "sim_size_test"), recursive = TRUE)
source(file.path(path.plots, this.folder, "plot-sim-size-test.R"))
remove(list = ls())


###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "sim_vary_effect"), recursive = TRUE)
source(file.path(path.plots, this.folder, "plot-sim-vary-effect.R"))
remove(list = ls())


###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "sim_vary_eta"), recursive = TRUE)
source(file.path(path.plots, this.folder, "plot-sim-vary-eta.R"))
remove(list = ls())


###############################################################################
# Visualize simulation study results
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "sim_vary_n4"), recursive = TRUE)
source(file.path(path.plots, this.folder, "plot-sim-vary-n4.R"))
remove(list = ls())


###############################################################################
# Visualize correlation matrix
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "corrviz_sim_size_test"), recursive = TRUE)
source(file.path(path.plots, this.folder, "corrviz-sim-size-test.R"))
remove(list = ls())


###############################################################################
# Visualize correlation matrix
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "corrviz_sim_vary_effect"), recursive = TRUE)
source(file.path(path.plots, this.folder, "corrviz-sim-vary-effect.R"))
remove(list = ls())


###############################################################################
# Visualize correlation matrix
###############################################################################
path.plots <- Sys.getenv("path.plots")
this.folder <- "autoregressive"
dir.create(path = file.path(path.plots, this.folder, "corrviz_sim_vary_eta"), recursive = TRUE)
source(file.path(path.plots, this.folder, "corrviz-sim-vary-eta.R"))
remove(list = ls())


